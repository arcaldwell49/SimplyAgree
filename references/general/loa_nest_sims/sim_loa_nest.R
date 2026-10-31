# Coverage of the LoA confidence bounds of agreement_limit(data_type = "nest"):
# the current decomposition against Zou's (see README.md).
# Run from the package root: Rscript references/general/loa_nest_sims/sim_loa_nest.R
library(parallel)

out_dir <- "references/general/loa_nest_sims"
N_SIM <- as.numeric(Sys.getenv("NEST_NSIM", "2000"))

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# Bounds from the variance pieces of the fitted random-intercept model.
# "current": as in calc_loa_nest(); "zou": the variance of the subject means
# (s_b^2 + s_w^2 / m_h) on n - 1 df, as in agree_nest() and Zou (2011).
nest_bounds <- function(dat, agree = 0.95, conf = 0.95) {
  m1 <- suppressMessages(lme4::lmer(delta ~ 1 + (1 | id), data = dat,
                                    REML = TRUE))
  vc <- as.data.frame(lme4::VarCorr(m1))$vcov
  sb2 <- vc[1]; sw2 <- vc[2]; tot <- sb2 + sw2
  m_i <- as.vector(table(dat$id)); n <- length(m_i); N <- sum(m_i)
  mh <- n / sum(1 / m_i)
  bias <- unname(lme4::fixef(m1)[1])
  za <- qnorm(1 - (1 - agree) / 2); zc <- qnorm(conf)
  au <- 1 - conf
  one <- function(v1, se2) {
    v2 <- (1 - 1 / mh) * sw2
    # MOVER
    u <- tot + sqrt((v1 * ((n - 1) / qchisq(au, n - 1) - 1))^2 +
                      (v2 * ((N - n) / qchisq(au, N - n) - 1))^2)
    lme_m <- sqrt(zc^2 * se2 + za^2 * (sqrt(u) - sqrt(tot))^2)
    # Bland-Altman
    s2 <- se2 + za^2 / (2 * tot) * (v1^2 / (n - 1) + v2^2 / (N - n))
    lme_b <- zc * sqrt(s2)
    c(mover_l = bias - za * sqrt(tot) - lme_m,
      mover_u = bias + za * sqrt(tot) + lme_m,
      ba_l = bias - za * sqrt(tot) - lme_b,
      ba_u = bias + za * sqrt(tot) + lme_b)
  }
  v1_zou <- sb2 + sw2 / mh
  list(current = one(sb2, sb2 / n), zou = one(v1_zou, v1_zou / n))
}

grid <- expand.grid(n = c(10, 20, 40), m = c("2", "5", "1-5"),
                    ratio = c(0.1, 1, 10), stringsAsFactors = FALSE)
one_cell <- function(i, g, n_sim) {
  set.seed(9000 + i)
  n <- g$n[i]; ratio <- g$ratio[i]
  sb <- sqrt(ratio / (1 + ratio)); sw <- sqrt(1 / (1 + ratio))
  za <- qnorm(0.975)
  true_l <- -za; true_u <- za   # bias 0, SD of a single difference 1
  res <- matrix(NA, n_sim, 8)
  for (s in seq_len(n_sim)) {
    m_i <- switch(g$m[i], "2" = rep(2, n), "5" = rep(5, n),
                  "1-5" = sample(1:5, n, replace = TRUE))
    id <- rep(seq_len(n), m_i)
    dat <- data.frame(id = id,
                      delta = rnorm(n, 0, sb)[id] + rnorm(length(id), 0, sw))
    b <- nest_bounds(dat)
    res[s, ] <- c(b$current[c("mover_l", "ba_l")] <= true_l,
                  b$current[c("mover_u", "ba_u")] >= true_u,
                  b$zou[c("mover_l", "ba_l")] <= true_l,
                  b$zou[c("mover_u", "ba_u")] >= true_u)
  }
  cm <- colMeans(res)
  data.frame(n = n, m = g$m[i], ratio = ratio, n_sim = n_sim,
             cur_mover_l = cm[1], cur_ba_l = cm[2],
             cur_mover_u = cm[3], cur_ba_u = cm[4],
             zou_mover_l = cm[5], zou_ba_l = cm[6],
             zou_mover_u = cm[7], zou_ba_u = cm[8])
}
clusterExport(cl, "nest_bounds")
out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell,
                                g = grid, n_sim = N_SIM))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary.csv"), row.names = FALSE)
print(out, digits = 3)
