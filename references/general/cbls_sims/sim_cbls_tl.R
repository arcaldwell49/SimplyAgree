# Coverage of the experimental tolerance limits of bls_reg() (README.md,
# study 4).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_tl.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# t ~ N(20, 5^2); x = t + c_x + e_x (SD 0.6), y = 1 + 1.1 (t + c_y) + e_y
# (SD 0.9); c_x, c_y ~ N(0, tau^2) (common-tau equation error, tau = 0 or
# 0.7). Known variances (one measurement each) or 2-3 replicates. For a new
# pair at x0, y0 - (a + b x0) ~ N(0, sigma^2), sigma^2 = 0.81 + b^2 0.36 +
# 2 b^2 tau^2, so the content of limits [L, U] at x0 is exact.
grid <- expand.grid(n = c(10, 20, 60), reps = c("known", "2", "3"),
                    tau = c(0, 0.7), stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim) {
  set.seed(9500 + i)
  n <- g$n[i]; reps <- g$reps[i]; tau <- g$tau[i]
  sx <- 0.6; sy <- 0.9; a <- 1; b <- 1.1
  p <- 0.95; conf <- 0.95
  x0 <- c(12, 20, 28)
  sig <- sqrt(sy^2 + b^2 * sx^2 + 2 * b^2 * tau^2)
  z <- qnorm((1 + p) / 2)
  gen <- function() {
    t <- rnorm(n, 20, 5)
    cx <- rnorm(n, 0, tau); cy <- rnorm(n, 0, tau)
    m <- if (reps == "known") 1 else as.numeric(reps)
    id <- rep(seq_len(n), each = m)
    data.frame(id = id, x = t[id] + cx[id] + rnorm(n * m, 0, sx),
               y = a + b * (t[id] + cy[id]) + rnorm(n * m, 0, sy))
  }
  fit_one <- function(dat) {
    if (reps == "known") {
      bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2,
               pred_level = p, tol_level = conf)
    } else {
      bls_reg(y ~ x, data = dat, id = "id", pred_level = p,
               tol_level = conf)
    }
  }
  content <- function(L, U) {
    mu <- a + b * x0
    pnorm((U - mu) / sig) - pnorm((L - mu) / sig)
  }
  res <- matrix(NA, n_sim, 5,
                dimnames = list(NULL, c("joint_conf", "iu_upper", "iu_lower",
                                        "pi_content", "joint_content")))
  for (s in seq_len(n_sim)) {
    f <- tryCatch(suppressWarnings(fit_one(gen())), error = function(e) NULL)
    if (is.null(f)) next
    tj <- predict(f, x0, interval = "tolerance")
    ti <- predict(f, x0, interval = "tolerance", bound_type = "iu")
    pi <- predict(f, x0, interval = "prediction")
    mu <- a + b * x0
    res[s, ] <- c(mean(content(tj$lwr, tj$upr) >= p),
                  mean(ti$upr >= mu + z * sig),
                  mean(ti$lwr <= mu - z * sig),
                  mean(content(pi$lwr, pi$upr)),
                  mean(content(tj$lwr, tj$upr)))
  }
  data.frame(n = n, reps = reps, tau = tau, n_sim = n_sim,
             n_failed = sum(is.na(res[, 1])),
             t(colMeans(res, na.rm = TRUE)))
}

out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell, g = grid,
                                n_sim = N_SIM))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary_tl.csv"), row.names = FALSE)
print(out, digits = 3)
