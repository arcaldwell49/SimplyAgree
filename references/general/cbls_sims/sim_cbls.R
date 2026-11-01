# Coverage of bls_reg() intervals and size of its joint test (see README.md).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "2000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# Design:
#   true values t ~ N(20, 5^2); x = t + e_x, y = a + b t + e_y
#   sd_x = 1; sd_y = 1 / sqrt(ratio), ratio = var_x / var_y in {0.5, 2}
#   reps: "known" (one measurement each, var_x and var_y given),
#         "2", "3" (balanced replicates), "1-4" (unbalanced)
#   line a = 1, b = 1.1 for coverage; a = 0, b = 1 for the joint test size
grid <- expand.grid(n = c(10, 20, 60), reps = c("known", "2", "3", "1-4"),
                    ratio = c(0.5, 2), stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim, conf) {
  set.seed(4000 + i)
  n <- g$n[i]; reps <- g$reps[i]; ratio <- g$ratio[i]
  sx <- 1; sy <- 1 / sqrt(ratio)
  x_at <- c(12, 20, 28)
  gen <- function(a, b) {
    t <- rnorm(n, 20, 5)
    m_i <- switch(reps, known = rep(1, n), "2" = rep(2, n), "3" = rep(3, n),
                  "1-4" = sample(1:4, n, replace = TRUE))
    id <- rep(seq_len(n), m_i)
    data.frame(id = id, x = t[id] + rnorm(length(id), 0, sx),
               y = a + b * t[id] + rnorm(length(id), 0, sy))
  }
  fit_one <- function(dat) {
    if (reps == "known") {
      bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2,
               conf.level = conf, pred_level = conf)
    } else {
      bls_reg(y ~ x, data = dat, id = "id", conf.level = conf,
               pred_level = conf)
    }
  }
  a <- 1; b <- 1.1
  # the CBLS line (D = x - y on M) implied by the true BLS line
  g0 <- -2 * a / (1 + b); g1 <- 2 * (1 - b) / (1 + b)
  res <- matrix(NA, n_sim, 10,
                dimnames = list(NULL, c("int", "slope", "line12", "line20",
                                        "line28", "pi_xy", "pi_md",
                                        "md_slope", "joint_size",
                                        "converged")))
  for (s in seq_len(n_sim)) {
    f <- tryCatch(suppressWarnings(fit_one(gen(a, b))), error = function(e) NULL)
    if (is.null(f)) next
    mt <- f$model_table
    ci_line <- predict(f, x_at, interval = "confidence")
    truth_line <- a + b * x_at
    # a new subject measured once by each method
    t0 <- rnorm(1, 20, 5)
    x0 <- t0 + rnorm(1, 0, sx); y0 <- a + b * t0 + rnorm(1, 0, sy)
    pxy <- predict(f, x0, interval = "prediction")
    pmd <- predict(f, (x0 + y0) / 2, type = "md", interval = "prediction")
    cb <- f$cbls$model_table
    # joint test size under the identity line
    f0 <- tryCatch(suppressWarnings(fit_one(gen(0, 1))), error = function(e) NULL)
    jt <- if (is.null(f0)) NA else joint_test(f0)$p.value < 1 - conf
    res[s, ] <- c(mt$lower.ci[1] <= a & a <= mt$upper.ci[1],
                  mt$lower.ci[2] <= b & b <= mt$upper.ci[2],
                  ci_line$lwr <= truth_line & truth_line <= ci_line$upr,
                  pxy$lwr <= y0 & y0 <= pxy$upr,
                  pmd$lwr <= x0 - y0 & x0 - y0 <= pmd$upr,
                  cb$lower.ci[2] <= g1 & g1 <= cb$upper.ci[2],
                  jt, f$converged)
  }
  data.frame(n = n, reps = reps, ratio = ratio, n_sim = n_sim,
             n_failed = sum(is.na(res[, "int"])),
             t(colMeans(res, na.rm = TRUE)))
}

out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell, g = grid,
                                n_sim = N_SIM, conf = conf))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary.csv"), row.names = FALSE)
print(out, digits = 3)
