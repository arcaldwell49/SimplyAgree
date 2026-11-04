# bls_reg() standard errors with equation error: does leaving the
# uncertainty in the estimated tau^2 out of the delta-method SEs make the
# CIs too narrow? Study 6 (sim_cbls_se.R) with common-tau equation error
# added (see README.md, study 7). Written 2026-11-03.
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_tau.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# As study 6, plus common-tau equation error (the model bls_reg() fits):
# x = t + c_x + e_x (SD 1), y = 1 + 1.1 (t + c_y) + e_y (SD 1.2), c_x, c_y
# ~ N(0, tau^2) per subject, independent; t ~ N(20, t_sd^2).
# - "known": one measurement each, measurement error variances given;
# - "reps": 3 exchangeable replicates of each method;
# - "linked": 3 occasions, occasion effect (SD 2) shared by both methods.
grid <- expand.grid(n = c(15, 40), t_sd = c(1.5, 3, 6), tau = c(0, 0.5, 1.5),
                    design = c("known", "reps", "linked"),
                    stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim) {
  set.seed(9800 + i)
  n <- g$n[i]; t_sd <- g$t_sd[i]; tau <- g$tau[i]; design <- g$design[i]
  sx <- 1; sy <- 1.2; a <- 1; b <- 1.1
  m <- if (design == "known") 1 else 3
  om <- if (design == "linked") 2 else 0
  gen <- function() {
    t <- rnorm(n, 20, t_sd)
    cx <- rnorm(n, 0, tau); cy <- rnorm(n, 0, tau)
    id <- rep(seq_len(n), each = m)
    occ <- rnorm(n * m, 0, om)
    data.frame(id = id, x = t[id] + cx[id] + occ + rnorm(n * m, 0, sx),
               y = a + b * (t[id] + cy[id] + occ) + rnorm(n * m, 0, sy))
  }
  fit <- function(dat) {
    switch(design,
           known = bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2),
           reps = bls_reg(y ~ x, data = dat, id = "id"),
           linked = bls_reg(y ~ x, data = dat, id = "id",
                            replicates = "linked"))
  }
  g1 <- 2 * (1 - b) / (1 + b)
  res <- matrix(NA, n_sim, 8,
                dimnames = list(NULL, c("slope_cov", "int_cov", "cbls_cov",
                                        "slope", "slope_se", "tau2",
                                        "cbls_slope", "converged")))
  for (s in seq_len(n_sim)) {
    f <- tryCatch(suppressWarnings(fit(gen())), error = function(e) NULL)
    if (is.null(f)) next
    mt <- f$model_table
    cb <- f$cbls$model_table
    res[s, ] <- c(mt$lower.ci[2] <= b & b <= mt$upper.ci[2],
                  mt$lower.ci[1] <= a & a <= mt$upper.ci[1],
                  cb$lower.ci[2] <= g1 & g1 <= cb$upper.ci[2],
                  mt$coef[2], mt$se[2], f$dispersion$tau2, cb$coef[2],
                  f$converged)
  }
  r <- res[!is.na(res[, 1]), , drop = FALSE]
  # sd_se: the actual SD of the slope over the root mean squared SE (> 1
  # means the SEs are too small)
  data.frame(g[i, ], n_ok = nrow(r),
             slope_cov = mean(r[, "slope_cov"]),
             int_cov = mean(r[, "int_cov"]),
             cbls_cov = mean(r[, "cbls_cov"]),
             slope_mean = mean(r[, "slope"]),
             sd_se = stats::sd(r[, "slope"]) / sqrt(mean(r[, "slope_se"]^2)),
             tau2_mean = mean(r[, "tau2"]),
             tau2_zero = mean(r[, "tau2"] == 0),
             converged = mean(r[, "converged"]))
}
clusterExport(cl, "one_cell")

out <- do.call(rbind, parLapplyLB(cl, seq_len(nrow(grid)), one_cell,
                                  g = grid, n_sim = N_SIM))
write.csv(out, file.path(out_dir, "summary_tau.csv"), row.names = FALSE)
print(out, digits = 3)
