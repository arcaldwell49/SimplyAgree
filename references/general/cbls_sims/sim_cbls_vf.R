# Coverage of cbls_reg() with error variances that change with the level
# (see README.md, study 2).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_vf.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# True values t ~ U(10, 100); x = t (1 + e_x), y = (a + b t)(1 + e_y):
# proportional error, CV 5% (x) and 8% (y).
# fits: "known" (one measurement each, the true variance functions given),
#       "proportional", "power", "constant" (from replicates)
grid <- expand.grid(n = c(20, 60), reps = c("2", "3", "1-4"),
                    fit = c("known", "proportional", "power", "constant"),
                    stringsAsFactors = FALSE)
grid <- grid[!(grid$fit == "known" & grid$reps != "2"), ]

one_cell <- function(i, g, n_sim, conf) {
  set.seed(7000 + i)
  n <- g$n[i]; reps <- g$reps[i]; fit_type <- g$fit[i]
  cv_x <- 0.05; cv_y <- 0.08
  vfx <- function(l) (cv_x * l)^2
  vfy <- function(l) (cv_y * l)^2
  gen <- function(a, b) {
    t <- runif(n, 10, 100)
    m_i <- if (fit_type == "known") rep(1, n) else
      switch(reps, "2" = rep(2, n), "3" = rep(3, n),
             "1-4" = sample(1:4, n, replace = TRUE))
    id <- rep(seq_len(n), m_i)
    data.frame(id = id, x = t[id] * (1 + rnorm(length(id), 0, cv_x)),
               y = (a + b * t[id]) * (1 + rnorm(length(id), 0, cv_y)))
  }
  fit_one <- function(dat) {
    if (fit_type == "known") {
      cbls_reg(y ~ x, data = dat, var_x = vfx, var_y = vfy,
               conf.level = conf, pred_level = conf)
    } else {
      cbls_reg(y ~ x, data = dat, id = "id", var_model = fit_type,
               conf.level = conf, pred_level = conf)
    }
  }
  a <- 2; b <- 1.05
  res <- matrix(NA, n_sim, 8,
                dimnames = list(NULL, c("int", "slope", "line50", "pi_low",
                                        "pi_high", "pi_md", "joint_size",
                                        "converged")))
  for (s in seq_len(n_sim)) {
    f <- tryCatch(suppressMessages(suppressWarnings(fit_one(gen(a, b)))),
                  error = function(e) NULL)
    if (is.null(f)) next
    mt <- f$model_table
    cl50 <- predict(f, 50, interval = "confidence")
    # new subjects at a low and a high level, measured once by each method
    pi_at <- function(t0) {
      x0 <- t0 * (1 + rnorm(1, 0, cv_x))
      y0 <- (a + b * t0) * (1 + rnorm(1, 0, cv_y))
      p <- predict(f, x0, interval = "prediction")
      pm <- predict(f, (x0 + y0) / 2, type = "md", interval = "prediction")
      c(p$lwr <= y0 & y0 <= p$upr, pm$lwr <= x0 - y0 & x0 - y0 <= pm$upr)
    }
    lo <- pi_at(runif(1, 10, 30))
    hi <- pi_at(runif(1, 80, 100))
    f0 <- tryCatch(suppressMessages(suppressWarnings(fit_one(gen(0, 1)))),
                   error = function(e) NULL)
    jt <- if (is.null(f0)) NA else joint_test(f0)$p.value < 1 - conf
    res[s, ] <- c(mt$lower.ci[1] <= a & a <= mt$upper.ci[1],
                  mt$lower.ci[2] <= b & b <= mt$upper.ci[2],
                  cl50$lwr <= a + 50 * b & a + 50 * b <= cl50$upr,
                  lo[1], hi[1], mean(c(lo[2], hi[2])), jt, f$converged)
  }
  data.frame(n = n, reps = if (fit_type == "known") "1" else reps,
             fit = fit_type, n_sim = n_sim,
             n_failed = sum(is.na(res[, "int"])),
             t(colMeans(res, na.rm = TRUE)))
}

out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell, g = grid,
                                n_sim = N_SIM, conf = conf))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary_vf.csv"), row.names = FALSE)
print(out, digits = 3)
