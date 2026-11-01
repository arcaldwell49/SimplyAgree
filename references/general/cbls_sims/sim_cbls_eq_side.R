# bls_reg() with equation error (see README.md, study 3).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_eq.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# t ~ N(20, 5^2); x = t + e_x (SD 0.6); y = 1 + 1.1 t + delta + e_y (SD 0.9),
# delta ~ N(0, sd_eq^2) per subject (equation error); replicates or known
# variances with one measurement each
grid <- expand.grid(n = c(20, 60), reps = c("known", "3"), side = c("y", "x", "both"),
                    sd_eq = 1.5, stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim, conf) {
  set.seed(9100 + i)
  n <- g$n[i]; reps <- g$reps[i]; sd_eq <- g$sd_eq[i]; side <- g$side[i]
  sx <- 0.6; sy <- 0.9; a <- 1; b <- 1.1
  # deviations of the true values from the line, adding sd_eq^2 to the
  # residual variance y - b x: all in y, all in x, or common tau (both)
  devs <- function(k) {
    switch(side,
           y = list(x = rep(0, k), y = rnorm(k, 0, sd_eq)),
           x = list(x = rnorm(k, 0, sd_eq / b), y = rep(0, k)),
           both = list(x = rnorm(k, 0, sd_eq / (sqrt(2) * b)),
                       y = b * rnorm(k, 0, sd_eq / (sqrt(2) * b))))
  }
  gen <- function() {
    t <- rnorm(n, 20, 5)
    dv <- devs(n)
    m <- if (reps == "known") 1 else as.numeric(reps)
    id <- rep(seq_len(n), each = m)
    data.frame(id = id, x = t[id] + dv$x[id] + rnorm(n * m, 0, sx),
               y = a + b * t[id] + dv$y[id] + rnorm(n * m, 0, sy))
  }
  fit_one <- function(dat, eq) {
    if (reps == "known") {
      bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2,
               equation_error = eq)
    } else {
      bls_reg(y ~ x, data = dat, id = "id", equation_error = eq)
    }
  }
  res <- matrix(NA, n_sim, 6,
                dimnames = list(NULL, c("slope", "pi_eq", "pi_noeq",
                                        "pi_md_eq", "pi_md_noeq",
                                        "disp_reject")))
  for (s in seq_len(n_sim)) {
    dat <- gen()
    f1 <- tryCatch(fit_one(dat, TRUE), error = function(e) NULL)
    if (is.null(f1)) next
    f0 <- fit_one(dat, FALSE)
    t0 <- rnorm(1, 20, 5)
    d0 <- devs(1)
    x0 <- t0 + d0$x + rnorm(1, 0, sx)
    y0 <- a + b * t0 + d0$y + rnorm(1, 0, sy)
    inside <- function(p, v) p$lwr <= v & v <= p$upr
    res[s, ] <- c(
      f1$model_table$lower.ci[2] <= b & b <= f1$model_table$upper.ci[2],
      inside(predict(f1, x0, interval = "prediction"), y0),
      inside(predict(f0, x0, interval = "prediction"), y0),
      inside(predict(f1, (x0 + y0) / 2, type = "md", interval = "prediction"),
             x0 - y0),
      inside(predict(f0, (x0 + y0) / 2, type = "md", interval = "prediction"),
             x0 - y0),
      f1$dispersion$p.value < 1 - conf)
  }
  data.frame(n = n, reps = reps, side = side, sd_eq = sd_eq, n_sim = n_sim,
             t(colMeans(res, na.rm = TRUE)))
}

out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell, g = grid,
                                n_sim = N_SIM, conf = conf))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary_eq_side.csv"), row.names = FALSE)
print(out, digits = 3)
