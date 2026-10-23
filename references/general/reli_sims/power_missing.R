# Cell-level missingness in power_icc()/precision_icc(): does an explicitly
# approximate method (the exact scaled-F calculation with observed-data
# quantities: N = n k (1 - p), kbar = N / n, error df N - n - k + 1 (two-way)
# or N - n (one-way)) predict what icc_test(method = "reml") actually does?
# Usage: Rscript power_missing.R <progress_log_file>
# (writes power_missing_partial.rds next to the log). Run from the package
# root. Written 2026-10-18 on branch reli-update; results in
# references/general/reli_redesign.md (Section 9).

library(parallel)
suppressMessages(devtools::load_all(".", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
R <- 1000; alpha <- 0.05; rho0 <- 0.6; v_it <- 0.09
if (nzchar(Sys.getenv("SIM_QUICK"))) R <- 10
cells <- expand.grid(model = c("twoway", "oneway"), n = c(15, 40), k = c(3, 5),
                     miss = c("none", "MCAR15", "MCAR30", "MAR30"),
                     stringsAsFactors = FALSE)

# Approximate method -------------------------------------------------------------
approx_quantities <- function(n, k, p, model) {
  N <- n * k * (1 - p)
  kbar <- N / n
  df2 <- if (model == "twoway") N - n - k + 1 else N - n
  list(kbar = kbar, df1 = n - 1, df2 = df2)
}
f_scale <- function(rho, k) (1 + (k - 1) * rho) / (1 - rho)
approx_power <- function(n, k, p, rho1, model) {
  q <- approx_quantities(n, k, p, model)
  r <- f_scale(rho0, q$kbar) / f_scale(rho1, q$kbar)
  pf(qf(1 - alpha, q$df1, q$df2) * r, q$df1, q$df2, lower.tail = FALSE)
}
approx_median_width <- function(n, k, p, rho1, model) {
  q <- approx_quantities(n, k, p, model)
  a <- qf(0.975, q$df1, q$df2)
  b <- qf(0.975, q$df2, q$df1)
  g <- function(x) (x - 1) / (x + q$kbar - 1)
  u <- seq(1e-5, 1 - 1e-5, length.out = 20001)
  Fr <- f_scale(rho1, q$kbar) * qf(u, q$df1, q$df2)
  median(g(Fr * b) - g(Fr / a))
}

# Simulation of icc_test()'s REML analysis ---------------------------------------------
one_rep <- function(seed, model, n, k, miss, rho1) {
  set.seed(seed)
  p <- switch(miss, none = 0, MCAR15 = 0.15, MCAR30 = 0.30, MAR30 = 0.30)
  mech <- if (miss == "MAR30") "MAR" else "MCAR"
  sim_once <- function(rho) {
    v_e <- (1 - rho) / rho                    # sigma2_id = 1
    if (model == "twoway") {
      Y <- outer(rnorm(n), rep(1, k)) + outer(rep(1, n), rnorm(k, 0, sqrt(v_it))) +
        matrix(rnorm(n * k, 0, sqrt(v_e)), n)
    } else {
      # one-way: within-subject variance (rater + error) = v_e
      Y <- outer(rnorm(n), rep(1, k)) + matrix(rnorm(n * k, 0, sqrt(v_e)), n)
    }
    if (p > 0) {
      M <- matrix(FALSE, n, k)
      pr <- if (mech == "MCAR") p * k / (k - 1) else
        plogis(qlogis(p * k / (k - 1)) + 1.5 * as.vector(scale(Y[, 1])))
      M[, -1] <- runif(n * (k - 1)) < pr
      Y[M] <- NA
    }
    prep <- .reli_prep_from_long(data.frame(values = as.vector(Y),
                                            items = rep(1:k, each = n),
                                            id = rep(1:n, k)))
    method <- if (prep$balanced) "anova" else "reml"
    f <- suppressWarnings(suppressMessages(.reli_fit(prep, method, model,
                                                    keep_fit = FALSE)))
    form <- if (model == "twoway") "C1" else "1"
    c(reject = .icc_ftest(f, form, rho0, "greater")$p.value < alpha,
      width = diff(as.numeric(.icc_ci(f, form, 0.95, "two.sided"))),
      p_obs = prep$n_missing / (n * k))
  }
  h1 <- sim_once(rho1)
  h0 <- sim_once(rho0)
  c(power = h1[["reject"]], width = h1[["width"]], size = h0[["reject"]],
    p_obs = h1[["p_obs"]])
}

clusterExport(cl, c("one_rep", "v_it", "rho0", "alpha"))
logp("start power_missing:", nrow(cells), "cells,", R, "reps")
rows <- list()
for (i in seq_len(nrow(cells))) {
  c0 <- cells[i, ]
  # rho1 giving power 0.60 with complete data
  rho1 <- power_icc(n = c0$n, k = c0$k, rho0 = rho0, power = 0.6,
                    alternative = "greater", model = c0$model)$rho1
  sims <- parLapply(cl, seq_len(R) + 10000 * i,
                    function(s, model, n, k, miss, rho1)
                      tryCatch(one_rep(s, model, n, k, miss, rho1),
                               error = function(e) rep(NA_real_, 4)),
                    model = c0$model, n = c0$n, k = c0$k, miss = c0$miss,
                    rho1 = rho1)
  sims <- do.call(rbind, sims)
  ok <- stats::complete.cases(sims)
  p_nom <- switch(c0$miss, none = 0, MCAR15 = 0.15, MCAR30 = 0.30, MAR30 = 0.30)
  rows[[i]] <- data.frame(
    c0, rho1 = rho1, reps = sum(ok),
    p_obs = mean(sims[ok, "p_obs"]),
    sim_power = mean(sims[ok, "power"]),
    approx_power = approx_power(c0$n, c0$k, p_nom, rho1, c0$model),
    # the MAR mechanism removes a little more than its nominal proportion
    approx_power_pobs = approx_power(c0$n, c0$k, mean(sims[ok, "p_obs"]), rho1,
                                     c0$model),
    sim_size = mean(sims[ok, "size"]),
    sim_med_width = median(sims[ok, "width"]),
    approx_med_width = approx_median_width(c0$n, c0$k, p_nom, rho1, c0$model))
  logp("cell", i, "of", nrow(cells), "done:", paste(c0, collapse = " "))
  saveRDS(rows, file.path(dirname(log_file), "power_missing_partial.rds"))
}
stopCluster(cl)
tab <- do.call(rbind, rows)
tab$power_diff <- tab$approx_power - tab$sim_power
tab$width_ratio <- tab$approx_med_width / tab$sim_med_width
saveRDS(tab, file.path(dirname(log_file), "power_missing.rds"))
print(format(tab[, c("model", "n", "k", "miss", "p_obs", "sim_power", "approx_power",
                     "approx_power_pobs",
                     "power_diff", "sim_size", "sim_med_width", "approx_med_width",
                     "width_ratio")], digits = 3), row.names = FALSE)
cat("\nMC SE: power ~", round(sqrt(.6 * .4 / R), 3), "; size ~",
    round(sqrt(.05 * .95 / R), 3), "\n")
logp("finished")
