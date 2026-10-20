# One data set -> all statistics, for the drivers sim_type1.R and
# sim_power.R. Every fit goes through the exported package functions
# (dem_reg(), pb_reg(), joint_test()), so the study tests what users run.

worker_init <- function(dir) {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
  source(file.path(dir, "gen.R"))
  source(file.path(dir, "sim_funs.R"))
  invisible(TRUE)
}

# Statistics from one simple_eiv fit, H0: intercept = h0[1], slope = h0[2].
# Columns: estimates, vcov entries, marginal CI limits, joint p-values with
# the F(2, n-2) and chi-squared(2) references.
fit_stats <- function(fit, h0 = c(0, 1)) {
  mt <- fit$model_table
  v <- fit$vcov
  # The Wald test from the fit's vcov. joint_test() defaults to Sadler for
  # Deming and refuses Dufey fits, so the Wald statistic is computed here
  # (identical to joint_test(method = "wald"), checked in check_fidelity.R).
  jp <- function(m) tryCatch({
    if (is.null(v) || any(!is.finite(v))) stop("no vcov")
    d <- h0 - unname(fit$coefficients)
    md <- as.numeric(t(d) %*% solve(v) %*% d)
    if (m == "F") pf(md / 2, 2, fit$df.residual, lower.tail = FALSE)
    else pchisq(md, 2, lower.tail = FALSE)
  }, error = function(e) NA_real_)
  c(b0 = unname(fit$coefficients[1]), b1 = unname(fit$coefficients[2]),
    v00 = if (is.null(v)) NA else v[1, 1],
    v11 = if (is.null(v)) NA else v[2, 2],
    v01 = if (is.null(v)) NA else v[1, 2],
    lo0 = mt$lower.ci[1], hi0 = mt$upper.ci[1],
    lo1 = mt$lower.ci[2], hi1 = mt$upper.ci[2],
    pF = jp("F"), pC = jp("asymptotic"))
}

stat_names <- c("b0", "b1", "v00", "v11", "v01", "lo0", "hi0", "lo1", "hi1",
                "pF", "pC")
na_stats <- setNames(rep(NA_real_, length(stat_names)), stat_names)

safe_stats <- function(expr, h0 = c(0, 1)) {
  fit <- tryCatch(suppressMessages(suppressWarnings(expr)),
                  error = function(e) NULL)
  if (is.null(fit)) c(na_stats, fail = 1) else c(fit_stats(fit, h0), fail = 0)
}

with_prefix <- function(x, p) setNames(x, paste0(p, "_", names(x)))

# Generalised Bradley-Blackwood joint test of H0: intercept b0, slope b1,
# with error ratio lambda = var(e_x) / var(e_y). d = y - b0 - b1 x and
# s = x + lambda b1 y have uncorrelated (so, normal, independent) error parts;
# under H0 d has mean 0 whatever the true values, so the OLS F test of
# d ~ s (intercept and slope both 0) is exactly F(2, n - 2) for constant
# normal errors and known lambda. Weights make it approximate.
bb_p <- function(x, y, lambda, b0 = 0, b1 = 1, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  d <- y - b0 - b1 * x
  s <- x + lambda * b1 * y
  rss1 <- sum(w * stats::lm.wfit(cbind(1, s), d, w)$residuals^2)
  rss0 <- sum(w * d^2)
  n <- length(x)
  pf(((rss0 - rss1) / 2) / (rss1 / (n - 2)), 2, n - 2, lower.tail = FALSE)
}

# Sadler (2010, Eq. 11; Draper & Smith) sum-of-squares joint test: no vcov.
# S(a, b) = sum W_i(b) (y - a - b x)^2 with Deming weights
# W_i(b) = w_i / (1 + lambda b^2) (i.e. 1 / (var_y + b^2 var_x), up to a
# constant that cancels), S-hat its minimum (the Deming fit for fixed w),
# F = ((S(b0, b1) - S-hat) / 2) / (S-hat / (n - 2)) ~ F(2, n - 2).
ss_S <- function(x, y, lambda, a, b, w) sum(w * (y - a - b * x)^2) / (1 + lambda * b^2)
ss_p <- function(x, y, lambda, b0 = 0, b1 = 1, w = NULL) {
  if (is.null(w)) w <- rep(1, length(x))
  fit <- calc_dem(x, y, w, lambda)
  s_hat <- ss_S(x, y, lambda, fit$b0, fit$b1, w)
  s0 <- ss_S(x, y, lambda, b0, b1, w)
  n <- length(x)
  pf(((s0 - s_hat) / 2) / (s_hat / (n - 2)), 2, n - 2, lower.tail = FALSE)
}

# Weights dem_reg(weighted = TRUE) uses
dem_weights <- function(x, y, lambda) 1 / ((x + lambda * y) / (1 + lambda))^2

# ---- Part A ----------------------------------------------------------------

one_dem <- function(cell, seed) {
  set.seed(seed)
  d <- gen_h0(cell$n, c(cell$lo, cell$hi), cell$lambda, cell$err, cell$var_type)
  w <- if (cell$var_type == "prop") dem_weights(d$x, d$y, cell$lambda)
  c(seed = seed,
    safe_stats(dem_reg(y ~ x, data = d, error.ratio = cell$lambda,
                       weighted = cell$var_type == "prop")),
    pBB = bb_p(d$x, d$y, cell$lambda, w = w),
    pSS = ss_p(d$x, d$y, cell$lambda, w = w))
}

one_reps <- function(cell, seed) {
  set.seed(seed)
  d <- gen_h0_reps(cell$n, c(cell$lo, cell$hi), cell$lambda, cell$err)
  fit <- tryCatch(suppressWarnings(dem_reg(y ~ x, data = d, id = "id")),
                  error = function(e) NULL)
  if (is.null(fit)) return(c(seed = seed, na_stats, fail = 1, er_hat = NA,
                             pBB = NA, pSS = NA))
  # BB and SS on the subject means with the estimated error ratio
  # (approximate)
  c(seed = seed, fit_stats(fit), fail = 0, er_hat = fit$error.ratio,
    pBB = bb_p(fit$model$x, fit$model$y, fit$error.ratio),
    pSS = ss_p(fit$model$x, fit$model$y, fit$error.ratio))
}

# Passing-Bablok: analytic (marginal CIs only), bootstrap, jackknife and
# (lambda = 1, scissors) Dufey on the same data set.
one_pb <- function(cell, seed) {
  set.seed(seed)
  d <- gen_h0(cell$n, c(cell$lo, cell$hi), cell$lambda, cell$err)
  pb <- function(...) pb_reg(y ~ x, data = d, method = cell$method,
                             error.ratio = cell$lambda, keep_data = FALSE, ...)
  out <- c(seed = seed, with_prefix(safe_stats(pb(replicates = 0)), "an"),
           with_prefix(safe_stats(pb(replicates = cell$B,
                                     se_method = "bootstrap")), "bs"))
  if (cell$method == "scissors") {
    out <- c(out, with_prefix(safe_stats(pb(replicates = cell$B,
                                            se_method = "jackknife")), "jk"))
    if (cell$lambda == 1) {
      out <- c(out, with_prefix(safe_stats(pb(se_method = "dufey")), "du"))
    }
  }
  out
}

run_chunk <- function(task, fun) {
  f <- get(fun)
  do.call(rbind, lapply(task$seeds, function(s) f(task$cell, s)))
}

# ---- Part B ----------------------------------------------------------------

sc_args <- function(sc) sc[setdiff(names(sc), "label")]

# Truth: what a user gets fitting dem_reg() with the error ratio
# deming_power_sim() would use, then joint_test() (default F, and
# chi-squared) and the marginal CIs, H0: intercept 0, slope 1.
one_truth <- function(task, seed) {
  sc <- task$sc
  set.seed(seed)
  d <- gen_power(task$n, sc)
  er <- power_ratio(d, sc)
  s <- safe_stats(dem_reg(y ~ x, data = d, error.ratio = er,
                          weighted = sc$weighted, keep_data = FALSE))
  w <- if (sc$weighted) dem_weights(d$x, d$y, er)
  c(seed = seed,
    jF = unname(s["pF"] < 0.05), jC = unname(s["pC"] < 0.05),
    jBB = bb_p(d$x, d$y, er, w = w) < 0.05,
    jSS = ss_p(d$x, d$y, er, w = w) < 0.05,
    either = unname(s["lo0"] > 0 | s["hi0"] < 0 | s["lo1"] > 1 | s["hi1"] < 1),
    fail = unname(s["fail"]))
}

run_truth_chunk <- function(task) {
  do.call(rbind, lapply(task$seeds, function(s) one_truth(task, s)))
}

one_powersim <- function(task) {
  set.seed(task$seed)
  r <- do.call(deming_power_sim,
               c(list(n_sims = task$n_sims, sample_size = task$n),
                 sc_args(task$sc)))
  c(seed = task$seed, n = task$n, joint = r$power_joint,
    either = r$power_either_ci, slope = r$power_ci_slope,
    intercept = r$power_ci_intercept)
}

one_samplesize <- function(task) {
  set.seed(task$seed)
  utils::capture.output(r <- suppressWarnings(
    do.call(deming_sample_size, c(list(target_power = task$target),
                                  sc_args(task$sc)))))
  c(seed = task$seed, n_ci = r$n_required_ci, n_joint = r$n_required_joint)
}
