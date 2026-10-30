# Coverage simulation: lavaan (full ML) fit of the scaled model ----
# Phase 1 of HANDOFF_method_convert_scaled_ci.md. Wald CIs for the conversion
# intercepts and slopes of every ordered pair from the structural scaled
# model fit by lavaan (lavaan_scaled_helpers.R), as an alternative to the
# alternating regression delta-method CIs (53-73% coverage,
# method_convert_coverage_summary.rds).
#
# Same true values as sim_method_convert_coverage.R: alpha = (0, 1, -0.5),
# beta = (1, 1.2, 0.9), sigma = (0.3, 0.4, 0.5), tau = omega = 0.3, R = 3,
# true values with mean 10 and SD 2. Cells: I in {15, 30, 60} x M in {2, 3}
# x linked / exchangeable x scenario:
#   normal   - balanced, normal true values
#   missing  - p_missing = 0.15, normal true values
#   uniform  - balanced, uniform true values (mean 10, SD 2)
#   gamma    - balanced, right-skewed gamma(shape 2) true values (mean 10,
#              SD 2, skewness 1.41)
# Each data set is fit with estimator = "ML" and "MLR". Nominal level 95%.
# Run from the package root:
#   Rscript data-raw/sims/sim_method_convert_lavaan.R
# Results: data-raw/sims/method_convert_lavaan_coverage.rds and
#          data-raw/sims/method_convert_lavaan_coverage_summary.rds

library(parallel)

n_sim = as.integer(Sys.getenv("MC_NSIM", "500"))
n_cores = as.integer(Sys.getenv("MC_CORES", max(1, detectCores() - 2)))
progress = Sys.getenv("MC_PROGRESS", file.path(tempdir(), "lavaan_progress.txt"))
out_dir = file.path("data-raw", "sims")

grid = expand.grid(I = c(15, 30, 60), M = c(2, 3),
                   replicates = c("linked", "exchangeable"),
                   scenario = c("normal", "missing", "uniform", "gamma"),
                   stringsAsFactors = FALSE)

draw_mu = function(I, scenario) {
  switch(scenario,
         uniform = 10 + 2 * sqrt(3) * runif(I, -1, 1),
         gamma = 10 + 2 * (rgamma(I, shape = 2) - 2) / sqrt(2),
         NULL)
}

one_rep = function(cell, seed) {
  g = grid[cell, ]
  M = g$M
  methods = LETTERS[seq_len(M)]
  set.seed(seed)
  mu = draw_mu(g$I, g$scenario)
  dat = sim_methods(n_items = g$I, n_reps = 3, methods = methods,
                    alpha = c(0, 1, -0.5)[seq_len(M)],
                    beta = c(1, 1.2, 0.9)[seq_len(M)],
                    mu_mean = 10, mu_sd = 2, tau = 0.3,
                    omega = if (g$replicates == "linked") 0.3 else 0,
                    sigma = c(0.3, 0.4, 0.5)[seq_len(M)],
                    replicates = g$replicates,
                    p_missing = if (g$scenario == "missing") 0.15 else 0,
                    mu_values = mu)
  truth = attr(dat, "truth")$conversion
  truth = truth[truth$to != truth$from, ]

  do.call(dplyr::bind_rows, lapply(c("ML", "MLR"), function(est) {
    t0 = proc.time()[["elapsed"]]
    fit = tryCatch(
      suppressWarnings(lav_scaled_fit(dat, methods, g$replicates,
                                      estimator = est)),
      error = function(e) NULL
    )
    secs = proc.time()[["elapsed"]] - t0
    conv = !is.null(fit) && lavaan::lavInspect(fit, "converged")
    if (!conv) {
      return(data.frame(cell = cell, seed = seed, estimator = est,
                        converged = FALSE, secs = secs))
    }
    cv = lav_scaled_conversion(fit, methods)
    cv = cv[match(paste(truth$to, truth$from), paste(cv$to, cv$from)), ]
    v = lav_scaled_variances(fit)
    data.frame(cell = cell, seed = seed, estimator = est, converged = TRUE,
               secs = secs,
               heywood = any(v <= 1e-8),
               heywood_om2 = isTRUE(v["om2"] <= 1e-8),
               heywood_t2 = v[["t2"]] <= 1e-8,
               heywood_s = any(v[grepl("^s", names(v))] <= 1e-8),
               min_var = min(v),
               to = cv$to, from = cv$from,
               alpha_hat = cv$alpha, alpha_se = cv$alpha_se,
               alpha_true = truth$alpha,
               beta_hat = cv$beta, beta_se = cv$beta_se,
               beta_true = truth$beta,
               sd_hat = cv$sd_pred, sd_se = cv$sd_se, sd_true = truth$sd_pred,
               cover_alpha = cv$alpha_lower <= truth$alpha &
                 truth$alpha <= cv$alpha_upper,
               cover_beta = cv$beta_lower <= truth$beta &
                 truth$beta <= cv$beta_upper)
  }))
}

jobs = list()
for (cell in seq_len(nrow(grid))) {
  seeds = cell * 100000 + seq_len(n_sim)
  for (st in seq(1, n_sim, by = 25)) {
    jobs[[length(jobs) + 1]] = list(cell = cell,
                                    seeds = seeds[st:min(n_sim, st + 24)])
  }
}
# larger cells first for load balancing
jobs = jobs[order(-grid$I[vapply(jobs, `[[`, numeric(1), "cell")] *
                    grid$M[vapply(jobs, `[[`, numeric(1), "cell")])]

if (file.exists(progress)) file.remove(progress)
t0 = Sys.time()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, {
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  source(file.path("data-raw", "sims", "lavaan_scaled_helpers.R"))
}))
invisible(clusterExport(cl, c("grid", "one_rep", "draw_mu", "progress")))
res = parLapplyLB(cl, jobs, function(job) {
  out = do.call(dplyr::bind_rows,
                lapply(job$seeds, function(s) one_rep(job$cell, s)))
  cat(format(Sys.time()), "cell", job$cell, "done", length(job$seeds), "\n",
      file = progress, append = TRUE)
  out
})
stopCluster(cl)
cat("Elapsed:", format(Sys.time() - t0), "\n")

res = do.call(dplyr::bind_rows, res)
saveRDS(list(grid = grid, results = res, n_sim = n_sim),
        file.path(out_dir, "method_convert_lavaan_coverage.rds"))

# summary ----
# fits: one row per data set and estimator; pairs: one row per ordered pair
fits = res[!duplicated(res[, c("cell", "seed", "estimator")]), ]
ok = res[res$converged, ]
fit_summ = dplyr::summarise(
  dplyr::group_by(fits, cell, estimator),
  conv_rate = mean(converged),
  heywood = mean(heywood, na.rm = TRUE),
  heywood_om2 = mean(heywood_om2, na.rm = TRUE),
  heywood_t2 = mean(heywood_t2, na.rm = TRUE),
  heywood_s = mean(heywood_s, na.rm = TRUE),
  secs = mean(secs),
  .groups = "drop")
# by cell and estimator, pooled over the ordered pairs
cell_summ = dplyr::summarise(
  dplyr::group_by(ok, cell, estimator),
  cover_alpha = mean(cover_alpha, na.rm = TRUE),
  cover_beta = mean(cover_beta, na.rm = TRUE),
  min_pair_alpha = NA_real_, min_pair_beta = NA_real_,
  beta_bias = mean(beta_hat - beta_true),
  beta_relbias = mean(beta_hat / beta_true - 1),
  sd_ratio = mean(sd_hat / sd_true, na.rm = TRUE),
  # mean SE over empirical SD of the estimates, averaged over pairs below
  .groups = "drop")
# by ordered pair
pair_summ = dplyr::summarise(
  dplyr::group_by(ok, cell, estimator, to, from),
  cover_alpha = mean(cover_alpha, na.rm = TRUE),
  cover_beta = mean(cover_beta, na.rm = TRUE),
  beta_bias = mean(beta_hat - beta_true),
  alpha_bias = mean(alpha_hat - alpha_true),
  sd_ratio = mean(sd_hat / sd_true, na.rm = TRUE),
  se_ratio_alpha = mean(alpha_se) / sd(alpha_hat),
  se_ratio_beta = mean(beta_se) / sd(beta_hat),
  n = dplyr::n(),
  .groups = "drop")
mins = dplyr::summarise(dplyr::group_by(pair_summ, cell, estimator),
                        min_pair_alpha = min(cover_alpha),
                        min_pair_beta = min(cover_beta),
                        se_ratio_beta = mean(se_ratio_beta),
                        .groups = "drop")
cell_summ$min_pair_alpha = mins$min_pair_alpha
cell_summ$min_pair_beta = mins$min_pair_beta
cell_summ$se_ratio_beta = mins$se_ratio_beta

grid_c = cbind(cell = seq_len(nrow(grid)), grid)
summ = merge(grid_c, merge(cell_summ, fit_summ))
summ = summ[order(summ$scenario, summ$replicates, summ$M, summ$I,
                  summ$estimator), ]
pair_summ = merge(grid_c, pair_summ)
saveRDS(list(cells = summ, pairs = pair_summ),
        file.path(out_dir, "method_convert_lavaan_coverage_summary.rds"))
options(width = 200)
print(summ[, setdiff(names(summ), "cell")], digits = 3, row.names = FALSE)
