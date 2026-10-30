# Coverage simulation for method_convert(bias = "scaled") ----
# Confidence intervals for the conversion intercepts and slopes from the
# scaled model (alternating regression):
#   Part A, "delta" (cheap): items I in {15, 30, 60}, methods M in {2, 3},
#     linked and exchangeable replicates, R = 3; MC_NDELTA data sets per cell.
#   Part B, "delta" vs "boot" (parametric bootstrap, MC_BOOT replicates;
#     each replicate refits the model): I in {15, 30}, M = 2, linked, R = 3;
#     MC_NBOOTSIM data sets per cell. Also records coverage of the bootstrap
#     intervals for the prediction SDs.
# True slopes differ from 1 (beta = 1, 1.2, 0.9), so the bias is not
# constant. Nominal level 95%.
# Run from the package root:
#   Rscript data-raw/sims/sim_method_convert_coverage.R
# Part B only (about 2 hours on 12 cores):
#   MC_PARTS=boot Rscript data-raw/sims/sim_method_convert_coverage.R
# Results: data-raw/sims/method_convert_coverage.rds and
#          data-raw/sims/method_convert_coverage_summary.rds

library(parallel)

n_delta = as.integer(Sys.getenv("MC_NDELTA", "500"))
n_bootsim = as.integer(Sys.getenv("MC_NBOOTSIM", "200"))
n_boot = as.integer(Sys.getenv("MC_BOOT", "199"))
n_cores = as.integer(Sys.getenv("MC_CORES", max(1, detectCores() - 2)))
out_dir = file.path("data-raw", "sims")

grid_a = expand.grid(I = c(15, 30, 60), M = c(2, 3),
                     replicates = c("linked", "exchangeable"),
                     part = "delta", stringsAsFactors = FALSE)
grid_b = expand.grid(I = c(15, 30), M = 2, replicates = "linked",
                     part = "boot", stringsAsFactors = FALSE)
# MC_NBOOTSIM = 0 runs Part A only; MC_PARTS = "boot" runs Part B only and
# writes method_convert_coverage_boot*.rds (keeps the Part A results)
parts = Sys.getenv("MC_PARTS", "both")
grid = if (parts == "boot") {
  grid_b
} else if (n_bootsim > 0) {
  rbind(grid_a, grid_b)
} else {
  grid_a
}
suffix = if (parts == "boot") "_boot" else ""

one_rep = function(cell, seed) {
  g = grid[cell, ]
  M = g$M
  set.seed(seed)
  dat = sim_methods(n_items = g$I, n_reps = 3,
                    methods = LETTERS[seq_len(M)],
                    alpha = c(0, 1, -0.5)[seq_len(M)],
                    beta = c(1, 1.2, 0.9)[seq_len(M)],
                    mu_mean = 10, mu_sd = 2, tau = 0.3,
                    omega = if (g$replicates == "linked") 0.3 else 0,
                    sigma = c(0.3, 0.4, 0.5)[seq_len(M)],
                    replicates = g$replicates)
  truth = attr(dat, "truth")$conversion
  truth = truth[truth$to != truth$from, ]
  ci = if (g$part == "boot") "boot" else "delta"
  fit = tryCatch(
    suppressMessages(suppressWarnings(
      method_convert(dat, y = "y", method = "method", id = "id",
                     replicate = "repl", replicates = g$replicates,
                     bias = "scaled", ci_method = ci, boot_reps = n_boot,
                     keep_model = FALSE)
    )),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(data.frame(cell = cell, seed = seed, failed = TRUE))
  }
  cv = fit$conversion
  cv = cv[cv$to != cv$from, ]
  key = match(paste(truth$to, truth$from), paste(cv$to, cv$from))
  cv = cv[key, ]
  out = data.frame(cell = cell, seed = seed, failed = FALSE,
                   converged = fit$fit_info$converged,
                   iterations = fit$fit_info$iterations,
                   to = cv$to, from = cv$from,
                   beta_hat = cv$beta, beta_true = truth$beta,
                   alpha_hat = cv$alpha, alpha_true = truth$alpha,
                   sd_hat = cv$sd_pred, sd_true = truth$sd_pred)
  # delta-method intervals (always available)
  q = qt(0.975, cv$df)
  out$delta_alpha = abs(cv$alpha - truth$alpha) <= q * cv$alpha_se
  out$delta_beta = abs(cv$beta - truth$beta) <= q * cv$beta_se
  if (ci == "boot") {
    out$boot_alpha = cv$alpha_lower <= truth$alpha &
      truth$alpha <= cv$alpha_upper
    out$boot_beta = cv$beta_lower <= truth$beta & truth$beta <= cv$beta_upper
    out$boot_sd = cv$sd_lower <= truth$sd_pred & truth$sd_pred <= cv$sd_upper
    out$n_boot_fail = fit$fit_info$n_boot_fail
  } else {
    out$boot_alpha = out$boot_beta = out$boot_sd = NA
    out$n_boot_fail = NA
  }
  out
}

jobs = list()
for (cell in seq_len(nrow(grid))) {
  n = if (grid$part[cell] == "boot") n_bootsim else n_delta
  seeds = cell * 100000 + seq_len(n)
  chunk = if (grid$part[cell] == "boot") 2 else 25
  for (st in seq(1, n, by = chunk)) {
    jobs[[length(jobs) + 1]] = list(cell = cell,
                                    seeds = seeds[st:min(n, st + chunk - 1)])
  }
}
# run the expensive bootstrap jobs first
jobs = jobs[order(-vapply(jobs, function(j) grid$part[j$cell] == "boot",
                          logical(1)))]

t0 = Sys.time()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".",
                                                              quiet = TRUE))))
invisible(clusterExport(cl, c("grid", "one_rep", "n_boot")))
res = parLapplyLB(cl, jobs, function(job) {
  out = do.call(rbind, lapply(job$seeds, function(s) one_rep(job$cell, s)))
  # progress: one line per finished job
  cat(format(Sys.time()), "cell", job$cell, "done", length(job$seeds), "\n",
      file = file.path("data-raw", "sims", "method_convert_progress.txt"),
      append = TRUE)
  out
})
stopCluster(cl)
cat("Elapsed:", format(Sys.time() - t0), "\n")

res = do.call(dplyr::bind_rows, res)
saveRDS(list(grid = grid, results = res, n_boot = n_boot),
        file.path(out_dir, paste0("method_convert_coverage", suffix, ".rds")))

# summary ----
ok = res[!res$failed, ]
summ = aggregate(cbind(delta_alpha, delta_beta, boot_alpha, boot_beta,
                       boot_sd, converged) ~ cell,
                 data = ok, FUN = mean, na.action = na.pass)
summ$beta_bias = aggregate((beta_hat - beta_true) ~ cell, data = ok,
                           FUN = mean)[, 2]
summ$sd_ratio = aggregate(I(sd_hat / sd_true) ~ cell, data = ok,
                          FUN = mean)[, 2]
summ$fail_rate = tapply(res$failed, res$cell, mean)[as.character(summ$cell)]
summ = merge(cbind(cell = seq_len(nrow(grid)), grid), summ)
saveRDS(summ, file.path(out_dir, paste0("method_convert_coverage", suffix,
                              "_summary.rds")))
print(summ, digits = 3)
