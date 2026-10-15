# Coverage simulation for method_vc() ----
# Grid: items I in {15, 30, 60}, replicates R in {2, 3}, methods M in {2, 3},
# linked and exchangeable replicates; 1000 data sets per cell.
# Records, from each fit (95% nominal throughout):
#   - two-sided CIs for the biases
#   - LoA confidence bounds, bound_type "iu" (each side, one-sided 95%) and
#     "joint" (both limits)
#   - tolerance limits, "joint" (content >= 95%) and "iu" (each side)
#   - two-sided CIs for RC, RpC (linked), and the component SDs
# The limit bounds are computed two ways from the same fit:
#   - "satterthwaite" (package default): one chi-square for the variance of
#     the difference with Satterthwaite df from the full covariance of the
#     components
#   - "components": MOVER over the components with their own df, treating
#     them as independent
# Run from the package root:
#   Rscript data-raw/sims/sim_method_vc_coverage.R
# Results: data-raw/sims/method_vc_coverage.rds (per-replicate) and
#          data-raw/sims/method_vc_coverage_summary.rds

library(parallel)

n_sims = as.integer(Sys.getenv("MC_NSIMS", "1000"))
n_cores = as.integer(Sys.getenv("MC_CORES", max(1, detectCores() - 2)))
out_dir = file.path("data-raw", "sims")

grid = expand.grid(I = c(15, 30, 60),
                   R = c(2, 3),
                   M = c(2, 3),
                   replicates = c("exchangeable", "linked"),
                   stringsAsFactors = FALSE)

# true parameters ----
true_pars = function(M, replicates) {
  list(methods = LETTERS[seq_len(M)],
       alpha = c(0, 0.3, -0.2)[seq_len(M)],
       tau = 0.5,
       omega = if (replicates == "linked") 0.5 else 0,
       sigma = c(0.4, 0.6, 0.8)[seq_len(M)],
       mu_mean = 10,
       mu_sd = 2)
}

one_rep = function(cell, seed) {
  g = grid[cell, ]
  p = true_pars(g$M, g$replicates)
  dat = sim_methods(n_items = g$I, n_reps = g$R, methods = p$methods,
                    alpha = p$alpha, mu_mean = p$mu_mean, mu_sd = p$mu_sd,
                    tau = p$tau, omega = p$omega, sigma = p$sigma,
                    replicates = g$replicates, seed = seed)
  truth = attr(dat, "truth")
  n_warn = 0
  fit = withCallingHandlers(
    tryCatch(
      suppressMessages(
        method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = g$replicates,
                  keep_model = FALSE)
      ),
      error = function(e) NULL
    ),
    warning = function(w) {
      n_warn <<- n_warn + 1
      invokeRestart("muffleWarning")
    }
  )
  if (is.null(fit)) {
    return(data.frame(cell = cell, seed = seed, failed = TRUE))
  }

  tp = truth$pairs
  z = qnorm(0.975)
  rows = list()
  for (sd_df in c("satterthwaite", "components")) {
    lim = function(interval, bound_type) {
      mc_limits(bias_tab = fit$bias, varcomp = fit$varcomp,
                cov_v = fit$vc_cov, design = fit$design,
                interval = interval, pred_level = 0.95, conf_level = 0.95,
                bound_type = bound_type, include_omega = FALSE,
                sd_df = sd_df)
    }
    a_iu = lim("agreement", "iu")
    a_jt = lim("agreement", "joint")
    t_jt = lim("tolerance", "joint")
    t_iu = lim("tolerance", "iu")
    content = pnorm((t_jt$upper.TL - tp$bias) / tp$sd) -
      pnorm((t_jt$lower.TL - tp$bias) / tp$sd)
    rows[[sd_df]] = data.frame(
      cell = cell, seed = seed, failed = FALSE, n_warn = n_warn,
      sd_df = sd_df,
      pair = paste0(tp$method1, "-", tp$method2),
      bias_cov = fit$bias$lower.CL <= tp$bias & tp$bias <= fit$bias$upper.CL,
      loa_iu_lower = a_iu$lower_loa_ci <= tp$lower_loa,
      loa_iu_upper = a_iu$upper_loa_ci >= tp$upper_loa,
      loa_joint = a_jt$lower_loa_ci <= tp$lower_loa &
        a_jt$upper_loa_ci >= tp$upper_loa,
      tol_joint = content >= 0.95,
      tol_iu_lower = t_iu$lower.TL <= tp$lower_loa,
      tol_iu_upper = t_iu$upper.TL >= tp$upper_loa,
      sd_ratio = a_iu$sd / tp$sd,
      df_sd = a_iu$df_sd
    )
  }
  lim_rows = do.call(rbind, rows)

  ## repeatability and component SDs ----
  rp = fit$repeatability
  tr = truth$repeatability
  vc = fit$varcomp
  sig = vc[vc$component == "sigma", ]
  rc_rows = data.frame(
    cell = cell, seed = seed, method = rp$method,
    rc_cov = rp$rc_lower <= tr$rc & tr$rc <= rp$rc_upper,
    rpc_cov = if ("rpc" %in% names(rp)) {
      rp$rpc_lower <= tr$rpc & tr$rpc <= rp$rpc_upper
    } else NA,
    sigma_cov = sig$lower <= p$sigma & p$sigma <= sig$upper
  )
  tau_row = vc[vc$component == "tau", ]
  om_row = vc[vc$component == "omega", ]
  comp_rows = data.frame(
    cell = cell, seed = seed,
    tau_cov = tau_row$lower <= p$tau & p$tau <= tau_row$upper,
    omega_cov = if (nrow(om_row) == 1) {
      om_row$lower <= p$omega & p$omega <= om_row$upper
    } else NA
  )
  list(limits = lim_rows, rc = rc_rows, comp = comp_rows)
}

run_cell_chunk = function(job) {
  lapply(job$seeds, function(s) one_rep(job$cell, s))
}

# jobs: each cell split into chunks ----
chunk = 50
jobs = list()
for (cell in seq_len(nrow(grid))) {
  seeds = cell * 100000 + seq_len(n_sims)
  for (st in seq(1, n_sims, by = chunk)) {
    jobs[[length(jobs) + 1]] = list(cell = cell,
                                    seeds = seeds[st:min(n_sims, st + chunk - 1)])
  }
}

t0 = Sys.time()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))
invisible(clusterExport(cl, c("grid", "true_pars", "one_rep")))
res = parLapplyLB(cl, jobs, run_cell_chunk)
stopCluster(cl)
cat("Elapsed:", format(Sys.time() - t0), "\n")

res = unlist(res, recursive = FALSE)
failed = vapply(res, function(r) is.data.frame(r) && isTRUE(r$failed),
                logical(1))
ok = res[!failed]
limits = do.call(rbind, lapply(ok, `[[`, "limits"))
rc = do.call(rbind, lapply(ok, `[[`, "rc"))
comp = do.call(rbind, lapply(ok, `[[`, "comp"))
fails = do.call(rbind, res[failed])

saveRDS(list(grid = grid, limits = limits, rc = rc, comp = comp,
             fails = fails, n_sims = n_sims),
        file.path(out_dir, "method_vc_coverage.rds"))

# summary ----
mean_na = function(x) mean(x, na.rm = TRUE)
lim_sum = aggregate(
  cbind(bias_cov, loa_iu_lower, loa_iu_upper, loa_joint, tol_joint,
        tol_iu_lower, tol_iu_upper, sd_ratio, df_sd) ~ cell + sd_df,
  data = limits, FUN = mean_na, na.action = na.pass
)
lim_sum = merge(cbind(cell = seq_len(nrow(grid)), grid), lim_sum)
rc_sum = aggregate(cbind(rc_cov, rpc_cov, sigma_cov) ~ cell, data = rc,
                   FUN = mean_na, na.action = na.pass)
comp_sum = aggregate(cbind(tau_cov, omega_cov) ~ cell, data = comp,
                     FUN = mean_na, na.action = na.pass)
rep_sum = merge(cbind(cell = seq_len(nrow(grid)), grid),
                merge(rc_sum, comp_sum))
na_rate = aggregate(is.na(loa_iu_lower) ~ cell + sd_df, data = limits,
                    FUN = mean, na.action = na.pass)
names(na_rate)[3] = "na_rate"
lim_sum = merge(lim_sum, na_rate)
fail_rate = tapply(failed, rep(seq_len(nrow(grid)),
                               each = n_sims)[seq_along(failed)], mean)

summ = list(limits = lim_sum[order(lim_sum$sd_df, lim_sum$cell), ],
            repeatability = rep_sum,
            fail_rate = fail_rate)
saveRDS(summ, file.path(out_dir, "method_vc_coverage_summary.rds"))
print(summ, digits = 3)
