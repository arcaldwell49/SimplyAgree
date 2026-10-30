# Post-hoc summary of sim_method_convert_lavaan.R ----
# Coverage of the lavaan ML Wald CIs with normal (z) vs t(I - 1) quantiles,
# from the stored estimates and SEs (no refitting), next to the AltReg
# delta-method coverage from method_convert_coverage_summary.rds, and the
# coverage of the sd_pred intervals.
# Run from the package root:
#   Rscript data-raw/sims/summarise_method_convert_lavaan.R

r = readRDS("data-raw/sims/method_convert_lavaan_coverage.rds")
g = cbind(cell = seq_len(nrow(r$grid)), r$grid)
x = merge(g, r$results[r$results$converged & r$results$estimator == "ML", ])
out = do.call(rbind, lapply(split(x, x$cell), function(d) {
  f = function(q) c(mean(abs(d$alpha_hat - d$alpha_true) <= q * d$alpha_se),
                    mean(abs(d$beta_hat - d$beta_true) <= q * d$beta_se))
  z = f(qnorm(.975)); t1 = f(qt(.975, d$I[1] - 1))
  data.frame(scenario = d$scenario[1], rep = d$replicates[1], M = d$M[1], I = d$I[1],
             z_a = z[1], z_b = z[2], t_a = t1[1], t_b = t1[2])
}))
out = out[order(out$I, out$scenario, out$rep, out$M), ]
options(width = 200); print(out, digits = 3, row.names = FALSE)
print(aggregate(cbind(z_a, z_b, t_a, t_b) ~ I, out, function(v) round(c(min = min(v), mean = mean(v), max = max(v)), 3)))
a = readRDS("data-raw/sims/method_convert_coverage_summary.rds")
print(a[a$part == "delta", c("I", "M", "replicates", "delta_alpha", "delta_beta", "beta_bias", "sd_ratio")], digits = 3, row.names = FALSE)

# sd_pred: Wald intervals with t(I - 1), natural vs log scale
x = x[is.finite(x$sd_hat) & is.finite(x$sd_se), ]
q = qt(0.975, x$I - 1)
x$cov_sd = abs(x$sd_hat - x$sd_true) <= q * x$sd_se
# log-scale Wald: se(log sd) = se / sd
x$cov_logsd = abs(log(x$sd_hat) - log(x$sd_true)) <= q * x$sd_se / x$sd_hat
print(aggregate(cbind(cov_sd, cov_logsd) ~ I + scenario, x, mean), digits = 3)
