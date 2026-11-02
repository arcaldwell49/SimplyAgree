# Step 0 of trending_agreement_plan.md -------------------------------------
# Do method_vc(replicates = "linked"), mdc_test() and icc_test() already give
# the trending-agreement quantities?
# Run: Rscript step0_checks.R <path-to-SimplyAgree>

args = commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[1], quiet = TRUE, export_all = FALSE)

# simulate ----------------------------------------------------------------
# true_ij = mu_i + w_ij; ref = true + e; alt = mu_i + b_w * w_ij + u_i + e
sim = function(n = 100, m = 6, sd_wt = 1, b_w = 1, sd_ref = 0.25,
               sd_alt = 0.25, sd_u = 0.25, seed = 1) {
  set.seed(seed)
  id = rep(seq_len(n), each = m)
  mu = rep(rnorm(n, 5, 1), each = m)
  w = rnorm(n * m, 0, sd_wt)
  u = rep(rnorm(n, 0, sd_u), each = m)
  data.frame(id = id, occ = rep(seq_len(m), n),
             ref = mu + w + rnorm(n * m, 0, sd_ref),
             alt = mu + b_w * w + u + rnorm(n * m, 0, sd_alt))
}

to_long = function(d) {
  rbind(data.frame(id = d$id, repl = d$occ, method = "ref", y = d$ref),
        data.frame(id = d$id, repl = d$occ, method = "alt", y = d$alt))
}

# centred long data for icc_test: target = subject-occasion, item = device
centred_long = function(d) {
  long = to_long(d)
  long$yc = long$y - ave(long$y, long$id, long$method)
  long$target = paste(long$id, long$repl, sep = ":")
  long
}

# one scenario ------------------------------------------------------------
one = function(sd_wt, b_w, sd_alt) {
  d = sim(sd_wt = sd_wt, b_w = b_w, sd_alt = sd_alt)
  fit = method_vc(to_long(d), y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  vc = as.data.frame(fit$varcomp)
  om = vc$sd[vc$component == "omega"]
  s_ref = vc$sd[vc$component == "sigma" & vc$method == "ref"]
  s_alt = vc$sd[vc$component == "sigma" & vc$method == "alt"]
  # change LoA from method_vc vs mdc_test on D
  cl_mvc = qnorm(0.975) * sqrt(2) * sqrt(s_ref^2 + s_alt^2)
  d$D = d$alt - d$ref
  mdc = mdc_test(D ~ occ | id, data = d, model = "oneway", method = "reml")
  # within-person reliability: method_vc ratio vs centred ICC
  rel_mvc = om^2 / (om^2 + (s_ref^2 + s_alt^2) / 2)
  cl = centred_long(d)
  icc = icc_test(yc ~ method | target, data = cl, model = "twoway",
                 type = "consistency")
  # permutation test refits the model n_perm times; only for b_w < 1
  lr_p = if (b_w < 1) {
    linked_reps_test(fit, n_perm = 19, seed = 1)$p.value
  } else NA_real_
  c(omega = om, sigma_ref = s_ref, sigma_alt = s_alt,
    changeLoA_mvc = cl_mvc, changeLoA_mdc = unname(mdc$estimate),
    rel_mvc = rel_mvc, icc_centred = unname(icc$estimate),
    icc_lower = icc$conf.int[1], icc_upper = icc$conf.int[2],
    linked_p = lr_p)
}

# questions 1-3: scenarios ------------------------------------------------
scen = expand.grid(sd_wt = c(0.3, 1.5), b_w = c(1, 0.5, 0), sd_alt = 0.25)
scen = rbind(scen, data.frame(sd_wt = c(0.3, 1.5), b_w = 1, sd_alt = 0.6))
res = t(sapply(seq_len(nrow(scen)), function(k) {
  s = scen[k, ]
  one(s$sd_wt, s$b_w, s$sd_alt)
}))
options(width = 200)
print(cbind(scen, round(res, 3)))
cat("\nTruth (unit slope): omega = sd_wt; sigma_ref = 0.25; sigma_alt = sd_alt;",
    "change LoA = 1.96*sqrt(2)*sqrt(0.25^2 + sd_alt^2)\n")

# question 2: coverage of the centred-ICC interval ------------------------
# Truth: omega^2 / (omega^2 + (s_ref^2 + s_alt^2) / 2), unit slope
cover = function(n, m, sd_wt, nsim = 300) {
  truth = sd_wt^2 / (sd_wt^2 + 0.25^2)
  hits = vapply(seq_len(nsim), function(r) {
    d = sim(n = n, m = m, sd_wt = sd_wt, seed = 1000 + r)
    ci = icc_test(yc ~ method | target, data = centred_long(d),
                  model = "twoway", type = "consistency")$conf.int
    ci[1] <= truth && truth <= ci[2]
  }, logical(1))
  c(n = n, m = m, sd_wt = sd_wt, truth = truth, coverage = mean(hits))
}
cat("\nCentred-ICC 95% CI coverage (300 data sets each):\n")
print(round(rbind(cover(20, 3, 0.3), cover(20, 6, 0.3),
                  cover(60, 3, 0.3), cover(20, 3, 1)), 3))
