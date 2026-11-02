context("method_vc: random raters and common residual SD")

rr_dat = function(n_items = 20, n_reps = 2, M = 5, replicates = "linked",
                  seed = 1) {
  sim_methods(n_items = n_items, n_reps = n_reps,
              methods = LETTERS[seq_len(M)],
              alpha = c(0, 0.5, -0.3, 0.2, 0.1, 0.4)[seq_len(M)],
              tau = 0.3, omega = if (replicates == "linked") 0.3 else 0,
              sigma = rep(0.4, M), replicates = replicates, seed = seed)
}

test_that("random raters with common SDs match lme4", {
  dat = rr_dat()
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  methods = "random")
  ref = lme4::lmer(y ~ factor(id) + (1 | method) + (1 | method:id) +
                     (1 | id:repl), data = dat)
  vc = as.data.frame(lme4::VarCorr(ref))
  sd_ref = stats::setNames(vc$sdcor, vc$grp)
  sd_fit = stats::setNames(fit$varcomp$sd, fit$varcomp$component)
  expect_equal(unname(sd_fit["rater"]), unname(sd_ref["method"]),
               tolerance = 1e-3)
  expect_equal(unname(sd_fit["tau"]), unname(sd_ref["method:id"]),
               tolerance = 1e-3)
  expect_equal(unname(sd_fit["omega"]), unname(sd_ref["id:repl"]),
               tolerance = 1e-3)
  expect_equal(unname(sd_fit["sigma"]), unname(sd_ref["Residual"]),
               tolerance = 1e-3)
  expect_equal(as.numeric(stats::logLik(fit$model)),
               as.numeric(stats::logLik(ref)), tolerance = 1e-5)

  # without replicates: items fixed, rater and residual only
  dat1 = dat[dat$repl == 1, ]
  fit1 = suppressMessages(method_vc(dat1, y = "y", method = "method",
                                    id = "id", methods = "random"))
  ref1 = lme4::lmer(y ~ factor(id) + (1 | method), data = dat1)
  vc1 = as.data.frame(lme4::VarCorr(ref1))
  expect_equal(fit1$design$model_type, "single_r")
  expect_equal(fit1$varcomp$component, c("rater", "sigma"))
  expect_equal(fit1$varcomp$sd, vc1$sdcor, tolerance = 1e-3)
  expect_null(fit1$repeatability)
})

test_that("fixed methods with a common residual SD match lme4", {
  dat = rr_dat(M = 3)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  sigma = "common")
  ref = lme4::lmer(y ~ method + factor(id) + (1 | method:id) +
                     (1 | id:repl), data = dat)
  vc = as.data.frame(lme4::VarCorr(ref))
  expect_equal(fit$varcomp$component, c("tau", "omega", "sigma"))
  expect_true(all(is.na(fit$varcomp$method)))
  expect_equal(fit$varcomp$sd[3], vc$sdcor[vc$grp == "Residual"],
               tolerance = 1e-3)
  expect_equal(nrow(fit$bias), 3)
  # every pair has the same SD of the differences
  expect_equal(fit$limits$sd, rep(sqrt(2 * sum(fit$varcomp$var[c(1, 3)])),
                                  3))
  expect_equal(fit$repeatability$method, NA_character_)
})

test_that("random-rater limits: one row, 2 * (rater + mean tau + mean sigma)", {
  dat = rr_dat(replicates = "exchangeable")
  fit = suppressWarnings(method_vc(dat, y = "y", method = "method",
                                   id = "id", replicate = "repl",
                                   methods = "random", tau = "method",
                                   sigma = "method"))
  vc = fit$varcomp
  v = 2 * (vc$var[vc$component == "rater"] +
             mean(vc$var[vc$component == "tau"]) +
             mean(vc$var[vc$component == "sigma"]))
  expect_equal(nrow(fit$limits), 1)
  expect_equal(fit$limits$bias, 0)
  expect_equal(fit$limits$sd, sqrt(v))
  z = qnorm(0.975)
  expect_equal(fit$limits$upper_loa, z * sqrt(v))
  expect_equal(fit$limits$lower_loa, -z * sqrt(v))
  expect_true(fit$limits$upper_loa_ci > fit$limits$upper_loa)
  expect_equal(fit$bias$method1, "random rater")
  expect_null(fit$alpha)
})

test_that("the rater variance has M - 1 df and a MOVER interval", {
  dat = rr_dat()
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  methods = "random", conf_level = 0.9)
  vc = fit$varcomp
  r = vc[vc$component == "rater", ]
  expect_equal(r$df, 4)
  # v = S2 - c: S2 = v + c on M - 1 df, c = (tau^2 + sigma^2 / r) / n on its
  # delta-method df (20 items, 2 replicates)
  w = c(0, 1 / 20, 0, 1 / 40)
  expect_equal(vc$component, c("rater", "tau", "omega", "sigma"))
  cc = sum(w * vc$var)
  nu_c = 2 * cc^2 / drop(t(w) %*% fit$vc_cov %*% w)
  s2 = r$var + cc
  lo = r$var - sqrt((s2 * (1 - 4 / qchisq(0.95, 4)))^2 +
                      (cc * (nu_c / qchisq(0.05, nu_c) - 1))^2)
  up = r$var + sqrt((s2 * (4 / qchisq(0.05, 4) - 1))^2 +
                      (cc * (1 - nu_c / qchisq(0.95, nu_c)))^2)
  expect_equal(r$lower, sqrt(max(lo, 0)))
  expect_equal(r$upper, sqrt(up))
  # wider than the chi-square interval for v on M - 1 df
  expect_gt(r$upper, sqrt(4 * r$var / qchisq(0.05, 4)))
  # with no item-level variance it is that chi-square interval
  vc0 = vc
  vc0$var[-1] = 0
  expect_equal(mc_vc_rater_ci(vc0, fit$vc_cov * 0, n_items = 20, n_obs = 200,
                              level = 0.9),
               sqrt(4 * r$var / qchisq(c(0.95, 0.05), 4)))
  expect_gt(diff(confint(fit, level = 0.95)["rater", ]), r$upper - r$lower)
  j = which(vc$component == "rater")
  expect_equal(fit$vc_cov[j, j], 2 * r$var^2 / 4)
  expect_equal(sum(abs(fit$vc_cov[j, -j])), 0)
  ci = confint(fit, level = 0.9)
  expect_equal(unname(ci["rater", ]), c(r$lower, r$upper))
  expect_false(any(grepl("^bias", rownames(ci))))
  # with include_omega the SD adds 2 omega^2
  fo = method_vc(dat, y = "y", method = "method", id = "id",
                 replicate = "repl", replicates = "linked",
                 methods = "random", include_omega = TRUE)
  expect_equal(fo$limits$sd^2,
               fit$limits$sd^2 + 2 * fit$varcomp$var[fit$varcomp$component ==
                                                       "omega"])
})

test_that("random raters: input checks and warnings", {
  dat = rr_dat(M = 3)
  expect_warning(method_vc(dat, y = "y", method = "method", id = "id",
                           replicate = "repl", replicates = "linked",
                           methods = "random"),
                 "Only 3 methods")
  d2 = dat[dat$method %in% c("A", "B"), ]
  expect_error(method_vc(d2, y = "y", method = "method", id = "id",
                         replicate = "repl", methods = "random"),
               "at least three methods")
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", methods = "random",
                         interval = "tolerance"),
               "not yet available")
  expect_error(suppressWarnings(suppressMessages(
    method_vc(dat[dat$repl == 1, ], y = "y", method = "method", id = "id",
              methods = "random", sigma = "method"))),
    "need replicate measurements")
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", sigma = "both"))
})

test_that("random raters: methods and downstream functions", {
  dat = rr_dat()
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  methods = "random")
  expect_output(print(fit), "random raters")
  expect_output(print(fit), "randomly")
  expect_output(print(summary(fit)), "random rater")
  expect_equal(names(coef(fit)), c("rater", "tau", "omega", "sigma"))
  expect_error(vcov(fit), "random")
  expect_error(confint(fit, parm = "bias"), "random")
  expect_error(method_convert(object = fit), "fixed method effects")
  expect_error(predict(fit, newdata = 1, from = "A", to = "B"),
               "fixed method effects")
  expect_error(plot(fit, type = "matrix"), "fixed method effects")

  p = plot(fit, pair_seed = 1)
  expect_s3_class(p, "ggplot")
  # all 10 pairs of 5 methods, 20 items x 2 occasions each
  expect_equal(nrow(p$data), 10 * 40)
  expect_message(plot(fit, pair = c("A", "B")), "ignored")
  expect_s3_class(plot(fit, type = "vc"), "ggplot")
  expect_false(anyNA(plot(fit, type = "vc")$data$facet))
  expect_s3_class(check(fit), "patchwork")

  td = tdi(fit, seed = 1, B = 2000)
  expect_equal(unname(td$estimate), qnorm(0.975) * fit$limits$sd,
               tolerance = 1e-6)
  # bounds from the components on their own df, as in method_vc(): with
  # bias 0 the tolerance bound at p = pred_level is the upper LoA bound
  td_t = tdi(fit, method = "tolerance")
  expect_equal(td_t$conf.int[2], fit$limits$upper_loa_ci, tolerance = 1e-8)
  expect_gt(td$conf.int[2],
            qnorm(0.975) * fit$limits$sd *
              sqrt(fit$limits$df_sd / qchisq(0.05, fit$limits$df_sd)))
  expect_error(tdi(fit, method = "delta"), "Satterthwaite")
  eq = model_equation(fit)
  expect_match(as.character(eq), "sigma^{2}_{\\alpha}", fixed = TRUE)
  expect_false(grepl("sum_{i}", as.character(eq), fixed = TRUE))
})

test_that("random raters on the log scale give ratio limits", {
  dat = rr_dat()
  dat$yp = exp(dat$y / 3 + 2)
  fit = method_vc(dat, y = "yp", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  methods = "random", transform = "log")
  expect_output(print(fit), "random rater / random rater")
  orig = mc_original_scale(fit)
  expect_equal(orig$bias, 0)
  expect_equal(orig$upper_loa, -orig$lower_loa)
})
