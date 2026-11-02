context("method_vc: refit when a variance component is estimated at zero")

bd_dat = function(seed, M = 5, n = 20, alpha_sd = 0, tau = 0.3, omega = 0.3,
                  n_reps = 2) {
  set.seed(seed)
  sim_methods(n_items = n, n_reps = n_reps, methods = LETTERS[seq_len(M)],
              alpha = stats::rnorm(M, 0, alpha_sd), tau = tau, omega = omega,
              sigma = rep(0.4, M), replicates = "linked", seed = seed)
}
bd_fit = function(dat, ...) {
  suppressMessages(method_vc(dat, y = "y", method = "method", id = "id",
                             replicate = "repl", replicates = "linked", ...))
}
vc_row = function(fit, comp) fit$varcomp[fit$varcomp$component == comp, ]

test_that("random raters: a zero rater SD gets bounds from a refit", {
  dat = bd_dat(3)
  expect_warning(fit <- bd_fit(dat, methods = "random"),
                 "refit without rater")
  expect_equal(fit$design$drop, "rater")
  r = vc_row(fit, "rater")
  expect_equal(r$sd, 0)
  expect_equal(r$df, 4)
  expect_equal(r$lower, 0)
  expect_gt(r$upper, 0)
  expect_equal(c(r$lower, r$upper),
               mc_vc_rater_ci(fit$varcomp, fit$vc_cov, n_items = 20,
                              n_obs = nrow(fit$data), level = 0.95))
  expect_false(anyNA(fit$limits$upper_loa_ci))
  expect_equal(unname(confint(fit)["rater", ]), c(r$lower, r$upper))
  # the refit is the model without rater effects: same REML fit
  df = fit$data
  m0 = nlme::lme(y_t ~ id, random = list(id = nlme::pdIdent(~ method - 1),
                                         repl = ~ 1), data = df)
  re0 = as.matrix(m0$modelStruct$reStruct)
  expect_equal(vc_row(fit, "tau")$sd, sqrt(re0$id[1, 1]) * m0$sigma,
               tolerance = 1e-4)
  expect_equal(vc_row(fit, "sigma")$sd, m0$sigma, tolerance = 1e-4)
  # the rater adds nothing to the bounds when its estimate is 0
  expect_equal(fit$limits$sd^2,
               2 * (vc_row(fit, "tau")$var + vc_row(fit, "sigma")$var))
  expect_output(print(fit), "Estimated at zero: rater")
  expect_equal(unname(tdi(fit, method = "tolerance")$conf.int[2]),
               fit$limits$upper_loa_ci, tolerance = 1e-8)
})

test_that("fixed methods: a zero omega gets an interval from 0 to NA", {
  dat = bd_dat(2, M = 3, omega = 0)
  expect_warning(fit <- bd_fit(dat), "refit without omega")
  expect_equal(fit$design$drop, "omega")
  om = vc_row(fit, "omega")
  expect_equal(c(om$sd, om$lower), c(0, 0))
  expect_true(is.na(om$upper))
  expect_true(is.na(om$df))
  expect_equal(unname(confint(fit)["omega", ]), c(0, NA))
  expect_false(anyNA(fit$limits$upper_loa_ci))
  expect_false(anyNA(fit$repeatability$rpc_upper))
  expect_output(print(fit), "no upper bound for omega")
  # the biases come from the original fit
  expect_equal(nrow(fit$bias), 3)
})

test_that("a fit with all components away from zero is not refit", {
  dat = bd_dat(1, M = 3)
  fit = bd_fit(dat)
  expect_null(fit$design$drop)
  expect_null(mc_vc_refit_boundary(NULL, df = NULL, design = fit$design,
                                   varcomp = fit$varcomp, control = NULL))
})

test_that("refits without tau, without everything, and without replicates", {
  skip_on_cran()
  # tau at zero: omega becomes an item-by-replicate intercept
  expect_warning(f_tau <- bd_fit(bd_dat(3, M = 3, tau = 0)),
                 "refit without tau")
  expect_equal(f_tau$design$drop, "tau")
  expect_gt(vc_row(f_tau, "omega")$sd, 0)
  expect_false(anyNA(vc_row(f_tau, "omega")$upper))
  expect_false(anyNA(f_tau$limits$upper_loa_ci))

  # tau and omega at zero, method-specific sigma: gls with varIdent
  expect_warning(f_gls <- bd_fit(bd_dat(2, M = 3, tau = 0, omega = 0)),
                 "refit without")
  expect_setequal(f_gls$design$drop, c("tau", "omega"))
  expect_false(anyNA(f_gls$limits$upper_loa_ci))
  expect_false(anyNA(vc_row(f_gls, "sigma")$upper))

  # random raters with every random component at zero: gls, common sigma,
  # exact chi-square interval for sigma on N - p df
  expect_warning(f_all <- bd_fit(bd_dat(2, tau = 0, omega = 0),
                                 methods = "random"),
                 "refit without")
  expect_setequal(f_all$design$drop, c("rater", "tau", "omega"))
  sg = vc_row(f_all, "sigma")
  nu = nrow(f_all$data) - 20
  expect_equal(sg$df, nu)
  expect_equal(sg$upper, sqrt(nu * sg$var / qchisq(0.025, nu)))
  expect_false(anyNA(f_all$limits$upper_loa_ci))
  expect_gt(vc_row(f_all, "rater")$upper, 0)

  # random raters without replicates (single_r): gls on the items
  d1 = bd_dat(1, n_reps = 1)
  expect_warning(f1 <- suppressMessages(
    method_vc(d1, y = "y", method = "method", id = "id", methods = "random")),
    "refit without rater")
  expect_equal(f1$design$drop, "rater")
  expect_false(anyNA(f1$limits$upper_loa_ci))
  expect_equal(vc_row(f1, "sigma")$df, nrow(f1$data) - 20)
})
