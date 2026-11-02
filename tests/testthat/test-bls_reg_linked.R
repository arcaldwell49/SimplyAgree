context("bls_reg and method_convert(cbls) with linked replicates")

# two methods on the same occasions (rows), with occasion effects omega
lk_dat = function(n_items = 30, n_reps = 3, omega = 0.6, seed = 3) {
  dat = sim_methods(n_items = n_items, n_reps = n_reps,
                    methods = c("A", "B"), alpha = c(0, 0.5),
                    beta = c(1, 1.1), tau = 0.2, omega = omega,
                    sigma = c(0.3, 0.4), replicates = "linked", seed = seed)
  w = stats::reshape(as.data.frame(dat[, c("id", "repl", "method", "y")]),
                     idvar = c("id", "repl"), timevar = "method",
                     direction = "wide")
  names(w) = sub("^y[.]", "", names(w))
  list(long = dat, wide = w[order(w$id, w$repl), ])
}

test_that("linked BLS uses the pooled within-subject error correlation", {
  w = lk_dat()$wide
  fit = bls_reg(B ~ A, data = w, id = "id", replicates = "linked",
                equation_error = FALSE)
  # pooled within-subject variances and cross-products
  dev = function(v) v - ave(v, w$id)
  n_i = table(w$id)
  df_w = sum(n_i - 1)
  vx = sum(dev(w$A)^2) / df_w
  vy = sum(dev(w$B)^2) / df_w
  rho = sum(dev(w$A) * dev(w$B)) / df_w / sqrt(vx * vy)
  e = fit$errors
  expect_equal(e$var_x, vx)
  expect_equal(e$var_y, vy)
  expect_equal(e$rho, rho)
  expect_equal(e$df_rho, df_w)
  expect_equal(e$cov_xy, rho * sqrt(vx * vy))
  expect_equal(e$replicates, "linked")

  # the fit is the BLS engine with the covariance of the subject means
  m = fit$model
  expect_equal(m$cov_xy, rep(rho * sqrt(vx * vy) * 3 / 9, nrow(m)))
  start = unname(coef(fit))
  k = nrow(m)
  eng = .bls_engine(u = m$x, v = m$y, var_u = rep(vx / 3, k),
                    var_v = rep(vy / 3, k),
                    cov_uv = rep(rho * sqrt(vx * vy) / 3, k),
                    start = start + 0.1)
  expect_equal(eng$coef, start, tolerance = 1e-8)
  # the CBLS line is the same line
  a = start[1]
  b = start[2]
  expect_equal(unname(fit$cbls$coefficients),
               c(-2 * a / (1 + b), 2 * (1 - b) / (1 + b)), tolerance = 1e-8)

  # prediction for new measurements on one occasion: the covariance is
  # subtracted, on the paired df
  pr = predict(fit, newdata = 1, interval = "prediction")
  var_line = fit$vcov[1, 1] + fit$vcov[2, 2] + 2 * fit$vcov[1, 2]
  v_meas = vy + b^2 * vx - 2 * b * rho * sqrt(vx * vy)
  var_pred = var_line + v_meas
  df_pred = var_pred^2 / (var_line^2 / fit$df.residual + v_meas^2 / df_w)
  half = qt(0.975, df_pred) * sqrt(var_pred)
  expect_equal(unname(pr$upr - pr$fit), half, tolerance = 1e-8)
  # narrower than ignoring the shared occasions
  fe = bls_reg(B ~ A, data = w, id = "id", equation_error = FALSE)
  pe = predict(fe, newdata = 1, interval = "prediction")
  expect_lt(pr$upr - pr$lwr, pe$upr - pe$lwr)
  # tolerance limits outside the prediction limits
  tl = predict(fit, newdata = 1, interval = "tolerance")
  expect_lt(tl$lwr, pr$lwr)
  expect_gt(tl$upr, pr$upr)

  expect_output(summary(fit), "Linked replicates: error correlation")
  expect_s3_class(check(fit), "patchwork")
  expect_equal(length(residuals(fit)), nrow(m))
})

test_that("without occasion effects, linked and exchangeable are close", {
  w = lk_dat(omega = 0, seed = 5)$wide
  fl = bls_reg(B ~ A, data = w, id = "id", replicates = "linked")
  fe = bls_reg(B ~ A, data = w, id = "id")
  expect_lt(abs(fl$errors$rho), 0.25)
  expect_equal(unname(coef(fl)), unname(coef(fe)), tolerance = 0.05)
})

test_that("linked replicates with a variance function", {
  w = lk_dat(seed = 7)$wide
  w$A = exp(w$A / 4 + 2)
  w$B = exp(w$B / 4 + 2)
  fit = bls_reg(B ~ A, data = w, id = "id", replicates = "linked",
                var_model = "proportional")
  expect_true(fit$converged)
  expect_gt(fit$errors$rho, 0.3)
  expect_equal(fit$errors$model_x, "proportional")
  expect_true(all(is.finite(predict(fit, newdata = c(7, 8),
                                    interval = "prediction")$upr)))
})

test_that("linked replicates: input checks", {
  w = lk_dat()$wide
  expect_error(bls_reg(B ~ A, data = w, replicates = "linked"), "needs id")
  expect_error(bls_reg(B ~ A, data = w, id = "id", replicates = "linked",
                       var_x = 0.1, var_y = 0.2), "estimated from the")
  # replicates of both methods, but never on the same occasion
  w2 = lk_dat(n_reps = 4)$wide
  w2$A[w2$repl > 2] = NA
  w2$B[w2$repl <= 2] = NA
  expect_error(bls_reg(B ~ A, data = w2, id = "id", replicates = "linked"),
               "two or more occasions")
  fit = bls_reg(B ~ A, data = w, id = "id", replicates = "linked")
  expect_error(error_ratio_profile(fit), "linked replicates")
})

test_that("method_convert(estimator = 'cbls') with linked replicates", {
  d = lk_dat(n_items = 40)
  cb = method_convert(d$long, y = "y", method = "method", id = "id",
                      replicate = "repl", replicates = "linked",
                      bias = "linear", estimator = "cbls")
  f = bls_reg(B ~ A, data = d$wide, id = "id", replicates = "linked")
  expect_equal(c(cb$da$a, cb$da$b), unname(f$cbls$coefficients),
               tolerance = 1e-10)
  cv = cb$conversion[cb$conversion$to == "B" & cb$conversion$from == "A", ]
  expect_equal(c(cv$alpha, cv$beta), unname(coef(f)), tolerance = 1e-8)
  expect_equal(cv$beta, 1.1, tolerance = 0.1)
  # SD of a new difference on one occasion, with the covariance
  e = f$errors
  b = cb$da$b
  beta = unname(coef(f)[2])
  tau2 = f$dispersion$tau2
  s = sqrt(e$var_x * (1 - b / 2)^2 + e$var_y * (1 + b / 2)^2 -
             2 * e$cov_xy * (1 - b / 2) * (1 + b / 2) +
             tau2 * ((1 - b / 2)^2 + beta^2 * (1 + b / 2)^2))
  expect_equal(cb$da$s, s)
  expect_output(print(cb), "correlated through the shared occasions")
})
