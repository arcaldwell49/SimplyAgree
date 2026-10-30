conv_data = function(...) {
  sim_methods(n_items = 50, n_reps = 2, methods = c("A", "B"),
              alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10, mu_sd = 3,
              tau = 0.2, sigma = 0.3, seed = 1, ...)
}

# DA model ----
test_that("linear bias, one measurement: matches the DA formulas", {
  dat = sim_methods(n_items = 60, methods = c("A", "B"), alpha = c(0, 1),
                    beta = c(1, 1.2), mu_mean = 10, mu_sd = 3, sigma = 0.3,
                    seed = 4)
  cv = method_convert(dat, y = "y", method = "method", id = "id")
  expect_s3_class(cv, "simple_mconv")

  w = tidyr::pivot_wider(dat, names_from = method, values_from = y)
  D = w$A - w$B
  A = (w$A + w$B) / 2
  fit = lm(D ~ A)
  a = unname(coef(fit)[1])
  b = unname(coef(fit)[2])
  s = sigma(fit)
  expect_equal(cv$da$a, a)
  expect_equal(cv$da$b, b)
  expect_equal(cv$da$s, s)
  expect_false(cv$da$cluster_se)
  expect_equal(cv$da$df, 58)

  # MethComp::DA2y
  ab = cv$conversion[cv$conversion$to == "A" & cv$conversion$from == "B", ]
  ba = cv$conversion[cv$conversion$to == "B" & cv$conversion$from == "A", ]
  expect_equal(c(ab$alpha, ab$beta, ab$sd_pred),
               c(a, 1 + b / 2, s) / (1 - b / 2))
  expect_equal(c(ba$alpha, ba$beta, ba$sd_pred),
               c(-a, 1 - b / 2, s) / (1 + b / 2))
  expect_equal(ba$beta, 1 / ab$beta)

  # delta method SEs against a numerical gradient
  V = vcov(fit)
  f = function(p) c(-p[1] / (1 + p[2] / 2), (1 - p[2] / 2) / (1 + p[2] / 2))
  h = 1e-6
  G = cbind((f(c(a + h, b)) - f(c(a - h, b))) / (2 * h),
            (f(c(a, b + h)) - f(c(a, b - h))) / (2 * h))
  se = sqrt(diag(G %*% V %*% t(G)))
  expect_equal(c(ba$alpha_se, ba$beta_se), se, tolerance = 1e-5)
  q = qt(0.975, 58)
  expect_equal(ba$beta_upper, ba$beta + q * ba$beta_se)

  # the conversion recovers the true relation (beta_B / beta_A = 1.2) with
  # equal measurement errors
  big = sim_methods(n_items = 600, methods = c("A", "B"), alpha = c(0, 1),
                    beta = c(1, 1.2), mu_mean = 10, mu_sd = 3, sigma = 0.3,
                    seed = 4)
  cb = method_convert(big, y = "y", method = "method", id = "id")$conversion
  expect_equal(cb$beta[cb$to == "B" & cb$from == "A"], 1.2, tolerance = 0.02)
  expect_equal(cb$alpha[cb$to == "B" & cb$from == "A"], 1, tolerance = 0.25)
})

test_that("each method to itself", {
  cv = method_convert(conv_data(), y = "y", method = "method", id = "id",
                      replicate = "repl", pair_seed = 1)
  self = cv$conversion[cv$conversion$to == cv$conversion$from, ]
  expect_equal(nrow(self), 2)
  expect_equal(self$alpha, c(0, 0))
  expect_equal(self$beta, c(1, 1))
  expect_true(all(is.na(self$sd_pred)))
  expect_equal(nrow(cv$conversion), 4)
})

test_that("replicates give cluster-robust SEs by item", {
  dat = conv_data()
  cv = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", pair_seed = 1)
  expect_true(cv$da$cluster_se)
  expect_equal(cv$da$df, 49)
  expect_equal(cv$da$n_pairs, 100)
  expect_equal(cv$da$n_items, 50)

  # CR1 sandwich computed directly
  pp = cv$pairs[["A-B"]]
  fit = lm(d ~ avg, data = pp)
  X = model.matrix(fit)
  e = resid(fit)
  meat = matrix(0, 2, 2)
  for (g in unique(pp$id)) {
    i = pp$id == g
    u = colSums(X[i, , drop = FALSE] * e[i])
    meat = meat + u %o% u
  }
  bread = solve(crossprod(X))
  V = bread %*% meat %*% bread * (50 / 49) * (99 / 98)
  expect_equal(c(cv$da$a_se, cv$da$b_se), unname(sqrt(diag(V))))
})

test_that("pairing of exchangeable replicates is reproducible", {
  dat = conv_data()
  c1 = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", pair_seed = 3)
  c2 = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", pair_seed = 3)
  expect_equal(c1$conversion, c2$conversion)
  # linked replicates are paired by replicate
  cl = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", replicates = "linked")
  expect_equal(cl$pairs[["A-B"]]$repl, rep(1:2, 50))
})

test_that("linear variance model", {
  dat = conv_data()
  cv = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", replicates = "linked",
                      variance = "linear")
  pp = cv$pairs[["A-B"]]
  fit = lm(d ~ avg, data = pp)
  sfit = lm(abs(resid(fit)) ~ pp$avg)
  expect_equal(c(cv$da$c_sd, cv$da$d_sd),
               unname(sqrt(pi / 2) * coef(sfit)))
  expect_true(all(is.na(cv$conversion$sd_pred)))

  # prediction limits from the transformed limit lines (eq. 4.11)
  z = qnorm(0.975)
  a_l = cv$da$a - z * cv$da$c_sd
  b_l = cv$da$b - z * cv$da$d_sd
  a_u = cv$da$a + z * cv$da$c_sd
  b_u = cv$da$b + z * cv$da$d_sd
  y2 = 12
  lim = sort(c(a_l / (1 - b_l / 2) + (1 + b_l / 2) / (1 - b_l / 2) * y2,
               a_u / (1 - b_u / 2) + (1 + b_u / 2) / (1 - b_u / 2) * y2))
  pr = predict(cv, newdata = y2, from = "B", to = "A")
  expect_equal(c(pr$lower, pr$upper), lim)
})

test_that("slopes outside (-2, 2) are rejected", {
  da = list(a = 0, b = 2.5, vcov = diag(2), df = 10, s = 1, d_sd = 0)
  expect_error(mc_da_conversion(da, "A", "B", 0.95), "between -2 and 2")
  par = mc_limits_par("da", to = "A", from = "B", m1 = "A", m2 = "B",
                      a = 0, b = 1.9, c_sd = 1, d_sd = 0.5)
  expect_error(mc_conv_lines(par, z = 1.96), "not defined at this level")
})

# constant bias from method_vc() ----
test_that("conversion from a method_vc() fit has slope 1 and the biases", {
  dat = sim_methods(n_items = 30, n_reps = 2, methods = c("A", "B", "C"),
                    alpha = c(0, 0.5, -0.3), tau = 0.3, omega = 0.2,
                    sigma = c(0.3, 0.4, 0.5), replicates = "linked",
                    seed = 2)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  cv = method_convert(object = fit)
  expect_equal(cv$fit_info$bias, "constant")
  cnv = cv$conversion
  expect_equal(nrow(cnv), 9)
  expect_true(all(cnv$beta == 1))
  for (j in seq_len(nrow(fit$bias))) {
    m = fit$bias$method1[j]
    k = fit$bias$method2[j]
    row_mk = cnv[cnv$to == m & cnv$from == k, ]
    row_km = cnv[cnv$to == k & cnv$from == m, ]
    expect_equal(row_mk$alpha, fit$bias$bias[j])
    expect_equal(row_km$alpha, -fit$bias$bias[j])
    expect_equal(row_mk$sd_pred, fit$limits$sd[j])
    expect_equal(c(row_mk$alpha_lower, row_mk$alpha_upper),
                 c(fit$bias$lower.CL[j], fit$bias$upper.CL[j]))
  }
  sig = fit$varcomp$sd[fit$varcomp$component == "sigma"]
  expect_equal(cnv$sd_pred[cnv$to == cnv$from], sqrt(2) * sig)

  # prediction limits are alpha +/- z * sd
  pr = predict(cv, newdata = c(1, 2), from = "A", to = "B")
  row = cnv[cnv$to == "B" & cnv$from == "A", ]
  expect_equal(pr$fit, c(1, 2) + row$alpha)
  expect_equal(pr$upper - pr$fit, rep(qnorm(0.975) * row$sd_pred, 2))
  # same method: +/- z * sqrt(2) * sigma
  pr_aa = predict(cv, newdata = 1, from = "A", to = "A")
  expect_equal(pr_aa$upper - 1, qnorm(0.975) * sqrt(2) * sig[1])

  # predict() on the method_vc() fit delegates
  expect_equal(predict(fit, newdata = c(1, 2), from = "A", to = "B"), pr)
})

test_that("predict back-transforms and respects the domain", {
  dat = sim_methods(n_items = 40, methods = c("A", "B"), mu_mean = 1,
                    alpha = c(0, 0.2), sigma = 0.2, transform = "pctlogit",
                    seed = 5)
  cv = method_convert(dat, y = "y", method = "method", id = "id",
                      transform = "pctlogit")
  pr = predict(cv, newdata = c(10, 50, 90, 99.9, 120), from = "A",
               to = "B")
  expect_named(pr, c("from_value", "fit", "lower", "upper"))
  ok = !is.na(pr$fit)
  expect_equal(ok, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_true(all(pr$lower[ok] > 0 & pr$upper[ok] < 100))
  expect_true(all(pr$lower[ok] < pr$fit[ok] & pr$fit[ok] < pr$upper[ok]))

  # data frame newdata and a different level
  pr2 = predict(cv, newdata = data.frame(A = c(10, 50)), from = "A",
                to = "B", level = 0.8)
  expect_true(all(pr2$upper - pr2$lower < pr$upper[1:2] - pr$lower[1:2]))
  expect_error(predict(cv, newdata = data.frame(x = 1), from = "A",
                       to = "B"),
               "column named 'A'")
  expect_error(predict(cv, newdata = 1, from = "A", to = "Z"), "methods")
  # default newdata: the observed values of the from method
  expect_equal(nrow(predict(cv)), 40)
})

test_that("input validation", {
  dat = conv_data()
  expect_error(method_convert(y = "y", method = "method", id = "id"),
               "Supply `data`")
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              replicate = "repl", ci_method = "boot"),
               "only available")
  expect_error(method_convert(object = list()), "simple_mvc")
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              replicate = "repl", conf_level = 2),
               "conf_level")
})

test_that("objects survive saveRDS/readRDS", {
  cv = method_convert(conv_data(), y = "y", method = "method", id = "id",
                      replicate = "repl", transform = "log", pair_seed = 1)
  f = tempfile(fileext = ".rds")
  saveRDS(cv, f)
  rm(cv)
  cv2 = readRDS(f)
  expect_output(print(cv2), "Linear Bias")
  expect_s3_class(plot(cv2), "ggplot")
  expect_equal(nrow(predict(cv2, newdata = c(5, 10))), 2)
  unlink(f)
})

# scaled model ----
test_that("oximetry, scaled model, reproduces MethComp AltReg", {
  skip_if_not(file.exists(test_path("fixtures", "ox.rds")))
  ox = readRDS(test_path("fixtures", "ox.rds"))
  sc = method_convert(ox, y = "y", method = "meth", id = "item",
                      replicate = "repl", replicates = "linked",
                      bias = "scaled")
  expect_true(sc$fit_info$converged)
  cv = sc$conversion
  get = function(to, from, col) cv[[col]][cv$to == to & cv$from == from]
  close = function(x, y) expect_true(abs(x - y) <= 0.002,
                                     info = paste(x, "vs", y))
  close(get("CO", "pulse", "alpha"), -2.257)
  close(get("CO", "pulse", "beta"), 1.065)
  close(get("pulse", "CO", "alpha"), 2.120)
  close(get("pulse", "CO", "beta"), 0.939)
  close(get("CO", "CO", "sd_pred"), 2.899)
  close(get("CO", "pulse", "sd_pred"), 6.390)
  close(get("pulse", "CO", "sd_pred"), 6.002)
  close(get("pulse", "pulse", "sd_pred"), 5.772)

  vc = sc$varcomp
  vget = function(comp, m) vc$sd[vc$component == comp & vc$method == m]
  ref = list(omega = c(3.524, 3.310), tau = c(2.979, 2.798),
             sigma = c(2.050, 4.081))
  for (comp in names(ref)) {
    close(vget(comp, "CO"), ref[[comp]][1])
    close(vget(comp, "pulse"), ref[[comp]][2])
  }
  # y-scale components have the ratio of the slopes
  expect_equal(vget("omega", "CO") / vget("omega", "pulse"),
               get("CO", "pulse", "beta"))

  # per-method parameters, first method as reference
  expect_equal(sc$scaled$methods$beta, c(1, get("pulse", "CO", "beta")))
  expect_equal(sc$scaled$methods$alpha[1], 0)
})

test_that("scaled model recovers non-unit slopes", {
  dat = sim_methods(n_items = 200, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 3, tau = 0.2, sigma = c(0.2, 0.5), seed = 8)
  sc = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", bias = "scaled", tol = 1e-5)
  cv = sc$conversion
  expect_equal(cv$beta[cv$to == "B" & cv$from == "A"], 1.2,
               tolerance = 0.03)
  expect_equal(cv$alpha[cv$to == "B" & cv$from == "A"], 1,
               tolerance = 0.3)
  expect_equal(sc$varcomp$sd[sc$varcomp$component == "sigma"],
               c(0.2, 0.5), tolerance = 0.1)
  # delta-method intervals
  expect_true(all(cv$beta_lower[cv$to != cv$from] <
                    cv$beta[cv$to != cv$from]))
  # conversions in both directions are inverse
  b_ba = cv$beta[cv$to == "B" & cv$from == "A"]
  b_ab = cv$beta[cv$to == "A" & cv$from == "B"]
  expect_equal(b_ab, 1 / b_ba)
  a_ba = cv$alpha[cv$to == "B" & cv$from == "A"]
  expect_equal(cv$alpha[cv$to == "A" & cv$from == "B"], -a_ba / b_ba)
  # prediction uses the conversion line
  pr = predict(sc, newdata = 10, from = "A", to = "B")
  expect_equal(pr$fit, a_ba + b_ba * 10)
  sd_ba = cv$sd_pred[cv$to == "B" & cv$from == "A"]
  expect_equal(pr$upper - pr$fit, qnorm(0.975) * sd_ba)
})

test_that("scaled conversions do not depend on the method order", {
  dat = sim_methods(n_items = 30, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.1), mu_mean = 10,
                    mu_sd = 2, tau = 0.2, sigma = 0.3, seed = 3)
  s1 = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", bias = "scaled")
  dat$method = factor(dat$method, levels = c("B", "A"))
  s2 = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", bias = "scaled")
  c1 = s1$conversion[order(s1$conversion$to, s1$conversion$from), ]
  c2 = s2$conversion[order(s2$conversion$to, s2$conversion$from), ]
  expect_equal(c1$beta, c2$beta, tolerance = 1e-4)
  expect_equal(c1$alpha, c2$alpha, tolerance = 1e-3)
  expect_equal(c1$sd_pred, c2$sd_pred, tolerance = 1e-4)
})

test_that("scaled model bootstrap", {
  dat = sim_methods(n_items = 15, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 3, tau = 0.2, omega = 0.2, sigma = 0.3,
                    replicates = "linked", seed = 2)
  set.seed(10)
  sb = suppressMessages(
    method_convert(dat, y = "y", method = "method", id = "id",
                   replicate = "repl", replicates = "linked",
                   bias = "scaled", tol = 1e-4, ci_method = "boot",
                   boot_reps = 8)
  )
  fi = sb$fit_info
  expect_equal(fi$ci_method, "boot")
  expect_equal(fi$boot_reps, 8)
  expect_equal(nrow(sb$boot), 8 - fi$n_boot_fail)
  cv = sb$conversion
  off = cv$to != cv$from
  expect_true(all(is.finite(c(cv$alpha_lower[off], cv$beta_upper[off],
                              cv$sd_lower, sb$varcomp$upper))))
  expect_true(all(cv$sd_lower <= cv$sd_upper))
  expect_output(print(sb), "parametric bootstrap")
})

test_that("scaled model input checks and convergence warning", {
  dat = sim_methods(n_items = 20, methods = c("A", "B"), seed = 1)
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              bias = "scaled"),
               "requires replicate")
  dat2 = sim_methods(n_items = 20, n_reps = 2, tau = 0.2, beta = c(1, 1.3),
                     mu_mean = 10, mu_sd = 2, seed = 1)
  expect_error(method_convert(dat2, y = "y", method = "method", id = "id",
                              replicate = "repl", bias = "scaled",
                              variance = "linear"),
               "only available with bias")
  expect_warning(method_convert(dat2, y = "y", method = "method", id = "id",
                                replicate = "repl", bias = "scaled",
                                max_iter = 2),
                 "did not converge")
})

test_that("sim_methods uses fixed item values", {
  mu = c(1, 5, 10)
  d = sim_methods(n_items = 3, methods = c("A", "B"), sigma = 0,
                  mu_values = mu, seed = 1)
  expect_equal(d$y[d$method == "A"], mu)
  expect_error(sim_methods(n_items = 3, mu_values = 1:2), "mu_values")
})

# scaled model, maximum likelihood (lavaan) ----
# Reference values from data-raw/sims/lavaan_ox_compare.R (Phase 1 of
# data-raw/sims/HANDOFF_method_convert_scaled_ci.md).
test_that("oximetry, scaled model by maximum likelihood", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not(file.exists(test_path("fixtures", "ox.rds")))
  ox = readRDS(test_path("fixtures", "ox.rds"))
  sc = method_convert(ox, y = "y", method = "meth", id = "item",
                      replicate = "repl", replicates = "linked",
                      bias = "scaled", estimator = "ml")
  fi = sc$fit_info
  expect_identical(fi$estimator, "ml")
  expect_true(fi$converged)
  expect_length(fi$heywood, 0)
  expect_equal(fi$logl, -1144.113, tolerance = 1e-5)
  expect_equal(unname(fi$fit_measures[c("chisq", "df")]), c(59.753, 19),
               tolerance = 1e-4)
  expect_s4_class(sc$model, "lavaan")

  cv = sc$conversion
  get = function(to, from, col) cv[[col]][cv$to == to & cv$from == from]
  expect_equal(get("pulse", "CO", "beta"), 0.897479, tolerance = 1e-5)
  expect_equal(get("pulse", "CO", "alpha"), 5.28364, tolerance = 1e-5)
  expect_equal(get("pulse", "CO", "sd_pred"), 5.81531, tolerance = 1e-5)
  expect_equal(get("pulse", "CO", "beta_se"), 0.0512259, tolerance = 1e-4)
  expect_equal(get("CO", "pulse", "beta"), 1.114232, tolerance = 1e-5)
  expect_equal(get("CO", "pulse", "beta_se"), 0.0635976, tolerance = 1e-4)

  # the two directions are consistent
  expect_equal(get("CO", "pulse", "beta"), 1 / get("pulse", "CO", "beta"))
  expect_equal(get("CO", "pulse", "alpha"),
               -get("pulse", "CO", "alpha") * get("CO", "pulse", "beta"))

  # t intervals with items - 1 df; sd_pred on the log scale
  off = cv$to != cv$from
  expect_equal(cv$df[off], rep(60, 2))
  q = qt(0.975, 60)
  expect_equal(cv$beta_lower[off], cv$beta[off] - q * cv$beta_se[off])
  expect_equal(unname(confint(sc, parm = "beta")[, 1]), cv$beta_lower[off])
  expect_true(all(cv$sd_lower[off] < cv$sd_pred[off] &
                    cv$sd_pred[off] < cv$sd_upper[off]))
  expect_equal(log(cv$sd_upper[off]) - log(cv$sd_pred[off]),
               log(cv$sd_pred[off]) - log(cv$sd_lower[off]))
  # same table layout as the alternating regression
  alt = method_convert(ox, y = "y", method = "meth", id = "item",
                       replicate = "repl", replicates = "linked",
                       bias = "scaled")
  expect_identical(names(cv), names(alt$conversion))
  expect_identical(cv[, c("to", "from")], alt$conversion[, c("to", "from")])

  # y-scale method-by-item SDs have the ratio of the slopes
  vc = sc$varcomp
  tau = vc$sd[vc$component == "tau"]
  expect_equal(tau[2] / tau[1], get("pulse", "CO", "beta"))
  expect_equal(sc$scaled$methods$beta, c(1, get("pulse", "CO", "beta")))

  # transformed scale
  tr = method_convert(ox, y = "y", method = "meth", id = "item",
                      replicate = "repl", replicates = "linked",
                      bias = "scaled", estimator = "ml",
                      transform = "pctlogit")
  pr = predict(tr, newdata = c(60, 75, 90), from = "CO", to = "pulse")
  expect_true(all(pr$lower > 0 & pr$upper < 100 & pr$lower < pr$fit))
})

test_that("ML fit with three methods and unbalanced data", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  sim3 = function(rp) {
    sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
                alpha = c(0, 1, -0.5), beta = c(1, 1.2, 0.9), mu_mean = 10,
                mu_sd = 2, tau = 0.3, omega = if (rp == "linked") 0.3 else 0,
                sigma = c(0.3, 0.4, 0.5), replicates = rp, p_missing = 0.15,
                seed = 1)
  }
  # values agree with the Phase 1 prototype
  # (data-raw/sims/lavaan_scaled_helpers.R) to 1e-6
  ref = list(
    exchangeable = list(beta = c(0.84518180, 1.19492758, 1.18317739,
                                 1.41381130, 0.83687080, 0.70730797),
                        alpha_AB = -1.036876312, logl = -199.3179),
    # with lavaan's default starting values this one converged to a local
    # optimum with a negative variance of the true values
    linked = list(beta = c(0.80540055, 1.14227717, 1.24161822,
                           1.41827215, 0.87544427, 0.70508329),
                  alpha_AB = -0.42803875, logl = -237.4163)
  )
  for (rp in names(ref)) {
    sc = expect_no_warning(
      method_convert(sim3(rp), y = "y", method = "method", id = "id",
                     replicate = "repl", replicates = rp, bias = "scaled",
                     estimator = "ml")
    )
    cv = sc$conversion[sc$conversion$to != sc$conversion$from, ]
    expect_equal(cv$beta, ref[[rp]]$beta, tolerance = 1e-5)
    expect_equal(cv$alpha[1], ref[[rp]]$alpha_AB, tolerance = 1e-5)
    expect_equal(sc$fit_info$logl, ref[[rp]]$logl, tolerance = 1e-5)
    expect_true(all(sc$varcomp$sd > 0))
    expect_length(sc$diagnostics$resid, nrow(sc$data))
  }
})

test_that("ML fit warns about zero or negative variances", {
  skip_if_not_installed("lavaan")
  dat = sim_methods(n_items = 15, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 2, tau = 0.05, sigma = 0.3, seed = 1)
  expect_warning(
    sc <- method_convert(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", bias = "scaled",
                         estimator = "ml"),
    "zero or negative"
  )
  expect_identical(sc$fit_info$heywood, "t2")
  expect_equal(sc$varcomp$sd[sc$varcomp$component == "tau"], c(0, 0))
  expect_true(any(grepl("Zero or negative", capture.output(print(sc)))))
})

test_that("ML estimator input checks", {
  dat = conv_data()
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              replicate = "repl", estimator = "ml"),
               "only available for bias = 'scaled'")
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  expect_error(method_convert(object = fit, bias = "scaled",
                              estimator = "ml"),
               "only available for bias = 'scaled'")
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              replicate = "repl", bias = "scaled",
                              estimator = "ml", ci_method = "boot"),
               "only available with estimator = 'altreg'")
  local_mocked_bindings(mc_has_lavaan = function() FALSE)
  expect_error(method_convert(dat, y = "y", method = "method", id = "id",
                              replicate = "repl", bias = "scaled",
                              estimator = "ml", tol = 1e-4),
               "requires the lavaan package")
})
