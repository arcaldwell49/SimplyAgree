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
                              replicate = "repl", bias = "scaled"),
               "not available yet")
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
