# Proportional bias diagnostics in check() (prop_bias_helpers.R)

pb_caption = function(p) {
  ggplot2::get_labs(p)$caption
}

# true values with unequal measurement error and no proportional bias
pb_reps_data = function(n = 30, k = 3, sx = 3, sy = 10, omega = 0,
                        beta = 1, seed = 1) {
  if (!is.null(seed)) set.seed(seed)
  tv = rlnorm(n, log(100), 0.1)
  id = rep(seq_len(n), each = k)
  occ = rnorm(n * k, 0, omega)
  data.frame(id = id,
             repl = rep(seq_len(k), n),
             x = tv[id] + occ + rnorm(n * k, 0, sx),
             y = 100 * (1 - beta) + beta * tv[id] + occ + rnorm(n * k, 0, sy))
}

pb_simple_data = function(n = 100, sx = 1, sy = 10, seed = 2) {
  set.seed(seed)
  tv = rnorm(n, 100, 10)
  data.frame(x = tv + rnorm(n, 0, sx), y = tv + rnorm(n, 0, sy))
}

test_that("uncorrected test is the Pitman-Morgan test", {
  d = pb_simple_data(n = 40, sx = 3, sy = 10)
  res = SimplyAgree:::.pm_slope_test(d$x, d$y)
  ct = cor.test(d$x - d$y, d$x + d$y)
  expect_equal(res$statistic, unname(ct$statistic), tolerance = 1e-10)
  expect_equal(res$parameter, 38)
  expect_equal(res$p.value, ct$p.value, tolerance = 1e-10)
  expect_equal(res$slope_obs,
               unname(coef(lm(I(d$x - d$y) ~ I((d$x + d$y) / 2)))[2]),
               tolerance = 1e-10)

  # subject means
  r = pb_reps_data()
  res2 = SimplyAgree:::.pm_slope_test(r$x, r$y, r$id)
  sm = aggregate(cbind(x, y) ~ id, r, mean)
  expect_equal(res2$p.value, cor.test(sm$x - sm$y, sm$x + sm$y)$p.value,
               tolerance = 1e-10)
  expect_false(res2$corrected)
})

test_that("check.loa tests on the analysis scale, also with prop_bias", {
  d = pb_simple_data(n = 40, sx = 3, sy = 10)
  pm = cor.test(d$x - d$y, d$x + d$y)$p.value
  a = agreement_limit(x = "x", y = "y", data = d)
  pn = SimplyAgree:::.loa_check_panels(a)
  expect_equal(pn$pb$p.value, pm, tolerance = 1e-10)
  expect_match(pb_caption(pn$p_bias), "Pitman-Morgan")

  # prop_bias = TRUE no longer compares two identical models
  ap = agreement_limit(x = "x", y = "y", data = d, prop_bias = TRUE)
  expect_equal(SimplyAgree:::.loa_check_panels(ap)$pb$p.value, pm,
               tolerance = 1e-10)

  # log scale
  al = agreement_limit(x = "x", y = "y", data = d, log_tf = TRUE)
  expect_equal(SimplyAgree:::.loa_check_panels(al)$pb$p.value,
               cor.test(log(d$x) - log(d$y), log(d$x) + log(d$y))$p.value,
               tolerance = 1e-10)
})

test_that("corrected statistic matches a hand computation", {
  r = data.frame(id = rep(1:5, each = 2),
                 x = c(10, 11, 14, 13, 9, 9.5, 20, 21, 16, 15),
                 y = c(10.5, 12, 13, 15, 8, 10, 22, 19, 15, 18))
  res = SimplyAgree:::.pm_slope_test(r$x, r$y, r$id, correct = TRUE)

  stat = function(dd) {
    xb = tapply(dd$x, dd$id, mean)
    yb = tapply(dd$y, dd$id, mean)
    m = tapply(dd$x, dd$id, length)
    sw2x = sum(tapply(dd$x, dd$id, function(v) sum((v - mean(v))^2))) /
      sum(m - 1)
    sw2y = sum(tapply(dd$y, dd$id, function(v) sum((v - mean(v))^2))) /
      sum(m - 1)
    cov(xb - yb, xb + yb) - (sw2x - sw2y) * mean(1 / m)
  }
  theta = stat(r)
  jk = sapply(1:5, function(i) stat(r[r$id != i, ]))
  se = sqrt(4 / 5 * sum((jk - mean(jk))^2))

  expect_true(res$corrected)
  expect_equal(res$theta, theta, tolerance = 1e-10)
  expect_equal(res$se, se, tolerance = 1e-10)
  expect_equal(res$parameter, 4)
  expect_equal(res$p.value, 2 * pt(-abs(theta / se), 4), tolerance = 1e-10)
})

test_that("agreement_limit (reps) and method_vc give the same corrected test", {
  r = pb_reps_data(n = 20)
  a = agreement_limit(x = "x", y = "y", id = "id", data = r,
                      data_type = "reps")
  pb = SimplyAgree:::.loa_check_panels(a)$pb
  expect_true(pb$corrected)
  expect_match(pb_caption(SimplyAgree:::.loa_check_panels(a)$p_bias),
               "Error-corrected")

  long = mc_long(r, methods = c("x", "y"), id = "id", replicate = "repl")
  fit = method_vc(long, y = "y", method = "method", id = "id",
                  replicate = "repl")
  p = suppressMessages(check(fit, pair_seed = 1))
  expect_match(pb_caption(p[[4]]),
               SimplyAgree:::.fmt_test_p(pb$p.value), fixed = TRUE)
})

test_that("check() messages explain what the slope can and cannot show", {
  d = pb_simple_data()
  a = agreement_limit(x = "x", y = "y", data = d)
  expect_message(check(a), "unequal measurement error")
  expect_message(check(tolerance_limit(x = "x", y = "y", data = d)),
                 "unequal measurement error")

  # replicates: the slope is explained by the unequal error variances
  r = pb_reps_data(n = 60, k = 2)
  ar = agreement_limit(x = "x", y = "y", id = "id", data = r,
                       data_type = "reps")
  pb = SimplyAgree:::.loa_check_panels(ar)$pb
  expect_lt(pb$p.uncorrected, 0.05)
  expect_gt(pb$p.value, 0.05)
  expect_message(check(ar), "consistent with unequal measurement error")

  # replicates with a true proportional bias
  rb = pb_reps_data(n = 60, sx = 3, sy = 3, beta = 1.3)
  ab = agreement_limit(x = "x", y = "y", id = "id", data = rb,
                       data_type = "reps")
  expect_message(check(ab), "suggests a proportional bias")
})

test_that("error.ratio gives the Deming test of dem_reg()", {
  d = pb_simple_data(n = 40, sx = 3, sy = 10)
  a = agreement_limit(x = "x", y = "y", data = d)
  pb = SimplyAgree:::.loa_check_panels(a, error.ratio = 0.09)$pb
  dr = dem_reg(y ~ x, data = d, error.ratio = 0.09)
  expect_equal(pb$beta, unname(coef(dr)[2]), tolerance = 1e-8)
  expect_equal(pb$p.value, dr$model_table$p.value[2], tolerance = 1e-8)
  expect_equal(pb$slope_adj, 2 * (1 - pb$beta) / (1 + pb$beta))
  expect_s3_class(suppressMessages(check(a, error.ratio = 0.09)), "patchwork")

  # ignored without a single-measurement design
  r = pb_reps_data(n = 10)
  ar = agreement_limit(x = "x", y = "y", id = "id", data = r,
                       data_type = "reps")
  expect_message(check(ar, error.ratio = 2), "ignored")
  expect_message(check(tolerance_limit(x = "x", y = "y", id = "id",
                                       data = r),
                       error.ratio = 2), "ignored")
  long = mc_long(r, methods = c("x", "y"), id = "id", replicate = "repl")
  fit = suppressWarnings(method_vc(long, y = "y", method = "method",
                                   id = "id", replicate = "repl"))
  expect_message(check(fit, error.ratio = 2), "ignored")

  expect_error(check(a, error.ratio = -1), "positive")
  expect_error(check(a, error.ratio = c(1, 2)), "positive")
})

test_that("check() output carries the proportional bias results", {
  r = pb_reps_data(n = 20)
  a = agreement_limit(x = "x", y = "y", id = "id", data = r,
                      data_type = "reps")
  tab = attr(suppressMessages(check(a)), "prop_bias")
  pb = SimplyAgree:::.loa_check_panels(a)$pb
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 1)
  expect_true(tab$corrected)
  expect_equal(tab$p.value, pb$p.value)
  expect_equal(tab$beta, (2 - pb$slope_adj) / (2 + pb$slope_adj))
  expect_equal(c(tab$sw2_x, tab$sw2_y), unname(pb$sw2))

  # nested data: likelihood ratio test on 1 df
  an = agreement_limit(x = "x", y = "y", id = "id", data = r,
                       data_type = "nest")
  tn = attr(suppressMessages(check(an)), "prop_bias")
  expect_equal(tn$df, 1)
  expect_false(tn$corrected)

  # one row per pair or condition
  long = mc_long(r, methods = c("x", "y"), id = "id", replicate = "repl")
  fit = method_vc(long, y = "y", method = "method", id = "id",
                  replicate = "repl")
  expect_equal(attr(suppressMessages(check(fit, pair_seed = 1)),
                    "prop_bias")$group, "x - y")
  r$cond = rep(c("A", "B"), length.out = nrow(r))
  t2 = tolerance_limit(x = "x", y = "y", data = r, condition = "cond")
  expect_equal(attr(suppressMessages(check(t2)), "prop_bias")$group,
               c("A", "B"))
})

test_that("check.tolerance_delta works without the model and by condition", {
  d = pb_simple_data(n = 40, sx = 3, sy = 3)
  t1 = tolerance_limit(x = "x", y = "y", data = d, keep_model = FALSE)
  expect_s3_class(check(t1), "patchwork")

  d$cond = rep(c("A", "B"), 20)
  t2 = tolerance_limit(x = "x", y = "y", data = d, condition = "cond")
  p = suppressMessages(check(t2))
  expect_match(pb_caption(p[[3]]), "A: Pitman-Morgan")
  expect_match(pb_caption(p[[3]]), "B: Pitman-Morgan")
})

test_that("corrected test holds its level under unequal error", {
  skip_on_cran()
  set.seed(42)
  # exchangeable and linked (shared occasion effect) replicates
  for (omega in c(0, 6)) {
    rej = replicate(300, {
      r = pb_reps_data(n = 30, k = 3, sx = 3, sy = 10, omega = omega,
                       seed = NULL)
      SimplyAgree:::.pm_slope_test(r$x, r$y, r$id, correct = TRUE)$p.value <
        0.05
    })
    expect_lt(mean(rej), 0.08)
  }
})
