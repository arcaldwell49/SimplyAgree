mconv_fit = function(...) {
  dat = sim_methods(n_items = 40, n_reps = 2, methods = c("A", "B", "C"),
                    alpha = c(0, 1, -0.5), beta = c(1, 1.1, 0.9),
                    mu_mean = 10, mu_sd = 2, tau = 0.2, sigma = 0.3,
                    replicates = "linked", seed = 7)
  method_convert(dat, y = "y", method = "method", id = "id",
                 replicate = "repl", replicates = "linked", ...)
}

test_that("print shows the conversion table", {
  out = capture.output(print(mconv_fit()))
  expect_true(any(grepl("Linear Bias", out)))
  expect_true(any(grepl("cluster-robust", out)))
  expect_true(any(grepl("unequal", out)))

  out_l = capture.output(print(mconv_fit(variance = "linear")))
  expect_true(any(grepl("depends on the level", out_l)))
  expect_true(any(grepl("A - B: ", out_l, fixed = TRUE)))

  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  out_c = capture.output(print(method_convert(object = fit)))
  expect_true(any(grepl("Constant Bias", out_c)))
})

test_that("coef and confint", {
  cv = mconv_fit()
  cf = coef(cv)
  expect_length(cf, 12)
  expect_true(all(c("alpha[B|A]", "beta[B|A]", "beta[A|C]") %in% names(cf)))
  row = cv$conversion[cv$conversion$to == "B" & cv$conversion$from == "A", ]
  expect_equal(unname(cf["beta[B|A]"]), row$beta)

  ci = confint(cv)
  expect_equal(dim(ci), c(12, 2))
  expect_equal(unname(ci["beta[B|A]", ]), c(row$beta_lower, row$beta_upper))
  ci90 = confint(cv, parm = "beta", level = 0.9)
  expect_equal(nrow(ci90), 6)
  expect_true(all(ci90[, 2] - ci90[, 1] < ci[7:12, 2] - ci[7:12, 1]))
  expect_equal(colnames(ci90), c("5 %", "95 %"))
})

test_that("confint returns the bootstrap percentile intervals", {
  dat = sim_methods(n_items = 15, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)
  set.seed(3)
  sb = suppressMessages(
    method_convert(dat, y = "y", method = "method", id = "id",
                   replicate = "repl", bias = "scaled", tol = 1e-4,
                   ci_method = "boot", boot_reps = 20)
  )
  cv = sb$conversion
  off = which(cv$to != cv$from)
  ci = confint(sb)
  # matches the printed (stored) percentile intervals at the fitted level
  expect_equal(unname(ci[1:2, ]),
               unname(cbind(cv$alpha_lower[off], cv$alpha_upper[off])))
  expect_equal(unname(ci[3:4, ]),
               unname(cbind(cv$beta_lower[off], cv$beta_upper[off])))
  # other levels come from the same bootstrap estimates
  ci80 = confint(sb, parm = "beta", level = 0.8)
  n_cv = nrow(cv)
  expect_equal(unname(ci80[1, ]),
               unname(stats::quantile(sb$boot[, n_cv + off[1]],
                                      c(0.1, 0.9))))
  expect_equal(colnames(ci80), c("10 %", "90 %"))
})

test_that("conversion and Bland-Altman plots", {
  cv = mconv_fit()
  p = plot(cv)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "A")
  expect_equal(p$labels$y, "B")
  p2 = plot(cv, pair = c("C", "A"))
  expect_equal(p2$labels$x, "C")

  pb = plot(cv, type = "ba")
  expect_s3_class(pb, "ggplot")
  # bias and limits as coloured lines
  l1 = geom_data(pb, "GeomLine")
  expect_equal(levels(l1$text), c("Upper Limit", "Bias", "Lower Limit"))
  # reversing the pair negates the bias line and swaps the limits
  pb2 = plot(cv, type = "ba", pair = c("B", "A"))
  l2 = geom_data(pb2, "GeomLine")
  expect_equal(l2$estimate[l2$text == "Bias"],
               -l1$estimate[l1$text == "Bias"])
  expect_equal(l2$estimate[l2$text == "Upper Limit"],
               -l1$estimate[l1$text == "Lower Limit"])
  expect_error(plot(cv, pair = c("A", "A")), "two different methods")
  expect_equal(geom_data(plot(cv, type = "ba", delta = 1), "GeomHline")$y1,
               c(1, -1))

  # the sign of the slope in the equation
  expect_match(p$labels$caption, "^B = [-0-9.e]+ \\+ [0-9.e]+ \\* A")
  cv_neg = cv
  k = cv_neg$conversion$to == "B" & cv_neg$conversion$from == "A"
  cv_neg$conversion$beta[k] = -cv_neg$conversion$beta[k]
  expect_match(plot(cv_neg)$labels$caption, "^B = [-0-9.e]+ - [0-9.e]+ \\* A")

  # constant bias: the plot of the method_vc() fit, at the levels of the
  # conversion
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  cc = method_convert(object = fit)
  cols = c("text", "estimate", "lower.ci", "upper.ci")
  pc = plot(cc, type = "ba")
  expect_equal(geom_data(pc, "GeomPointrange")[cols],
               geom_data(plot(fit), "GeomPointrange")[cols])
  expect_equal(geom_data(pc, "GeomPointrange")$estimate[2], fit$bias$bias)
  fit90 = method_vc(dat, y = "y", method = "method", id = "id",
                    replicate = "repl", pred_level = 0.9, conf_level = 0.9)
  cc90 = method_convert(object = fit, pred_level = 0.9, conf_level = 0.9)
  expect_equal(geom_data(plot(cc90, type = "ba"), "GeomPointrange")[cols],
               geom_data(plot(fit90), "GeomPointrange")[cols])
  expect_s3_class(plot(cc), "ggplot")
})

test_that("variance = 'linear' warns when the fitted SD is not positive", {
  set.seed(3)
  mu = seq(1, 20, length.out = 60)
  s = ifelse(mu < 8, 1, 0.01)
  dat = data.frame(id = rep(1:60, 2), method = rep(c("A", "B"), each = 60),
                   y = c(mu + rnorm(60, 0, s), mu + rnorm(60, 0, s)))
  expect_warning(method_convert(dat, y = "y", method = "method", id = "id",
                                variance = "linear"),
                 "not positive")
})

test_that("summary and check", {
  cv = mconv_fit()
  s = summary(cv)
  expect_s3_class(s, "summary.simple_mconv")
  out = capture.output(print(s))
  expect_true(any(grepl("Regressions of differences", out)))
  expect_true(any(grepl("Pairs of measurements", out)))
  ch_l = check(cv)
  expect_s3_class(ch_l, "patchwork")
  pl = patch_plots(ch_l)
  expect_length(pl, 3)
  for (q in pl) {
    expect_false(is.null(q$labels$subtitle))
  }
  expect_match(pl[[1]]$labels$caption, "Shapiro-Wilk Test: A - B: p")
  expect_match(pl[[2]]$labels$caption, "Breusch-Pagan Test: A - B: p")
  expect_match(pl[[3]]$labels$caption, "RESET Test: A - B: p")
  # spread and linearity use item means (40 items, 3 pairs)
  expect_equal(nrow(pl[[2]]$data), 3 * 40)
  expect_equal(nrow(pl[[3]]$data), 3 * 40)
  expect_s3_class(check(mconv_fit(variance = "linear")), "patchwork")

  dat = sim_methods(n_items = 25, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 2, tau = 0.2, sigma = 0.3, seed = 5)
  sc = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", bias = "scaled", tol = 1e-4)
  out_s = capture.output(print(summary(sc)))
  expect_true(any(grepl("Alternating regression", out_s)))
  expect_true(any(grepl("Methods on the scale", out_s)))
  out_p = capture.output(print(sc))
  expect_true(any(grepl("Scaled Model", out_p)))
  expect_true(any(grepl("delta method", out_p)))
  ch = check(sc)
  expect_s3_class(ch, "patchwork")
  pl_s = patch_plots(ch)
  for (q in pl_s) {
    expect_false(is.null(q$labels$subtitle))
  }
  expect_match(pl_s[[1]]$labels$caption, "Shapiro-Wilk Test: A: p")
  expect_match(pl_s[[2]]$labels$caption, "Breusch-Pagan Test: A: p")
  expect_match(pl_s[[3]]$labels$caption, "converged")
  expect_equal(nrow(sc$scaled$trace), 2 * sc$fit_info$iterations)

  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  expect_error(check(method_convert(object = fit)), "use check")
})

test_that("Bland-Altman plot for the scaled model", {
  dat = sim_methods(n_items = 25, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 2, tau = 0.2, sigma = 0.3, seed = 5)
  sc = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", bias = "scaled", tol = 1e-4)
  p = plot(sc, type = "ba")
  l = geom_data(p, "GeomLine")
  l = l[l$text == "Bias", ]
  cv = sc$conversion
  b = cv$beta[cv$to == "A" & cv$from == "B"]
  a = cv$alpha[cv$to == "A" & cv$from == "B"]
  # D = A - B = 2a/(1+b) + 2(b-1)/(1+b) * avg
  expect_equal(l$estimate, 2 * a / (1 + b) + 2 * (b - 1) / (1 + b) * l$x)
  expect_s3_class(plot(sc), "ggplot")
})
