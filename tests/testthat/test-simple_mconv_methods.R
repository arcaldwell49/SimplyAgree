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
  # reversing the pair negates the bias line
  pb2 = plot(cv, type = "ba", pair = c("B", "A"))
  l1 = pb$layers[[2]]$data
  l2 = pb2$layers[[2]]$data
  expect_equal(l2$bias, -l1$bias)
  expect_error(plot(cv, pair = c("A", "A")), "two different methods")

  # constant bias
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  cc = method_convert(object = fit)
  pc = plot(cc, type = "ba")
  expect_equal(unique(pc$layers[[2]]$data$bias), fit$bias$bias)
  expect_s3_class(plot(cc), "ggplot")
})
