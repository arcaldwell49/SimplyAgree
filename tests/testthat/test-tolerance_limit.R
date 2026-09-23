context("tolerance_limit")

# Test that the function returns a tolerance_delta class
test_that("tolerance_limit returns a tolerance_delta class", {
  # generic
  expect_s3_class(tolerance_limit(data = mtcars, x = "mpg", y = "disp"), "tolerance_delta")

  # all iterations
  data(temps)
  temps2 = temps
  temps2$x = temps$trec_pre
  temps2$y = temps$teso_pre
  temps2$condition = temps$tod
  temps2$ts = as.numeric(temps$trial_num)
  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                  x = "x",
                                  y = "y",
                                  tol_method = "perc",
                                  replicates = 20),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      keep_model = FALSE),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      cor_type = "ar1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      cor_type = "car1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      time = "ts",
                                      cor_type = "ar1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      time = "ts",
                                      cor_type = "car1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      condition = "condition",
                                      correlation = nlme::corAR1(form=~1|id),
                                      weights = nlme::varIdent(form=~condition)),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      condition = "condition",
                                      tol_method = "p",
                                      replicates = 20),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      log_tf = TRUE,
                                      prop_bias = TRUE),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                  x = "x",
                                  y = "y",
                                  id = "id",
                                  log_tf = TRUE,
                                  log_tf_display = "sym"),
                  "tolerance_delta")






})

test_that("check methods",{

  # all iterations
  data(temps)
  temps2 = temps
  temps2$x = temps$trec_pre
  temps2$y = temps$teso_pre
  temps2$condition = temps$tod
  temps2$ts = as.numeric(temps$trial_num)
  test1 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y")

  print(test1)
  check(test1)
  plot(test1,
       delta = 2)
  plot(test1, geom =  "geom_bin2d")
  plot(test1, geom =  "geom_density_2d")
  plot(test1, geom =  "geom_density_2d_filled")
  plot(test1, geom =  "stat_density_2d")

  expect_error(plot(test1, geom =  "geom_bar"))

  test1p = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          prop_bias =TRUE)

  print(test1p)
  check(test1p)
  plot(test1p,
       delta = 2)
  plot(test1p,
       delta = c(-1,2))
  plot(test1p, geom =  "geom_bin2d")
  plot(test1p, geom =  "geom_density_2d")
  plot(test1p, geom =  "geom_density_2d_filled")
  plot(test1p, geom =  "stat_density_2d")



  test2 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id")

  print(test2)
  check(test2)
  plot(test2)


  test3 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          cor_type = "ar1")

  print(test3)
  check(test3)
  plot(test3)

  test4 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          cor_type = "car1")

  print(test4)
  check(test4)
  plot(test4)

  test5 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          correlation = nlme::corAR1(form=~1|id),
                          weights = nlme::varIdent(form=~condition))

  print(test5)
  check(test5)
  plot(test5)

  test6 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          tol_method = "p",
                          replicates = 20)

  print(test6)
  check(test6)
  plot(test6)


  test7 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          log_tf = TRUE,
                          prop_bias = TRUE)

  testthat::expect_identical(class(test7), "tolerance_delta")
  print(test7)
  check(test7)
  plot(test7)

  test7_b = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          log_tf = TRUE,
                          log_tf_display = "sym",
                          prop_bias = TRUE)

  testthat::expect_identical(class(test7_b), "tolerance_delta")
  print(test7_b)
  check(test7_b)
  plot(test7_b)

  test8 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE)

  testthat::expect_identical(class(test8), "tolerance_delta")
  print(test8)
  check(test8)
  plot(test8)

  test9 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE,
                          tol_method = "perc",
                          replicates = 20,
                          )

  testthat::expect_identical(class(test8), "tolerance_delta")
  print(test9)
  check(test9)
  plot(test9)

  test10 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                         # condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE,
                          tol_method = "perc",
                          replicates = 20,
  )

  testthat::expect_identical(class(test10), "tolerance_delta")
  print(test10)
  check(test10)
  plot(test10)

  x = rnorm(5500)
  z = rnorm(5500,sd=.2)
  y = x+z

  df1 = data.frame(x,y,z)

  test_big = tolerance_limit(data = df1,
                          x = "x",
                          y = "y")

  check(test_big)

})


test_that("limits use the condition-specific residual SD", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y",
                       condition = "condition",
                       cor_type = "none")
  lim = t1$limits[order(t1$limits$condition), ]

  # separate means and variances: REML SDs equal the per-condition sample SDs
  emp_sd = tapply(d1$x - d1$y, d1$condition, sd)
  expect_equal(lim$SD, as.vector(emp_sd), tolerance = 1e-4)
  expect_equal(lim$SEP, sqrt(lim$SD^2 + lim$SEM^2), tolerance = 1e-8)

  width = lim$upper.PL - lim$lower.PL
  expect_gt(width[2] / width[1], 3.5)
})

test_that("limits follow a varPower variance function with prop_bias", {
  set.seed(1)
  d2 = data.frame(id = 1:200)
  d2$x = runif(200, 10, 200)
  d2$y = d2$x + rnorm(200, 0, 0.05 * d2$x)

  t2 = tolerance_limit(d2, x = "x", y = "y",
                       prop_bias = TRUE,
                       weights = nlme::varPower(form = ~avg),
                       cor_type = "none")
  lim = t2$limits[order(t2$limits$avg), ]
  pow = coef(t2$model$modelStruct$varStruct)

  expect_equal(lim$SD, sigma(t2$model) * lim$avg^pow, tolerance = 1e-6)
  expect_true(all(diff(lim$SD) > 0))
})

test_that("an avg-dependent variance expands the grid without prop_bias", {
  set.seed(1)
  d2 = data.frame(id = 1:200)
  d2$x = runif(200, 10, 200)
  d2$y = d2$x + rnorm(200, 0, 0.05 * d2$x)
  avg = (d2$x + d2$y) / 2

  t3 = tolerance_limit(d2, x = "x", y = "y",
                       weights = nlme::varPower(form = ~avg),
                       cor_type = "none")
  lim = t3$limits[order(t3$limits$avg), ]

  expect_equal(nrow(lim), 3)
  expect_equal(lim$avg, c(min(avg), median(avg), max(avg)))
  expect_equal(length(unique(lim$bias)), 1)
  expect_true(all(diff(lim$SEP) > 0))

  expect_output(print(t3))
  expect_s3_class(plot(t3), "ggplot")
})

test_that("perc limits use the condition-specific residual SD", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y",
                       condition = "condition",
                       cor_type = "none",
                       tol_method = "perc",
                       replicates = 20)
  lim = t1$limits[order(t1$limits$condition), ]

  expect_false(anyNA(lim$lower.TL))
  expect_false(anyNA(lim$upper.TL))
  width = lim$upper.TL - lim$lower.TL
  expect_gt(width[2] / width[1], 3)
})

test_that("returned model can be updated and re-queried", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y", id = "id",
                       condition = "condition")

  # evaluate away from the environment tolerance_limit() ran in
  refit_elsewhere = function(m) {
    e = new.env(parent = baseenv())
    e$m = m
    eval(quote(stats::update(m, . ~ .)), e)
  }
  refit = refit_elsewhere(t1$model)
  expect_equal(coef(refit), coef(t1$model))
  expect_equal(logLik(refit), logLik(t1$model))
  expect_equal(nrow(nlme::getData(t1$model)), nrow(d1))
  expect_identical(t1$model$call[[1]], quote(nlme::gls))

  # print should not deparse the whole data frame
  expect_lt(length(capture.output(print(t1$model))), 30)

  # emmeans can find the data without it being passed explicitly
  expect_s4_class(emmeans::emmeans(t1$model, ~ condition), "emmGrid")

  # user-supplied weights and correlation are stored too
  t2 = tolerance_limit(d1, x = "x", y = "y", id = "id",
                       weights = nlme::varPower(form = ~avg),
                       correlation = nlme::corAR1(form = ~1 | id))
  refit2 = refit_elsewhere(t2$model)
  expect_equal(logLik(refit2), logLik(t2$model))
})

test_that("conf_level sets the level of the bias CI", {
  set.seed(2)
  n = 40
  d1 = data.frame(x = rnorm(n, 100, 10))
  d1$y = d1$x + rnorm(n, 0, 1)

  t95 = tolerance_limit(d1, x = "x", y = "y", cor_type = "none")
  t90 = tolerance_limit(d1, x = "x", y = "y", cor_type = "none",
                        conf_level = 0.90)

  expect_equal(t95$limits$upper.CL - t95$limits$lower.CL,
               2 * qt(0.975, t95$limits$df) * t95$limits$SEM,
               tolerance = 1e-8)
  expect_equal(t90$limits$upper.CL - t90$limits$lower.CL,
               2 * qt(0.95, t90$limits$df) * t90$limits$SEM,
               tolerance = 1e-8)

  # prediction and tolerance limits are unaffected
  expect_equal(t90$limits[, c("lower.PL", "upper.PL", "lower.TL", "upper.TL")],
               t95$limits[, c("lower.PL", "upper.PL", "lower.TL", "upper.TL")])
  expect_output(print(t90), "90% CI for Bias")
})

test_that("Checked against BivRegBLS", {
  data(reps)
  # test2 = BivRegBLS::MD.horiz.lines(data = reps, xcol = "y", ycol = "x", pred.level = .95, .95)

  test1 = tolerance_limit(x = "x",
                          y = "y",
                          data = reps)

  expect_equal(test1$limits$bias, .4383,
               tolerance = .001)

  expect_equivalent(c(test1$limits$lower.PL,
                      test1$limits$upper.PL),
                    c(-2.199752,3.076419),
               tolerance = .001)

  expect_equivalent(test1$limits$bias, .4383,
               tolerance = .001)
} )
