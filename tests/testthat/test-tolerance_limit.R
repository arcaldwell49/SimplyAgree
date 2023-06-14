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
  agreement_limit(data = temps2,
                  x = "x",
                  y = "y")
  print(test1)
  check(test1)
  plot(test1)


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
