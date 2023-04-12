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
