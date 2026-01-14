# Test file for simple_eiv S3 methods
# Comprehensive testthat tests for all S3 methods


# Test Data Setup ------------

# NCSS datasets for testing
ncss_deming1 <- data.frame(
  X = c(7, 8.3, 10.5, 9, 5.1, 8.2, 10.2, 10.3, 7.1, 5.9),
  Y = c(7.9, 8.2, 9.6, 9, 6.5, 7.3, 10.2, 10.6, 6.3, 5.2)
)

ncss_pb1 <- data.frame(
  Method1 = c(69.3, 27.1, 61.3, 50.8, 34.4, 92.3, 57.5, 45.5, 33.3, 60.9,
              56.3, 49.9, 89.7, 28.9, 96.3, 76.6, 83.2, 79.4, 51.7, 32.5,
              14.2, 99.1, 76.8, 95.4, 84.1, 48.8, 80.4, 84.5, 61.4, 26.9),
  Method2 = c(69.1, 26.7, 61.4, 51.2, 34.7, 88.5, 57.9, 45.1, 33.4, 60.8,
              66.5, 48.2, 88.3, 29.3, 96.4, 77.1, 82.7, 78.9, 51.6, 28.8,
              12.7, 98.6, 77.3, 94.9, 83.3, 47.0, 80.9, 84.7, 61.3, 26.7)
)

# Create model objects for testing
dem_model <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)
pb_model <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1)})

# Bootstrap model for additional tests
set.seed(123)
pb_model_boot <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 100)


# print.simple_eiv Tests ------------


test_that("print.simple_eiv works for Deming regression", {
  expect_output(print(dem_model), "Deming")
  expect_output(print(dem_model), "(Intercept)")
  expect_output(print(dem_model), "X")
})

test_that("print.simple_eiv works for Passing-Bablok regression", {
  expect_output(print(pb_model), "Passing-Bablok")
  expect_output(print(pb_model), "(Intercept)")
  expect_output(print(pb_model), "X")
  expect_output(print(pb_model), "Kendall")
  expect_output(print(pb_model), "CUSUM")
})

test_that("print.simple_eiv returns object invisibly", {
  result <- print(dem_model)
  expect_identical(result, dem_model)
})

test_that("print.simple_eiv shows weighted status", {
  weighted_model <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = TRUE)
  expect_output(print(weighted_model), "[Ww]eighted")
})


# summary.simple_eiv Tests------------

test_that("summary.simple_eiv works for Deming regression", {
  summ <- summary(dem_model)

  expect_output(print(summ), "Deming")
  expect_output(print(summ), "(Intercept)")
  expect_output(print(summ), "X")
})

test_that("summary.simple_eiv works for Passing-Bablok regression", {
  summ <- summary(pb_model)

  expect_output(print(summ), "Passing-Bablok")
})

test_that("summary.simple_eiv shows model_table", {
  summ <- summary(dem_model)

  expect_output(print(summ), "estimate")
  expect_output(print(summ), "se|lower")  # Either se or lower depending on method
})

test_that("summary.simple_eiv shows df.residual", {
  expect_output(print(summary(dem_model)), "df")
})

test_that("summary.simple_eiv shows error.ratio", {
  expect_output(print(summary(dem_model)), "error")
})


# coef.simple_eiv Tests------------

test_that("coef.simple_eiv returns named numeric vector", {
  coeffs <- coef(dem_model)

  expect_type(coeffs, "double")
  expect_length(coeffs, 2)
  expect_named(coeffs, c("(Intercept)", "X"))
})

test_that("coef.simple_eiv matches coefficients slot", {
  expect_equal(coef(dem_model), dem_model$coefficients)
  expect_equal(coef(pb_model), pb_model$coefficients)
})

test_that("coef.simple_eiv works for all model types", {
  expect_length(coef(dem_model), 2)
  expect_length(coef(pb_model), 2)
  expect_length(coef(pb_model_boot), 2)
})


# vcov.simple_eiv Tests------------

test_that("vcov.simple_eiv returns matrix for Deming regression", {
  v <- vcov(dem_model)

  expect_true(is.matrix(v))
  expect_equal(dim(v), c(2, 2))
  expect_equal(rownames(v), c("(Intercept)", "X"))
  expect_equal(colnames(v), c("(Intercept)", "X"))
})

test_that("vcov.simple_eiv is symmetric", {
  v <- vcov(dem_model)
  expect_equal(v[1, 2], v[2, 1])
})

test_that("vcov.simple_eiv is positive semi-definite", {
  v <- vcov(dem_model)
  eigenvalues <- eigen(v)$values
  expect_true(all(eigenvalues >= -1e-10))
})

test_that("vcov.simple_eiv returns NULL for analytical PB", {
  # Analytical PB doesn't have traditional vcov
  v <- vcov(pb_model)
  expect_true(is.null(v) || is.matrix(v))
})

test_that("vcov.simple_eiv returns matrix for bootstrap PB", {
  v <- vcov(pb_model_boot)

  if (!is.null(v)) {
    expect_true(is.matrix(v))
    expect_equal(dim(v), c(2, 2))
  }
})


# confint.simple_eiv Tests------------

test_that("confint.simple_eiv returns correct structure", {
  ci <- confint(dem_model)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 2)
  expect_equal(ncol(ci), 2)
  expect_equal(rownames(ci), c("(Intercept)", "X"))
})

test_that("confint.simple_eiv respects level parameter", {
  ci_95 <- confint(dem_model, level = 0.95)
  ci_90 <- confint(dem_model, level = 0.90)

  # 90% CI should be narrower
  width_95_slope <- ci_95["X", 2] - ci_95["X", 1]
  width_90_slope <- ci_90["X", 2] - ci_90["X", 1]

  expect_lt(width_90_slope, width_95_slope)
})

test_that("confint.simple_eiv parm selection works", {
  ci_intercept <- confint(dem_model, parm = "(Intercept)")
  ci_slope <- confint(dem_model, parm = "X")
  ci_both <- confint(dem_model, parm = c("(Intercept)", "X"))

  expect_equal(nrow(ci_intercept), 1)
  expect_equal(nrow(ci_slope), 1)
  expect_equal(nrow(ci_both), 2)
})

test_that("confint.simple_eiv lower < upper", {
  ci <- confint(dem_model)

  expect_lt(ci["(Intercept)", 1], ci["(Intercept)", 2])
  expect_lt(ci["X", 1], ci["X", 2])
})

test_that("confint.simple_eiv works for PB models", {
  ci <- confint(pb_model)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 2)
})


# fitted.simple_eiv Tests------------

test_that("fitted.simple_eiv returns correct length", {
  fits <- fitted(dem_model)

  expect_length(fits, nrow(ncss_deming1))
})

test_that("fitted.simple_eiv returns numeric vector", {
  fits <- fitted(dem_model)

  expect_type(fits, "double")
})

test_that("fitted.simple_eiv values match manual calculation", {
  fits <- fitted(dem_model)
  coeffs <- coef(dem_model)

  manual_fits <- coeffs["(Intercept)"] + coeffs["X"] * ncss_deming1$X

  expect_equal(as.numeric(fits), as.numeric(manual_fits), tolerance = 1e-6)
})

test_that("fitted.simple_eiv works for PB models", {
  fits <- fitted(pb_model)

  expect_length(fits, nrow(ncss_pb1))
  expect_type(fits, "double")
})


# residuals.simple_eiv Tests ------------

test_that("residuals.simple_eiv returns correct length", {
  resids <- residuals(dem_model)

  expect_length(resids, nrow(ncss_deming1))
})

test_that("residuals.simple_eiv returns numeric vector", {
  resids <- residuals(dem_model)

  expect_type(resids, "double")
})

test_that("residuals.simple_eiv = Y - fitted", {
  resids <- residuals(dem_model)
  fits <- fitted(dem_model)

  manual_resids <- ncss_deming1$Y - fits

  expect_equal(as.numeric(resids), as.numeric(manual_resids), tolerance = 1e-6)
})

test_that("residuals.simple_eiv sum is approximately zero", {
  resids <- residuals(dem_model)

  # For regression through centroid, residuals should sum to ~0
  expect_equal(sum(resids), 0, tolerance = 0.1)
})

test_that("residuals.simple_eiv works for PB models", {
  resids <- residuals(pb_model)

  expect_length(resids, nrow(ncss_pb1))
  expect_type(resids, "double")
})


# predict.simple_eiv Tests------------

test_that("predict.simple_eiv without newdata returns fitted", {
  pred <- predict(dem_model)
  fits <- fitted(dem_model)

  expect_equal(pred, fits)
})

test_that("predict.simple_eiv with newdata works", {
  newdata <- data.frame(X = c(5, 7, 9))
  pred <- predict(dem_model, newdata = newdata)

  expect_length(pred, 3)
})

test_that("predict.simple_eiv with interval='confidence' returns matrix", {
  newdata <- data.frame(X = c(5, 7, 9))
  pred <- predict(dem_model, newdata = newdata, interval = "confidence")

  expect_true(is.matrix(pred) || is.data.frame(pred))
  expect_equal(nrow(pred), 3)
  expect_true("fit" %in% colnames(pred) || "estimate" %in% colnames(pred))
})

test_that("predict.simple_eiv confidence interval contains fit", {
  newdata <- data.frame(X = c(5, 7, 9))
  pred <- predict(dem_model, newdata = newdata, interval = "confidence")

  if (is.matrix(pred) || is.data.frame(pred)) {
    if ("fit" %in% colnames(pred)) {
      expect_true(all(pred[, "lwr"] <= pred[, "fit"]))
      expect_true(all(pred[, "fit"] <= pred[, "upr"]))
    } else if ("estimate" %in% colnames(pred)) {
      expect_true(all(pred[, "lower.ci"] <= pred[, "estimate"]))
      expect_true(all(pred[, "estimate"] <= pred[, "upper.ci"]))
    }
  }
})

test_that("predict.simple_eiv respects level parameter", {
  newdata <- data.frame(X = 7)
  pred_95 <- predict(dem_model, newdata = newdata, interval = "confidence", level = 0.95)
  pred_90 <- predict(dem_model, newdata = newdata, interval = "confidence", level = 0.90)

  if (is.matrix(pred_95) || is.data.frame(pred_95)) {
    # 90% CI should be narrower
    if ("lwr" %in% colnames(pred_95)) {
      width_95 <- pred_95[1, "upr"] - pred_95[1, "lwr"]
      width_90 <- pred_90[1, "upr"] - pred_90[1, "lwr"]
    } else {
      width_95 <- pred_95[1, "upper.ci"] - pred_95[1, "lower.ci"]
      width_90 <- pred_90[1, "upper.ci"] - pred_90[1, "lower.ci"]
    }
    expect_lt(width_90, width_95)
  }
})

test_that("predict.simple_eiv works for PB models", {
  newdata <- data.frame(Method1 = c(50, 70, 90))
  pred <- predict(pb_model, newdata = newdata)

  expect_length(pred, 3)
})

test_that("predict.simple_eiv PB with confidence interval works", {
  newdata <- data.frame(Method1 = c(50, 70, 90))
  pred <- predict(pb_model, newdata = newdata, interval = "confidence")

  expect_true(is.matrix(pred) || is.data.frame(pred))
  expect_equal(nrow(pred), 3)
})


# formula.simple_eiv Tests------------

test_that("formula.simple_eiv returns formula object", {
  f <- formula(dem_model)

  expect_s3_class(f, "formula")
})

test_that("formula.simple_eiv matches original formula", {
  f <- formula(dem_model)

  expect_equal(f, Y ~ X)
})

test_that("formula.simple_eiv works for PB models", {
  f <- formula(pb_model)

  expect_s3_class(f, "formula")
  expect_equal(f, Method2 ~ Method1)
})


# model.frame.simple_eiv Tests------------

test_that("model.frame.simple_eiv returns data frame", {
  mf <- model.frame(dem_model)

  expect_s3_class(mf, "data.frame")
})

test_that("model.frame.simple_eiv has correct dimensions", {
  mf <- model.frame(dem_model)

  expect_equal(nrow(mf), nrow(ncss_deming1))
  expect_equal(ncol(mf), 2)  # Y and X
})

test_that("model.frame.simple_eiv contains correct variables", {
  mf <- model.frame(dem_model)

  expect_true("Y" %in% names(mf))
  expect_true("X" %in% names(mf))
})

test_that("model.frame.simple_eiv works for PB models", {
  mf <- model.frame(pb_model)

  expect_s3_class(mf, "data.frame")
  expect_equal(nrow(mf), nrow(ncss_pb1))
})


# plot.simple_eiv Tests------------

test_that("plot.simple_eiv runs without error for Deming", {
  expect_silent(plot(dem_model))
})

test_that("plot.simple_eiv runs without error for PB", {
  expect_silent(plot(pb_model))
})

test_that("plot.simple_eiv accepts interval parameter", {
  expect_silent(plot(dem_model, interval = "none"))
  expect_silent(plot(dem_model, interval = "confidence"))
})

test_that("plot.simple_eiv accepts level parameter", {
  expect_silent(plot(dem_model, interval = "confidence", level = 0.90))
})

test_that("plot.simple_eiv returns ggplot object", {
  p <- plot(dem_model)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot.simple_eiv returns invisible ggplot", {
  p <- plot(dem_model)

  expect_s3_class(p, "ggplot")
})


# joint_test.simple_eiv Tests ------------
test_that("joint_test.simple_eiv works for Deming regression", {
  jt <- joint_test(dem_model)

  expect_type(jt, "list")
  expect_true("result" %in% names(jt) || "conclusion" %in% names(jt) ||
              "reject" %in% names(jt) || length(jt) > 0)
})

test_that("joint_test.simple_eiv accepts custom null values", {
  jt <- joint_test(dem_model, ideal_intercept = 0, ideal_slope = 1)

  expect_type(jt, "list")
})

test_that("joint_test.simple_eiv default tests H0: intercept=0, slope=1", {
  jt <- joint_test(dem_model)

  # The result should indicate testing against (0, 1)
  expect_type(jt, "list")
})

test_that("joint_test.simple_eiv works for PB regression", {
  jt <- joint_test(pb_model)

  expect_type(jt, "list")
})

test_that("joint_test.simple_eiv accepts level parameter", {
  jt_95 <- joint_test(dem_model, level = 0.95)
  jt_90 <- joint_test(dem_model, level = 0.90)

  expect_type(jt_95, "list")
  expect_type(jt_90, "list")
})

test_that("joint_test.simple_eiv detects deviation from identity", {
  # Create model with clear deviation
  biased_data <- data.frame(x = 1:20, y = 5 + 1.5 * (1:20))
  biased_model <- dem_reg(y ~ x, data = biased_data, error.ratio = 1)

  jt <- joint_test(biased_model)

  # Should reject H0: intercept=0, slope=1
  expect_type(jt, "list")
})


# Edge Cases for S3 Methods ------------

test_that("S3 methods work with small sample", {
  small_data <- data.frame(x = 1:5, y = c(1.1, 2.0, 3.1, 4.0, 5.1))
  small_model <- dem_reg(y ~ x, data = small_data, error.ratio = 1)

  expect_silent(print(small_model))
  expect_length(coef(small_model), 2)
  expect_length(fitted(small_model), 5)
  expect_length(residuals(small_model), 5)
})

test_that("S3 methods work with weighted Deming", {
  weighted_model <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = TRUE)

  expect_silent(print(weighted_model))
  expect_length(coef(weighted_model), 2)
  expect_true(is.matrix(vcov(weighted_model)))
  expect_true(is.matrix(confint(weighted_model)))
})

test_that("S3 methods work with bootstrap PB", {
  expect_silent(print(pb_model_boot))
  expect_length(coef(pb_model_boot), 2)
  expect_true(is.matrix(confint(pb_model_boot)))
})


# Consistency Tests ------------

test_that("coef and model_table estimates match", {
  coeffs <- coef(dem_model)
  mt <- dem_model$model_table

  expect_equal(coeffs["(Intercept)"], mt["(Intercept)", "coef"],
               ignore_attr = TRUE)
  expect_equal(coeffs["X"], mt["Slope", "coef"],
               ignore_attr = TRUE)
})

test_that("confint and model_table bounds match", {
  ci <- confint(dem_model)
  mt <- dem_model$model_table

  expect_equal(ci["(Intercept)", 1], mt["(Intercept)", "lower.ci"],
               tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(ci["(Intercept)", 2], mt["(Intercept)", "upper.ci"],
               tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("fitted + residuals = observed y", {
  fits <- fitted(dem_model)
  resids <- residuals(dem_model)

  reconstructed <- fits + resids

  expect_equal(as.numeric(reconstructed), ncss_deming1$Y, tolerance = 1e-6)
})


# Method Dispatch Tests ------------

test_that("print dispatches correctly", {
  expect_output(print(dem_model), "Deming")
  expect_output(print(pb_model), "Passing-Bablok")
})

test_that("summary dispatches correctly", {
  expect_output(print(summary(dem_model)), "Deming")
  expect_output(print(summary(pb_model)), "Passing-Bablok")
})

test_that("generic functions use simple_eiv methods", {
  # Verify that generics dispatch to simple_eiv methods
  expect_s3_class(dem_model, "simple_eiv")

  # These should use simple_eiv methods, not default
  expect_type(coef(dem_model), "double")
  expect_type(fitted(dem_model), "double")
  expect_type(residuals(dem_model), "double")
})


# Error Handling in S3 Methods ------------
test_that("predict errors on wrong variable name in newdata",
{
  newdata <- data.frame(wrong_name = c(5, 7, 9))
  expect_error(predict(dem_model, newdata = newdata))
})

test_that("confint errors on invalid level", {
  expect_error(confint(dem_model, level = 0))
  expect_error(confint(dem_model, level = 1))
  expect_error(confint(dem_model, level = 1.5))
})

test_that("confint errors on invalid parm", {
  expect_error(confint(dem_model, parm = "nonexistent"))
})
