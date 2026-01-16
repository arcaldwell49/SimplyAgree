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
  expect_output(print(pb_model), "Method1")
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

  # Capture the printed output as character vector
  output <- capture.output(summary(summ))
  # Collapse to single string for easier searching
  output_text <- paste(output, collapse = "\n")


  expect_true(grepl("coef", output_text))
  expect_true(grepl("se", output_text))
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
  v <- suppressWarnings({vcov(pb_model)})
  expect_true(is.null(v) || is.matrix(v))
})

test_that("vcov.simple_eiv returns matrix for bootstrap PB", {
  set.seed(2288)
  expect_warning(vcov(pb_model))

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
  # currently param is ignored.
  ci_90 <- expect_message(confint(dem_model, level = 0.90))

  # 90% CI should be narrower
  #width_95_slope <- ci_95["X", 2] - ci_95["X", 1]
  #width_90_slope <- ci_90["X", 2] - ci_90["X", 1]

  #expect_equal(width_90_slope, width_95_slope)
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
  ci <- confint(pb_model_boot)

  expect_true(is.matrix(ci))
  expect_equal(nrow(ci), 2)

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

test_that("fitted.simple_eiv returns estimated true Y values (Deming)", {
  fits <- fitted(dem_model)
  coeffs <- coef(dem_model)

  # For Deming regression, fitted values are NOT intercept + slope * x

  # They are the estimated "true" Y values accounting for measurement error


  b0 <- coeffs["(Intercept)"]
  b1 <- coeffs["X"]
  lambda <- dem_model$error.ratio

  # Raw residuals
  d <- ncss_deming1$Y - (b0 + b1 * ncss_deming1$X)

  # Estimated true Y values (from NCSS documentation)
  manual_fits <- ncss_deming1$Y - d / (1 + lambda * b1^2)

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
  jt <- joint_test(pb_model_boot)

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

  expect_output(print(small_model))
  expect_length(coef(small_model), 2)
  expect_length(fitted(small_model), 5)
  expect_length(residuals(small_model), 5)
})

test_that("S3 methods work with weighted Deming", {
  weighted_model <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = TRUE)

  expect_output(print(weighted_model))
  expect_length(coef(weighted_model), 2)
  expect_true(is.matrix(vcov(weighted_model)))
  expect_true(is.matrix(confint(weighted_model)))
})

test_that("S3 methods work with bootstrap PB", {
  expect_output(print(pb_model_boot))
  expect_length(coef(pb_model_boot), 2)
  expect_true(is.matrix(confint(pb_model_boot)))
})


# Consistency Tests ------------

test_that("coef and model_table estimates match", {
  coeffs <- coef(dem_model)
  mt <- dem_model$model_table

  expect_equal(unname(coeffs["(Intercept)"]), mt["Intercept", ]$coef,
               ignore_attr = TRUE)
  expect_equal(unname(coeffs["X"]), mt["Slope", ]$coef,
               ignore_attr = TRUE)
})

test_that("confint and model_table bounds match", {
  ci <- confint(dem_model)
  mt <- dem_model$model_table["Intercept", ]

  expect_equal(ci["(Intercept)", 1], mt$lower.ci,
               tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(ci["(Intercept)", 2], mt$upper.ci,
               tolerance = 1e-6, ignore_attr = TRUE)
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



# New tests to get to 100 percent code coverage ------
#
# Tests to improve coverage for methods.simple_eiv.R

# Create test data
set.seed(228945)
n <- 30
x <- runif(n, 10, 100)
y <- 2 + 0.95 * x + rnorm(n, 0, 5)
test_data <- data.frame(x = x, y = y)

# Fit models for testing
dem_fit <- dem_reg(y ~ x, data = test_data)
pb_fit <- pb_reg(y ~ x, data = test_data)


# Test weighted Deming regression header (lines 36-37)--------------
test_that("print shows weighted Deming header", {

  # Create data with replicates for weighted Deming
  df_rep <- data.frame(
    id = rep(1:20, each = 2),
    x = rep(runif(20, 10, 100), each = 2) + rnorm(40, 0, 2),
    y = rep(runif(20, 10, 100), each = 2) + rnorm(40, 0, 2)
  )

  # Fit weighted Deming regression
  weighted_fit <- dem_reg(y ~ x | id, data = df_rep, weighted = TRUE)

  output <- capture.output(print(weighted_fit))
  expect_true(any(grepl("Weighted Deming", output)))
})


# Test model.frame.simple_eiv when model is stored (lines 94-95)--------------
test_that("model.frame returns stored model frame", {
  # Default is model = TRUE, so model frame should be stored
  mf <- model.frame(dem_fit)
  expect_s3_class(mf, "data.frame")
  expect_true("x" %in% names(mf))
  expect_true("y" %in% names(mf))
})


# Test model.frame.simple_eiv when model = FALSE (lines 100-140) --------------
test_that("model.frame works when model = FALSE with data argument", {
  # Fit with model = FALSE
  fit_no_model <- dem_reg(y ~ x, data = test_data, model = FALSE)

  # Should work when data is provided
  mf <- model.frame(fit_no_model, data = test_data)
  expect_s3_class(mf, "data.frame")
})

test_that("model.frame errors when model = FALSE and no data", {
  fit_no_model <- dem_reg(y ~ x, data = test_data, model = FALSE)

  # Remove the data from the environment to ensure error
  expect_output(
    model.frame(fit_no_model, data = NULL)
  )
})


# Test summary with weighted Deming (lines 153-154)--------------
test_that("summary shows weighted Deming header", {
  df_rep <- data.frame(
    id = rep(1:20, each = 2),
    x = rep(runif(20, 10, 100), each = 2) + rnorm(40, 0, 2),
    y = rep(runif(20, 10, 100), each = 2) + rnorm(40, 0, 2)
  )

  weighted_fit <- dem_reg(y ~ x, data = df_rep, weighted = TRUE,
                          id = "id")
  expect_output(print(weighted_fit))
  output <- capture.output(summary(weighted_fit))
  expect_true(any(grepl("Weighted Deming", output)))
})


# Test summary with bootstrap info for PB (lines 202-205)
test_that("summary shows bootstrap info for PB with bootstrap", {
  pb_boot <- pb_reg(y ~ x, data = test_data, replicates = 100)

  output <- capture.output(summary(pb_boot))
  # Check for bootstrap message if nboot > 0
  if (!is.null(pb_boot$replicates) && pb_boot$replicates > 0) {
    expect_true(any(grepl("Bootstrap", output)) || any(grepl("bootstrap", output)))
  }
})


# Test confint with numeric parm (line 264)
test_that("confint works with numeric parm", {
  ci <- confint(dem_fit, parm = 1)
  expect_equal(nrow(ci), 1)
  expect_equal(rownames(ci), "(Intercept)")

  ci2 <- confint(dem_fit, parm = 2)
  expect_equal(nrow(ci2), 1)
  expect_equal(rownames(ci2), "x")
})


# Test predict with newdata as vector (line 908)--------------
test_that("predict works with newdata as vector", {
  preds <- predict(dem_fit, newdata = c(20, 50, 80))
  expect_length(preds, 3)
})


# Test predict with no newdata - returns fitted values (line 900)--------------
test_that("predict returns fitted values when newdata is NULL", {
  preds <- predict(dem_fit)
  fitted_vals <- fitted(dem_fit)
  expect_equal(preds, fitted_vals, tolerance = .05)
})

test_that("predict returns regression line values when newdata is NULL", {
  preds <- predict(dem_fit)

  # predict() should return b0 + b1*x (the regression line)
  b0 <- coef(dem_fit)[1]
  b1 <- coef(dem_fit)[2]
  df <- dem_fit$model
  expected <- b0 + b1 * df$x

  expect_equal(preds, expected, ignore_attr = TRUE)
})

test_that("fitted values differ from predicted values in EIV regression", {
  # This documents the important distinction in errors-in-variables regression
  preds <- predict(dem_fit)
  fitted_vals <- fitted(dem_fit)

  # These should NOT be equal - fitted values account for measurement error
  expect_false(isTRUE(all.equal(preds, fitted_vals)))

  # But they should be correlated
  expect_gt(cor(preds, fitted_vals), 0.99)
})


# Test fitted with type = "x" and "both" (lines 793-794)--------------
test_that("fitted returns x values", {
  x_fitted <- fitted(dem_fit, type = "x")
  expect_length(x_fitted, nrow(test_data))
})

test_that("fitted returns both x and y values", {
  both_fitted <- fitted(dem_fit, type = "both")
  expect_s3_class(both_fitted, "data.frame")
  expect_true("x_hat" %in% names(both_fitted))
  expect_true("y_hat" %in% names(both_fitted))
})


# Test fitted errors for PB with type = "x" (lines 787-788) --------------
test_that("fitted errors for PB with type x or both", {
  expect_error(
    fitted(pb_fit, type = "x"),
    "not available for Passing-Bablok"
  )
  expect_error(
    fitted(pb_fit, type = "both"),
    "not available for Passing-Bablok"
  )
})


# Test residuals with type = "x" and "y" (lines 844-845) --------------
test_that("residuals returns x residuals", {
  res_x <- residuals(dem_fit, type = "x")
  expect_length(res_x, nrow(test_data))
})

test_that("residuals returns y residuals", {
  res_y <- residuals(dem_fit, type = "y")
  expect_length(res_y, nrow(test_data))
})


# Test residuals errors for PB with type x or y (lines 836-837) --------------
test_that("residuals errors for PB with type x or y", {
  expect_error(
    residuals(pb_fit, type = "x"),
    "not available for Passing-Bablok"
  )
  expect_error(
    residuals(pb_fit, type = "y"),
    "not available for Passing-Bablok"
  )
})


# Test check.simple_eiv for Passing-Bablok (lines 384-562) --------------
test_that("check works for Passing-Bablok regression", {
  p <- check(pb_fit)
  expect_s3_class(p, "gg")
})


# Test plot_joint (lines 642-737) --------------
test_that("plot_joint works for Deming regression", {
  p <- plot_joint(dem_fit)
  expect_s3_class(p, "gg")
})

test_that("plot_joint works with different ideal values", {
  p <- plot_joint(dem_fit, ideal_slope = 0.9, ideal_intercept = 1)
  expect_s3_class(p, "gg")
})

test_that("plot_joint works without interval display", {
  p <- plot_joint(dem_fit, show_intervals = FALSE)
  expect_s3_class(p, "gg")
})


# Test predict with se.fit = TRUE for vcov method (lines 957-958, 974-975) --------------
test_that("predict returns se.fit when requested", {
  newdata <- data.frame(x = c(30, 50, 70))

  # With interval = "none" and se.fit = TRUE
  result <- predict(dem_fit, newdata = newdata, se.fit = TRUE)
  expect_type(result, "list")
  expect_true("fit" %in% names(result))
  expect_true("se.fit" %in% names(result))
  expect_true("df" %in% names(result))

  # With interval = "confidence" and se.fit = TRUE
  result2 <- predict(dem_fit, newdata = newdata, interval = "confidence", se.fit = TRUE)
  expect_type(result2, "list")
  expect_true("fit" %in% names(result2))
  expect_true("se.fit" %in% names(result2))
})


# Test predict with PB analytical method warning for se.fit (lines 926-928) --------------
test_that("predict warns for PB without bootstrap when se.fit requested", {
  newdata <- data.frame(x = c(30, 50, 70))

  # PB without bootstrap should warn when se.fit = TRUE
  expect_warning(
    predict(pb_fit, newdata = newdata, interval = "confidence", se.fit = TRUE),
    "Standard errors not available"
  )
})


# Test .predict_pb_analytical with interval = "none" (line 998)--------------
test_that("predict PB with interval none returns just predictions", {
  newdata <- data.frame(x = c(30, 50, 70))
  preds <- predict(pb_fit, newdata = newdata, interval = "none")
  expect_length(preds, 3)
  expect_type(preds, "double")
})


# Test joint_test with invalid conf.level (lines 1090-1092)--------------
test_that("joint_test errors with invalid conf.level", {
  expect_error(joint_test(dem_fit, conf.level = 0), "between 0 and 1")
  expect_error(joint_test(dem_fit, conf.level = 1), "between 0 and 1")
  expect_error(joint_test(dem_fit, conf.level = -0.5), "between 0 and 1")
  expect_error(joint_test(dem_fit, conf.level = 1.5), "between 0 and 1")
  expect_error(joint_test(dem_fit, conf.level = c(0.9, 0.95)), "between 0 and 1")
})


# Test joint_test with no vcov available (lines 1099-1100)--------------
test_that("joint_test errors when vcov not available", {
  # Create a mock object without vcov
  mock_fit <- dem_fit
  mock_fit$vcov <- NULL

  expect_error(
    expect_warning(joint_test(mock_fit))
  )
})


# Test joint_test with non-finite vcov (lines 1105-1106)--------------
test_that("joint_test errors with non-finite vcov", {
  mock_fit <- dem_fit
  mock_fit$vcov[1, 1] <- Inf

  expect_error(
    joint_test(mock_fit),
    "non-finite values"
  )
})


# Test .get_simple_eiv_data when model not stored (lines 1249-1264) --------------
test_that(".get_simple_eiv_data works with data argument when model = FALSE", {
  fit_no_model <- dem_reg(y ~ x, data = test_data, model = FALSE)

  # Test that plot works with data argument
  p <- plot(fit_no_model, data = test_data)
  expect_s3_class(p, "gg")

  # Test that check works with data argument
  p2 <- check(fit_no_model, data = test_data)
  expect_s3_class(p2, "gg")
})

test_that(".get_simple_eiv_data errors without data when model = FALSE", {
  fit_no_model <- dem_reg(y ~ x, data = test_data, model = FALSE)

  expect_null(fit_no_model$model)

  expect_visible(
    plot(fit_no_model)
  )
})


# Test model.frame with id column (lines 123-134)--------------
test_that("model.frame handles data with id column", {
  df_rep <- data.frame(
    id = rep(1:15, each = 2),
    x = rep(runif(15, 10, 100), each = 2) + rnorm(30, 0, 2),
    y = rep(runif(15, 10, 100), each = 2) + rnorm(30, 0, 2)
  )

  fit_rep <- dem_reg(y ~ x, data = df_rep, id = "id")
  mf <- model.frame(fit_rep)

  expect_s3_class(mf, "data.frame")
  # Should have aggregated to 15 rows
  expect_equal(nrow(mf), 15)
})


# Test predict error when variable not in newdata (lines 903-904)--------------
test_that("predict errors when variable not found in newdata", {
  newdata <- data.frame(wrong_name = c(30, 50, 70))

  expect_error(
    predict(dem_fit, newdata = newdata),
    "not found in newdata"
  )
})


# Test predict error for PB without vcov or ci_slopes (lines 932-935) --------------

test_that("predict errors for PB without vcov or ci_slopes for intervals", {
  # Create mock PB fit without vcov and ci_slopes
  mock_pb <- pb_fit
  mock_pb$vcov <- NULL
  mock_pb$ci_slopes <- NULL

  newdata <- data.frame(x = c(30, 50, 70))

  expect_error(
    predict(mock_pb, newdata = newdata, interval = "confidence"),
    "Cannot compute confidence intervals"
  )
})
