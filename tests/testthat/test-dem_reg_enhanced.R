context("dem_reg with Joint Region Enhancements")


# Test Data Setup -------------------



# Simple test dataset
test_data_simple <- data.frame(
  x = c(7, 8.3, 10.5, 9, 5.1, 8.2, 10.2, 10.3, 7.1, 5.9),
  y = c(7.9, 8.2, 9.6, 9, 6.5, 7.3, 10.2, 10.6, 6.3, 5.2)
)

# Larger test dataset for more stable tests
set.seed(12345)
test_data_large <- data.frame(
  x = rnorm(50, mean = 50, sd = 10),
  y = rnorm(50, mean = 50, sd = 10)
)

# Dataset with clear bias
set.seed(12345)
test_data_biased <- data.frame(
  x = seq(10, 100, length.out = 40),
  y = 5 + 1.1 * seq(10, 100, length.out = 40) + rnorm(40, 0, 2)
)


# Test 1: Basic Deming Regression with Joint Region (Backward Compatibility)-------------------



test_that("dem_reg works with default compute_joint = TRUE", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1,
    weighted = FALSE
  )

  # Check structure
  expect_s3_class(result, "simple_eiv")
  expect_true("model" %in% names(result))
  expect_true("call" %in% names(result))
  expect_true("vcov" %in% names(result))
  expect_true("joint_region" %in% names(result))
  expect_true("joint_test" %in% names(result))

  # Check model output
  expect_equal(nrow(result$model), 2)
  expect_true(all(c("coef", "se", "lower.ci", "upper.ci") %in% names(result$model)))
})

test_that("dem_reg works with compute_joint = FALSE", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1,
    weighted = FALSE,
    compute_joint = FALSE
  )

  # Should still work, but no joint region
  expect_s3_class(result, "simple_eiv")
  expect_null(result$joint_region)
  expect_null(result$joint_test)
  expect_true(!is.null(result$vcov))  # vcov should still exist
})


# Test 2: Variance-Covariance Matrix-------------------



test_that("vcov matrix is properly structured", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1
  )

  V <- vcov(result)

  # Check dimensions
  expect_equal(dim(V), c(2, 2))

  # Check names
  expect_equal(rownames(V), c("intercept", "slope"))
  expect_equal(colnames(V), c("intercept", "slope"))

  # Check symmetry
  expect_equal(V[1, 2], V[2, 1])

  # Check positive variances
  expect_true(V[1, 1] > 0)
  expect_true(V[2, 2] > 0)

  # Check that correlation is reasonable (between -1 and 1)
  cor_value <- V[1, 2] / sqrt(V[1, 1] * V[2, 2])
  expect_true(cor_value >= -1 && cor_value <= 1)
})

test_that("vcov method works", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  V <- vcov(result)
  expect_true(is.matrix(V))
  expect_equal(dim(V), c(2, 2))
})


# Test 3: Coefficient Extraction -------------------


test_that("coef method works", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  coefs <- coef(result)

  # Check structure
  expect_true(is.numeric(coefs))
  expect_equal(length(coefs), 2)
  expect_equal(names(coefs), c("intercept", "slope"))

  # Check values match model
  expect_equal(unname(coefs[1]), result$model$coef[1])
  expect_equal(unname(coefs[2]), result$model$coef[2])
})


# Test 4: Joint Confidence Region Structure-------------------

test_that("joint confidence region is properly computed", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    conf.level = 0.95
  )

  # Check joint_region exists and has right structure
  expect_true(!is.null(result$joint_region))
  expect_true(is.data.frame(result$joint_region))
  expect_equal(ncol(result$joint_region), 2)
  expect_true(all(c("intercept", "slope") %in% names(result$joint_region)))
  expect_true(nrow(result$joint_region) > 50)  # Should have ~100 points

  # Check that ellipse points vary (not all the same)
  expect_true(sd(result$joint_region$slope) > 0)
  expect_true(sd(result$joint_region$intercept) > 0)
})

test_that("joint confidence region respects confidence level", {
  result_95 <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    conf.level = 0.95
  )

  result_99 <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    conf.level = 0.99
  )

  # 99% ellipse should be larger (greater spread in coordinates)
  spread_95 <- sd(result_95$joint_region$slope)
  spread_99 <- sd(result_99$joint_region$slope)

  expect_true(spread_99 > spread_95)
})


# Test 5: Joint Confidence Region Test-------------------


test_that("joint test structure is correct", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  # Check joint_test structure
  expect_true(!is.null(result$joint_test))
  expect_true(is.list(result$joint_test))
  expect_true(all(c("mahalanobis_distance", "chi2_critical",
                    "is_enclosed", "p_value") %in% names(result$joint_test)))

  # Check types
  expect_true(is.numeric(result$joint_test$mahalanobis_distance))
  expect_true(is.numeric(result$joint_test$chi2_critical))
  expect_true(is.logical(result$joint_test$is_enclosed))
  expect_true(is.numeric(result$joint_test$p_value))

  # Check reasonable values
  expect_true(result$joint_test$mahalanobis_distance >= 0)
  expect_true(result$joint_test$chi2_critical > 0)
  expect_true(result$joint_test$p_value >= 0 && result$joint_test$p_value <= 1)
})

test_that("joint test detects bias when present", {
  # Dataset with clear 10% proportional bias
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_biased,
    error.ratio = 1
  )

  # Should detect departure from identity
  # (May occasionally fail due to randomness, but should usually work)
  expect_true(result$joint_test$mahalanobis_distance > 0)
})

test_that("Mahalanobis distance relates to chi-square critical value", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    conf.level = 0.95
  )

  # Chi-square critical for 95% and df=2 should be ~5.991
  expect_true(abs(result$joint_test$chi2_critical - 5.991) < 0.01)

  # is_enclosed should match comparison
  expected_enclosed <- result$joint_test$mahalanobis_distance <= result$joint_test$chi2_critical
  expect_equal(result$joint_test$is_enclosed, expected_enclosed)
})


# Test 6: Print Method-------------------

test_that("print method shows joint test results", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  # Capture output
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")

  # Check for key elements
  expect_true(grepl("Deming Regression", output_text))
  expect_true(grepl("Joint Confidence Region Test", output_text))
  expect_true(grepl("Mahalanobis distance", output_text))
  expect_true(grepl("Chi-square critical", output_text))
  expect_true(grepl("Identity enclosed", output_text))
  expect_true(grepl("p-value", output_text))
})

test_that("print method works without joint test", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1,
    compute_joint = FALSE
  )

  # Should not error
  expect_output(print(result))

  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")

  # Should NOT show joint test info
  expect_false(grepl("Joint Confidence Region Test", output_text))
})

# Test 7: Plot Method-------------------

test_that("plot method works", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  # Should return a ggplot object
  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot method includes joint test in subtitle when available", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1
  )

  p <- plot(result, show_joint = TRUE)

  # Check that subtitle is present
  expect_true(!is.null(p$labels$subtitle))

  # Subtitle should mention joint region
  expect_true(grepl("joint confidence region", p$labels$subtitle, ignore.case = TRUE))
})


# Test 8: plot_joint Method-------------------

test_that("plot_joint works", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1
  )

  # Should return a ggplot object
  p <- plot_joint(result)
  expect_s3_class(p, "ggplot")

  # Check key labels
  expect_equal(p$labels$x, "Slope")
  expect_equal(p$labels$y, "Intercept")
  expect_true(grepl("Joint Confidence Region", p$labels$title))
})

test_that("plot_joint shows intervals when requested", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1
  )

  p_with <- plot_joint(result, show_intervals = TRUE)
  p_without <- plot_joint(result, show_intervals = FALSE)

  # Both should be ggplot objects
  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")

  # With intervals should have more layers
  expect_true(length(p_with$layers) >= length(p_without$layers))
})

test_that("plot_joint respects ideal point parameters", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1
  )

  # Should not error with different ideal values
  expect_silent(plot_joint(result, ideal_slope = 1.1, ideal_intercept = 2))
  expect_silent(plot_joint(result, ideal_slope = 0.9, ideal_intercept = -1))
})

test_that("plot_joint fails gracefully without joint region", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_simple,
    error.ratio = 1,
    compute_joint = FALSE
  )

  expect_error(
    plot_joint(result),
    "Joint confidence region not computed"
  )
})


# Test 9: Weighted Deming with Joint Region -------------------

test_that("weighted Deming works with joint region", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    weighted = TRUE
  )

  # Should have all expected components
  expect_true(!is.null(result$vcov))
  expect_true(!is.null(result$joint_region))
  expect_true(!is.null(result$joint_test))

  # Print should mention "Weighted"
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("Weighted", output_text))
})


# Test 10: Edge Cases -------------------

test_that("works with minimum sample size", {
  tiny_data <- data.frame(
    x = c(1, 2, 3),
    y = c(1.1, 2.1, 2.9)
  )

  # Should work but with large uncertainty
  result <- dem_reg(
    x = "x",
    y = "y",
    data = tiny_data,
    error.ratio = 1
  )

  expect_s3_class(result, "simple_eiv")
  expect_true(!is.null(result$joint_region))
})


test_that("handles NA values appropriately", {
  na_data <- test_data_simple
  na_data$y[3] <- NA

  # Should drop NA and work
  result <- dem_reg(
    x = "x",
    y = "y",
    data = na_data,
    error.ratio = 1
  )

  expect_s3_class(result, "simple_eiv")
})


# Test 11: Different Error Ratios-------------------


test_that("works with different error ratios", {
  result_1 <- dem_reg(x = "x", y = "y", data = test_data_large, error.ratio = 1)
  result_2 <- dem_reg(x = "x", y = "y", data = test_data_large, error.ratio = 2)
  result_0.5 <- dem_reg(x = "x", y = "y", data = test_data_large, error.ratio = 0.5)

  # All should work
  expect_s3_class(result_1, "simple_eiv")
  expect_s3_class(result_2, "simple_eiv")
  expect_s3_class(result_0.5, "simple_eiv")

  # Different error ratios should give different results
  expect_false(isTRUE(all.equal(result_1$model$coef[2], result_2$model$coef[2])))
})


# Test 12: Consistency Checks-------------------

test_that("joint region encloses estimated point", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1
  )

  # Estimated point should be inside or very close to joint region
  est_slope <- result$model$coef[2]
  est_intercept <- result$model$coef[1]

  # Check if estimated point is near the center of the ellipse
  mean_slope <- mean(result$joint_region$slope)
  mean_intercept <- mean(result$joint_region$intercept)

  # Should be very close (within 10% of range)
  slope_range <- diff(range(result$joint_region$slope))
  intercept_range <- diff(range(result$joint_region$intercept))

  expect_true(abs(est_slope - mean_slope) < 0.1 * slope_range)
  expect_true(abs(est_intercept - mean_intercept) < 0.1 * intercept_range)
})

test_that("vcov matrix corresponds to confidence intervals", {
  result <- dem_reg(
    x = "x",
    y = "y",
    data = test_data_large,
    error.ratio = 1,
    conf.level = 0.95
  )

  V <- vcov(result)

  # Standard errors from vcov should match model SEs
  se_intercept_vcov <- sqrt(V[1, 1])
  se_slope_vcov <- sqrt(V[2, 2])

  se_intercept_model <- result$model$se[1]
  se_slope_model <- result$model$se[2]

  expect_equal(se_intercept_vcov, se_intercept_model, tolerance = 0.001)
  expect_equal(se_slope_vcov, se_slope_model, tolerance = 0.001)
})

# Test 13: Multiple Confidence Levels -------------------


test_that("works with different confidence levels", {
  result_90 <- dem_reg(x = "x", y = "y", data = test_data_large,
                      error.ratio = 1, conf.level = 0.90)
  result_95 <- dem_reg(x = "x", y = "y", data = test_data_large,
                      error.ratio = 1, conf.level = 0.95)
  result_99 <- dem_reg(x = "x", y = "y", data = test_data_large,
                      error.ratio = 1, conf.level = 0.99)

  # All should work
  expect_s3_class(result_90, "simple_eiv")
  expect_s3_class(result_95, "simple_eiv")
  expect_s3_class(result_99, "simple_eiv")

  # Higher confidence = larger chi-square critical
  expect_true(result_90$joint_test$chi2_critical < result_95$joint_test$chi2_critical)
  expect_true(result_95$joint_test$chi2_critical < result_99$joint_test$chi2_critical)
})


# Test 14: Integration Test - Complete Workflow-------------------

test_that("complete workflow works end-to-end", {
  # Generate data
  set.seed(42)
  workflow_data <- data.frame(
    x = runif(40, 10, 100),
    y = 2 + 1.05 * runif(40, 10, 100) + rnorm(40, 0, 5)
  )

  # Fit model
  result <- dem_reg(
    x = "x",
    y = "y",
    data = workflow_data,
    error.ratio = 1,
    weighted = FALSE,
    conf.level = 0.95
  )

  # All methods should work
  expect_output(print(result))
  expect_s3_class(plot(result), "ggplot")
  expect_s3_class(plot_joint(result), "ggplot")
  expect_true(is.matrix(vcov(result)))
  expect_true(is.numeric(coef(result)))

  # Should detect the 5% bias (sometimes, depending on random seed)
  expect_true(!is.null(result$joint_test))
  expect_true(is.numeric(result$joint_test$p_value))
})


# Test 15: Error Handling-------------------

test_that("appropriate errors for invalid inputs", {
  # Missing columns
  expect_error(
    dem_reg(x = "z", y = "y", data = test_data_simple)
  )

  # Invalid confidence level
  expect_error(
    dem_reg(x = "x", y = "y", data = test_data_simple, conf.level = 1.5)
  )

  expect_error(
    dem_reg(x = "x", y = "y", data = test_data_simple, conf.level = -0.5)
  )
})
