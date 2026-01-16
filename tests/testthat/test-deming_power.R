context("deming-power")

# Note: Power simulations are stochastic, so we use tolerances and ranges
# rather than exact values for most tests

# Test 1: Basic Power Simulation-----------

test_that("deming_power_sim runs without errors", {
  expect_message({
    result <- deming_power_sim(
      n_sims = 50,  # Small for speed
      sample_size = 30,
      x_range = c(10, 100),
      actual_slope = 1.05,
      ideal_slope = 1.0,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    )
  })
})

test_that("deming_power_sim returns correct structure", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Check class
  expect_s3_class(result, "deming_power")

  # Check required components
  expect_true("power_ci_slope" %in% names(result))
  expect_true("power_ci_intercept" %in% names(result))
  expect_true("power_either_ci" %in% names(result))
  expect_true("power_joint" %in% names(result))
  expect_true("advantage" %in% names(result))
  expect_true("settings" %in% names(result))

  # Check power values are proportions
  expect_true(result$power_ci_slope >= 0 && result$power_ci_slope <= 1)
  expect_true(result$power_ci_intercept >= 0 && result$power_ci_intercept <= 1)
  expect_true(result$power_either_ci >= 0 && result$power_either_ci <= 1)
  expect_true(result$power_joint >= 0 && result$power_joint <= 1)

  # Check settings stored
  expect_equal(result$settings$sample_size, 30)
  expect_equal(result$settings$actual_slope, 1.05)
})

test_that("power increases with sample size", {
  # Small N should have lower power
  result_small <- deming_power_sim(
    n_sims = 50,
    sample_size = 20,
    x_range = c(10, 100),
    actual_slope = 1.1,  # Large bias for clearer effect
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Large N should have higher power
  result_large <- deming_power_sim(
    n_sims = 50,
    sample_size = 100,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Large sample should have higher power (allowing some stochastic variation)
  expect_true(result_large$power_joint >= result_small$power_joint - 0.1)
})

test_that("power increases with larger bias", {
  # Small bias
  set.seed(31125)
  result_small_bias <- deming_power_sim(
    n_sims = 150,
    sample_size = 25,
    x_range = c(10, 100),
    actual_slope = 1.02,  # 2% bias
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Large bias
  result_large_bias <- deming_power_sim(
    n_sims = 150,
    sample_size = 25,
    x_range = c(10, 100),
    actual_slope = 1.1,  # 10% bias
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Larger bias should have higher power
  expect_true(result_large_bias$power_joint > result_small_bias$power_joint)
})


# Test 2: Variance Function Types-----------

test_that("constant variance type works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 25, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 20, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
})

test_that("proportional variance type works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 0.05, beta2 = 0, J = 1, type = "proportional"),
    x_var_params = list(beta1 = 0.04, beta2 = 0, J = 1, type = "proportional")
  )

  expect_s3_class(result, "deming_power")
})

test_that("power variance type works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 0.5, beta2 = 0.05, J = 2, type = "power"),
    x_var_params = list(beta1 = 0.4, beta2 = 0.04, J = 2, type = "power")
  )

  expect_s3_class(result, "deming_power")
})



# Test 3: X Distribution Types-----------

test_that("uniform x distribution works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    x_dist = "uniform",
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
})

test_that("central x distribution works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    x_dist = "central",
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
})

test_that("right_skewed x distribution works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    x_dist = "right_skewed",
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
})


# Test 4: Weighted vs Unweighted-----------

test_that("unweighted power simulation works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    weighted = FALSE,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
  expect_equal(result$settings$weighted, FALSE)
})

test_that("weighted power simulation works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    weighted = TRUE,
    y_var_params = list(beta1 = 0.5, beta2 = 0.05, J = 2, type = "power"),
    x_var_params = list(beta1 = 0.4, beta2 = 0.04, J = 2, type = "power")
  )

  expect_s3_class(result, "deming_power")
  expect_equal(result$settings$weighted, TRUE)
})


# Test 5: Joint Region Advantage-----------


test_that("joint region advantage is computed", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Advantage should equal difference
  expected_advantage <- result$power_joint - result$power_either_ci
  expect_equal(result$advantage, expected_advantage)
})

test_that("narrow range shows larger joint advantage", {
  # Narrow range (high correlation)
  result_narrow <- deming_power_sim(
    n_sims = 50,
    sample_size = 50,
    x_range = c(45, 55),  # 1.2:1 ratio
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Wide range (low correlation)
  result_wide <- deming_power_sim(
    n_sims = 50,
    sample_size = 50,
    x_range = c(1, 100),  # 100:1 ratio
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Narrow range should have larger advantage (allowing stochastic variation)
  expect_true(result_narrow$advantage >= result_wide$advantage - 0.1)
})


# Test 6: Print Method for Power Results -----------

test_that("print.deming_power works", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Should not error
  expect_output(print(result), "Deming Regression Power Analysis")

  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")

  # Check for key elements
  expect_true(grepl("Sample Size", output_text))
  expect_true(grepl("Statistical Power", output_text))
  expect_true(grepl("Joint Region", output_text))
  expect_true(grepl("Advantage", output_text))
})


# Test 7: Sample Size Determination-----------

test_that("deming_sample_size runs without errors", {
  skip_on_cran()  # This takes a while

  expect_message({
    result <- deming_sample_size(
      target_power = 0.80,
      initial_n = 20,
      max_n = 60,
      n_sims = 50,  # Small for speed
      step_size = 10,
      x_range = c(10, 100),
      actual_slope = 1.1,  # Large bias for faster convergence
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    )
  })
})

test_that("deming_sample_size returns correct structure", {
  skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.80,
    initial_n = 20,
    max_n = 60,
    n_sims = 50,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Check class
  expect_s3_class(result, "deming_sample_size")

  # Check required components
  expect_true("n_required_ci" %in% names(result))
  expect_true("n_required_joint" %in% names(result))
  expect_true("target_power" %in% names(result))
  expect_true("power_curve" %in% names(result))
  expect_true("reduction_n" %in% names(result))
  expect_true("reduction_pct" %in% names(result))

  # Check target power stored
  expect_equal(result$target_power, 0.80)

  # Check power curve is a data frame
  expect_true(is.data.frame(result$power_curve))
  expect_true("n" %in% names(result$power_curve))
  expect_true("power_ci" %in% names(result$power_curve))
  expect_true("power_joint" %in% names(result$power_curve))
})

test_that("joint method requires smaller N than CI method", {
  skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.80,
    initial_n = 20,
    max_n = 80,
    n_sims = 50,
    step_size = 10,
    x_range = c(20, 40),  # Narrow range for clear advantage
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Joint should require smaller or equal N (allowing for cases where both achieve target at same N)
  if (!is.na(result$n_required_ci) && !is.na(result$n_required_joint)) {
    expect_true(result$n_required_joint <= result$n_required_ci)
  }
})

test_that("sample size increases with lower target power", {
  skip_on_cran()

  # Lower target power (80%)
  result_80 <- deming_sample_size(
    target_power = 0.80,
    initial_n = 20,
    max_n = 80,
    n_sims = 50,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Higher target power (90%)
  result_90 <- deming_sample_size(
    target_power = 0.90,
    initial_n = 20,
    max_n = 100,
    n_sims = 50,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # 90% power should require larger N
  if (!is.na(result_80$n_required_joint) && !is.na(result_90$n_required_joint)) {
    expect_true(result_90$n_required_joint >= result_80$n_required_joint)
  }
})


# Test 8: Print Method for Sample Size Results-----------

test_that("print.deming_sample_size works", {
  skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.80,
    initial_n = 20,
    max_n = 60,
    n_sims = 50,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  expect_output(print(result), "Sample Size Determination")

  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")

  # Check for key elements
  expect_true(grepl("Target Power", output_text))
  expect_true(grepl("Required Sample Sizes", output_text))
})


# Test 9: Plot Method for Sample Size Results-----------

test_that("plot.deming_sample_size works", {
  skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.80,
    initial_n = 20,
    max_n = 60,
    n_sims = 50,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.1,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Should return ggplot
  p <- plot(result)
  expect_s3_class(p, "ggplot")

  # Check labels
  expect_true(grepl("Power", p$labels$y, ignore.case = TRUE))
  expect_true(grepl("Sample Size", p$labels$x, ignore.case = TRUE))
})


# Test 10: Input Validation-----------

test_that("deming_power_sim validates inputs", {
  # Sample size too small
  expect_error(
    deming_power_sim(
      sample_size = 2,
      n_sims = 50,
      x_range = c(10, 100),
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "sample_size must be at least 3"
  )

  # Invalid x_range
  expect_error(
    deming_power_sim(
      sample_size = 30,
      n_sims = 50,
      x_range = c(100, 10),  # Wrong order
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "x_range\\[2\\] must be greater than x_range\\[1\\]"
  )

  # Invalid variance type
  expect_error(
    deming_power_sim(
      sample_size = 30,
      n_sims = 50,
      x_range = c(10, 100),
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "invalid"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "variance_type must be"
  )
})

test_that("deming_sample_size validates inputs", {
  # Invalid target power
  expect_error(
    deming_sample_size(
      target_power = 1.5,
      x_range = c(10, 100),
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "target_power must be between 0 and 1"
  )

  # max_n <= initial_n
  expect_error(
    deming_sample_size(
      target_power = 0.80,
      initial_n = 50,
      max_n = 40,
      x_range = c(10, 100),
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "max_n must be greater than initial_n"
  )
})


# Test 11: Edge Cases-----------

test_that("handles very small bias", {

  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.005,  # Very small 1% bias
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Should run but have low power
  expect_s3_class(result, "deming_power")
  expect_true(result$power_joint < 0.5)  # Likely low power for small bias
})

test_that("handles no bias (null case)", {
  result <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.0,  # No bias
    actual_intercept = 0,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Power should be close to alpha (Type I error rate)
  expect_s3_class(result, "deming_power")
  # With alpha = 0.05, power should be close to 0.05 (allowing stochastic variation)
  expect_true(result$power_joint < 0.20)
})


# Test 12: Reproducibility-----------

test_that("results are reproducible with same seed", {
  set.seed(42)
  result1 <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  set.seed(42)
  result2 <- deming_power_sim(
    n_sims = 50,
    sample_size = 30,
    x_range = c(10, 100),
    actual_slope = 1.05,
    y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
  )

  # Results should be identical
  expect_equal(result1$power_joint, result2$power_joint)
  expect_equal(result1$power_either_ci, result2$power_either_ci)
})


# Additional tests -------
#
# Test deming_power functions ------------------------------------------------

test_that("deming_power_sim validates x_range length", {
  expect_error(
    deming_power_sim(
      n_sims = 100,
      sample_size = 20,
      x_range = c(10, 50, 100),  # Wrong length
      actual_slope = 1.05
    ),
    "x_range must be vector of length 2"
  )
})

test_that("deming_power_sim handles simulation errors gracefully", {
  # This tests the tryCatch error handling (lines 163-165)
  # We need to trigger a fitting error - use extreme/degenerate data
  # Very small sample with extreme variance should occasionally fail
  set.seed(999)

  # Run with parameters that might cause occasional fitting issues

  result <- deming_power_sim(
    n_sims = 100,
    sample_size = 5,  # Very small
    x_range = c(1, 2),  # Very narrow range
    actual_slope = 1.0,
    actual_intercept = 0,
    y_var_params = list(beta1 = 100, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 100, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_power")
})

# Test deming_sample_size function -------------------------------------------

test_that("deming_sample_size validates initial_n", {
  expect_error(
    deming_sample_size(
      target_power = 0.80,
      initial_n = 2,  # Too small
      max_n = 50,
      n_sims = 100,
      x_range = c(10, 100),
      actual_slope = 1.05,
      y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant")
    ),
    "initial_n must be at least 3"
  )
})

test_that("deming_sample_size runs and finds required N", {
  #skip_on_cran()

  # Use parameters where target is achievable quickly
  result <- deming_sample_size(
    target_power = 0.70,  # Lower target for faster convergence
    initial_n = 30,
    max_n = 80,
    n_sims = 200,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.10,  # Larger effect for easier detection
    actual_intercept = 0,
    ideal_slope = 1.0,
    ideal_intercept = 0,
    y_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant")
  )

  expect_s3_class(result, "deming_sample_size")
  expect_true(!is.na(result$n_required_joint) || !is.na(result$n_required_ci))
  expect_true(is.data.frame(result$power_curve))
  expect_equal(result$target_power, 0.70)
})

test_that("deming_sample_size warns when target not achieved", {
  #skip_on_cran()

  # Use parameters where target is NOT achievable within max_n
  expect_warning(
    result <- deming_sample_size(
      target_power = 0.99,  # Very high target
      initial_n = 20,
      max_n = 30,  # Very limited range
      n_sims = 100,
      step_size = 10,
      x_range = c(10, 100),
      actual_slope = 1.01,  # Very small effect
      actual_intercept = 0,
      ideal_slope = 1.0,
      ideal_intercept = 0,
      y_var_params = list(beta1 = 25, beta2 = 0, J = 1, type = "constant"),
      x_var_params = list(beta1 = 25, beta2 = 0, J = 1, type = "constant")
    ),
    "Target power not achieved"
  )

  expect_s3_class(result, "deming_sample_size")
})

# Test print.deming_sample_size ----------------------------------------------

test_that("print.deming_sample_size works with achieved targets", {
  #skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.60,
    initial_n = 25,
    max_n = 60,
    n_sims = 150,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.15,
    actual_intercept = 0,
    ideal_slope = 1.0,
    ideal_intercept = 0,
    y_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant")
  )

  expect_output(print(result), "Deming Regression Sample Size Determination")
  expect_output(print(result), "Target Power")
  expect_output(print(result), "Required Sample Sizes")
})

test_that("print.deming_sample_size works when targets not achieved", {
  # Create a result object manually with NA values
  result <- structure(
    list(
      n_required_ci = NA,
      n_required_joint = NA,
      target_power = 0.90,
      power_curve = data.frame(n = c(20, 30), power_ci = c(0.3, 0.4), power_joint = c(0.35, 0.45)),
      reduction_n = NA,
      reduction_pct = NA,
      settings = list()
    ),
    class = "deming_sample_size"
  )

  expect_output(print(result), "Target not achieved")
})

# Test plot.deming_sample_size -----------------------------------------------

test_that("plot.deming_sample_size creates ggplot object", {
  #skip_on_cran()

  result <- deming_sample_size(
    target_power = 0.60,
    initial_n = 25,
    max_n = 55,
    n_sims = 150,
    step_size = 10,
    x_range = c(10, 100),
    actual_slope = 1.15,
    actual_intercept = 0,
    ideal_slope = 1.0,
    ideal_intercept = 0,
    y_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant"),
    x_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant")
  )

  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot.deming_sample_size errors with empty power curve", {
  result <- structure(
    list(
      n_required_ci = NA,
      n_required_joint = NA,
      target_power = 0.90,
      power_curve = data.frame(n = numeric(0), power_ci = numeric(0), power_joint = numeric(0)),
      reduction_n = NA,
      reduction_pct = NA,
      settings = list()
    ),
    class = "deming_sample_size"
  )

  expect_error(plot(result), "No power curve data available")
})

test_that("plot.deming_sample_size works without achieved targets", {
  # Create result with power curve but NA required N values
  result <- structure(
    list(
      n_required_ci = NA,
      n_required_joint = NA,
      target_power = 0.90,
      power_curve = data.frame(
        n = c(20, 30, 40),
        power_ci = c(0.3, 0.4, 0.5),
        power_joint = c(0.35, 0.45, 0.55)
      ),
      reduction_n = NA,
      reduction_pct = NA,
      settings = list()
    ),
    class = "deming_sample_size"
  )

  p <- plot(result)
  expect_s3_class(p, "ggplot")
})

# Test .generate_x_values distribution validation ----------------------------
test_that(".generate_x_values rejects invalid distribution", {
  expect_error(
    SimplyAgree:::.generate_x_values(50, c(10, 100), "invalid_dist"),
    "distribution must be"
  )
})

# Test .calculate_variance type validation -----------------------------------

test_that(".calculate_variance rejects invalid variance type", {
  expect_error(
    SimplyAgree:::.calculate_variance(50, 1, 0.05, 2, "invalid_type"),
    "variance_type must be"
  )
})
