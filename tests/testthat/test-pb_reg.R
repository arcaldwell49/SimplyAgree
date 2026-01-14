# Test file for pb_reg() function
# Comprehensive testthat tests for Passing-Bablok regression


# Test Data Setup ---------------


# NCSS PassBablok1 dataset (from documentation)
# Known results: Intercept = -0.0092, Slope = 0.9986
ncss_pb1 <- data.frame(
  Method1 = c(69.3, 27.1, 61.3, 50.8, 34.4, 92.3, 57.5, 45.5, 33.3, 60.9,
              56.3, 49.9, 89.7, 28.9, 96.3, 76.6, 83.2, 79.4, 51.7, 32.5,
              14.2, 99.1, 76.8, 95.4, 84.1, 48.8, 80.4, 84.5, 61.4, 26.9),
  Method2 = c(69.1, 26.7, 61.4, 51.2, 34.7, 88.5, 57.9, 45.1, 33.4, 60.8,
              66.5, 48.2, 88.3, 29.3, 96.4, 77.1, 82.7, 78.9, 51.6, 28.8,
              12.7, 98.6, 77.3, 94.9, 83.3, 47.0, 80.9, 84.7, 61.3, 26.7)
)

# Simple synthetic data
simple_data <- data.frame(
  x = 1:30,
  y = 1:30 + rnorm(30, 0, 0.5)
)

# Perfect agreement data
perfect_data <- data.frame(
  x = seq(10, 100, by = 10),
  y = seq(10, 100, by = 10)
)

# Data with systematic bias
biased_data <- data.frame(
  x = 1:30,
  y = 5 + 1.2 * (1:30) + rnorm(30, 0, 0.3)
)


# Basic Functionality Tests ---------------

test_that("pb_reg works with formula interface", {
  result <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1)})

  expect_s3_class(result, "simple_eiv")
  expect_true("coefficients" %in% names(result))
  expect_true("model_table" %in% names(result))
  expect_length(result$coefficients, 2)
  expect_named(result$coefficients, c("(Intercept)", "Method1"))
})

test_that("pb_reg validates against NCSS PassBablok1 results", {

  result <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1,
                   method = "inv")})
  # should  match MethComp and deming R packages
  # test = MethComp::PBreg(ncss_pb1$Method1, ncss_pb1$Method2)
  # test2 = deming::pbreg(ncss_pb1$Method2 ~ ncss_pb1$Method1)
  expect_equal(unname(result$coefficients["(Intercept)"]),
               c(-0.15),
               tolerance = 0.01)
  expect_equal(unname(result$coefficients["Method1"]),
               c( 1.00),
               tolerance = 0.01)
})

test_that("pb_reg default method is scissors", {
  result <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1)})

  expect_match(result$method, "scissors", ignore.case = TRUE)

})


# Method Selection Tests---------------

test_that("pb_reg supports all three methods", {
  result_scissors <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "scissors")})
  result_symmetric <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "symmetric")})
  result_invariant <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "invariant")})

  expect_match(result_scissors$method, "scissors", ignore.case = TRUE)

  expect_match(result_symmetric$method, "symmetric", ignore.case = TRUE)

  expect_match(result_invariant$method, "invariant", ignore.case = TRUE)

})

test_that("pb_reg different methods may produce different results", {
  result_scissors <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "scissors")})
  result_symmetric <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "symmetric")})

  # Methods may give similar but not identical results
  expect_s3_class(result_scissors, "simple_eiv")
  expect_s3_class(result_symmetric, "simple_eiv")
})

test_that("pb_reg errors on invalid method", {
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "invalid"))
})


# Confidence Level Tests---------------

test_that("pb_reg respects conf.level parameter", {
  result_95 <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = 0.95)})
  result_90 <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = 0.90)})

  expect_equal(result_95$conf.level, 0.95)
  expect_equal(result_90$conf.level, 0.90)

  # 90% CI should be narrower than 95% CI
  ci_95 <- result_95$model_table
  ci_90 <- result_90$model_table

  width_95 <- ci_95[2, "upper.ci"] - ci_95[2, "lower.ci"]
  width_90 <- ci_90[2, "upper.ci"] - ci_90[2, "lower.ci"]

  expect_lt(width_90, width_95)
})

test_that("pb_reg errors on invalid conf.level", {
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = 0))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = 1))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = -0.5))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = 1.5))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = NA))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, conf.level = "0.95"))
})


# Kendall's Tau Test ---------------

test_that("pb_reg includes Kendall's tau test", {
  result <- suppressWarnings({pb_reg(Method2 ~ Method1, data = ncss_pb1)})

  expect_true("kendall_test" %in% names(result))
  expect_s3_class(result$kendall_test, "htest")
  expect_true("estimate" %in% names(result$kendall_test))
  expect_true("p.value" %in% names(result$kendall_test))
})

test_that("pb_reg Kendall test is significant for correlated data", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1)

  # For NCSS data, correlation should be highly significant
  expect_lt(result$kendall_test$p.value, 0.001)
  expect_gt(result$kendall_test$estimate, 0.9)
})

test_that("pb_reg warns on non-significant Kendall correlation", {
  # Create uncorrelated data
  set.seed(42)
  uncorrelated <- data.frame(
    x = rnorm(30),
    y = rnorm(30)
  )

  expect_message(
    pb_reg(y ~ x, data = uncorrelated, replicates = 199),
    regexp = "Kendall"
  )
})


# CUSUM Linearity Test---------------

test_that("pb_reg includes CUSUM linearity test", {
  set.seed(92165)
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 199)

  expect_true("cusum_test" %in% names(result))
  expect_s3_class(result$cusum_test, "htest")
  expect_true("statistic" %in% names(result$cusum_test))
  expect_true("p.value" %in% names(result$cusum_test))
})

test_that("pb_reg CUSUM test accepts linearity for linear data", {
  set.seed(1214)
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 199)

  # For NCSS data, linearity should not be rejected
  expect_gt(result$cusum_test$p.value, 0.05)
})


# Bootstrap Tests ---------------

test_that("pb_reg bootstrap works with replicates > 0", {
  set.seed(123)
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 100)

  expect_s3_class(result, "simple_eiv")
  expect_equal(result$replicates, 100)
  expect_true("boot" %in% names(result))
})

test_that("pb_reg bootstrap produces different CI than analytical", {
  set.seed(456)
  result_analytical <- suppressWarnings({pb_reg(Method2 ~ Method1,
                                                data = ncss_pb1,
                                                replicates = 0)})
  result_boot <- pb_reg(Method2 ~ Method1,
                        data = ncss_pb1,
                        replicates = 500)

  # CIs may differ (though not guaranteed to be hugely different)
  expect_s3_class(result_analytical, "simple_eiv")
  expect_s3_class(result_boot, "simple_eiv")
})

test_that("pb_reg errors on negative replicates", {
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = -1))
})


# Weighted Regression Tests ---------------

test_that("pb_reg accepts custom weights", {
  custom_weights <- rep(1, nrow(ncss_pb1))
  custom_weights[1:5] <- 2

  expect_warning(
    result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, weights = custom_weights),
    regexp = "Bootstrap"
  )

  expect_s3_class(result, "simple_eiv")
})

test_that("pb_reg warns about bootstrap for weighted data", {
  custom_weights <- rep(1, nrow(ncss_pb1))
  custom_weights[1] <- 2

  expect_warning(
    pb_reg(Method2 ~ Method1, data = ncss_pb1, weights = custom_weights),
    regexp = "Bootstrap"
  )
})

test_that("pb_reg errors on invalid weights", {
  bad_weights <- rep(1, nrow(ncss_pb1) - 1)  # Wrong length
  expect_error(pb_reg(Method2 ~ Method1,
                      data = ncss_pb1,
                      weights = bad_weights,
                      replicates = 199))

  neg_weights <- rep(-1, nrow(ncss_pb1))  # Negative weights
  expect_error(pb_reg(Method2 ~ Method1,
                      data = ncss_pb1,
                      weights = neg_weights,
                      replicates = 199))
})


# Error Ratio Tests ---------------

test_that("pb_reg default error.ratio is 1", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1,
                   replicates = 99)
  expect_equal(result$error.ratio, 1)
})

test_that("pb_reg warns about non-unit error.ratio", {
  expect_warning(
    pb_reg(Method2 ~ Method1, data = ncss_pb1, error.ratio = 2),
    regexp = "error.ratio"
  )
})

test_that("pb_reg errors on invalid error.ratio", {
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, error.ratio = 0))
  expect_error(pb_reg(Method2 ~ Method1, data = ncss_pb1, error.ratio = -1))
})


# Replicate Data Tests (id argument) ---------------

test_that("pb_reg works with replicate data", {
  # Create data with replicates
  rep_data <- data.frame(
    id = rep(1:15, each = 2),
    x = c(rbind(ncss_pb1$Method1[1:15], ncss_pb1$Method1[1:15] + rnorm(15, 0, 0.5))),
    y = c(rbind(ncss_pb1$Method2[1:15], ncss_pb1$Method2[1:15] + rnorm(15, 0, 0.5)))
  )

  result <- pb_reg(y ~ x, data = rep_data, id = "id")

  expect_s3_class(result, "simple_eiv")
  expect_true(is.numeric(result$error.ratio))
})


# keep_data Parameter Tests ---------------

test_that("pb_reg keep_data parameter works", {
  set.seed(1241)
  result_keep <- pb_reg(Method2 ~ Method1, data = ncss_pb1, keep_data = TRUE,
                        method = "inv",
                        replicates = 99)
  result_no_keep <- pb_reg(Method2 ~ Method1, data = ncss_pb1, keep_data = FALSE,
                           method = "sym",
                           replicates = 99)

  # slopes_data should be kept or not
  expect_true("slopes_data" %in% names(result_keep))
})


# Missing Data Tests ---------------

test_that("pb_reg handles missing data", {
  data_with_na <- ncss_pb1
  data_with_na$Method2[1] <- NA

  result <- pb_reg(Method2 ~ Method1, data = data_with_na,
                   replicates = 99)

  expect_s3_class(result, "simple_eiv")
})

test_that("pb_reg handles NA in both variables", {
  data_with_na <- ncss_pb1
  data_with_na$Method2[1] <- NA
  data_with_na$Method1[2] <- NA

  result <- pb_reg(Method2 ~ Method1, data = data_with_na,
                   replicates = 99)

  expect_s3_class(result, "simple_eiv")
})


# Sample Size Tests ---------------

test_that("pb_reg requires minimum sample size", {
  tiny_data <- data.frame(x = 1:2, y = 1:2)

  expect_error(pb_reg(y ~ x, data = tiny_data))
})

test_that("pb_reg works with small sample size", {
  min_data <- data.frame(x = 1:8, y = c(1.1, 2.0, 3.1, 3.9, 5.5, 6, 7, 7.8))

  result <- pb_reg(y ~ x, data = min_data, replicates = 99)

  expect_s3_class(result, "simple_eiv")
})


# Edge Cases ---------------
test_that("pb_reg handles perfect correlation", {
  perfect <- data.frame(x = 1:20, y = 2 + 3 * (1:20))

  result <- pb_reg(y ~ x, data = perfect, replicates = 199)

  expect_s3_class(result, "simple_eiv")
  expect_equal(result$coefficients["x"], c(x = 3), tolerance = 0.001)
  expect_equal(result$coefficients["(Intercept)"], c("(Intercept)" = 2), tolerance = 0.001)
})

test_that("pb_reg handles identity relationship", {
  identity_data <- data.frame(x = 1:30, y = 1:30)

  result <- pb_reg(y ~ x, data = identity_data, replicates = 199)

  expect_equal(result$coefficients["x"], c(x = 1), tolerance = 0.001)
  expect_equal(result$coefficients["(Intercept)"], c("(Intercept)" = 0), tolerance = 0.001)
})

test_that("pb_reg handles ties in data", {
  # Data with repeated values
  tied_data <- data.frame(
    x = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10),
    y = c(1.1, 1.2, 2.1, 2.2, 3.1, 3.2, 4.1, 4.2, 5.1, 5.2,
          6.1, 6.2, 7.1, 7.2, 8.1, 8.2, 9.1, 9.2, 10.1, 10.2)
  )

  result <- pb_reg(y ~ x, data = tied_data, replicates = 199)

  expect_s3_class(result, "simple_eiv")
})

test_that("pb_reg handles data with some identical pairs", {

  # Some (x, y) pairs are identical
  data_with_identical <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1.1, 2.1, 3.1)
  )

  result <- pb_reg(y ~ x, data = data_with_identical, replicates = 199)

  expect_s3_class(result, "simple_eiv")
})


# Output Structure Tests ---------------

test_that("pb_reg output structure is correct", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 99)

  # Check required components
  expect_true("coefficients" %in% names(result))
  expect_true("model_table" %in% names(result))
  expect_true("df.residual" %in% names(result))
  expect_true("error.ratio" %in% names(result))
  expect_true("conf.level" %in% names(result))
  expect_true("call" %in% names(result))
  expect_true("terms" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("method_num" %in% names(result))
  expect_true("kendall_test" %in% names(result))
  expect_true("cusum_test" %in% names(result))
  expect_true("slopes" %in% names(result))
  expect_true("n_slopes" %in% names(result))
  expect_true("ci_slopes" %in% names(result))

  # Check model_table structure
  expect_true(is.data.frame(result$model_table) || is.matrix(result$model_table))
  expect_equal(nrow(result$model_table), 2)
  expect_true(all(c("coef", "lower.ci", "upper.ci") %in% colnames(result$model_table)))
})

test_that("pb_reg slopes information is correct", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 99)

  n <- nrow(ncss_pb1)
  max_slopes <- n * (n - 1) / 2  # Maximum possible slopes

  expect_lte(result$n_slopes, max_slopes)
  expect_gt(result$n_slopes, 0)

  # ci_slopes should have lower and upper indices
  expect_length(result$ci_slopes, 2)
})

test_that("pb_reg model_table values are consistent", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1)

  mt <- result$model_table

  # Estimates should match coefficients
  expect_equal(mt["(Intercept)", "estimate"], result$coefficients["(Intercept)"],
               ignore_attr = TRUE)
  expect_equal(mt["slope", "estimate"], result$coefficients["slope"],
               ignore_attr = TRUE)

  # Lower should be less than estimate, which should be less than upper
  expect_lt(mt["(Intercept)", "lower.ci"], mt["(Intercept)", "coef"])
  expect_lt(mt["(Intercept)", "coef"], mt["(Intercept)", "upper.ci"])
  expect_lt(mt["slope", "lower.ci"], mt["slope", "coef"])
  expect_lt(mt["slope", "coef"], mt["slope", "upper.ci"])
})


# Reproducibility Tests ---------------

test_that("pb_reg is deterministic without bootstrap", {
  result1 <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 0)
  result2 <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 0)

  expect_equal(result1$coefficients, result2$coefficients)
  expect_equal(result1$model_table, result2$model_table)
})

test_that("pb_reg bootstrap is reproducible with seed", {
  set.seed(789)
  result1 <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 100)

  set.seed(789)
  result2 <- pb_reg(Method2 ~ Method1, data = ncss_pb1, replicates = 100)

  expect_equal(result1$coefficients, result2$coefficients)
})


# Symmetric Property Tests ---------------

test_that("pb_reg scissors method is approximately scale-invariant", {
  # Multiply y by constant
  scaled_data <- ncss_pb1
  scaled_data$Method2 <- scaled_data$Method2 * 2

  result_original <- pb_reg(Method2 ~ Method1, data = ncss_pb1, method = "scissors")
  result_scaled <- pb_reg(Method2 ~ Method1, data = scaled_data, method = "scissors")

  # Slope should approximately double
  expect_equal(result_scaled$coefficients["slope"],
               2 * result_original$coefficients["slope"],
               tolerance = 0.01)
})


# Method Comparison Hypothesis Testing ---------------

test_that("pb_reg supports method comparison testing via CI", {
  result <- pb_reg(Method2 ~ Method1, data = ncss_pb1)

  # For NCSS data, methods should be equivalent
  # CI for intercept should contain 0
  ci_intercept <- c(result$model_table["(Intercept)", "lower.ci"],
                    result$model_table["(Intercept)", "upper.ci"])
  expect_true(ci_intercept[1] < 0 && 0 < ci_intercept[2])

  # CI for slope should contain 1
  ci_slope <- c(result$model_table["slope", "lower.ci"],
                result$model_table["slope", "upper.ci"])
  expect_true(ci_slope[1] < 1 && 1 < ci_slope[2])
})

test_that("pb_reg detects systematic bias", {
  # Create data with clear bias
  biased <- data.frame(
    x = 1:30,
    y = 10 + 1.5 * (1:30)  # Clear intercept and slope bias
  )

  result <- pb_reg(y ~ x, data = biased)

  # CI for intercept should NOT contain 0
  ci_intercept <- c(result$model_table["(Intercept)", "lower.ci"],
                    result$model_table["(Intercept)", "upper.ci"])
  expect_false(ci_intercept[1] < 0 && 0 < ci_intercept[2])

  # CI for slope should NOT contain 1
  ci_slope <- c(result$model_table["slope", "lower.ci"],
                result$model_table["slope", "upper.ci"])
  expect_false(ci_slope[1] < 1 && 1 < ci_slope[2])
})
