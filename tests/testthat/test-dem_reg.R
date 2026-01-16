context("dem_reg")


testthat::test_that("Compare to NCSS 303-26", {
  X=c(7,8.3,10.5,9,5.1,8.2,10.2,10.3,7.1,5.9)
  Y=c(7.9,8.2,9.6,9,6.5,7.3,10.2,10.6,6.3,5.2)
  df1 = data.frame(x = X,
         y = Y)
  dm_u = dem_reg(y ~ x,
                data = df1,
                weighted = FALSE,
                error.ratio = 4)
  dm_u = dem_reg(y ~ x,
                 data = df1,
                 weighted = FALSE,
                 error.ratio = 4,
                 keep_data = TRUE)
  dm_u_jam = jmvdeming(data = df1,
                       method1 = 'x',
                       method2 = 'y')
  dm_u_jam2 = jmvdeming(data = df1,
                       method1 = 'x',
                       method2 = 'y',
                       plotcon = TRUE,
                       plotcheck = TRUE,
                       weighted = TRUE)
  expect_visible(dm_u_jam2)
  expect_s3_class(dm_u_jam2$plotcon, "Image")
  expect_s3_class(dm_u_jam2$plotcheck, "Image")
  expect_equivalent(round(dm_u$model_table$coef,4),
                    c(-0.0897, 1.0012))

  expect_equivalent(round(dm_u$model_table$se,4),
                    c(1.7220, 0.1872))
  p1 = plot(dm_u)
  c1 = check(dm_u)

  dm_w = suppressWarnings({dem_reg(x = 'x',
                  y = 'y',
                  data = df1,
                  weighted = TRUE,
                 error.ratio = 4)})

  dm_w = dem_reg(y ~ x,
                 data = df1,
                 weighted = TRUE,
                 error.ratio = 4)

  expect_equivalent(round(dm_w$model_table$coef,4),
                    c(-0.3251, 1.0309))

  expect_equivalent(round(dm_w$model_table$se,3),
                    c(1.961, 0.219))

  p1 = plot(dm_w)
  c1 = check(dm_w)
  print(dm_w)
  })

testthat::test_that("Simple Run Through with Nested", {

  data('reps')

  dm1 = dem_reg(data = reps,
                formula = y~ x,
                weighted = TRUE,
                id = "id")

  dm2 = dem_reg(data = reps,
                formula = y~ x,
                weighted = FALSE,
                id = "id")

  expect_error( dem_reg(data = reps,
                          formula = y~ x,
                        weighted = TRUE,
                        weights = c(1,2),
                        id = "id"))
  p1 = plot(dm1)
  p2 = plot(dm2)
  c1 = check(dm1)
  c2 = check(dm2)
  print(dm1)
})


# Test Data Setup -----------------

# NCSS DemingReg1 dataset (from documentation)
# Known error variances: Var(Y) = 0.008, Var(X) = 0.032, lambda = 4
ncss_deming1 <- data.frame(
  X = c(7, 8.3, 10.5, 9, 5.1, 8.2, 10.2, 10.3, 7.1, 5.9),
  Y = c(7.9, 8.2, 9.6, 9, 6.5, 7.3, 10.2, 10.6, 6.3, 5.2)
)


# NCSS DemingReg2 dataset (unknown error, replicates)
ncss_deming2 <- data.frame(
  X1 = c(34, 72, 83, 102, 122, 138, 152, 176, 186, 215),
  X2 = c(35, 75, 85, 104, 125, 136, 152, 173, 182, 212),
  Y1 = c(31, 50, 52, 60, 84, 95, 101, 115, 132, 146),
  Y2 = c(30, 46, 56, 60, 84, 90, 99, 116, 133, 145)
)

# Simple synthetic data for basic tests
simple_data <- data.frame(
  x = 1:20,
  y = 1:20 + rnorm(20, 0, 0.5)
)

# Perfect correlation data
perfect_data <- data.frame(
  x = 1:10,
  y = 2 + 1.5 * (1:10)
)


# Basic Functionality Tests -----------------


test_that("dem_reg works with formula interface", {

  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  expect_s3_class(result, "simple_eiv")
  expect_true("coefficients" %in% names(result))
  expect_true("model_table" %in% names(result))
  expect_true("vcov" %in% names(result))
  expect_length(result$coefficients, 2)
  expect_named(result$coefficients, c("(Intercept)", "X"))
})

test_that("dem_reg validates against NCSS DemingReg1 results", {

  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  expect_equal(unname(result$coefficients["(Intercept)"]),
               c(-0.09),
               tolerance = 0.01)
  expect_equal(unname(result$coefficients["X"]),
               c(1.00),
               tolerance = 0.01)
})

test_that("dem_reg simple (unweighted) regression works", {

  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = FALSE)

  expect_s3_class(result, "simple_eiv")
  expect_false(result$call$weighted)
  expect_equal(result$error.ratio, 4)
})

test_that("dem_reg weighted regression works", {

  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = TRUE)

  expect_s3_class(result, "simple_eiv")
  expect_true(result$call$weighted)
})

test_that("dem_reg default error.ratio is 1",
          {
            result <- dem_reg(Y ~ X, data = ncss_deming1)
            expect_equal(result$error.ratio, 1)
          })


# Confidence Level Tests -----------------


test_that("dem_reg respects conf.level parameter", {
  result_95 <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, conf.level = 0.95)
  result_90 <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, conf.level = 0.90)

  expect_equal(result_95$conf.level, 0.95)
  expect_equal(result_90$conf.level, 0.90)

  # 90% CI should be narrower than 95% CI
  ci_95 <- result_95$model_table
  ci_90 <- result_90$model_table

  width_95 <- ci_95[2, "upper.ci"] - ci_95[2, "lower.ci"]
  width_90 <- ci_90[2, "upper.ci"] - ci_90[2, "lower.ci"]

  expect_true(width_90 < width_95)
})

test_that("dem_reg errors on invalid conf.level", {
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = 0))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = 1))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = -0.5))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = 1.5))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = NA))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, conf.level = "0.95"))
})


# Error Ratio Tests -----------------

test_that("dem_reg errors on invalid error.ratio", {
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 0))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, error.ratio = -1))
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, error.ratio = NA))
})

test_that("dem_reg produces different results with different error.ratio", {
  result_1 <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 1)
  result_4 <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  # Results should differ

  expect_false(isTRUE(all.equal(result_1$coefficients, result_4$coefficients)))
})


# Replicate Data Tests (id argument) -----------------


test_that("dem_reg works with replicate data using id", {
  # Reshape DemingReg2 to long format with id
  long_data <- data.frame(
    id = rep(1:10, each = 2),
    x = c(rbind(ncss_deming2$X1, ncss_deming2$X2)),
    y = c(rbind(ncss_deming2$Y1, ncss_deming2$Y2))
  )

  result <- dem_reg(y ~ x, data = long_data, id = "id")

  expect_s3_class(result, "simple_eiv")
  # Error ratio should be calculated from replicates
  expect_true(is.numeric(result$error.ratio))
  expect_gt(result$error.ratio, 0)
})


# keep_data Parameter Tests -----------------


test_that("dem_reg keep_data parameter works", {
  result_keep <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, keep_data = TRUE)
  result_no_keep <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, keep_data = FALSE)

  expect_true(!is.null(result_keep$resamples))
  expect_true(is.null(result_no_keep$resamples))
})


# Weights Parameter Tests -----------------


test_that("dem_reg accepts custom weights", {
  custom_weights <- rep(1, nrow(ncss_deming1))
  custom_weights[1] <- 2  # Give first observation more weight

  result_weighted <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4,
                             weighted = TRUE, weights = custom_weights)
  result_unweighted <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4,
                               weighted = TRUE)

  expect_s3_class(result_weighted, "simple_eiv")
  # Custom weights should produce different results
  expect_false(isTRUE(all.equal(result_weighted$coefficients,
                                result_unweighted$coefficients)))
})

test_that("dem_reg errors on invalid weights", {
  bad_weights <- rep(1, nrow(ncss_deming1) - 1)  # Wrong length
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, weights = bad_weights))

  neg_weights <- rep(-1, nrow(ncss_deming1))  # Negative weights
  expect_error(dem_reg(Y ~ X, data = ncss_deming1, weights = neg_weights))
})


# Missing Data Tests -----------------


test_that("dem_reg handles missing data", {
  data_with_na <- ncss_deming1
  data_with_na$Y[1] <- NA

  # Should work with na.omit behavior
  result <- dem_reg(Y ~ X, data = data_with_na, error.ratio = 4)

  expect_s3_class(result, "simple_eiv")
  expect_equal(result$df.residual, nrow(ncss_deming1) - 2 - 1)  # One less observation
})

test_that("dem_reg handles NA in both variables", {
  data_with_na <- ncss_deming1
  data_with_na$Y[1] <- NA
  data_with_na$X[2] <- NA

  result <- dem_reg(Y ~ X, data = data_with_na, error.ratio = 4)

  expect_s3_class(result, "simple_eiv")
  expect_equal(result$df.residual, nrow(ncss_deming1) - 2 - 2)  # Two less observations
})


# Deprecated Interface Tests -----------------

test_that("dem_reg deprecated x/y interface produces warning", {
  expect_warning(
    dem_reg(x = "X", y = "Y", data = ncss_deming1, error.ratio = 4),
    regexp = "deprecated"
  )
})

test_that("dem_reg deprecated interface produces same results as formula", {
  result_formula <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  result_deprecated <- suppressWarnings(
    dem_reg(x = "X", y = "Y", data = ncss_deming1, error.ratio = 4)
  )

  expect_equal(result_formula$coefficients, result_deprecated$coefficients)
})


# Edge Cases -----------------


test_that("dem_reg handles small sample sizes", {
  small_data <- data.frame(x = 1:5, y = c(1.1, 2.0, 3.1, 4.0, 5.1))

  result <- dem_reg(y ~ x, data = small_data, error.ratio = 1)

  expect_s3_class(result, "simple_eiv")
  expect_equal(result$df.residual, 3)  # n - 2
})

test_that("dem_reg handles perfect correlation", {
  perfect <- data.frame(x = 1:10, y = 2 + 3 * (1:10))

  result <- dem_reg(y ~ x, data = perfect, error.ratio = 1)

  expect_s3_class(result, "simple_eiv")
  expect_equal(unname(result$coefficients["x"]), c(3), tolerance = 0.001)
  expect_equal(unname(result$coefficients["(Intercept)"]), c(2), tolerance = 0.001)
})

test_that("dem_reg handles near-identical x values", {
  # Data with very little variation in x
  tricky_data <- data.frame(
    x = c(5, 5.001, 5.002, 5.003, 5.004, 5.005, 5.006, 5.007, 5.008, 5.009),
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )

  # Should still work, though results may be unstable
  result <- dem_reg(y ~ x, data = tricky_data, error.ratio = 1)
  expect_s3_class(result, "simple_eiv")
})


# Output Structure Tests -----------------


test_that("dem_reg output structure is correct", {
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  # Check required components
  expect_true("coefficients" %in% names(result))
  expect_true("model_table" %in% names(result))
  expect_true("vcov" %in% names(result))
  expect_true("df.residual" %in% names(result))
  expect_true("error.ratio" %in% names(result))
  expect_true("weighted" %in% names(result$call))
  expect_true("conf.level" %in% names(result))
  expect_true("call" %in% names(result))
  expect_true("terms" %in% names(result))

  # Check model_table structure
  expect_true(is.data.frame(result$model_table) || is.matrix(result$model_table))
  expect_equal(nrow(result$model_table), 2)  # intercept and slope
  expect_true(all(c("coef", "se", "lower.ci", "upper.ci") %in% colnames(result$model_table)))

  # Check vcov structure
  expect_true(is.matrix(result$vcov))
  expect_equal(dim(result$vcov), c(2, 2))
})

test_that("dem_reg model_table values are consistent", {
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  mt <- result$model_table

  # Estimates should match coefficients
  expect_equal(mt["Intercept", "coef"], unname(result$coefficients["(Intercept)"]),
               ignore_attr = TRUE)
  expect_equal(mt["Slope", "coef"], unname(result$coefficients["X"]),
               ignore_attr = TRUE)

  # Lower should be less than estimate, which should be less than upper
  expect_lt(mt["Intercept", "lower.ci"], mt["Intercept", "coef"])
  expect_lt(mt["Intercept", "coef"], mt["Intercept", "upper.ci"])
  expect_lt(mt["Slope", "lower.ci"], mt["Slope", "coef"])
  expect_lt(mt["Slope", "coef"], mt["Slope", "upper.ci"])

  # SE should be positive
  expect_gt(mt["Intercept", "se"], 0)
  expect_gt(mt["Slope", "se"], 0)
})


# vcov Matrix Tests -----------------


test_that("dem_reg vcov matrix is symmetric", {
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  expect_equal(result$vcov[1, 2], result$vcov[2, 1])
})
test_that("dem_reg vcov matrix is positive semi-definite", {
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  eigenvalues <- eigen(result$vcov)$values
  expect_true(all(eigenvalues >= -1e-10))  # Allow for numerical error
})


# Degrees of Freedom Tests -----------------


test_that("dem_reg df.residual is correct", {
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4)

  expect_equal(result$df.residual, nrow(ncss_deming1) - 2)
})


# Validation against NCSS weighted Deming (Example 6) -----------------


test_that("dem_reg weighted validates against NCSS results", {
  # NCSS Weighted Deming results: Intercept = -0.3283761, Slope = 1.0312280
  result <- dem_reg(Y ~ X, data = ncss_deming1, error.ratio = 4, weighted = TRUE)

  expect_equal(unname(result$coefficients["(Intercept)"]),
               c(-0.33),
               tolerance = 0.01)
  expect_equal(unname(result$coefficients["X"]),
               c( 1.03),
               tolerance = 0.01)
})

