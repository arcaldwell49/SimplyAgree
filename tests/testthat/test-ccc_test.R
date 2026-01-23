context("ccc_test")

test_that("ccc_test returns htest class for simple data (vector interface)", {
  # Simple paired data
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result <- ccc_test(x, y)

  expect_s3_class(result, "htest")
  expect_true("statistic" %in% names(result))
  expect_true("parameter" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_true("conf.int" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("null.value" %in% names(result))
  expect_true("alternative" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("data.name" %in% names(result))
})

test_that("ccc_test returns htest class for simple data (data frame interface)", {
  data("reps")
  # Use first observation per subject for simple data
  reps_simple <- reps[!duplicated(reps$id), ]

  result <- ccc_test(x = "x", y = "y", data = reps_simple)

  expect_s3_class(result, "htest")
  expect_equal(names(result$estimate), "CCC")
  expect_equal(names(result$null.value), "CCC")
  expect_equal(names(result$statistic), "Z")
  expect_equal(names(result$parameter), "n")
})

test_that("ccc_test CCC estimate matches agree_test output", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  ccc_result <- ccc_test(x = "x", y = "y", data = reps_simple)

  # Compare with agree_test (which takes vectors, not data frame)
  agree_result <- suppressWarnings(
    agree_test(x = reps_simple$x, y = reps_simple$y)
  )

  expect_equal(
    unname(ccc_result$estimate),
    agree_result$ccc.xy$est.ccc,
    tolerance = 0.001
  )
})

test_that("ccc_test works with reps data type", {
  data("reps")

  result <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")

  expect_s3_class(result, "htest")
  expect_true(grepl("U-statistics", result$method))
  expect_true(result$estimate > -1 && result$estimate < 1)
})

test_that("ccc_test CCC matches agree_reps output for reps data", {
  data("reps")

  ccc_result <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")

  # Compare with agree_reps
  agree_reps_result <- suppressWarnings(
    agree_reps(x = "x", y = "y", id = "id", data = reps)
  )

  expect_equal(
    unname(ccc_result$estimate),
    agree_reps_result$ccc.xy$est.ccc,
    tolerance = 0.001
  )
})

test_that("ccc_test works with nest data type", {
  data("reps")

  result <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")

  expect_s3_class(result, "htest")
  expect_true(grepl("U-statistics", result$method))
})

test_that("ccc_test CCC matches agree_nest output for nest data", {
  data("reps")

  ccc_result <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")

  # Compare with agree_nest
  agree_nest_result <- suppressWarnings(
    agree_nest(x = "x", y = "y", id = "id", data = reps)
  )

  expect_equal(
    unname(ccc_result$estimate),
    agree_nest_result$ccc.xy$est.ccc,
    tolerance = 0.001
  )
})

test_that("alternative hypothesis affects p-values correctly", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result_two <- ccc_test(x, y, alternative = "two.sided", null.value = 0)
  result_greater <- ccc_test(x, y, alternative = "greater", null.value = 0)
  result_less <- ccc_test(x, y, alternative = "less", null.value = 0)

  # For highly correlated data, greater should have smaller p-value than two-sided
  # and less should have larger p-value (close to 1)
  expect_true(result_greater$p.value < result_two$p.value)
  expect_true(result_less$p.value > 0.5)

  # Two-sided p-value should be approximately 2x the one-sided
  expect_equal(result_two$p.value, 2 * result_greater$p.value, tolerance = 0.001)
})

test_that("null.value affects the test correctly", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result_0 <- ccc_test(x, y, null.value = 0)
  result_09 <- ccc_test(x, y, null.value = 0.9)

  # P-value should be different for different null values
  expect_true(result_0$p.value != result_09$p.value)

  # Null value should be stored correctly

  expect_equal(unname(result_0$null.value), 0)
  expect_equal(unname(result_09$null.value), 0.9)
})

test_that("confidence interval has correct attributes", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result_95 <- ccc_test(x, y, conf.level = 0.95)
  result_90 <- ccc_test(x, y, conf.level = 0.90)

  expect_equal(attr(result_95$conf.int, "conf.level"), 0.95)
  expect_equal(attr(result_90$conf.int, "conf.level"), 0.90)

  # CI should contain the estimate
  expect_true(result_95$conf.int[1] < result_95$estimate)
  expect_true(result_95$conf.int[2] > result_95$estimate)

  # 90% CI should be narrower than 95% CI
  width_95 <- result_95$conf.int[2] - result_95$conf.int[1]
  width_90 <- result_90$conf.int[2] - result_90$conf.int[1]
  expect_true(width_90 < width_95)
})

test_that("input validation works correctly", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2)

  # Invalid conf.level
  expect_error(ccc_test(x, y, conf.level = 0), "'conf.level' must be a single number between 0 and 1")
  expect_error(ccc_test(x, y, conf.level = 1), "'conf.level' must be a single number between 0 and 1")
  expect_error(ccc_test(x, y, conf.level = -0.5), "'conf.level' must be a single number between 0 and 1")
  expect_error(ccc_test(x, y, conf.level = 1.5), "'conf.level' must be a single number between 0 and 1")
  expect_error(ccc_test(x, y, conf.level = c(0.9, 0.95)), "'conf.level' must be a single number between 0 and 1")

  # Invalid null.value
  expect_error(ccc_test(x, y, null.value = -2), "'null.value' must be a single number between -1 and 1")
  expect_error(ccc_test(x, y, null.value = 2), "'null.value' must be a single number between -1 and 1")

  # Mismatched lengths
  expect_error(ccc_test(x, c(1, 2, 3)), "'x' and 'y' must have the same length")

  # Missing y
  expect_error(ccc_test(x), "'y' must be provided when 'data' is NULL")
})

test_that("data frame interface validation works", {
  data("reps")

  # Missing id for reps data type
  expect_error(
    ccc_test(x = "x", y = "y", data = reps, data_type = "reps"),
    "'id' must be provided when data_type is 'reps' or 'nest'"
  )

  # Non-existent column
  expect_error(
    ccc_test(x = "nonexistent", y = "y", data = reps),
    "Column 'nonexistent' not found in data"
  )

  # Invalid data argument
  expect_error(
    ccc_test(x = "x", y = "y", data = "not a data frame"),
    "'data' must be a data frame"
  )
})

test_that("print method works for htest object", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result <- ccc_test(x, y)

  # Should print without error
  expect_output(print(result), "Lin's Concordance Correlation Coefficient")
  expect_output(print(result), "CCC")
})

test_that("method description is correct for different data types", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  result_simple <- ccc_test(x = "x", y = "y", data = reps_simple)
  result_reps <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")

  expect_equal(result_simple$method, "Lin's Concordance Correlation Coefficient")
  expect_equal(result_reps$method, "Concordance Correlation Coefficient (U-statistics)")
})

test_that("warning issued when data_type specified with vector input", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2)

  expect_warning(
    ccc_test(x, y, data_type = "reps"),
    "'data_type' is ignored when vectors are provided directly"
  )
})

test_that("CCC values are within valid range", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  result_simple <- ccc_test(x = "x", y = "y", data = reps_simple)
  result_reps <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
  result_nest <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")

  # CCC should be between -1 and 1
  expect_true(result_simple$estimate >= -1 && result_simple$estimate <= 1)
  expect_true(result_reps$estimate >= -1 && result_reps$estimate <= 1)
  expect_true(result_nest$estimate >= -1 && result_nest$estimate <= 1)

  # CI bounds should be between -1 and 1
  expect_true(all(result_simple$conf.int >= -1 & result_simple$conf.int <= 1))
  expect_true(all(result_reps$conf.int >= -1 & result_reps$conf.int <= 1))
  expect_true(all(result_nest$conf.int >= -1 & result_nest$conf.int <= 1))
})

test_that("p-values are within valid range", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  result <- ccc_test(x = "x", y = "y", data = reps_simple)

  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("parameter n is correctly computed", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  result_simple <- ccc_test(x = "x", y = "y", data = reps_simple)
  result_reps <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")

  # For simple data, n should equal number of complete pairs
  expect_equal(unname(result_simple$parameter), nrow(reps_simple))

  # For reps data, n should equal number of unique subjects
  expect_equal(unname(result_reps$parameter), length(unique(reps$id)))
})
