test_that("ccc_test returns htest class for simple data (vector interface)", {
  # Simple paired data
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1.1, 2.2, 2.9, 4.1, 5.2, 5.8, 7.1, 8.0, 9.2, 10.1)

  result <- ccc_test(x, y, null.value = 0)

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

  result <- ccc_test(x = "x", y = "y", data = reps_simple, null.value = 0)

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
  expect_true(grepl("variance components|nested pairs", result$method))
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
  expect_true(grepl("variance components|nested pairs", result$method))
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
  expect_error(ccc_test(x, y, null.value = -2), "'null.value' must be NULL or a single number between -1 and 1")
  expect_error(ccc_test(x, y, null.value = 2), "'null.value' must be NULL or a single number between -1 and 1")

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
  expect_equal(result_reps$method, "Concordance Correlation Coefficient (variance components)")
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

  result <- ccc_test(x = "x", y = "y", data = reps_simple, null.value = 0)

  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("parameter n is correctly computed", {
  data("reps")
  reps_simple <- reps[!duplicated(reps$id), ]

  result_simple <- ccc_test(x = "x", y = "y", data = reps_simple, null.value = 0)
  result_reps <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps",
                          null.value = 0)

  # For simple data, n should equal number of complete (non-missing) x-y pairs
  expected_n_simple <- sum(complete.cases(reps_simple[, c("x", "y")]))
  expect_equal(unname(result_simple$parameter), expected_n_simple)


  # for reps data, the t test has (number of subjects - 1) df
  expect_equal(unname(result_reps$parameter), length(unique(reps$id)) - 1)
  expect_named(result_reps$parameter, "df")
  expect_named(result_reps$statistic, "t")
  expect_equal(result_reps$settings$n, length(unique(reps$id)))
})

# ccc_htest class, defaults and intervals (0.4.0) -------------------------------

test_that("ccc_test returns a ccc_htest with no test by default", {
  testthat::local_edition(3)
  res <- ccc_test(x = "x", y = "y", data = reps)
  expect_s3_class(res, c("ccc_htest", "htest"))
  expect_null(res$statistic)
  expect_null(res$p.value)
  expect_null(res$null.value)
  expect_equal(res$alternative, "two.sided")
  # alternative without a null value: still a two-sided interval
  g <- ccc_test(x = "x", y = "y", data = reps, alternative = "greater")
  expect_equal(g$conf.int, res$conf.int)
  expect_equal(res$settings$data_type, "simple")
  expect_named(res$data, c("x", "y"))
  expect_equal(nrow(res$data), sum(complete.cases(reps$x, reps$y)))
})

test_that("two-sided intervals are unchanged (Fisher z)", {
  testthat::local_edition(3)
  old <- SimplyAgree:::ccc.xy(reps$x, reps$y, conf.level = 0.95,
                              agree.level = 0.95, TOST = FALSE,
                              prop_bias = FALSE)$rho.c
  res <- ccc_test(x = "x", y = "y", data = reps)
  expect_equal(as.numeric(res$conf.int), c(old$lower.ci, old$upper.ci))
  r <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
  z <- atanh(unname(r$estimate))
  expect_equal(as.numeric(r$conf.int),
               tanh(z + c(-1, 1) * qt(0.975, 3) * r$stderr[["SE Z"]]))
})

# External reference values -------------------------------------------------------
# Replicate data: ba1986 (Bland & Altman, 1986), Wright vs mini Wright peak
# flow meters, 17 subjects with 2 replicates per meter.
ba_reps <- rbind(
  data.frame(id = ba1986$id, x = ba1986$wright1, y = ba1986$mini1),
  data.frame(id = ba1986$id, x = ba1986$wright2, y = ba1986$mini2)
)

test_that("simple CCC and SE match Lin (1989) and DescTools/epiR", {
  testthat::local_edition(3)
  res <- ccc_test(x = "wright1", y = "mini1", data = ba1986)
  # Lin's estimator, variances and covariance with denominator n
  x <- ba1986$wright1
  y <- ba1986$mini1
  n <- length(x)
  sxy <- sum((x - mean(x)) * (y - mean(y))) / n
  lin <- 2 * sxy / (sum((x - mean(x))^2) / n + sum((y - mean(y))^2) / n +
                      (mean(x) - mean(y))^2)
  expect_equal(unname(res$estimate), lin, tolerance = 1e-10)
  # estimate and interval from DescTools 0.99.60 CCC(ci = "z-transform") and
  # epiR 3.0.0 epi.ccc(ci = "z-transform"), which agree to 11 digits; neither
  # reports the SE, but their z interval fixes it (Lin's SE, kept as a
  # regression check)
  expect_equal(unname(res$estimate), 0.94274243143, tolerance = 1e-8)
  expect_equal(as.numeric(res$conf.int), c(0.85049187317, 0.97872627917),
               tolerance = 1e-8)
  expect_equal(res$stderr[["SE CCC"]], 0.02861673746, tolerance = 1e-8)
})

test_that("replicate CCC matches cccrm::ccc_vc() (variance components)", {
  testthat::local_edition(3)
  # cccrm 3.0.6: ccc_vc(long, "measure", "id", rmet = "method", int = TRUE)
  res <- ccc_test(x = "x", y = "y", id = "id", data = ba_reps,
                  data_type = "reps")
  expect_equal(unname(res$estimate), 0.9464972725, tolerance = 1e-6)
  expect_equal(unname(res$settings$n), 17)
  # unequal, unpaired replicates (reps has missing y values)
  r2 <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
  expect_equal(unname(r2$estimate), 0.4341188916, tolerance = 1e-6)
  # the jackknife SE is in line with cccrm's delta-method SE of z (0.220)
  expect_gt(res$stderr[["SE Z"]], 0.220 / 1.5)
  expect_lt(res$stderr[["SE Z"]], 0.220 * 1.5)
})

test_that("replicate CCC does not depend on how replicates are paired", {
  testthat::local_edition(3)
  res <- ccc_test(x = "x", y = "y", id = "id", data = ba_reps,
                  data_type = "reps")
  # reverse the order of the y replicates within each subject
  swapped <- ba_reps
  swapped$y <- c(ba1986$mini2, ba1986$mini1)
  res2 <- ccc_test(x = "x", y = "y", id = "id", data = swapped,
                   data_type = "reps")
  expect_equal(res2$estimate, res$estimate, tolerance = 1e-6)
  expect_equal(res2$stderr, res$stderr, tolerance = 1e-5)
  # a missing value drops one measurement, not the pairing of the rest
  miss <- ba_reps
  miss$y[1] <- NA
  long <- tidyr::drop_na(tidyr::pivot_longer(miss, !id, names_to = "method",
                                             values_to = "measure"))
  expect_equal(unname(ccc_test(x = "x", y = "y", id = "id", data = miss,
                               data_type = "reps")$estimate),
               SimplyAgree:::.ccc_vc(as.data.frame(long)))
})

test_that("nested CCC matches cccrm::cccUst() on the complete pairs", {
  testthat::local_edition(3)
  # recpre_long: 10 subjects, AM vs PM in 3 conditions each (nested)
  res <- ccc_test(x = "AM", y = "PM", id = "id", data = recpre_long,
                  data_type = "nest")
  # cccrm 3.0.6: cccUst(long, "measure", "method") on the 30 pairs
  expect_equal(unname(res$estimate), 0.16139281245, tolerance = 1e-8)
  # ... which is Lin's CCC over all pairs
  simple <- ccc_test(x = "AM", y = "PM", data = recpre_long)
  expect_equal(res$estimate, simple$estimate)
  expect_equal(unname(res$settings$n), 10)
  # pairs stay paired when values are missing
  r2 <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")
  expect_equal(r2$estimate,
               ccc_test(x = "x", y = "y", data = stats::na.omit(reps))$estimate)
})

test_that("reps/nest SEs are delete-one-subject jackknife SEs of z", {
  testthat::local_edition(3)
  d <- as.data.frame(recpre_long)
  res <- ccc_test(x = "AM", y = "PM", id = "id", data = d, data_type = "nest")
  lin <- function(x, y) {
    2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2 *
                       length(x) / (length(x) - 1))
  }
  ids <- unique(d$id)
  z <- vapply(ids, function(i) {
    k <- d$id != i
    atanh(lin(d$AM[k], d$PM[k]))
  }, numeric(1))
  g <- length(ids)
  se_z <- sqrt((g - 1) / g * sum((z - mean(z))^2))
  expect_equal(res$stderr[["SE Z"]], se_z, tolerance = 1e-10)
  expect_equal(res$stderr[["SE CCC"]],
               se_z * (1 - unname(res$estimate)^2), tolerance = 1e-10)
  # pairs within a subject are correlated: treating each pair as its own
  # subject gives a smaller SE here
  d$pair <- seq_len(nrow(d))
  naive <- ccc_test(x = "AM", y = "PM", id = "pair", data = d,
                    data_type = "nest")
  expect_lt(naive$stderr[["SE Z"]], res$stderr[["SE Z"]])
})

test_that("agree_reps() and agree_nest() report the same CCC as ccc_test()", {
  testthat::local_edition(3)
  r <- ccc_test(x = "x", y = "y", id = "id", data = ba_reps,
                data_type = "reps")
  a <- suppressWarnings(agree_reps(x = "x", y = "y", id = "id",
                                   data = ba_reps))
  expect_equal(unlist(a$ccc.xy[1, ]),
               c(est.ccc = unname(r$estimate), lower.ci = r$conf.int[[1]],
                 upper.ci = r$conf.int[[2]], SE = r$stderr[["SE CCC"]]))
  n <- ccc_test(x = "AM", y = "PM", id = "id", data = recpre_long,
                data_type = "nest")
  b <- suppressWarnings(agree_nest(x = "AM", y = "PM", id = "id",
                                   data = recpre_long))
  expect_equal(unlist(b$ccc.xy[1, ]),
               c(est.ccc = unname(n$estimate), lower.ci = n$conf.int[[1]],
                 upper.ci = n$conf.int[[2]], SE = n$stderr[["SE CCC"]]))
})

test_that("reps/nest need at least 3 subjects", {
  testthat::local_edition(3)
  d <- reps[reps$id %in% 1:2, ]
  expect_error(ccc_test(x = "x", y = "y", id = "id", data = d,
                        data_type = "reps"), "At least 3 subjects")
  expect_error(ccc_test(x = "x", y = "y", id = "id", data = d,
                        data_type = "nest"), "At least 3 subjects")
})

test_that("the SE uses the complete pairs, matching the CCC and interval", {
  testthat::local_edition(3)
  res <- ccc_test(x = "x", y = "y", data = reps)
  # the half-width of the Fisher z interval is qnorm(0.975) * SE(z)
  z_ci <- atanh(as.numeric(res$conf.int))
  expect_equal(diff(z_ci) / 2, qnorm(0.975) * res$stderr[["SE Z"]])
})

test_that("one-sided alternatives give one-sided intervals matching the test", {
  testthat::local_edition(3)
  g <- ccc_test(x = "x", y = "y", data = reps, null.value = 0.2,
                alternative = "greater")
  expect_equal(g$alternative, "greater")
  expect_equal(g$conf.int[2], 1)
  at_bound <- ccc_test(x = "x", y = "y", data = reps,
                       null.value = g$conf.int[[1]], alternative = "greater")
  expect_equal(at_bound$p.value, 0.05, tolerance = 1e-8)
  l <- ccc_test(x = "x", y = "y", data = reps, null.value = 0.8,
                alternative = "less")
  expect_equal(l$conf.int[1], -1)
  expect_output(print(g), "true CCC is greater than 0.2")
})

test_that("components decompose the CCC into precision and accuracy", {
  testthat::local_edition(3)
  res <- ccc_test(x = "x", y = "y", data = reps)
  d <- res$data
  expect_equal(res$components[["r"]], cor(d$x, d$y))
  expect_equal(res$components[["r"]] * res$components[["C_b"]],
               unname(res$estimate))
  expect_null(ccc_test(x = "x", y = "y", id = "id", data = reps,
                       data_type = "reps")$components)
})

test_that("the CCC is close to the absolute-agreement ICC for two methods", {
  testthat::local_edition(3)
  d <- stats::na.omit(reps[c("x", "y")])
  ccc <- unname(ccc_test(x = "x", y = "y", data = d)$estimate)
  icc <- unname(icc_test(col.names = c("x", "y"), data = d,
                         type = "agreement")$estimate)
  expect_equal(ccc, icc, tolerance = 0.05)
})

# Methods --------------------------------------------------------------------------

test_that("summary describes the CCC in both styles", {
  testthat::local_edition(3)
  res <- ccc_test(x = "x", y = "y", data = reps, null.value = 0.2,
                  alternative = "greater")
  h <- summary(res)
  expect_s3_class(h, "summary.ccc_htest")
  expect_equal(h$style, "hypothesis")
  expect_match(h$text, "Z = ")
  expect_match(h$text, "greater than 0.2")
  expect_match(h$text, "Pearson r = ")
  expect_match(h$text, "bivariate normal")
  e <- summary(res, style = "estimation")
  expect_equal(e$s.value, -log2(res$p.value))
  expect_match(e$text, "lower compatibility bound")
  expect_no_match(e$text, "significant")

  est <- summary(ccc_test(x = "x", y = "y", data = reps))
  expect_equal(est$style, "estimation")
  expect_message(summary(ccc_test(x = "x", y = "y", data = reps),
                         style = "hypothesis"), "No test was requested")

  r <- summary(ccc_test(x = "x", y = "y", id = "id", data = reps,
                        data_type = "nest"))
  expect_match(r$text, "nested measurements")
  expect_match(r$text, "jackknife standard error")

  expect_snapshot(summary(res))
  expect_snapshot(summary(ccc_test(x = "x", y = "y", id = "id", data = reps,
                                   data_type = "reps")))
})

test_that("plot draws the identity and Bland-Altman plots", {
  testthat::local_edition(3)
  s <- ccc_test(x = "x", y = "y", data = reps)
  r <- ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
  for (res in list(s, r)) {
    expect_s3_class(plot(res), "ggplot")
    expect_s3_class(plot(res, type = "ba"), "ggplot")
  }
  expect_match(plot(s)$labels$caption, "line of identity")
  expect_equal(plot(r)$labels$colour, "Subject")
  expect_error(plot(s, type = "bar"))
})

test_that("check gives agreement diagnostics plus bivariate normality", {
  testthat::local_edition(3)
  s <- ccc_test(x = "x", y = "y", data = reps)
  chk <- check(s)
  expect_s3_class(chk, "patchwork")
  expect_equal(chk$labels$title, "Bivariate Normality")
  expect_length(chk$patches$plots, 3)
  r <- check(ccc_test(x = "x", y = "y", id = "id", data = reps,
                      data_type = "reps"))
  expect_s3_class(r, "patchwork")
  expect_equal(r$labels$title, "Linearity")
})

test_that("check shows linearity instead of proportional bias", {
  testthat::local_edition(3)
  titles <- function(p) {
    c(vapply(p$patches$plots, function(q) q$labels$title, character(1)),
      p$labels$title)
  }
  s <- check(ccc_test(x = "x", y = "y", data = reps))
  expect_equal(titles(s), c("Normality of Residuals", "Homogeneity of Variance",
                            "Linearity", "Bivariate Normality"))
  r <- check(ccc_test(x = "x", y = "y", id = "id", data = reps,
                      data_type = "reps"))
  expect_equal(titles(r), c("Normality of Residuals", "Homogeneity of Variance",
                            "Linearity"))
  expect_match(r$labels$caption, "approximate")
  # the RESET test is the F test of a quadratic term in y ~ x
  d <- stats::na.omit(reps[c("x", "y")])
  tst <- SimplyAgree:::.fitted_square_test(y ~ x, d)
  ref <- anova(lm(y ~ x, d), lm(y ~ x + I(x^2), d))
  expect_equal(tst$p.value, ref$`Pr(>F)`[2], tolerance = 1e-8)
  # check.loa() itself is unchanged
  loa <- check(agreement_limit(x = "x", y = "y", data = reps))
  expect_equal(loa$labels$title, "Proportional Bias")
})
