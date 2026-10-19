# Weir (2005) data set A: 8 subjects, 2 trials
weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                   t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
weir_cols <- c("t1", "t2")

test_that("the MDC is 1.96 * sqrt(2) * SEM, with the scaled interval", {
  sem <- reli_err_test(col.names = weir_cols, data = weir)
  res <- mdc_test(col.names = weir_cols, data = weir)
  mult <- stats::qnorm(0.975) * sqrt(2)
  expect_s3_class(res, c("reli_htest", "htest"))
  expect_equal(unname(res$estimate), mult * unname(sem$estimate))
  expect_equal(as.numeric(res$conf.int), mult * as.numeric(sem$conf.int))
  expect_equal(attr(res$conf.int, "conf.level"), 0.95)
  expect_equal(unname(res$stderr[1]), mult * unname(sem$stderr[1]))
  expect_equal(names(res$estimate), "MDC")
  expect_null(res$p.value)
  # sqrt(MS_E) for Weir's data set A, by hand
  d <- weir$t1 - weir$t2
  expect_equal(unname(res$estimate), mult * stats::sd(d) / sqrt(2))
})

test_that("with two trials the MDC is the half-width of the limits of agreement", {
  res <- mdc_test(col.names = weir_cols, data = weir)
  d <- weir$t1 - weir$t2
  expect_equal(unname(res$estimate), stats::qnorm(0.975) * stats::sd(d))
})

test_that("the test of the MDC is the SEM test of the scaled null value", {
  mult <- stats::qnorm(0.975) * sqrt(2)
  res <- mdc_test(col.names = weir_cols, data = weir, null.value = 30)
  sem <- reli_err_test(col.names = weir_cols, data = weir,
                       null.value = 30 / mult)
  expect_equal(res$p.value, sem$p.value)
  expect_equal(res$statistic, sem$statistic)
  expect_equal(res$alternative, "less")
  expect_equal(unname(res$null.value), 30)
  expect_equal(res$conf.int[1], 0)
  expect_equal(res$conf.int[2], mult * sem$conf.int[2])
})

test_that("mdc.level, sided and k_avg set the multiplier", {
  sem <- unname(reli_err_test(col.names = weir_cols, data = weir)$estimate)
  one <- mdc_test(col.names = weir_cols, data = weir, sided = "one.sided")
  expect_equal(unname(one$estimate), stats::qnorm(0.95) * sqrt(2) * sem)
  mdc90 <- mdc_test(col.names = weir_cols, data = weir, mdc.level = 0.90)
  expect_equal(one$estimate, mdc90$estimate)
  avg <- mdc_test(col.names = weir_cols, data = weir, k_avg = 3)
  expect_equal(unname(avg$estimate), stats::qnorm(0.975) * sqrt(2 / 3) * sem)
  expect_match(avg$method, "mean of 3 trials")
  expect_equal(avg$settings$mdc$k_avg, 3)
})

test_that("an existing SEM result is reused, with a new interval or test", {
  sem <- reli_err_test(col.names = weir_cols, data = weir, conf.level = 0.9)
  from_obj <- mdc_test(sem, null.value = 30)
  from_data <- mdc_test(col.names = weir_cols, data = weir, null.value = 30)
  expect_equal(from_obj$estimate, from_data$estimate)
  expect_equal(from_obj$conf.int, from_data$conf.int)
  expect_equal(from_obj$p.value, from_data$p.value)
  expect_error(mdc_test(sem, scale = "log"), "used only when fitting")
  sep <- reli_err_test(col.names = weir_cols, data = weir, stat = "SEP")
  expect_error(mdc_test(sep), "stat = \"SEM\"")
})

test_that("the log scale gives percentage increase and decrease", {
  res <- mdc_test(col.names = weir_cols, data = weir, scale = "log")
  expect_equal(names(res$estimate), "MDC (sympercent)")
  p <- res$percent
  mdc <- unname(res$estimate)
  expect_equal(p$estimate, 100 * c(exp(mdc / 100) - 1, 1 - exp(-mdc / 100)))
  expect_gt(p$estimate[1], p$estimate[2])
  expect_null(mdc_test(col.names = weir_cols, data = weir)$percent)
})

test_that("summary describes the MDC as an individual threshold", {
  res <- mdc_test(col.names = weir_cols, data = weir, null.value = 30)
  s <- summary(res)
  expect_match(s$text, "minimal detectable change")
  expect_match(s$text, "not to an average \\(group\\) effect")
  expect_match(s$text, "chi-square test are exact")
  log_res <- mdc_test(col.names = weir_cols, data = weir, scale = "log")
  expect_match(summary(log_res)$text, "an increase of")
  expect_s3_class(plot(res), "ggplot")
})

test_that("bootstrap SEMs are rescaled", {
  skip_on_cran()
  set.seed(1)
  res <- mdc_test(col.names = weir_cols, data = weir, boot = TRUE,
                  boot_ci = "perc", replicates = 199)
  sem <- res$sem
  mult <- stats::qnorm(0.975) * sqrt(2)
  expect_equal(as.numeric(res$conf.int), mult * as.numeric(sem$conf.int))
  expect_equal(res$boot$estimates, mult * sem$boot$estimates)
  expect_error(mdc_test(sem, conf.level = 0.9), "bootstrap SEM")
  expect_s3_class(mdc_test(sem), "reli_htest")
})

test_that("mdc_test validates its input", {
  expect_error(mdc_test(col.names = weir_cols, data = weir, mdc.level = 1),
               "'mdc.level'")
  expect_error(mdc_test(col.names = weir_cols, data = weir, k_avg = 1.5),
               "'k_avg'")
  expect_error(mdc_test(col.names = weir_cols, data = weir, null.value = -1),
               "'null.value'")
})
