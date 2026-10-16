testthat::local_edition(3)

# Weir (2005) data sets A and C: 8 subjects, 2 trials
weir_a <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                     t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
weir_c <- data.frame(t1 = weir_a$t1,
                     t2 = c(161, 162, 189, 100, 175, 171, 195, 219))
wcols <- c("t1", "t2")

sf_wide <- data.frame(J1 = c(9, 6, 8, 7, 10, 6),
                      J2 = c(2, 1, 4, 1, 5, 2),
                      J3 = c(5, 3, 6, 2, 6, 4),
                      J4 = c(8, 2, 8, 6, 9, 7))
sf_cols <- names(sf_wide)

# Weir data set A mean squares from the data (his Table 2 rounds the SS:
# subjects 14,689.8, trials 30.2, error 399.8)
ya <- as.matrix(weir_a)
msb_a <- 2 * sum((rowMeans(ya) - mean(ya))^2) / 7
msj_a <- 8 * sum((colMeans(ya) - mean(ya))^2)
mse_a <- sum((ya - outer(rowMeans(ya), colMeans(ya), "+") + mean(ya))^2) / 7

# Structure ------------------------------------------------------------------------

test_that("reli_err_test returns a reli_htest; estimation only by default", {
  res <- reli_err_test(col.names = wcols, data = weir_a)
  expect_s3_class(res, c("reli_htest", "htest"))
  expect_named(res$estimate, "SEM")
  expect_null(res$statistic)
  expect_null(res$p.value)
  expect_null(res$null.value)
  # alternative defaults to "less", but without a test the interval is two-sided
  expect_equal(res$alternative, "two.sided")
  expect_true(is.finite(res$conf.int[1]) && res$conf.int[1] > 0)
  expect_named(res$stderr, c("SEM", "log"))
  expect_match(res$method, "Standard error of measurement \\(two-way, consistency\\)")
  expect_output(print(res), "SEM")
})

test_that("the three input interfaces give the same result", {
  long <- data.frame(id = rep(1:8, 2), trial = rep(wcols, each = 8),
                     y = c(weir_a$t1, weir_a$t2))
  wide <- reli_err_test(col.names = wcols, data = weir_a, stat = "SEP")
  lng <- reli_err_test(measure = "y", item = "trial", id = "id", data = long,
                       stat = "SEP")
  frm <- reli_err_test(y ~ trial | id, data = long, stat = "SEP")
  expect_equal(lng$estimate, wide$estimate)
  expect_equal(frm$conf.int, wide$conf.int)
})

# Published values ---------------------------------------------------------------------

test_that("SEM matches Weir (2005): sqrt(MSE) and the one-way sqrt(MSW)", {
  expect_equal(unname(reli_err_test(col.names = wcols, data = weir_a)$estimate),
               sqrt(mse_a), tolerance = 1e-8)
  expect_equal(round(unname(reli_err_test(col.names = wcols,
                                          data = weir_a)$estimate), 1), 7.6)
  expect_equal(round(unname(reli_err_test(col.names = wcols,
                                          data = weir_c)$estimate), 1), 2.2)
  expect_equal(unname(reli_err_test(col.names = wcols, data = weir_a,
                                    model = "oneway")$estimate),
               sqrt(53.75), tolerance = 1e-8)
  expect_equal(round(unname(reli_err_test(col.names = wcols, data = weir_c,
                                          model = "oneway")$estimate), 1),
               round(sqrt(124.25), 1))
})

test_that("SEE and SEP use the matched SD (Weir data set A: 7.35, 10.54)", {
  see <- reli_err_test(col.names = wcols, data = weir_a, stat = "SEE")
  sep <- reli_err_test(col.names = wcols, data = weir_a, stat = "SEP")
  expect_equal(round(unname(see$estimate), 2), 7.35)
  expect_equal(round(unname(sep$estimate), 2), 10.54)
  # by hand: matched SD^2 = (MSB + MSE) / 2, ICC(C,1) = (MSB - MSE) / (MSB + MSE)
  msb <- msb_a
  sd2 <- (msb + mse_a) / 2
  rho <- (msb - mse_a) / (msb + mse_a)
  expect_equal(unname(see$estimate), sqrt(sd2 * rho * (1 - rho)), tolerance = 1e-6)
  expect_equal(unname(sep$estimate), sqrt(sd2 * (1 - rho^2)), tolerance = 1e-6)
  expect_match(see$method, "from ICC\\(C,1\\)")
})

test_that("with the matched SD, SD * sqrt(1 - ICC) equals the SEM for every form", {
  for (spec in list(c("twoway", "consistency"), c("twoway", "agreement"),
                    c("oneway", "agreement"))) {
    p <- SimplyAgree:::.reli_prepare(col.names = sf_cols, data = sf_wide)
    f <- SimplyAgree:::.reli_fit(p, model = spec[1])
    rho <- SimplyAgree:::.icc_estimate(f, SimplyAgree:::.err_icc_form(spec[1], spec[2]))
    sd <- SimplyAgree:::.sd_total(f, spec[1], spec[2])
    sem <- reli_err_test(col.names = sf_cols, data = sf_wide, model = spec[1],
                         type = spec[2])
    expect_equal(sd * sqrt(1 - rho), unname(sem$estimate), tolerance = 1e-10)
  }
})

# SEM inference --------------------------------------------------------------------------

test_that("SEM interval and test are the chi-square interval and test of MSE", {
  res <- reli_err_test(col.names = wcols, data = weir_a, null.value = 10,
                       alternative = "two.sided")
  expect_equal(as.numeric(res$conf.int),
               sqrt(c(7 * mse_a / qchisq(0.975, 7), 7 * mse_a / qchisq(0.025, 7))),
               tolerance = 1e-8)
  expect_equal(unname(res$statistic), 7 * mse_a / 100, tolerance = 1e-8)
  expect_equal(unname(res$parameter), 7)
  pl <- pchisq(7 * mse_a / 100, 7)
  expect_equal(res$p.value, 2 * min(pl, 1 - pl), tolerance = 1e-8)
})

test_that("the default one-sided test matches its upper bound", {
  res <- reli_err_test(col.names = wcols, data = weir_a, null.value = 10)
  expect_equal(res$alternative, "less")
  expect_equal(res$conf.int[1], 0)
  at_bound <- reli_err_test(col.names = wcols, data = weir_a,
                            null.value = res$conf.int[[2]])
  expect_equal(at_bound$p.value, 0.05, tolerance = 1e-8)
  greater <- reli_err_test(col.names = wcols, data = weir_a, null.value = 5,
                           alternative = "greater")
  expect_equal(greater$conf.int[2], Inf)
})

test_that("agreement and one-way SEM are equal with complete data; df differ", {
  agr <- reli_err_test(col.names = wcols, data = weir_a, type = "agreement",
                       null.value = 10)
  one <- reli_err_test(col.names = wcols, data = weir_a, model = "oneway",
                       null.value = 10)
  expect_equal(agr$estimate, one$estimate)
  expect_equal(unname(one$parameter), 8)
  nu <- (msj_a + 7 * mse_a)^2 / (msj_a^2 / 1 + (7 * mse_a)^2 / 7)
  expect_equal(unname(agr$parameter), nu, tolerance = 1e-8)
  expect_match(agr$method, "absolute agreement")
  expect_match(one$method, "one-way")
})

test_that("for two trials, 1.96 * sqrt(2) * SEM is 1.96 * SD of the differences", {
  res <- reli_err_test(col.names = wcols, data = weir_a)
  expect_equal(qnorm(0.975) * sqrt(2) * unname(res$estimate),
               qnorm(0.975) * sd(weir_a$t1 - weir_a$t2), tolerance = 1e-10)
})

# SEE and SEP inference --------------------------------------------------------------------

test_that("SEE and SEP intervals are Wald intervals on the log scale", {
  for (s in c("SEE", "SEP")) {
    res <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = s)
    est <- unname(res$estimate)
    expect_equal(log(res$conf.int[2]) - log(est), log(est) - log(res$conf.int[1]),
                 tolerance = 1e-8)
    se <- SimplyAgree:::.err_se_log(NULL, res$fit, s, "twoway", "consistency",
                                    "normal")
    expect_equal(log(res$conf.int[2] / est), qnorm(0.975) * se, tolerance = 1e-8)
  }
})

test_that("the normal-theory delta SE matches a direct calculation", {
  p <- SimplyAgree:::.reli_prepare(col.names = sf_cols, data = sf_wide)
  f <- SimplyAgree:::.reli_fit(p)
  # SEP^2 = SD^2 (1 - rho^2) with consistency SD^2 = (MSB + 3 MSE) / 4
  g <- function(m) {
    rho <- (m[1] - m[2]) / (m[1] + 3 * m[2])
    0.5 * log((m[1] + 3 * m[2]) / 4 * (1 - rho^2))
  }
  m <- unname(f$ms[c("MSB", "MSE")])
  h <- 1e-6
  grad <- c((g(m + c(h, 0)) - g(m - c(h, 0))) / (2 * h),
            (g(m + c(0, h)) - g(m - c(0, h))) / (2 * h))
  v <- 2 * m^2 / c(5, 15)
  expect_equal(SimplyAgree:::.err_se_log(p, f, "SEP", "twoway", "consistency",
                                         "normal"),
               sqrt(sum(grad^2 * v)), tolerance = 1e-5)
})

test_that("the SEE/SEP z test inverts the interval", {
  res <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = "SEP",
                       null.value = 3)
  expect_named(res$statistic, "z")
  expect_null(res$parameter)
  at_bound <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = "SEP",
                            null.value = res$conf.int[[2]])
  expect_equal(at_bound$p.value, 0.05, tolerance = 1e-8)
})

test_that("a negative ICC is truncated at zero for SEE and SEP", {
  neg <- data.frame(a = c(1, 2, 3, 4, 5, 6), b = c(6, 5, 4, 3, 2, 1))
  p <- SimplyAgree:::.reli_prepare(col.names = c("a", "b"), data = neg)
  f <- SimplyAgree:::.reli_fit(p)
  expect_lt(SimplyAgree:::.icc_estimate(f, "C1"), 0)
  expect_equal(SimplyAgree:::.err_estimate(f, "SEE", "twoway", "consistency"), 0)
  expect_equal(SimplyAgree:::.err_estimate(f, "SEP", "twoway", "consistency"),
               SimplyAgree:::.sd_total(f, "twoway", "consistency"))
})

# Standard errors -----------------------------------------------------------------------------

test_that("normal SE of log(SEM) is the exact chi-square value", {
  res <- reli_err_test(col.names = wcols, data = weir_a, se_method = "normal")
  expect_equal(res$stderr[["log"]], 0.5 * sqrt(trigamma(7 / 2)), tolerance = 1e-10)
  expect_equal(res$stderr[["SEM"]], unname(res$estimate) * res$stderr[["log"]])
})

test_that("jackknife SE of log(SEM) matches a manual delete-one-subject loop", {
  res <- reli_err_test(col.names = sf_cols, data = sf_wide)
  z_i <- vapply(1:6, function(i) {
    Y <- as.matrix(sf_wide[-i, ])
    r <- Y - outer(rowMeans(Y), colMeans(Y), "+") + mean(Y)
    0.5 * log(sum(r^2) / (4 * 3))
  }, numeric(1))
  expect_equal(res$stderr[["log"]], sqrt(5 / 6 * sum((z_i - mean(z_i))^2)),
               tolerance = 1e-8)
})

test_that("se_method changes stderr but not the analytic interval", {
  j <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = "SEE")
  i <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = "SEE",
                     se_method = "influence")
  expect_equal(j$conf.int, i$conf.int)
  expect_false(isTRUE(all.equal(j$stderr, i$stderr)))
})

test_that("agreement SEs add the item mean square variation", {
  for (m in c("jackknife", "influence")) {
    agr <- reli_err_test(col.names = sf_cols, data = sf_wide, type = "agreement",
                         stat = "SEP", se_method = m)
    p <- SimplyAgree:::.reli_prepare(col.names = sf_cols, data = sf_wide)
    f <- SimplyAgree:::.reli_fit(p)
    no_items <- SimplyAgree:::.subject_se(
      p, f, function(ff) log(SimplyAgree:::.err_estimate(ff, "SEP", "twoway",
                                                         "agreement")),
      names = c("MSB", "MSE"), random_items = FALSE, se_method = m)
    expect_gt(agr$stderr[["log"]], no_items)
  }
})

# Argument checks -----------------------------------------------------------------------------

test_that("invalid arguments are rejected with informative errors", {
  expect_error(reli_err_test(col.names = wcols, data = weir_a, model = "oneway",
                             type = "consistency"), "not defined")
  expect_error(reli_err_test(col.names = wcols, data = weir_a, null.value = -1),
               "positive")
  expect_error(reli_err_test(col.names = wcols, data = weir_a, null.value = c(1, 2)),
               "positive")
  expect_error(reli_err_test(col.names = wcols, data = weir_a, boot = TRUE,
                             boot_ci = "stud", se_method = "normal"),
               "cannot studentize")
  expect_error(reli_err_test(col.names = wcols, data = weir_a, stat = "SDD"))
})

# Missing data -----------------------------------------------------------------------------------

test_that("missing data need REML, which warns and uses observed-data df", {
  w <- sf_wide
  w$J2[1] <- NA
  expect_error(reli_err_test(col.names = sf_cols, data = w), "method = \"reml\"")
  expect_warning(res <- reli_err_test(col.names = sf_cols, data = w,
                                      method = "reml", null.value = 2),
                 "4.2% of subject-by-item cells are missing")
  expect_equal(unname(res$parameter), 23 - 6 - 4 + 1)
  expect_match(summary(res)$text, "observed data")
})

test_that("REML on complete data matches ANOVA", {
  for (s in c("SEM", "SEE", "SEP")) {
    a <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = s)
    r <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = s,
                       method = "reml")
    expect_equal(r$estimate, a$estimate, tolerance = 1e-5)
    expect_equal(r$conf.int, a$conf.int, tolerance = 1e-4)
  }
})

# Bootstrap -------------------------------------------------------------------------------------

test_that("bootstrap results omit the statistic and invert the interval", {
  set.seed(123)
  res <- reli_err_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                       boot_ci = "perc", replicates = 300, null.value = 2,
                       alternative = "two.sided")
  expect_null(res$statistic)
  expect_null(res$parameter)
  expect_named(res$stderr, c("SEM", "log", "boot"))
  expect_equal(res$boot$n_used + res$boot$n_failed, 300)
  expect_match(res$method, "percentile interval")
  excluded <- res$conf.int[1] > 2 || res$conf.int[2] < 2
  expect_equal(res$p.value < 0.05, excluded)
  expect_match(summary(res)$text, "approximate")
})

test_that("bootstrap without a null gives an interval but no p-value", {
  set.seed(9)
  res <- reli_err_test(col.names = sf_cols, data = sf_wide, stat = "SEE",
                       boot = TRUE, replicates = 100)
  expect_null(res$p.value)
  expect_equal(res$settings$boot_ci, "stud")
  expect_true(res$conf.int[1] > 0 && res$conf.int[1] < res$conf.int[2])
})

test_that("each bootstrap interval type runs and respects the bounds", {
  for (type in c("bca", "stud", "perc", "basic")) {
    set.seed(7)
    res <- reli_err_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                         boot_ci = type, replicates = 199, null.value = 3)
    expect_equal(res$conf.int[1], 0)
    expect_gt(res$conf.int[2], 0)
    expect_true(res$p.value >= 0 && res$p.value <= 1)
  }
})

test_that("agreement statistics use the hybrid bootstrap", {
  set.seed(11)
  hyb <- reli_err_test(col.names = sf_cols, data = sf_wide, type = "agreement",
                       boot = TRUE, boot_ci = "perc", replicates = 400)
  prep <- SimplyAgree:::.reli_prepare(col.names = sf_cols, data = sf_wide)
  stat <- function(p, resample) {
    f <- SimplyAgree:::.reli_fit(p, keep_fit = FALSE)
    c(est = SimplyAgree:::.err_estimate(f, "SEM", "twoway", "agreement"),
      z = NA, se_z = NA)
  }
  set.seed(11)
  subj <- SimplyAgree:::.reli_boot(prep, stat, R = 400)
  expect_gt(hyb$boot$boot_se, sd(subj$t[, "est"]))
})

# Methods ------------------------------------------------------------------------------------------

test_that("summary describes error statistics in words", {
  s <- summary(reli_err_test(col.names = wcols, data = weir_a, null.value = 10))
  expect_match(s$text, "not statistically significant")
  expect_match(s$text, "X-squared\\(7\\)")
  expect_match(s$text, "95% upper bound")
  expect_match(s$text, "less than 10")
  expect_match(s$text, "typical error of a single measurement")
  expect_match(s$text, "chi-square")
  expect_no_match(s$text, "ICC\\(3,1\\)")

  e <- summary(reli_err_test(col.names = wcols, data = weir_a, stat = "SEP",
                             type = "agreement"))
  expect_match(e$text, "^The estimate was SEP = ")
  expect_match(e$text, "95% CI")
  expect_match(e$text, "ICC\\(A,1\\)")
  expect_match(e$text, "delta method")
  expect_match(e$text, "including systematic differences")
})

test_that("plot and check work for error results", {
  res <- reli_err_test(col.names = sf_cols, data = sf_wide)
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(check(res), "patchwork")
})
