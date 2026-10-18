testthat::local_edition(3)

sf_wide <- data.frame(J1 = c(9, 6, 8, 7, 10, 6),
                      J2 = c(2, 1, 4, 1, 5, 2),
                      J3 = c(5, 3, 6, 2, 6, 4),
                      J4 = c(8, 2, 8, 6, 9, 7))
sf_long <- data.frame(id = rep(1:6, times = 4),
                      judge = rep(names(sf_wide), each = 6),
                      score = unlist(sf_wide, use.names = FALSE))
sf_cols <- names(sf_wide)

weir_a <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                     t2 = c(140, 152, 152, 99, 145, 153, 167, 218))

# Structure ------------------------------------------------------------------------

test_that("icc_test returns a reli_htest with the htest elements", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0)
  expect_s3_class(res, c("reli_htest", "htest"))
  expect_named(res$estimate, "ICC(C,1)")
  expect_named(res$statistic, "F")
  expect_named(res$parameter, c("df1", "df2"))
  expect_equal(res$null.value, c(ICC = 0))
  expect_equal(res$alternative, "two.sided")
  expect_equal(attr(res$conf.int, "conf.level"), 0.95)
  expect_match(res$method, "Two-way, consistency, single-measure ICC")
  expect_match(res$method, "ICC\\(3,1\\)")
  expect_equal(res$data.name, "J1, J2, J3, J4")
  expect_output(print(res), "ICC\\(C,1\\)")
})

test_that("the three input interfaces give the same result", {
  wide <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0)
  long <- icc_test(measure = "score", item = "judge", id = "id", data = sf_long)
  form <- icc_test(score ~ judge | id, data = sf_long, null.value = 0)
  expect_equal(long$estimate, wide$estimate)
  expect_equal(form$conf.int, wide$conf.int)
  expect_equal(form$p.value, wide$p.value)
})

# Published values -------------------------------------------------------------------

test_that("all six forms match Shrout & Fleiss (1979) and psych 2.6.3", {
  specs <- list(list("oneway", "agreement", "single", "ICC(1)", 0.17, c(-0.13293, 0.72256)),
                list("twoway", "agreement", "single", "ICC(A,1)", 0.29, c(0.01879, 0.76108)),
                list("twoway", "consistency", "single", "ICC(C,1)", 0.71, c(0.34246, 0.94586)),
                list("oneway", "agreement", "average", "ICC(k)", 0.44, c(-0.88444, 0.91242)),
                list("twoway", "agreement", "average", "ICC(A,k)", 0.62, c(0.07114, 0.92723)),
                list("twoway", "consistency", "average", "ICC(C,k)", 0.91, c(0.67567, 0.98589)))
  for (s in specs) {
    res <- icc_test(col.names = sf_cols, data = sf_wide, model = s[[1]],
                    type = s[[2]], unit = s[[3]])
    expect_named(res$estimate, s[[4]])
    expect_equal(round(unname(res$estimate), 2), s[[5]])
    expect_equal(round(as.numeric(res$conf.int), 5), s[[6]])
  }
})

test_that("consistency ICC matches Weir (2005) data set A", {
  res <- icc_test(col.names = c("t1", "t2"), data = weir_a, null.value = 0)
  expect_equal(round(unname(res$estimate), 2), 0.95)
  # F for subjects = MSB / MSE = 2098.54 / 57.11
  expect_equal(round(unname(res$statistic), 1), round(2098.5375 / (399.8 / 7), 1))
})

# Tests and one-sided intervals ----------------------------------------------------------

test_that("one-sided alternatives give one-sided intervals consistent with the test", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.5,
                  alternative = "greater")
  expect_equal(res$conf.int[2], 1)
  at_bound <- icc_test(col.names = sf_cols, data = sf_wide,
                       null.value = res$conf.int[[1]], alternative = "greater")
  expect_equal(at_bound$p.value, 0.05, tolerance = 1e-6)

  less <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.9,
                   alternative = "less")
  expect_equal(less$conf.int[1], -Inf)
})

test_that("by default there is no test and the interval is two-sided", {
  res <- icc_test(col.names = sf_cols, data = sf_wide)
  expect_null(res$statistic)
  expect_null(res$parameter)
  expect_null(res$p.value)
  expect_null(res$null.value)
  expect_equal(res$alternative, "two.sided")
  expect_output(print(res), "95 percent confidence interval")
  # a one-sided alternative without a null value still gives a two-sided interval
  g <- icc_test(col.names = sf_cols, data = sf_wide, alternative = "greater")
  expect_equal(g$conf.int, res$conf.int)
})

test_that("a two-sided test equals twice the smaller one-sided p-value", {
  g <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.6,
                alternative = "greater")
  l <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.6,
                alternative = "less")
  t2 <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.6)
  expect_equal(g$p.value + l$p.value, 1)
  expect_equal(t2$p.value, 2 * min(g$p.value, l$p.value))
})

test_that("conf.level passed as a variable is stored evaluated", {
  cl <- 0.9
  res <- icc_test(col.names = sf_cols, data = sf_wide, conf.level = cl)
  expect_equal(attr(res$conf.int, "conf.level"), 0.9)
  expect_equal(res$settings$conf.level, 0.9)
  expect_match(summary(res)$text, "90% compatibility interval")
})

# Standard errors ----------------------------------------------------------------------------

test_that("stderr is reported on the ICC and z scales", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0)
  expect_named(res$stderr, c("ICC", "z"))
  # ICC(C,1) = (F - 1) / (F + k - 1) with F = exp(2 z): d rho / d z = 2 k F / (F + k - 1)^2
  k <- 4
  F_obs <- exp(2 * 0.5 * log(res$statistic[[1]]))
  expect_equal(res$stderr[["ICC"]],
               res$stderr[["z"]] * 2 * k * F_obs / (F_obs + k - 1)^2,
               tolerance = 1e-6)
  expect_equal(res$settings$se_method, "jackknife")
})

test_that("se_method changes stderr but not the F interval or test", {
  infl <- icc_test(col.names = sf_cols, data = sf_wide, se_method = "influence",
                   null.value = 0)
  jack <- icc_test(col.names = sf_cols, data = sf_wide, se_method = "jackknife",
                   null.value = 0)
  norm <- icc_test(col.names = sf_cols, data = sf_wide, se_method = "normal",
                   null.value = 0)
  expect_false(isTRUE(all.equal(infl$stderr, jack$stderr)))
  expect_equal(infl$conf.int, jack$conf.int)
  expect_equal(infl$p.value, norm$p.value)
  expect_equal(norm$stderr[["z"]], 0.5 * sqrt(trigamma(5 / 2) + trigamma(15 / 2)))
})

test_that("studentized bootstrap rejects SEs that cannot studentize", {
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                        boot_ci = "stud", se_method = "normal"),
               "cannot studentize")
  w <- sf_wide
  w$J2[1] <- NA
  expect_error(icc_test(col.names = sf_cols, data = w, method = "reml",
                        boot = TRUE, boot_ci = "stud", se_method = "jackknife"),
               "se_method = \"influence\"")
})

test_that("studentized bootstrap is no longer the basic interval on the z scale", {
  set.seed(21)
  res <- icc_test(col.names = c("t1", "t2"), data = weir_a, boot = TRUE,
                  boot_ci = "stud", replicates = 300)
  set.seed(21)
  basic <- icc_test(col.names = c("t1", "t2"), data = weir_a, boot = TRUE,
                    boot_ci = "basic", replicates = 300)
  expect_false(isTRUE(all.equal(as.numeric(res$conf.int),
                                as.numeric(basic$conf.int))))
  expect_true(res$conf.int[1] < res$estimate && res$estimate < res$conf.int[2])
})

# Argument checks --------------------------------------------------------------------------

test_that("invalid arguments are rejected with informative errors", {
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, model = "oneway",
                        type = "consistency"),
               "not defined for model = \"oneway\"")
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, conf.level = 95),
               "conf.level")
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, null.value = 1),
               "below 1")
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, null.value = -0.5),
               "-1/\\(k - 1\\)")
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, boot = "yes"),
               "'boot'")
  expect_error(icc_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                        replicates = 1.5),
               "replicates")
})

test_that("one-way model defaults to agreement", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, model = "oneway")
  expect_equal(res$settings$type, "agreement")
  expect_named(res$estimate, "ICC(1)")
})

# Missing data ------------------------------------------------------------------------------------

test_that("missing data need REML, which warns and reports observed-data df", {
  w <- sf_wide
  w$J2[1] <- NA
  expect_error(icc_test(col.names = sf_cols, data = w), "method = \"reml\"")
  expect_warning(res <- icc_test(col.names = sf_cols, data = w, method = "reml",
                                 null.value = 0),
                 "4.2% of subject-by-item cells are missing")
  expect_equal(unname(res$parameter), c(5, 23 - 6 - 4 + 1))
  expect_match(summary(res)$text, "1 of 24 subject-by-item cells missing")
})

test_that("REML on complete data matches ANOVA", {
  a <- icc_test(col.names = sf_cols, data = sf_wide)
  r <- icc_test(col.names = sf_cols, data = sf_wide, method = "reml")
  expect_equal(r$estimate, a$estimate, tolerance = 1e-5)
  expect_equal(r$conf.int, a$conf.int, tolerance = 1e-5)
  expect_match(r$method, "REML")
})

# Bootstrap ------------------------------------------------------------------------------------------

test_that("bootstrap results omit the statistic and report replicates", {
  set.seed(123)
  res <- icc_test(col.names = c("t1", "t2"), data = weir_a, boot = TRUE,
                  boot_ci = "perc", replicates = 200, null.value = 0)
  expect_null(res$statistic)
  expect_null(res$parameter)
  expect_named(res$stderr, c("ICC", "z", "boot"))
  expect_equal(res$stderr[["boot"]], sd(res$boot$estimates))
  expect_equal(res$boot$n_used + res$boot$n_failed, 200)
  expect_match(res$method, "percentile interval")
  expect_true(res$conf.int[1] <= res$estimate && res$estimate <= res$conf.int[2])
  expect_match(summary(res)$text, "approximate")
})

test_that("each bootstrap interval type runs and respects the parameter bounds", {
  for (type in c("bca", "stud", "perc", "basic")) {
    set.seed(7)
    res <- icc_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                    boot_ci = type, replicates = 199, null.value = 0,
                    alternative = "greater")
    expect_equal(res$conf.int[2], 1)
    expect_gte(res$conf.int[1], -1 / 3)
    expect_true(res$p.value >= 0 && res$p.value <= 1)
  }
})

test_that("agreement ICCs use the hybrid bootstrap, which adds item variation", {
  set.seed(11)
  expect_no_warning(hyb <- icc_test(col.names = sf_cols, data = sf_wide,
                                    type = "agreement", boot = TRUE,
                                    replicates = 400))
  # subject-only resampling of the same statistic, for comparison
  prep <- SimplyAgree:::.reli_prepare(col.names = sf_cols, data = sf_wide)
  stat <- function(p, resample) {
    f <- SimplyAgree:::.reli_fit(p, keep_fit = FALSE)
    c(est = SimplyAgree:::.icc_estimate(f, "A1"), z = NA, se_z = NA)
  }
  set.seed(11)
  subj <- SimplyAgree:::.reli_boot(prep, stat, R = 400)
  expect_gt(hyb$boot$boot_se, sd(subj$t[, "est"]))
  # the estimate itself is not perturbed
  expect_equal(unname(hyb$estimate),
               unname(icc_test(col.names = sf_cols, data = sf_wide,
                               type = "agreement")$estimate))
})

test_that("REML bootstrap with missing data runs without the unbalanced warning", {
  w <- sf_wide
  w$J2[1] <- NA
  set.seed(5)
  expect_no_warning(res <- icc_test(col.names = sf_cols, data = w,
                                    method = "reml", boot = TRUE,
                                    boot_ci = "perc", replicates = 30))
  expect_true(is.finite(res$conf.int[1]))
})

test_that("boot_ci = 'auto' picks studentized for ANOVA and percentile for REML", {
  set.seed(31)
  a <- icc_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                replicates = 60)
  expect_equal(a$settings$boot_ci, "stud")
  expect_match(a$method, "studentized interval")

  w <- sf_wide
  w$J2[1] <- NA
  set.seed(31)
  r <- icc_test(col.names = sf_cols, data = w, method = "reml", boot = TRUE,
                replicates = 30)
  expect_equal(r$settings$boot_ci, "perc")

  set.seed(31)
  n <- icc_test(col.names = sf_cols, data = sf_wide, boot = TRUE,
                replicates = 60, se_method = "normal")
  expect_equal(n$settings$boot_ci, "perc")
})

test_that("requesting the studentized bootstrap with REML warns", {
  w <- sf_wide
  w$J2[1] <- NA
  set.seed(32)
  expect_warning(icc_test(col.names = sf_cols, data = w, method = "reml",
                          boot = TRUE, boot_ci = "stud",
                          se_method = "influence", replicates = 30),
                 "studentized bootstrap with method = \"reml\" can be unreliable")
})

# Methods ------------------------------------------------------------------------------------------

test_that("summary describes the result in words", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0)
  s <- summary(res)
  expect_s3_class(s, "summary.reli_htest")
  expect_equal(s$style, "hypothesis")
  expect_match(s$text, "statistically significant")
  expect_match(s$text, "Shrout & Fleiss ICC\\(3,1\\)")
  expect_match(s$text, "excluding systematic differences between items")
  expect_match(s$text, "6 subjects and 4 items")
  expect_match(s$text, "exact")
  expect_no_match(s$text, "excellent|good|poor")
  expect_output(print(s), "ICC\\(C,1\\) = 0.715")

  avg <- summary(icc_test(col.names = sf_cols, data = sf_wide,
                          type = "agreement", unit = "average"))
  expect_match(avg$text, "mean of all k = 4 items")
  expect_match(avg$text, "Satterthwaite")
})

test_that("summary words one-sided intervals as bounds", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.5,
                  alternative = "greater")
  expect_match(summary(res)$text, "95% lower bound")
  expect_match(summary(res)$text, "greater than 0.5")
})

test_that("summary style 'auto' follows whether a test was requested", {
  est <- summary(icc_test(col.names = sf_cols, data = sf_wide))
  expect_equal(est$style, "estimation")
  expect_null(est$s.value)
  expect_match(est$text, "95% compatibility interval")
  expect_match(est$text, "F-based interval is exact")
  expect_no_match(est$text, "significant|reject|alpha|F test")

  expect_message(fallback <- summary(icc_test(col.names = sf_cols, data = sf_wide),
                                     style = "hypothesis"),
                 "No test was requested")
  expect_equal(fallback$style, "estimation")
})

test_that("estimation style reports the S-value for the tested hypothesis", {
  res <- icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.5,
                  alternative = "greater")
  s <- summary(res, style = "estimation")
  expect_equal(s$s.value, -log2(res$p.value))
  expect_match(s$text, "95% lower compatibility bound")
  expect_match(s$text, "hypothesis that the true ICC\\(C,1\\) is at most 0.5")
  expect_match(s$text, "3 heads in a row from 3 tosses")
  expect_no_match(s$text, "significant|reject")

  two <- summary(icc_test(col.names = sf_cols, data = sf_wide, null.value = 0.7),
                 style = "estimation")
  expect_match(two$text, "is equal to 0.7")
  expect_match(two$text, "less surprising than a single head")
})

test_that("summary paragraphs are stable", {
  expect_snapshot(summary(icc_test(col.names = sf_cols, data = sf_wide)))
  res <- icc_test(col.names = sf_cols, data = sf_wide, type = "agreement",
                  null.value = 0.5, alternative = "greater")
  expect_snapshot(summary(res))
  expect_snapshot(summary(res, style = "estimation"))
})

test_that("each plot type returns a ggplot", {
  res <- icc_test(col.names = sf_cols, data = sf_wide)
  for (tp in c("profile", "subjects", "items", "pairs")) {
    expect_s3_class(plot(res, type = tp), "ggplot")
  }
  expect_error(plot(res, type = "bar"))
  # pairs: one panel per pair of items
  expect_equal(nlevels(plot(res, type = "pairs")$data$pair), choose(4, 2))
  # subjects ordered by their mean
  expect_equal(levels(plot(res, type = "subjects")$data$id),
               as.character(order(rowMeans(sf_wide))))
})

test_that("profile item means are adjusted for subjects with missing data", {
  df <- icc_test(col.names = sf_cols, data = sf_wide)$data
  m <- SimplyAgree:::.adjusted_item_means(df)
  expect_equal(m$values, unname(colMeans(sf_wide)))
  # 95% CI from the two-way error mean square
  mse <- sum(residuals(lm(values ~ id + items, data = df))^2) / 15
  expect_equal(m$upper - m$values, rep(qt(0.975, 15) * sqrt(mse / 6), 4))

  w <- sf_wide
  w$J2[1] <- NA
  res_na <- suppressWarnings(icc_test(col.names = sf_cols, data = w,
                                      method = "reml"))
  m_na <- SimplyAgree:::.adjusted_item_means(res_na$data)
  # subject 1 is high, so the raw J2 mean without it is too low
  expect_gt(m_na$values[2], mean(w$J2, na.rm = TRUE))
  expect_s3_class(plot(res_na, type = "pairs"), "ggplot")
})

test_that("check adds the within-subject SD against the subject mean", {
  res <- icc_test(col.names = sf_cols, data = sf_wide)
  chk <- check(res)
  expect_s3_class(chk, "patchwork")
  # normality, homogeneity (check_reli), additivity, then error vs mean last
  expect_length(chk$patches$plots, 3)
  p_err <- chk
  expect_match(p_err$labels$caption, "Kendall's tau")
  expect_match(p_err$labels$caption, "scale = \"log\"")
  sd_i <- apply(sf_wide, 1, sd)
  expect_equal(sort(p_err$data$sd), sort(unname(sd_i)))

  # on the log scale the panel uses the log values and drops the hint
  lg <- reli_err_test(col.names = sf_cols, data = sf_wide, scale = "log")
  p_lg <- check(lg)
  expect_equal(sort(p_lg$data$sd), sort(unname(apply(log(sf_wide), 1, sd))))
  expect_no_match(p_lg$labels$caption, "consider")

  # too few subjects with repeated measurements: the two residual panels only
  two <- data.frame(a = c(1, 2), b = c(1.5, 2.6), c = c(0.7, 2.9),
                    d = c(1.4, 1.8))
  r2 <- icc_test(col.names = c("a", "b", "c", "d"), data = two)
  expect_null(SimplyAgree:::.plot_error_vs_mean(r2$data$id, r2$data$values))
  expect_s3_class(check(r2), "patchwork")
})

test_that("plot and check return plots", {
  res <- icc_test(col.names = sf_cols, data = sf_wide)
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(check(res), "patchwork")
  w <- sf_wide
  w$J2[1] <- NA
  res_reml <- suppressWarnings(icc_test(col.names = sf_cols, data = w,
                                        method = "reml"))
  expect_s3_class(check(res_reml), "patchwork")
})

# Diagnostics: non-additivity ------------------------------------------------------

panel_titles <- function(p) {
  c(vapply(p$patches$plots, function(q) {
    t <- q$labels$title
    if (is.null(t)) NA_character_ else t
  }, character(1)), if (is.null(p$labels$title)) NA_character_ else p$labels$title)
}

to_long <- function(w) {
  data.frame(values = unlist(w, use.names = FALSE),
             id = factor(rep(seq_len(nrow(w)), ncol(w))),
             items = factor(rep(names(w), each = nrow(w)), levels = names(w)))
}

tukey_F <- function(Y) {
  n <- nrow(Y); k <- ncol(Y)
  gm <- mean(Y)
  a <- rowMeans(Y) - gm
  b <- colMeans(Y) - gm
  r <- Y - outer(rowMeans(Y), colMeans(Y), "+") + gm
  ss_n <- sum(Y * outer(a, b))^2 / (sum(a^2) * sum(b^2))
  ss_n / ((sum(r^2) - ss_n) / ((n - 1) * (k - 1) - 1))
}

test_that("the fitted-value test equals Tukey's test with complete data", {
  Y <- as.matrix(sf_wide)
  tst <- SimplyAgree:::.fitted_square_test(values ~ id + items, to_long(sf_wide))
  expect_equal(tst$statistic, tukey_F(Y), tolerance = 1e-8)
  expect_equal(tst$df, c(1, 14))
})

test_that("Mandel's test has k - 1 df and equals Tukey's test for two items", {
  m <- SimplyAgree:::.mandel_test(to_long(sf_wide))
  expect_equal(m$df, c(3, 15 - 3))
  expect_true(is.finite(m$p.value))
  two <- weir_a
  m2 <- SimplyAgree:::.mandel_test(to_long(two))
  expect_equal(m2$statistic, tukey_F(as.matrix(two)), tolerance = 1e-8)
  expect_equal(m2$df, c(1, 6))
})

test_that("Mandel's test detects items that differ in scale; Tukey's does not", {
  set.seed(4)
  s <- rnorm(30, 50, 10)
  scaled <- data.frame(a = s + rnorm(30), b = 1.6 * s - 30 + rnorm(30),
                       c = s + 5 + rnorm(30))
  d <- to_long(scaled)
  expect_lt(SimplyAgree:::.mandel_test(d)$p.value, 1e-6)
  expect_gt(SimplyAgree:::.fitted_square_test(values ~ id + items, d)$p.value,
            0.05)
})

test_that("check shows the additivity panel for two-way models only", {
  chk <- check(icc_test(col.names = sf_cols, data = sf_wide))
  expect_true("Additivity" %in% panel_titles(chk))
  expect_match(chk$patches$plots[[3]]$labels$caption,
               "Mandel's test (items differ in scale): p = ", fixed = TRUE)
  one <- check(icc_test(col.names = sf_cols, data = sf_wide, model = "oneway"))
  expect_false("Additivity" %in% panel_titles(one))

  # missing data: the regression form still gives an exact F test
  w <- sf_wide
  w$J2[1] <- NA
  res_na <- suppressWarnings(icc_test(col.names = sf_cols, data = w,
                                      method = "reml"))
  m <- SimplyAgree:::.mandel_test(res_na$data)
  expect_equal(m$df, c(3, 23 - 6 - 4 + 1 - 3))
  expect_true(is.finite(m$p.value))
  expect_true("Additivity" %in% panel_titles(check(res_na)))
})
