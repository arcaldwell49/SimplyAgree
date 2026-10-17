testthat::local_edition(3)

sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6),
                 J2 = c(2, 1, 4, 1, 5, 2),
                 J3 = c(5, 3, 6, 2, 6, 4),
                 J4 = c(8, 2, 8, 6, 9, 7))
sf_cols <- names(sf)
sf_long <- data.frame(id = rep(1:6, times = 4),
                      it = rep(1:4, each = 6),
                      va = unlist(sf, use.names = FALSE))
weir_a <- data.frame(x = c(146, 148, 170, 90, 157, 156, 176, 205),
                     y = c(140, 152, 152, 99, 145, 153, 167, 218))

# Structure ----------------------------------------------------------------------

test_that("reli_stats and reli_aov keep the simple_reli structure", {
  for (f in list(reli_stats, reli_aov)) {
    res <- f(data = sf, wide = TRUE, col.names = sf_cols)
    expect_s3_class(res, "simple_reli")
    expect_named(res, c("icc", "lmer", "anova", "var_comp", "n.id", "n.item",
                        "call", "cv", "SEM", "SEE", "SEP"))
    expect_named(res$icc, c("model", "measures", "type", "icc", "lower.ci",
                            "upper.ci"))
    expect_equal(res$icc$type, c("ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k",
                                 "ICC3k"))
    expect_equal(dimnames(res$anova),
                 list(c("df", "SumSq", "MS", "F", "p"),
                      c("Subjects", "items", "Residual")))
    expect_equal(rownames(res$var_comp), c("ID", "Items", "Residual", "Total"))
    expect_named(res$SEM, c("estimate", "bias", "se", "lower.ci", "upper.ci"))
    expect_equal(res$n.id, 6)
    expect_equal(res$n.item, 4)
  }
  expect_s4_class(reli_stats(data = sf, wide = TRUE, col.names = sf_cols)$lmer,
                  "lmerMod")
  expect_s3_class(reli_aov(data = sf, wide = TRUE, col.names = sf_cols)$lmer,
                  "lm")
})

test_that("wide and long input give the same results", {
  wide <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  long <- reli_aov(data = sf_long, measure = "va", item = "it", id = "id")
  expect_equal(long$icc, wide$icc)
  expect_equal(long$SEM, wide$SEM)
  expect_error(reli_aov(data = sf_long), "measure, item, and id")
  expect_error(reli_aov(data = sf, wide = TRUE), "col.names")
})

# ICCs ----------------------------------------------------------------------------

test_that("ICC intervals are two-sided and match Shrout & Fleiss / psych", {
  res <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  expect_equal(round(res$icc$icc, 2), c(0.17, 0.29, 0.71, 0.44, 0.62, 0.91))
  expect_equal(round(res$icc$lower.ci, 5),
               c(-0.13293, 0.01879, 0.34246, -0.88444, 0.07114, 0.67567))
  expect_equal(round(res$icc$upper.ci, 5),
               c(0.72256, 0.76108, 0.94586, 0.91242, 0.92723, 0.98589))
  # same as icc_test()
  expect_equal(res$icc$lower.ci[3],
               icc_test(col.names = sf_cols, data = sf)$conf.int[[1]])
})

test_that("old one-sided-per-tail output is reproduced by conf.level = 1 - 2 * (1 - c)", {
  res <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols, conf.level = 0.9)
  # previous versions with conf.level = 0.95: qf(0.95) in each tail for ICC3
  F_obs <- res$anova["F", "Subjects"]
  FL <- F_obs / qf(0.95, 5, 15)
  expect_equal(res$icc$lower.ci[3], (FL - 1) / (FL + 3))
})

test_that("reli_stats (REML) equals reli_aov with complete data", {
  s <- reli_stats(data = sf, wide = TRUE, col.names = sf_cols)
  a <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  expect_equal(s$icc, a$icc, tolerance = 1e-5)
  expect_equal(s$var_comp, a$var_comp, tolerance = 1e-5)
  expect_equal(s$SEM$estimate, a$SEM$estimate, tolerance = 1e-5)
})

test_that("variance components are the ANOVA estimates", {
  res <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  ms <- res$anova["MS", ]
  expect_equal(res$var_comp["Items", "variance"],
               max((ms[["items"]] - ms[["Residual"]]) / 6, 0))
  expect_equal(res$var_comp["ID", "variance"],
               (ms[["Subjects"]] - ms[["Residual"]]) / 4)
  expect_equal(sum(res$var_comp$percent[1:3]), 1)
})

# SEM, SEE, SEP, CV ---------------------------------------------------------------

test_that("SEM, SEE and SEP match Weir (2005) data set A", {
  a <- reli_aov(data = weir_a, col.names = c("x", "y"), wide = TRUE)
  expect_equal(round(a$SEM$estimate, 1), 7.6)
  a3 <- reli_aov(data = weir_a, col.names = c("x", "y"), wide = TRUE,
                 se_type = "ICC3")
  expect_equal(round(a3$SEP$estimate, 1), 10.2)
  expect_equal(round(a3$SEE$estimate, 1), 7.1)
  # observed SD of all measurements
  sd_obs <- sd(c(weir_a$x, weir_a$y))
  rho <- a3$icc$icc[3]
  expect_equal(a3$SEM$estimate, sd_obs * sqrt(1 - rho))
})

test_that("cv_calc options", {
  a <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  ms_e <- a$anova["MS", "Residual"]
  expect_equal(a$cv$estimate, sqrt(ms_e) / mean(unlist(sf)))
  s <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols, cv_calc = "SEM",
                se_type = "ICC2")
  expect_equal(s$cv$estimate, s$SEM$estimate / mean(unlist(sf)))
  expect_warning(r <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols,
                               cv_calc = "residuals"),
                 "no longer supported")
  expect_equal(r$cv, a$cv)
})

test_that("analytic intervals: exact chi-square for sqrt(MSE), delta otherwise", {
  a <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols, other_ci = TRUE)
  ms_e <- a$anova["MS", "Residual"]
  expect_equal(c(a$SEM$lower.ci, a$SEM$upper.ci),
               sqrt(15 * ms_e / qchisq(c(0.975, 0.025), 15)))
  for (stat in c("cv", "SEE", "SEP")) {
    row <- a[[stat]]
    # log-scale Wald: symmetric on the log scale
    expect_equal(log(row$upper.ci / row$estimate),
                 log(row$estimate / row$lower.ci), tolerance = 1e-8)
    expect_true(is.finite(row$se))
  }
})

test_that("bootstrap intervals resample subjects", {
  for (tp in c("perc", "basic", "norm")) {
    set.seed(3)
    a <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols, other_ci = TRUE,
                  type = tp, replicates = 99)
    expect_true(all(is.finite(c(a$SEM$lower.ci, a$SEM$upper.ci, a$SEM$se,
                                a$SEM$bias))))
    expect_lt(a$SEP$lower.ci, a$SEP$upper.ci)
  }
  set.seed(3)
  s <- suppressMessages(reli_stats(data = weir_a, wide = TRUE,
                                   col.names = c("x", "y"), other_ci = TRUE,
                                   type = "perc", replicates = 49))
  expect_true(is.finite(s$cv$upper.ci))
})

# Missing data -------------------------------------------------------------------

test_that("reli_aov drops incomplete subjects, including missing rows", {
  w <- sf
  w$J2[1] <- NA
  expect_message(res <- reli_aov(data = w, wide = TRUE, col.names = sf_cols),
                 "1 subject\\(s\\) without a measurement on every item")
  expect_equal(res$n.id, 5)
  # a missing row in long data is caught too (bug 3)
  long <- sf_long[-1, ]
  expect_message(res_long <- reli_aov(data = long, measure = "va", item = "it",
                                      id = "id"), "dropped")
  expect_equal(res_long$icc, res$icc)
})

test_that("reli_stats keeps incomplete subjects with REML and warns", {
  w <- sf
  w$J2[1] <- NA
  expect_warning(res <- reli_stats(data = w, wide = TRUE, col.names = sf_cols),
                 "4.2% of subject-by-item cells are missing")
  expect_equal(res$n.id, 6)
  expect_equal(res$anova["df", "Residual"], 23 - 6 - 4 + 1)
})

test_that("reli_stats suggests reli_aov for two items", {
  expect_message(reli_stats(data = weir_a, wide = TRUE, col.names = c("x", "y")),
                 "Only 2 items")
})

# Methods -----------------------------------------------------------------------------

test_that("print works with settings passed as variables (bug 2)", {
  cl <- 0.9
  oc <- TRUE
  res <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols,
                  conf.level = cl, other_ci = oc)
  expect_equal(res$call$conf.level, 0.9)
  expect_true(res$call$other_ci)
  expect_output(print(res), "90% C.I.")
  expect_output(print(reli_aov(data = sf, wide = TRUE, col.names = sf_cols)),
                "Intraclass Correlation Coefficients with  95 % C.I.")
})

test_that("plot and check return plots", {
  a <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  s <- reli_stats(data = sf, wide = TRUE, col.names = sf_cols)
  expect_s3_class(plot(a), "ggplot")
  expect_s3_class(plot(s), "ggplot")
  expect_s3_class(check(a), "patchwork")
  expect_s3_class(check(s), "patchwork")
})

# jamovi --------------------------------------------------------------------------------

test_that("jmvreli uses icc_test() and reli_err_test()", {
  skip_if_not_installed("jmvcore")
  # jmvcore resolves `vars` as a quosure, so pass the names literally
  jmv <- jmvreli(data = sf, vars = c("J1", "J2", "J3", "J4"), desc = TRUE,
                 plots = TRUE)
  expect_s3_class(jmv$plots, "Image")
  tab <- jmv$icctab$asDF
  legacy <- reli_aov(data = sf, wide = TRUE, col.names = sf_cols)
  expect_equal(tab$icc, legacy$icc$icc, ignore_attr = TRUE)
  expect_equal(tab$lower.ci, legacy$icc$lower.ci, ignore_attr = TRUE)
  expect_equal(tab$upper.ci, legacy$icc$upper.ci, ignore_attr = TRUE)
  expect_equal(jmv$vartab$asDF$variance, legacy$var_comp$variance,
               ignore_attr = TRUE)
  text <- jmv$text$content
  expect_match(text, "sympercent")
  expect_match(text, "two-sided")

  w <- sf
  w$J2[1] <- NA
  jmv_na <- jmvreli(data = w, vars = c("J1", "J2", "J3", "J4"))
  expect_match(jmv_na$text$content, "REML")

  # the CV falls back to % of the mean when a value is not positive, and is
  # omitted when the mean is not positive
  z <- sf
  z$J2[2] <- 0
  expect_match(jmvreli(data = z, vars = c("J1", "J2", "J3", "J4"))$text$content,
               "% of mean")
  neg <- sf - 20
  expect_match(jmvreli(data = neg, vars = c("J1", "J2", "J3", "J4"))$text$content,
               "not available")
})
