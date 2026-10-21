testthat::local_edition(3)

# Same generator as references/general/reli_sims/gstudy_crosscheck.R (written for the
# function's development name, gstudy_test()), which
# produced the reference values from VCA 1.5.2, rptR 0.9.23, SixSigma 0.11.1
# and gtheoryr 0.1.0 used below.
facet_test_data <- function(seed, n = 15, k = 3, r = 3,
                             v = c(p = 1, i = 0.2, pi = 0.15, e = 0.3)) {
  set.seed(seed)
  d <- expand.grid(rep = seq_len(r), items = paste0("R", seq_len(k)),
                   id = seq_len(n))
  p <- rnorm(n, 0, sqrt(v[["p"]]))
  it <- rnorm(k, 0, sqrt(v[["i"]]))
  pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
  d$values <- 20 + p[d$id] + it[as.integer(d$items)] +
    pi[cbind(d$id, as.integer(d$items))] + rnorm(nrow(d), 0, sqrt(v[["e"]]))
  d$id <- factor(d$id)
  d
}
bal <- facet_test_data(101)
unb <- facet_test_data(202)
set.seed(203)
unb <- unb[-sample(nrow(unb), 20), ]

fit_of <- function(d, method = "anova") {
  SimplyAgree:::.facet_fit(
    SimplyAgree:::.facet_prepare(values ~ items | id, data = d), method)
}
form_facet <- function(form) if (form == "intra") "intra" else "inter"
form_type <- function(form) if (form == "agreement") "agreement" else "consistency"
spec_of <- function(stat = "ICC", form = "intra", k_avg = 1, r_avg = 1,
                    scale = "raw") {
  list(stat = stat, facet = form_facet(form), type = form_type(form),
       k_avg = k_avg, m_avg = 1, r_avg = r_avg, scale = scale)
}
cell_means <- function(d) stats::aggregate(values ~ items + id, data = d, FUN = mean)

# Structure --------------------------------------------------------------------

test_that("reli_facet_test returns a reli_htest with the expected parts", {
  res <- reli_facet_test(values ~ items | id, data = bal)
  expect_s3_class(res, c("reli_htest", "htest"))
  expect_named(res$estimate, "ICC(intra)")
  expect_null(res$p.value)
  expect_equal(res$alternative, "two.sided")
  expect_named(res$stderr, c("ICC(intra)", "z"))
  expect_s3_class(res$fit, "facet_fit")
  expect_equal(nrow(res$components), 4)
  expect_equal(sum(res$components$percent), 100)
  expect_match(res$method, "Intra-rater ICC (same item, another repeat)",
               fixed = TRUE)
  expect_output(print(res), "ICC\\(intra\\)")

  labels <- c(
    reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "consistency")$estimate,
    reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement")$estimate,
    reli_facet_test(values ~ items | id, data = bal, stat = "SEM")$estimate,
    reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                facet = "inter", type = "agreement", scale = "log")$estimate)
  expect_named(labels, c("ICC(inter,C)", "ICC(inter,A)", "SEM(intra)",
                         "SEM(inter,A) (sympercent)"))
})

test_that("formula and long interfaces agree", {
  a <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement")
  b <- reli_facet_test(measure = "values", item = "items", id = "id", data = bal,
                   facet = "inter", type = "agreement")
  expect_equal(a$estimate, b$estimate)
  expect_equal(a$conf.int, b$conf.int)
})

test_that("input validation errors are informative", {
  one <- bal[bal$rep == 1, ]
  expect_error(reli_facet_test(values ~ items | id, data = one),
               "icc_test\\(\\) or reli_err_test\\(\\)")
  expect_error(reli_facet_test(col.names = c("a", "b"), data = bal),
               "unused argument")
  expect_error(SimplyAgree:::.reli_prepare(col.names = c("values", "rep"),
                                           data = bal, repeats = TRUE),
               "long data")
  expect_error(reli_facet_test(values ~ items | id, data = bal, k_avg = 2),
               "'k_avg' must be 1")
  expect_error(reli_facet_test(values ~ items | id, data = bal, k_avg = 1.5,
                           facet = "inter", type = "agreement"), "whole number")
  expect_error(reli_facet_test(values ~ items | id, data = bal, scale = "log"),
               "stat = \"SEM\" only")
  expect_error(reli_facet_test(values ~ items | id, data = bal, null.value = 1),
               "below 1")
  expect_error(reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                           null.value = -1), "positive")
  expect_error(reli_facet_test(values ~ items | id, data = unb),
               "method = \"reml\"")
  expect_error(reli_facet_test(values ~ items | id, data = bal, boot = TRUE,
                           boot_ci = "stud", se_method = "normal"),
               "jackknife")
  expect_error(reli_facet_test(values ~ items | id, data = unb, method = "reml",
                           boot = TRUE, boot_ci = "stud"), "perc")
  neg <- bal
  neg$values[1] <- -1
  expect_error(reli_facet_test(values ~ items | id, data = neg, stat = "SEM",
                           scale = "log"), "positive")
})

test_that("icc_test and reli_err_test point to reli_facet_test for repeats", {
  expect_error(icc_test(values ~ items | id, data = bal),
               "reli_facet_test\\(\\)")
  expect_error(reli_err_test(values ~ items | id, data = bal),
               "reli_facet_test\\(\\)")
})

# Estimation: cross-package checks (criterion 1) --------------------------------

test_that("ANOVA mean squares and components match VCA::anovaVCA", {
  f <- fit_of(bal)
  expect_equal(unname(f$ms), c(7.4605069, 2.7870888, 0.6961503, 0.2908892),
               tolerance = 1e-7)
  expect_equal(unname(f$df), c(14, 2, 28, 90))
  expect_equal(unname(f$vc), c(0.7515952, 0.0464653, 0.1350870, 0.2908892),
               tolerance = 1e-6)
})

test_that("the within-cell variance interval matches VCA::VCAinference", {
  comp <- SimplyAgree:::.facet_components(fit_of(bal))
  expect_equal(c(comp$lower[4], comp$upper[4]), c(0.2216094, 0.3988024),
               tolerance = 1e-6)
  sem <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM")
  expect_equal(as.numeric(sem$conf.int)^2, c(0.2216094, 0.3988024),
               tolerance = 1e-6)
})

test_that("REML components match VCA::remlVCA, rptR and the ANOVA", {
  fb <- fit_of(bal, "reml")
  expect_equal(fb$vc, fit_of(bal)$vc, tolerance = 1e-5)
  # unbalanced: rptR 0.9.23 (lme4) and VCA 1.5.2 remlVCA
  fu <- fit_of(unb, "reml")
  expect_equal(unname(fu$vc), c(0.9551287, 0.1960877, 0.1323509, 0.3256478),
               tolerance = 1e-6)
  expect_equal(unname(fu$vc), c(0.9551257, 0.1960945, 0.1323512, 0.3256476),
               tolerance = 1e-4)
  expect_false(fu$balanced)
  expect_equal(fu$n_cells, 45)
  expect_equal(unname(fu$df[c("pi", "e")]), c(45 - 15 - 3 + 1, 115 - 45))
})

test_that("repeatability and reproducibility match SixSigma::ss.rr", {
  f <- fit_of(bal)
  # ss.rr(alphaLim = 1): repeatability 0.2908892, reproducibility 0.1815523
  expect_equal(f$vc[["e"]], 0.2908892, tolerance = 1e-6)
  expect_equal(f$vc[["i"]] + f$vc[["pi"]], 0.1815523, tolerance = 1e-6)
})

test_that("D-study coefficients with r' = r match gtheoryr on cell means", {
  f <- fit_of(bal)
  est <- function(form, ka) {
    SimplyAgree:::.facet_estimate(f, spec_of("ICC", form, ka, 3))
  }
  expect_equal(c(est("consistency", 1), est("agreement", 1)),
               c(0.7640917, 0.7296257), tolerance = 1e-6)
  expect_equal(c(est("consistency", 3), est("agreement", 3)),
               c(0.9066886, 0.8900583), tolerance = 1e-6)
})

test_that("ANOVA formulas match the expected mean squares (hand check)", {
  f <- fit_of(bal)
  vc <- f$vc
  expect_equal(f$ms[["e"]], vc[["e"]])
  expect_equal(f$ms[["pi"]], vc[["e"]] + 3 * vc[["pi"]])
  expect_equal(f$ms[["p"]], vc[["e"]] + 3 * vc[["pi"]] + 9 * vc[["p"]])
  expect_equal(f$ms[["i"]], vc[["e"]] + 3 * vc[["pi"]] + 45 * vc[["i"]])
})

# Exact special cases -------------------------------------------------------------

test_that("with r' = r, the inter consistency ICC and its interval and test equal icc_test on cell means", {
  cm <- cell_means(bal)
  for (unit in c("single", "average")) {
    ka <- if (unit == "single") 1 else 3
    g <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "consistency",
                     k_avg = ka, r_avg = 3, null.value = 0.5,
                     alternative = "greater")
    ic <- icc_test(values ~ items | id, data = cm, unit = unit,
                   null.value = 0.5, alternative = "greater")
    expect_equal(unname(g$estimate), unname(ic$estimate), tolerance = 1e-10)
    expect_equal(as.numeric(g$conf.int), as.numeric(ic$conf.int),
                 tolerance = 1e-8)
    expect_equal(unname(g$statistic), unname(ic$statistic), tolerance = 1e-10)
    expect_equal(g$p.value, ic$p.value, tolerance = 1e-10)
    expect_equal(g$settings$ci_method, "exact")
  }
  two <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "consistency",
                     r_avg = 3)
  expect_equal(as.numeric(two$conf.int),
               as.numeric(icc_test(values ~ items | id, data = cm)$conf.int),
               tolerance = 1e-8)
})

test_that("with r' = r, the consistency SEM equals reli_err_test on cell means", {
  g <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                   facet = "inter", type = "consistency", r_avg = 3, null.value = 0.5,
                   alternative = "less")
  re <- reli_err_test(values ~ items | id, data = cell_means(bal),
                      null.value = 0.5)
  expect_equal(unname(g$estimate), unname(re$estimate))
  expect_equal(as.numeric(g$conf.int), as.numeric(re$conf.int))
  expect_equal(g$p.value, re$p.value)
  expect_equal(g$settings$ci_method, "exact")
})

test_that("the intra SEM interval is the exact chi-square interval", {
  f <- fit_of(bal)
  g <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM", r_avg = 2)
  nu <- 90
  expect_equal(unname(g$estimate), sqrt(f$ms[["e"]] / 2))
  expect_equal(as.numeric(g$conf.int),
               sqrt(nu * f$ms[["e"]] / 2 / stats::qchisq(c(0.975, 0.025), nu)))
})

# MLS and Satterthwaite machinery ------------------------------------------------

test_that("the MLS bounds reduce to exact intervals", {
  ms <- c(MSP = 7.46, MSI = 2.79, MSPI = 0.696, MSW = 0.291)
  dfs <- c(dfP = 14, dfI = 2, dfPI = 28, dfW = 90)
  a <- 0.025
  # one mean square: Graybill-Wang is the chi-square interval
  b <- SimplyAgree:::.mls_bounds(c(0, 0, 0, 2), ms, dfs, a)
  expect_equal(b, 2 * 0.291 * 90 / stats::qchisq(c(1 - a, a), 90))
  # c1 MSP - c2 MSPI: Ting's bound is 0 exactly at the F boundary
  for (q in c(stats::qf(1 - a, 14, 28), stats::qf(a, 14, 28))) {
    cf <- c(1, 0, -ms[["MSP"]] / (q * ms[["MSPI"]]), 0)
    b <- SimplyAgree:::.mls_bounds(cf, ms, dfs, a)
    expect_equal(min(abs(b)), 0, tolerance = 1e-12)
  }
})

test_that("tests agree with their intervals for every non-exact estimand", {
  for (ci_method in c("mls", "satterthwaite")) {
    for (form in c("intra", "agreement")) {
      res <- reli_facet_test(values ~ items | id, data = bal, facet = form_facet(form), type = form_type(form),
                         ci_method = ci_method)
      lim <- as.numeric(res$conf.int)
      for (nv in c(lim[1], lim[2])) {
        p <- reli_facet_test(values ~ items | id, data = bal, facet = form_facet(form), type = form_type(form),
                         ci_method = ci_method, null.value = nv)$p.value
        expect_equal(p, 0.05, tolerance = 1e-5)
      }
      g <- reli_facet_test(values ~ items | id, data = bal, facet = form_facet(form), type = form_type(form),
                       ci_method = ci_method, null.value = 0.3,
                       alternative = "greater")
      expect_equal(reli_facet_test(values ~ items | id, data = bal, facet = form_facet(form), type = form_type(form),
                               ci_method = ci_method,
                               null.value = g$conf.int[1],
                               alternative = "greater")$p.value,
                   0.05, tolerance = 1e-5)
    }
    s <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                     facet = "inter", type = "agreement", ci_method = ci_method)
    p <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                     facet = "inter", type = "agreement", ci_method = ci_method,
                     null.value = s$conf.int[2])$p.value
    expect_equal(p, 0.05, tolerance = 1e-5)
  }
})

test_that("MLS p-values above 0.5 are consistent with the other tail", {
  g <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                   ci_method = "mls", null.value = 0.95,
                   alternative = "greater")
  l <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                   ci_method = "mls", null.value = 0.95, alternative = "less")
  expect_gt(g$p.value, 0.5)
  expect_equal(g$p.value + l$p.value, 1)
  expect_null(g$statistic)
  s <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                   ci_method = "satterthwaite", null.value = 0.5)
  expect_named(s$statistic, "F")
  expect_named(s$parameter, c("df1", "df2"))
})

test_that("one-sided intervals use the full alpha in one tail", {
  two <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                     conf.level = 0.9)
  one <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                     null.value = 0.2, alternative = "greater")
  expect_equal(one$conf.int[1], two$conf.int[1])
  expect_equal(one$conf.int[2], 1)
  less <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                      null.value = 1, alternative = "less")
  expect_equal(less$conf.int[1], 0)
})

# D-study, scales, negative components --------------------------------------------

test_that("D-study coefficients rise and SEMs fall with averaging", {
  phi <- function(ka, ra) {
    unname(reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                       k_avg = ka, r_avg = ra)$estimate)
  }
  expect_lt(phi(1, 1), phi(1, 2))
  expect_lt(phi(1, 2), phi(2, 2))
  sem <- function(ra) {
    unname(reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                       r_avg = ra)$estimate)
  }
  expect_equal(sem(4), sem(1) / 2)
  expect_match(reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                           k_avg = 2, r_avg = 3)$method,
               "mean of 2 items x 3 repeats")
})

test_that("log and mean scales", {
  raw_log <- bal
  raw_log$values <- log(bal$values)
  g_log <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                       facet = "inter", type = "agreement", scale = "log")
  g_raw <- reli_facet_test(values ~ items | id, data = raw_log, stat = "SEM",
                       facet = "inter", type = "agreement")
  expect_equal(unname(g_log$estimate), 100 * unname(g_raw$estimate))
  expect_equal(as.numeric(g_log$conf.int), 100 * as.numeric(g_raw$conf.int))

  g_mean <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                        scale = "mean", null.value = 3, alternative = "less")
  expect_equal(unname(g_mean$estimate),
               100 * sqrt(fit_of(bal)$ms[["e"]]) / mean(bal$values))
  expect_equal(g_mean$settings$ci_method, "delta")
  expect_named(g_mean$statistic, "z")
  expect_equal(g_mean$conf.int[1], 0)
})

test_that("negative component estimates are kept (unconstrained)", {
  # repeats noisier than the subject-by-item interaction: sigma2_pi < 0
  neg <- facet_test_data(7, v = c(p = 1, i = 0.1, pi = 0, e = 1))
  f <- fit_of(neg)
  expect_lt(f$vc[["pi"]], 0)
  intra <- reli_facet_test(values ~ items | id, data = neg)
  vc <- f$vc
  expect_equal(unname(intra$estimate),
               (vc[["p"]] + vc[["pi"]]) / (vc[["p"]] + vc[["pi"]] + vc[["e"]]))
  expect_equal(intra$components$percent[3], 0)
  expect_warning(s <- reli_facet_test(values ~ items | id, data = neg,
                                  stat = "SEM", facet = "inter", type = "consistency",
                                  r_avg = 50),
                 "reported as 0")
  expect_equal(unname(s$estimate), 0)
})

# REML ----------------------------------------------------------------------------

test_that("REML with unbalanced data warns and uses observed-data df", {
  expect_warning(res <- reli_facet_test(values ~ items | id, data = unb,
                                    method = "reml", facet = "inter", type = "agreement"),
                 "unbalanced")
  expect_match(res$method, "REML")
  expect_true(all(is.finite(res$conf.int)))
  empty <- unb[!(unb$id == "1" & unb$items == "R1"), ]
  expect_warning(reli_facet_test(values ~ items | id, data = empty, method = "reml"),
                 "cells are empty")
  bal_reml <- reli_facet_test(values ~ items | id, data = bal, method = "reml",
                          facet = "inter", type = "agreement")
  bal_anova <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement")
  expect_equal(bal_reml$estimate, bal_anova$estimate, tolerance = 1e-5)
})

# Bootstrap -----------------------------------------------------------------------

test_that("bootstrap intervals run for every type and give p-values", {
  set.seed(11)
  res <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                     boot = TRUE, replicates = 49, null.value = 0.3,
                     alternative = "greater")
  expect_null(res$statistic)
  expect_true(res$p.value >= 0 && res$p.value <= 1)
  expect_equal(res$boot$n_used + res$boot$n_failed, 49)
  expect_named(res$stderr, c("ICC(inter,A)", "z", "boot"))
  expect_match(res$method, "bootstrap")
  # boot_ci = "auto" (simulations): stud, but BCa for the agreement SEM
  expect_equal(res$settings$boot_ci, "stud")
  set.seed(14)
  sa <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                    facet = "inter", type = "agreement", boot = TRUE, replicates = 19)
  expect_equal(sa$settings$boot_ci, "bca")
  for (bc in c("perc", "bca", "basic")) {
    set.seed(12)
    b <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                     facet = "inter", type = "agreement", boot = TRUE, boot_ci = bc,
                     replicates = 49)
    expect_true(b$conf.int[1] > 0 && b$conf.int[1] < b$conf.int[2])
  }
  set.seed(13)
  r <- suppressWarnings(reli_facet_test(values ~ items | id, data = unb,
                                    method = "reml", boot = TRUE,
                                    replicates = 19))
  expect_equal(r$settings$boot_ci, "perc")
})

test_that("the hybrid step widens agreement intervals relative to the jackknife SE", {
  res <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement")
  jk <- SimplyAgree:::.facet_jackknife_var(
    SimplyAgree:::.facet_prepare(values ~ items | id, data = bal), res$fit,
    function(f) SimplyAgree:::.facet_z_fit(f, spec_of("ICC", "agreement")),
    hold = "i")
  expect_gt(res$stderr[["z"]], sqrt(jk))
  nrm <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                     se_method = "normal")
  expect_true(is.finite(nrm$stderr[["z"]]))
})

test_that("REML jackknife standard errors refit the model", {
  res <- suppressWarnings(reli_facet_test(values ~ items | id, data = unb,
                                      method = "reml", stat = "SEM"))
  expect_true(is.finite(res$stderr[["z"]]) && res$stderr[["z"]] > 0)
})

# Methods -------------------------------------------------------------------------

test_that("summary describes the estimand and prints the components", {
  res <- reli_facet_test(values ~ items | id, data = bal, facet = "inter", type = "agreement",
                     null.value = 0.5, alternative = "greater")
  s <- summary(res)
  expect_s3_class(s, "summary.reli_htest")
  expect_match(s$text, "inter-rater ICC (absolute agreement", fixed = TRUE)
  expect_match(s$text, "3 measurements per subject-by-item cell")
  expect_match(s$text, "modified large-sample")
  expect_output(print(s), "Variance components")
  expect_output(print(s), "repeats \\(within cells\\)")
  ex <- summary(reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                            r_avg = 3))
  expect_match(ex$text, "exact")
  sat <- summary(reli_facet_test(values ~ items | id, data = bal,
                             ci_method = "satterthwaite"))
  expect_match(sat$text, "Satterthwaite")
  un <- summary(suppressWarnings(reli_facet_test(values ~ items | id, data = unb,
                                             method = "reml")))
  expect_match(un$text, "harmonic mean")
  expect_match(un$text, "approximate")
  mn <- summary(reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                            scale = "mean"))
  expect_match(mn$text, "percentage of the grand mean")
  expect_match(mn$text, "delta method")
})

test_that("plot and check work for reli_facet_test results", {
  res <- reli_facet_test(values ~ items | id, data = bal)
  for (type in c("profile", "subjects", "items", "pairs", "components")) {
    expect_s3_class(plot(res, type = type), "ggplot")
  }
  expect_error(plot(icc_test(col.names = c("t1", "t2"),
                             data = data.frame(t1 = 1:5, t2 = c(2, 1, 4, 3, 6))),
                    type = "components"),
               "reli_facet_test")
  expect_s3_class(check(res), "patchwork")
  expect_s3_class(check(suppressWarnings(
    reli_facet_test(values ~ items | id, data = unb, method = "reml",
                stat = "SEM", scale = "log"))), "patchwork")
})

# mdc_test ------------------------------------------------------------------------

test_that("mdc_test accepts a reli_facet_test SEM", {
  sem <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                     facet = "inter", type = "agreement")
  m <- mdc_test(sem)
  mult <- stats::qnorm(0.975) * sqrt(2)
  expect_equal(unname(m$estimate), mult * unname(sem$estimate))
  expect_equal(as.numeric(m$conf.int), mult * as.numeric(sem$conf.int))
  m2 <- mdc_test(sem, null.value = 3, conf.level = 0.9)
  sem2 <- reli_facet_test(values ~ items | id, data = bal, stat = "SEM",
                      facet = "inter", type = "agreement", null.value = 3 / mult,
                      alternative = "less", conf.level = 0.9)
  expect_equal(m2$p.value, sem2$p.value)
  expect_equal(as.numeric(m2$conf.int), mult * as.numeric(sem2$conf.int))
  expect_match(summary(m)$text, "minimal detectable change from the inter-rater SEM")
  expect_error(mdc_test(sem, k_avg = 2), "set the averaging there")
  expect_error(mdc_test(reli_facet_test(values ~ items | id, data = bal)),
               "stat = \"SEM\"")
})

# Crossed occasions ---------------------------------------------------------------

# Same generator as references/general/reli_sims/occasion_crosscheck.R, which
# produced the reference values from VCA 1.5.2 and gtheoryr 0.2.0 below.
occasion_test_data <- function(seed, n = 15, k = 3, m = 3, r = 1,
                               v = c(p = 1, i = 0.2, o = 0.15, pi = 0.1,
                                     po = 0.1, io = 0.05, e = 0.3)) {
  set.seed(seed)
  d <- expand.grid(rep = seq_len(r), occ = paste0("day", seq_len(m)),
                   items = paste0("R", seq_len(k)), id = seq_len(n))
  ii <- as.integer(d$items); oo <- as.integer(d$occ)
  p <- rnorm(n, 0, sqrt(v[["p"]])); it <- rnorm(k, 0, sqrt(v[["i"]]))
  o <- rnorm(m, 0, sqrt(v[["o"]]))
  pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
  po <- matrix(rnorm(n * m, 0, sqrt(v[["po"]])), n, m)
  io <- matrix(rnorm(k * m, 0, sqrt(v[["io"]])), k, m)
  d$values <- 20 + p[d$id] + it[ii] + o[oo] + pi[cbind(d$id, ii)] +
    po[cbind(d$id, oo)] + io[cbind(ii, oo)] + rnorm(nrow(d), 0, sqrt(v[["e"]]))
  d$id <- factor(d$id)
  d
}
cr <- occasion_test_data(301)
cr2 <- occasion_test_data(302, r = 2)
set.seed(303)
cr_unb <- cr[-sample(nrow(cr), 15), ]
occ_fit <- function(d, method = "anova") {
  SimplyAgree:::.facet_fit(
    SimplyAgree:::.facet_prepare(values ~ items | id, data = d,
                                 occasion = "occ"), method)
}
occ_spec <- function(stat, facet, type, k_avg = 1, m_avg = 1) {
  list(stat = stat, facet = facet, type = type, k_avg = k_avg,
       m_avg = m_avg, r_avg = 1, scale = "raw")
}

test_that("crossed ANOVA mean squares and components match VCA::anovaVCA", {
  f <- occ_fit(cr)
  expect_equal(f$design, "crossed")
  expect_equal(unname(f$ms), c(12.2361368, 1.0492918, 4.1955494, 0.7237313,
                               0.4893473, 1.7647724, 0.3705795),
               tolerance = 1e-7)
  expect_equal(unname(f$df), c(14, 2, 2, 28, 28, 4, 56))
  expect_equal(unname(f$vc), c(1.26595974, -0.02374739, 0.05137798,
                               0.11771728, 0.03958928, 0.09294620,
                               0.37057946), tolerance = 1e-7)
  # with repeats within subject x item x occasion: the three-way
  # interaction and the repeat error separate
  f2 <- occ_fit(cr2)
  expect_equal(names(f2$vc), c("p", "i", "o", "pi", "po", "io", "pio", "e"))
  expect_equal(unname(f2$vc), c(1.471575989, 0.124305947, 0.032037265,
                                0.099636830, 0.122593281, -0.003269574,
                                -0.002208500, 0.261420375), tolerance = 1e-7)
})

test_that("crossed REML components match VCA::remlVCA", {
  expect_equal(unname(occ_fit(cr, "reml")$vc),
               c(1.26703711, 0, 0.05725765, 0.11448601, 0.03920474,
                 0.07530697, 0.37173307), tolerance = 1e-4)
  fu <- occ_fit(cr_unb, "reml")
  expect_false(fu$balanced)
  expect_equal(unname(fu$vc),
               c(1.19430169, 0, 0.07562918, 0.13433057, 0.04423600,
                 0.06432129, 0.37588404), tolerance = 1e-4)
  expect_equal(unname(fu$df), c(14, 2, 2, 28, 28, 4, 120 - 1 - 78))
})

test_that("overall coefficients and errors match gtheoryr::dstudy_pxio", {
  f <- occ_fit(cr)
  ref <- list(c(1, 1, 0.5278860, 0.6484628, 0.7057239, 0.6612750),
              c(3, 3, 0.09361101, 0.11314857, 0.93114664, 0.91795527),
              c(2, 1, 0.2837376, 0.3697150, 0.8169077, 0.7739679))
  for (rf in ref) {
    est <- function(stat, type) {
      SimplyAgree:::.facet_estimate(f, occ_spec(stat, "overall", type,
                                                rf[1], rf[2]))
    }
    expect_equal(c(est("SEM", "consistency")^2, est("SEM", "agreement")^2,
                   est("ICC", "consistency"), est("ICC", "agreement")),
                 rf[3:6], tolerance = 1e-6)
  }
})

test_that("crossed estimands use the documented components", {
  vc <- occ_fit(cr)$vc
  sem2 <- function(facet, type, m_avg = 1) {
    unname(reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                           stat = "SEM", facet = facet, type = type,
                           m_avg = m_avg)$estimate)^2
  }
  expect_equal(sem2("intra", "consistency"), vc[["po"]] + vc[["pio"]])
  expect_equal(sem2("intra", "agreement", 2),
               (vc[["o"]] + vc[["po"]] + vc[["io"]] + vc[["pio"]]) / 2)
  expect_equal(sem2("inter", "consistency"), vc[["pi"]] + vc[["pio"]])
  expect_equal(sem2("inter", "agreement"),
               vc[["i"]] + vc[["pi"]] + vc[["io"]] + vc[["pio"]])
  intra <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ")
  expect_named(intra$estimate, "ICC(intra,C)")
  expect_equal(unname(intra$estimate),
               (vc[["p"]] + vc[["pi"]]) /
                 (vc[["p"]] + vc[["pi"]] + vc[["po"]] + vc[["pio"]]))
  inter <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                           facet = "inter", type = "agreement")
  expect_equal(unname(inter$estimate),
               (vc[["p"]] + vc[["po"]]) /
                 (vc[["p"]] + vc[["po"]] + vc[["i"]] + vc[["pi"]] +
                    vc[["io"]] + vc[["pio"]]))
  phi <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                         facet = "overall", type = "agreement")
  expect_named(phi$estimate, "Phi")
  expect_match(phi$method, "Dependability coefficient")
  expect_equal(nrow(phi$components), 7)
  expect_match(phi$components$component[7], "residual")
  expect_match(phi$data.name, "occasions: occ")
})

test_that("ignoring occasions: same intra agreement SEM, narrower interval", {
  r <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                       stat = "SEM", type = "agreement")
  expect_equal(unname(r$nested$estimate), unname(r$estimate))
  nested <- reli_facet_test(values ~ items | id,
                            data = cr[c("values", "items", "id")],
                            stat = "SEM")
  expect_equal(unname(nested$estimate), unname(r$estimate))
  expect_gt(r$conf.int[2], nested$conf.int[2])
  expect_match(summary(r)$text, "the same estimate")
  cons <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                          stat = "SEM")
  expect_lt(unname(cons$estimate), unname(cons$nested$estimate))
  expect_match(summary(cons)$text, "day-to-day shifts counted as repeat error")
  expect_null(reli_facet_test(values ~ items | id, data = cr,
                              occasion = "occ", facet = "inter")$nested)
})

test_that("crossed tests agree with their intervals", {
  for (ci_method in c("mls", "satterthwaite")) {
    res <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                           facet = "overall", type = "agreement",
                           ci_method = ci_method)
    for (nv in as.numeric(res$conf.int)) {
      p <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                           facet = "overall", type = "agreement",
                           ci_method = ci_method, null.value = nv)$p.value
      expect_equal(p, 0.05, tolerance = 1e-5)
    }
  }
})

test_that("crossed design validation", {
  expect_error(reli_facet_test(values ~ items | id, data = cr, m_avg = 2),
               "m_avg")
  expect_error(reli_facet_test(values ~ items | id, data = bal,
                               occasion = "rep", facet = "inter",
                               m_avg = 2), "facet = \"inter\"")
  expect_error(reli_facet_test(values ~ items | id, data = cr,
                               occasion = "occ", r_avg = 2),
               "'r_avg' must be 1")
  expect_error(reli_facet_test(values ~ items | id, data = cr,
                               occasion = "day"), "not found")
  expect_error(reli_facet_test(values ~ items | id, data = cr,
                               occasion = c("occ", "id")), "single column")
  one <- cr[cr$occ == "day1", ]
  expect_error(reli_facet_test(values ~ items | id, data = one,
                               occasion = "occ"), "2 occasions")
  expect_error(reli_facet_test(values ~ items | id, data = cr_unb,
                               occasion = "occ"), "method = \"reml\"")
  # without occasions, "overall" is "inter"
  expect_equal(reli_facet_test(values ~ items | id, data = bal,
                               facet = "overall")$estimate,
               reli_facet_test(values ~ items | id, data = bal,
                               facet = "inter")$estimate)
  # a repeat index used as the occasion is a crossed fit
  rr <- reli_facet_test(values ~ items | id, data = bal, occasion = "rep",
                        facet = "overall")
  expect_equal(rr$settings$design, "crossed")
})

test_that("crossed REML, bootstrap, methods and mdc_test", {
  expect_warning(u <- reli_facet_test(values ~ items | id, data = cr_unb,
                                      occasion = "occ", method = "reml",
                                      facet = "overall", type = "agreement"),
                 "15 of 135 cells are empty")
  expect_true(all(is.finite(u$conf.int)))

  r <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                       stat = "SEM", type = "agreement")
  s <- summary(r)
  expect_match(s$text, "3 occasions")
  expect_match(s$text, "subject-by-item-by-occasion cell")
  expect_output(print(s), "subjects x occasions")
  expect_s3_class(check(r), "patchwork")
  expect_s3_class(check(u), "patchwork")
  expect_s3_class(plot(r, type = "components"), "ggplot")
  m <- mdc_test(r)
  expect_equal(unname(m$estimate),
               stats::qnorm(0.975) * sqrt(2) * unname(r$estimate))
  expect_error(mdc_test(r, k_avg = 2), "m_avg, r_avg")
})

test_that("crossed bootstrap uses the auto interval types", {
  skip_on_cran()
  set.seed(21)
  b <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                       facet = "overall", type = "agreement", boot = TRUE,
                       replicates = 19)
  expect_equal(b$settings$boot_ci, "stud")
  set.seed(22)
  b2 <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                        stat = "SEM", type = "agreement", boot = TRUE,
                        replicates = 19)
  expect_equal(b2$settings$boot_ci, "bca")
  set.seed(23)
  b3 <- reli_facet_test(values ~ items | id, data = cr, occasion = "occ",
                        stat = "SEM", boot = TRUE, replicates = 19)
  expect_equal(b3$settings$boot_ci, "stud")
})
