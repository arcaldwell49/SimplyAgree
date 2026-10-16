testthat::local_edition(3)

# Shrout & Fleiss (1979) Table 2: 6 targets rated by 4 judges
sf_wide <- data.frame(J1 = c(9, 6, 8, 7, 10, 6),
                      J2 = c(2, 1, 4, 1, 5, 2),
                      J3 = c(5, 3, 6, 2, 6, 4),
                      J4 = c(8, 2, 8, 6, 9, 7))
sf_long <- data.frame(id = rep(1:6, times = 4),
                      judge = rep(names(sf_wide), each = 6),
                      score = unlist(sf_wide, use.names = FALSE))

# Weir (2005) Tables 1 and 6: 8 subjects, 2 trials
weir <- list(
  A = data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                 t2 = c(140, 152, 152, 99, 145, 153, 167, 218)),
  B = data.frame(t1 = c(166, 168, 160, 150, 147, 146, 156, 155),
                 t2 = c(160, 172, 142, 159, 135, 143, 147, 168)),
  C = data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                 t2 = c(161, 162, 189, 100, 175, 171, 195, 219))
)

fit_wide <- function(data, ...) {
  SimplyAgree:::.reli_fit(
    SimplyAgree:::.reli_prepare(col.names = names(data), data = data), ...)
}

icc_forms <- c("1", "A1", "C1", "k", "Ak", "Ck")

# Input interface -------------------------------------------------------------

test_that("formula, long and wide interfaces give the same fit", {
  p_wide <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  p_long <- SimplyAgree:::.reli_prepare(measure = "score", item = "judge",
                                        id = "id", data = sf_long)
  p_form <- SimplyAgree:::.reli_prepare(score ~ judge | id, data = sf_long)

  f_wide <- SimplyAgree:::.reli_fit(p_wide)
  expect_equal(SimplyAgree:::.reli_fit(p_long)$ms, f_wide$ms)
  expect_equal(SimplyAgree:::.reli_fit(p_form)$ms, f_wide$ms)
  expect_equal(p_form$data.name, "score ~ judge | id")
  expect_equal(c(p_wide$n, p_wide$k, p_wide$N), c(6, 4, 24))
  expect_true(p_wide$balanced)
})

test_that("item order follows the data", {
  p <- SimplyAgree:::.reli_prepare(col.names = c("J3", "J1", "J2"), data = sf_wide)
  expect_equal(levels(p$df$items), c("J3", "J1", "J2"))
})

test_that("input validation errors are informative", {
  expect_error(SimplyAgree:::.reli_prepare(data = sf_wide), "Specify the data")
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge | id, data = sf_long,
                                           col.names = "J1"),
               "only one")
  expect_error(SimplyAgree:::.reli_prepare(col.names = names(sf_wide)), "'data'")
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge, data = sf_long),
               "values ~ item \\| id")
  expect_error(SimplyAgree:::.reli_prepare(measure = "score", item = "judge",
                                           data = sf_long),
               "must all be supplied")
  expect_error(SimplyAgree:::.reli_prepare(col.names = c("J1", "J9"), data = sf_wide),
               "not found.*J9")
  expect_error(SimplyAgree:::.reli_prepare(col.names = "J1", data = sf_wide),
               "at least 2")

  chr <- sf_long
  chr$score <- as.character(chr$score)
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge | id, data = chr),
               "numeric")

  na_id <- sf_long
  na_id$id[3] <- NA
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge | id, data = na_id),
               "item and/or id")

  dup <- rbind(sf_long, sf_long[1, ])
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge | id, data = dup),
               "at most one measurement")

  one_subject <- sf_long[sf_long$id == 1, ]
  expect_error(SimplyAgree:::.reli_prepare(score ~ judge | id, data = one_subject),
               "2 subjects")
})

test_that("missing measurements make the design unbalanced", {
  w <- sf_wide
  w$J2[c(1, 4)] <- NA
  p <- SimplyAgree:::.reli_prepare(col.names = names(w), data = w)
  expect_false(p$balanced)
  expect_equal(c(p$n, p$k, p$N, p$n_missing), c(6, 4, 22, 2))
  expect_error(SimplyAgree:::.reli_fit(p, "anova"), "method = \"reml\"")
})

# Published values: Shrout & Fleiss (1979) ------------------------------------

test_that("ICC estimates match Shrout & Fleiss (1979) Table 4", {
  f <- fit_wide(sf_wide)
  est <- vapply(icc_forms, function(fm) SimplyAgree:::.icc_estimate(f, fm), 1)
  # S&F report ICC(1,1) .17, (2,1) .29, (3,1) .71, (1,4) .44, (2,4) .62, (3,4) .91
  expect_equal(round(unname(est), 2), c(0.17, 0.29, 0.71, 0.44, 0.62, 0.91))
})

test_that("two-sided ICC intervals match psych 2.6.3 and are 95% two-sided", {
  f <- fit_wide(sf_wide)
  ci <- t(vapply(icc_forms, function(fm) SimplyAgree:::.icc_ci(f, fm), c(0, 0)))
  # psych::ICC(sf_wide, alpha = .05, lmer = FALSE)$results[, 7:8], psych 2.6.3
  psych_ci <- rbind(c(-0.13293, 0.72256), c(0.01879, 0.76108),
                    c(0.34246, 0.94586), c(-0.88444, 0.91242),
                    c(0.07114, 0.92723), c(0.67567, 0.98589))
  expect_equal(unname(round(ci, 5)), psych_ci)
})

test_that("ICC(A,k) interval equals the Spearman-Brown transform of ICC(A,1)", {
  f <- fit_wide(sf_wide)
  a1 <- SimplyAgree:::.icc_ci(f, "A1")
  ak <- SimplyAgree:::.icc_ci(f, "Ak")
  k <- 4
  expect_equal(as.numeric(ak), as.numeric(k * a1 / (1 + (k - 1) * a1)),
               tolerance = 1e-6)
})

test_that("exact F tests invert to their intervals", {
  f <- fit_wide(sf_wide)
  for (fm in c("1", "k", "C1", "Ck")) {
    ci <- SimplyAgree:::.icc_ci(f, fm)
    expect_equal(SimplyAgree:::.icc_ftest(f, fm, ci[[1]])$p.value, 0.05,
                 tolerance = 1e-6)
    expect_equal(SimplyAgree:::.icc_ftest(f, fm, ci[[2]])$p.value, 0.05,
                 tolerance = 1e-6)
    lower <- SimplyAgree:::.icc_ci(f, fm, alternative = "greater")
    expect_equal(SimplyAgree:::.icc_ftest(f, fm, lower[[1]], "greater")$p.value,
                 0.05, tolerance = 1e-6)
    expect_equal(lower[[2]], 1)
  }
})

test_that("F tests at a null of zero reduce to the ANOVA row F test", {
  f <- fit_wide(sf_wide)
  ms <- f$ms
  for (fm in c("C1", "Ck", "A1", "Ak")) {
    res <- SimplyAgree:::.icc_ftest(f, fm, 0, "greater")
    expect_equal(unname(res$statistic), ms[["MSB"]] / ms[["MSE"]])
    expect_equal(unname(res$parameter), c(5, 15))
  }
  res <- SimplyAgree:::.icc_ftest(f, "1", 0, "greater")
  expect_equal(unname(res$statistic), ms[["MSB"]] / ms[["MSW"]])
  expect_equal(unname(res$parameter), c(5, 18))
})

test_that("agreement F test uses McGraw & Wong's a and b at the null value", {
  f <- fit_wide(sf_wide)
  n <- 6; k <- 4; r0 <- 0.3
  a <- k * r0 / (n * (1 - r0))
  b <- 1 + k * r0 * (n - 1) / (n * (1 - r0))
  ms <- f$ms
  res <- SimplyAgree:::.icc_ftest(f, "A1", r0, "greater")
  expect_equal(unname(res$statistic), ms[["MSB"]] / (a * ms[["MSJ"]] + b * ms[["MSE"]]))
  v <- (a * ms[["MSJ"]] + b * ms[["MSE"]])^2 /
    ((a * ms[["MSJ"]])^2 / (k - 1) + (b * ms[["MSE"]])^2 / ((n - 1) * (k - 1)))
  expect_equal(unname(res$parameter[2]), v)
})

# Published values: Weir (2005) -----------------------------------------------

test_that("ANOVA tables match Weir (2005) Tables 2, 3 and 7", {
  fa <- fit_wide(weir$A)
  fb <- fit_wide(weir$B)
  fc <- fit_wide(weir$C)
  ss <- function(f) round(unname(f$ms[c("MSB", "MSJ", "MSE")] *
                                   f$df[c("dfB", "dfJ", "dfE")]), 1)
  expect_equal(ss(fa), c(14689.8, 30.2, 399.8))
  expect_equal(ss(fb), c(1329.8, 30.2, 399.8))
  expect_equal(ss(fc), c(15925.0, 961.0, 33.0))
  expect_equal(round(fa$ms[["MSW"]], 2), 53.75)
  expect_equal(round(fc$ms[["MSW"]], 2), 124.25)
  expect_equal(round(fc$ms[["MSE"]], 2), 4.71)
})

test_that("ICCs match Weir (2005) Table 4 and the data set C text", {
  icc <- function(f) {
    vapply(c("1", "k", "A1", "Ak", "C1", "Ck"),
           function(fm) SimplyAgree:::.icc_estimate(f, fm), 1)
  }
  expect_equal(round(unname(icc(fit_wide(weir$A))), 2),
               c(0.95, 0.97, 0.95, 0.97, 0.95, 0.97))
  expect_equal(round(unname(icc(fit_wide(weir$B))), 2),
               c(0.56, 0.72, 0.55, 0.71, 0.54, 0.70))
  ic <- icc(fit_wide(weir$C))
  expect_equal(round(ic[["1"]], 3), 0.896)
  expect_equal(round(ic[["A1"]], 3), 0.901)
  # Weir reports 0.998 for ICC(3,1); the data give 0.996 (0.998 is ICC(3,k))
  expect_equal(round(ic[["C1"]], 3), 0.996)
  expect_equal(round(ic[["Ck"]], 3), 0.998)
})

test_that("SEMs match Weir (2005)", {
  sem <- function(f, model, type) sqrt(SimplyAgree:::.sem_var(f, model, type)$var)
  fa <- fit_wide(weir$A)
  fc <- fit_wide(weir$C)
  expect_equal(round(sem(fa, "twoway", "consistency"), 1), 7.6)
  expect_equal(round(sem(fa, "oneway", "agreement"), 1), 7.3)
  expect_equal(round(sem(fc, "twoway", "consistency"), 1), 2.2)
  expect_equal(round(sem(fc, "oneway", "agreement"), 1), 11.1)
  # Weir's minimal difference, from his rounded SEM of 7.6
  expect_equal(round(7.6 * 1.96 * sqrt(2), 2), 21.07)
  expect_equal(round(sem(fa, "twoway", "consistency") * 1.96 * sqrt(2), 2), 20.95)
})

# Matched total SD ---------------------------------------------------------------

test_that("matched total SD makes SD^2 * (1 - ICC) equal the error variance", {
  for (d in c(weir, list(sf = sf_wide))) {
    f <- fit_wide(d)
    for (spec in list(c("oneway", "agreement", "1"),
                      c("twoway", "consistency", "C1"),
                      c("twoway", "agreement", "A1"))) {
      sd_t <- SimplyAgree:::.sd_total(f, spec[1], spec[2])
      icc <- SimplyAgree:::.icc_estimate(f, spec[3])
      err <- SimplyAgree:::.sem_var(f, spec[1], spec[2])$var
      expect_equal(sd_t^2 * (1 - icc), err, tolerance = 1e-10)
    }
  }
})

test_that("SEE and SEP with the matched SD on Weir data set A", {
  f <- fit_wide(weir$A)
  sd_t <- SimplyAgree:::.sd_total(f, "twoway", "consistency")
  r <- SimplyAgree:::.icc_estimate(f, "C1")
  expect_equal(round(sd_t, 2), 32.83)
  expect_equal(round(sd_t * sqrt(r * (1 - r)), 2), 7.35)
  expect_equal(round(sd_t * sqrt(1 - r^2), 2), 10.54)
})

# REML engine ------------------------------------------------------------------

test_that("REML equals ANOVA on balanced data away from the boundary", {
  fa <- fit_wide(sf_wide, method = "anova")
  fr <- fit_wide(sf_wide, method = "reml")
  expect_equal(fr$ms, fa$ms, tolerance = 1e-4)
  expect_equal(fr$df, fa$df)
  for (fm in icc_forms) {
    expect_equal(SimplyAgree:::.icc_ci(fr, fm), SimplyAgree:::.icc_ci(fa, fm),
                 tolerance = 1e-4)
  }
})

test_that("REML with missing data uses observed-data design quantities", {
  w <- sf_wide
  w$J2[c(1, 4)] <- NA
  w$J4[2] <- NA
  p <- SimplyAgree:::.reli_prepare(col.names = names(w), data = w)
  f <- SimplyAgree:::.reli_fit(p, "reml")
  expect_equal(f$kbar, 21 / 6)
  expect_equal(unname(f$df), c(6 - 1, 4 - 1, 21 - 6 - 4 + 1, 21 - 6))
  # average-measure forms refer to all k = 4 items, not the 3.5 observed
  for (fm in c("Ck", "Ak")) {
    single <- SimplyAgree:::.icc_single_form(fm)
    expect_equal(SimplyAgree:::.icc_estimate(f, fm),
                 SimplyAgree:::.spearman_brown(SimplyAgree:::.icc_estimate(f, single), 4))
    expect_equal(as.numeric(SimplyAgree:::.icc_ci(f, fm)),
                 SimplyAgree:::.spearman_brown(as.numeric(SimplyAgree:::.icc_ci(f, single)), 4))
  }
  f1 <- SimplyAgree:::.reli_fit(p, "reml", model = "oneway")
  expect_equal(unname(f1$df[c("dfB", "dfW")]), c(5, 15))
  expect_true(is.na(f1$ms[["MSE"]]))
  expect_warning(SimplyAgree:::.reli_warn_unbalanced(p), "12.5% of")
})

# Chi-square, Satterthwaite and delta-method helpers ----------------------------

test_that("variance interval and test match hand calculations", {
  ci <- SimplyAgree:::.var_ci(4, 10, 0.95)
  expect_equal(as.numeric(ci), c(40 / qchisq(0.975, 10), 40 / qchisq(0.025, 10)))
  expect_equal(as.numeric(SimplyAgree:::.var_ci(4, 10, 0.95, "less")),
               c(0, 40 / qchisq(0.05, 10)))
  res <- SimplyAgree:::.var_test(4, 10, 9, "less")
  expect_equal(unname(res$statistic), 40 / 9)
  expect_equal(res$p.value, pchisq(40 / 9, 10))
  # the less-than test rejects exactly at the one-sided upper bound
  upper <- SimplyAgree:::.var_ci(4, 10, 0.95, "less")[2]
  expect_equal(SimplyAgree:::.var_test(4, 10, upper, "less")$p.value, 0.05)
})

test_that("agreement SEM uses the documented Satterthwaite df", {
  f <- fit_wide(weir$C)
  ms <- f$ms
  n <- 8; k <- 2
  nu <- (ms[["MSJ"]] + (n - 1) * ms[["MSE"]])^2 /
    (ms[["MSJ"]]^2 / (k - 1) + ((n - 1) * ms[["MSE"]])^2 / ((n - 1) * (k - 1)))
  sv <- SimplyAgree:::.sem_var(f, "twoway", "agreement")
  expect_equal(sv$df, nu)
  # with balanced data the agreement error variance equals MSW
  expect_equal(sv$var, ms[["MSW"]])
})

test_that("delta-method SE matches an analytic result", {
  # SE of sqrt(MSE): sqrt(MSE) / sqrt(2 * dfE)
  f <- fit_wide(weir$A)
  mse <- f$ms[["MSE"]]
  se <- SimplyAgree:::.delta_se(function(x) sqrt(x[[1]]), c(MSE = mse),
                                SimplyAgree:::.ms_var(f)[["MSE"]])
  expect_equal(se, sqrt(mse) / sqrt(2 * f$df[["dfE"]]), tolerance = 1e-6)
})

test_that("z working scale is 0.5 * log F and inverts", {
  f <- fit_wide(sf_wide)
  for (fm in c("1", "C1")) {
    F_obs <- f$ms[["MSB"]] / f$ms[[if (fm == "1") "MSW" else "MSE"]]
    r <- SimplyAgree:::.icc_estimate(f, fm)
    expect_equal(SimplyAgree:::.icc_z(r, fm, 4), 0.5 * log(F_obs))
  }
  for (fm in icc_forms) {
    r <- SimplyAgree:::.icc_estimate(f, fm)
    expect_equal(SimplyAgree:::.icc_z_inv(SimplyAgree:::.icc_z(r, fm, 4), fm, 4), r)
  }
  expect_equal(SimplyAgree:::.icc_z_se(f, "C1"),
               0.5 * sqrt(trigamma(5 / 2) + trigamma(15 / 2)))
  expect_true(is.finite(SimplyAgree:::.icc_z_se(f, "A1")))
})

# Standard errors ------------------------------------------------------------------

test_that("closed-form leave-one-out mean squares equal refits", {
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  loo <- SimplyAgree:::.loo_ms(SimplyAgree:::.reli_matrix(p))
  for (i in 1:6) {
    expect_equal(loo[i, ], fit_wide(sf_wide[-i, ])$ms, tolerance = 1e-10)
  }
})

test_that("jackknife SE of z matches deleting each subject and refitting", {
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  f <- SimplyAgree:::.reli_fit(p)
  for (fm in c("C1", "1", "Ck")) {
    model <- if (fm == "1") "oneway" else "twoway"
    z_i <- vapply(1:6, function(i) {
      SimplyAgree:::.icc_z_fit(fit_wide(sf_wide[-i, ], model = model), fm)
    }, 1)
    manual <- sqrt(5 / 6 * sum((z_i - mean(z_i))^2))
    expect_equal(SimplyAgree:::.icc_se_z(p, f, fm, "jackknife"), manual,
                 tolerance = 1e-8)
  }
})

test_that("influence SE matches the per-subject formula for ICC(C,1)", {
  p <- SimplyAgree:::.reli_prepare(col.names = names(weir$A), data = weir$A)
  f <- SimplyAgree:::.reli_fit(p)
  Y <- as.matrix(weir$A)
  n <- 8; k <- 2
  D <- sweep(Y, 2, colMeans(Y))
  m <- rowMeans(D)
  r <- D - m
  psi_b <- k * m^2 * n / (n - 1)
  psi_e <- rowSums(r^2) * n / ((n - 1) * (k - 1))
  psi <- 0.5 * ((psi_b - mean(psi_b)) / f$ms[["MSB"]] -
                  (psi_e - mean(psi_e)) / f$ms[["MSE"]])
  expect_equal(SimplyAgree:::.icc_se_z(p, f, "C1", "influence"),
               sqrt(sum(psi^2)) / n, tolerance = 1e-6)
  infl <- SimplyAgree:::.icc_influence(p, f)
  expect_equal(unname(colMeans(infl)), c(0, 0, 0))
})

test_that("normal-theory SE is the trigamma form", {
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  f <- SimplyAgree:::.reli_fit(p)
  expect_equal(SimplyAgree:::.icc_se_z(p, f, "C1", "normal"),
               0.5 * sqrt(trigamma(5 / 2) + trigamma(15 / 2)))
})

test_that("agreement SEs add the normal-theory item mean-square variance", {
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  f <- SimplyAgree:::.reli_fit(p)
  f_no_items <- f
  f_no_items$df[["dfJ"]] <- Inf   # removes the MSJ term
  for (m in c("influence", "jackknife")) {
    expect_gt(SimplyAgree:::.icc_se_z(p, f, "A1", m),
              SimplyAgree:::.icc_se_z(p, f_no_items, "A1", m))
  }
})

test_that("influence SE works for REML fits with missing data", {
  w <- sf_wide
  w$J2[c(1, 4)] <- NA
  p <- SimplyAgree:::.reli_prepare(col.names = names(w), data = w)
  for (spec in list(c("twoway", "C1"), c("twoway", "A1"), c("oneway", "1"))) {
    f <- SimplyAgree:::.reli_fit(p, "reml", spec[1])
    se <- SimplyAgree:::.icc_se_z(p, f, spec[2], "influence")
    expect_true(is.finite(se) && se > 0)
  }
})

test_that("jackknife and bootstrap survive deletions that leave too few items", {
  # items J2 and J3 are observed only for subject 1, so dropping subject 1
  # leaves a single item
  w <- sf_wide[, c("J1", "J2", "J3")]
  w$J2[-1] <- NA
  w$J3[-1] <- NA
  p <- SimplyAgree:::.reli_prepare(col.names = names(w), data = w)
  f <- suppressWarnings(SimplyAgree:::.reli_fit(p, "reml"))
  expect_warning(se <- SimplyAgree:::.icc_se_z(p, f, "C1", "jackknife"),
                 "1 of 6 leave-one-subject-out fits failed")
  expect_true(is.finite(se))

  set.seed(4)
  stat <- function(pp, resample) {
    ff <- suppressWarnings(SimplyAgree:::.reli_fit(pp, "reml", keep_fit = FALSE))
    c(est = SimplyAgree:::.icc_estimate(ff, "C1"), z = NA, se_z = NA)
  }
  bo <- SimplyAgree:::.reli_boot(p, stat, R = 40)
  expect_gt(bo$n_failed, 0)
  expect_equal(nrow(bo$t) + bo$n_failed, 40)
})

# Bootstrap ----------------------------------------------------------------------

# Fixed bootstrap output, matching the inputs used for the TOSTER 0.9.0
# reference values below.
make_bo <- function() {
  set.seed(42)
  b <- round(rnorm(200, 0.6, 0.1), 3)
  jack <- round(rnorm(12, 0.62, 0.03), 4)
  tv <- round(rt(200, 8), 3)
  list(t0 = c(est = 0.62, z = 0.62, se_z = 0.15),
       t = cbind(est = b, z = 0.62 + tv, se_z = 1),
       jack = cbind(est = jack, z = NA, se_z = NA),
       R = 200, n_failed = 0)
}

test_that("bootstrap p-values match TOSTER 0.9.0", {
  bo <- make_bo()
  nulls <- c(0.45, 0.6, 0.62, 0.75)
  pv <- function(type, alt) {
    vapply(nulls, function(r0) SimplyAgree:::.boot_pvalue(bo, type, r0, alt), 1)
  }
  # TOSTER:::.pval_perc / .pval_basic / .pval_bca / .pval_stud
  expect_equal(pv("perc", "two.sided"), c(0.120, 0.970, 0.785, 0.100))
  expect_equal(pv("basic", "two.sided"), c(0.04, 0.67, 0.785, 0.27))
  expect_equal(pv("basic", "greater"), c(0.02, 0.335, 0.3925, 0.865))
  expect_equal(pv("basic", "less"), c(0.98, 0.665, 0.6075, 0.135))
  expect_equal(pv("bca", "two.sided"),
               c(0.048579745848632, 0.612959547150749, 0.788955645899196,
                 0.249485483048757), tolerance = 1e-10)
  expect_equal(pv("bca", "greater"),
               c(0.024289872924316, 0.306479773575374, 0.394477822949598,
                 0.875257258475622), tolerance = 1e-10)
  expect_equal(pv("stud", "two.sided"), c(0.17, 0.68, 0.79, 0.58))
  expect_equal(pv("stud", "greater"), c(0.085, 0.34, 0.395, 0.71))
  expect_equal(pv("stud", "less"), c(0.915, 0.66, 0.605, 0.29))
})

test_that("one-sided percentile p-values use the same tie handling as two-sided", {
  bo <- make_bo()
  b <- bo$t[, "est"]
  r0 <- 0.6
  phat <- (sum(b < r0) + 0.5 * sum(b == r0)) / length(b)
  expect_equal(SimplyAgree:::.boot_pvalue(bo, "perc", r0, "greater"), phat)
  expect_equal(SimplyAgree:::.boot_pvalue(bo, "perc", r0, "less"), 1 - phat)
})

test_that("bootstrap p-values agree with the bootstrap intervals", {
  bo <- make_bo()
  for (type in c("perc", "basic", "bca", "stud")) {
    for (alt in c("greater", "less")) {
      ci <- SimplyAgree:::.boot_interval(bo, type, 0.95, alt)
      bound <- if (alt == "greater") ci[1] else ci[2]
      inside <- if (alt == "greater") bound + 0.02 else bound - 0.02
      outside <- if (alt == "greater") bound - 0.02 else bound + 0.02
      expect_gt(SimplyAgree:::.boot_pvalue(bo, type, inside, alt), 0.05)
      expect_lt(SimplyAgree:::.boot_pvalue(bo, type, outside, alt), 0.05)
    }
  }
})

test_that("one-sided bootstrap intervals use the parameter bounds", {
  bo <- make_bo()
  expect_equal(SimplyAgree:::.boot_interval(bo, "perc", 0.95, "greater",
                                            bounds = c(-1, 1))[2], 1)
  expect_equal(SimplyAgree:::.boot_interval(bo, "perc", 0.95, "less",
                                            bounds = c(-1, 1))[1], -1)
  ci <- SimplyAgree:::.boot_interval(bo, "perc", 0.95)
  expect_equal(as.numeric(ci),
               unname(quantile(bo$t[, "est"], c(0.025, 0.975))))
})

test_that("subject resampling keeps whole subjects and renumbers duplicates", {
  set.seed(3)
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  rs <- SimplyAgree:::.reli_resample(p)
  expect_equal(rs$n, 6)
  expect_true(all(table(rs$df$id) == 4))
  expect_true(rs$balanced)

  w <- sf_wide
  w$J2[1] <- NA
  pu <- SimplyAgree:::.reli_prepare(col.names = names(w), data = w)
  rsu <- SimplyAgree:::.reli_resample(pu)
  expect_equal(rsu$n, 6)
})

test_that("bootstrap drops and counts failed replicates; jackknife leaves out subjects", {
  set.seed(10)
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  stat <- function(pp, resample) {
    f <- SimplyAgree:::.reli_fit(pp, keep_fit = FALSE)
    r <- SimplyAgree:::.icc_estimate(f, "C1")
    if (r > 0.85) stop("fail")
    c(est = r, z = NA, se_z = NA)
  }
  bo <- SimplyAgree:::.reli_boot(p, stat, R = 50, jackknife = TRUE)
  expect_equal(nrow(bo$t) + bo$n_failed, 50)
  expect_gt(bo$n_failed, 0)
  expect_equal(nrow(bo$jack), 6)
  manual <- vapply(1:6, function(i) {
    f <- fit_wide(sf_wide[-i, ])
    SimplyAgree:::.icc_estimate(f, "C1")
  }, 1)
  expect_equal(unname(bo$jack[, "est"]), manual)
})

test_that("the resample flag is TRUE only for bootstrap replicates", {
  set.seed(2)
  p <- SimplyAgree:::.reli_prepare(col.names = names(sf_wide), data = sf_wide)
  stat <- function(pp, resample) c(est = as.numeric(resample), z = NA, se_z = NA)
  bo <- SimplyAgree:::.reli_boot(p, stat, R = 20, jackknife = TRUE)
  expect_equal(bo$t0[["est"]], 0)
  expect_true(all(bo$t[, "est"] == 1))
  expect_true(all(bo$jack[, "est"] == 0))
})

test_that("the hybrid step rescales only the item mean square by a chi-square draw", {
  f <- fit_wide(sf_wide)
  set.seed(8)
  h <- SimplyAgree:::.hybrid_item_ms(f)
  set.seed(8)
  draw <- rchisq(1, 3) / 3
  expect_equal(h$ms[["MSJ"]], f$ms[["MSJ"]] * draw)
  expect_equal(h$ms[c("MSB", "MSE", "MSW")], f$ms[c("MSB", "MSE", "MSW")])
})

# Slow simulation checks (set SIMPLYAGREE_SLOW_TESTS=true to run) ----------------

skip_slow <- function() {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("SIMPLYAGREE_SLOW_TESTS"), "true"),
              "set SIMPLYAGREE_SLOW_TESTS=true to run simulation checks")
}

test_that("F tests hold their nominal type I error (simulation)", {
  skip_slow()
  set.seed(2026)
  n <- 15; k <- 3; r0 <- 0.3; v_items <- 0.2; v_e <- 0.5
  v_id <- function(fm) switch(fm,
    "1" = r0 * (v_items + v_e) / (1 - r0), "k" = r0 * (v_items + v_e) / k / (1 - r0),
    "C1" = r0 * v_e / (1 - r0), "Ck" = r0 * v_e / k / (1 - r0),
    "A1" = r0 * (v_items + v_e) / (1 - r0), "Ak" = r0 * (v_items + v_e) / k / (1 - r0))
  fixed_items <- rnorm(k, 0, sqrt(v_items))
  for (fm in icc_forms) {
    reject <- replicate(2000, {
      one_way <- fm %in% c("1", "k")
      it <- if (one_way) rep(0, k) else if (fm %in% c("C1", "Ck")) fixed_items else
        rnorm(k, 0, sqrt(v_items))
      e_sd <- sqrt(v_e + if (one_way) v_items else 0)
      Y <- outer(rnorm(n, 0, sqrt(v_id(fm))), rep(1, k)) + outer(rep(1, n), it) +
        matrix(rnorm(n * k, 0, e_sd), n)
      f <- fit_wide(as.data.frame(Y))
      SimplyAgree:::.icc_ftest(f, fm, r0, "greater")$p.value < 0.05
    })
    expect_lt(abs(mean(reject) - 0.05), 0.015)
  }
})
