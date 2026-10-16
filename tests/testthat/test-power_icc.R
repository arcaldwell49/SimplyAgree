testthat::local_edition(3)

g <- "greater"

# power_icc: published values --------------------------------------------------------

test_that("approximate n matches Walter, Eliasziw & Donner (1998)", {
  res <- power_icc(rho0 = 0.2, rho1 = 0.3, power = 0.8, alternative = g,
                   method = "approximate")
  expect_equal(round(res$n, 2), 543.16)
  # closed form computed from the published formula
  specs <- list(c(0.6, 0.8, 2, 38.61), c(0.4, 0.6, 2, 86.12),
                c(0.7, 0.9, 2, 17.90), c(0.8, 0.9, 2, 45.29),
                c(0.5, 0.7, 3, 39.60))
  for (s in specs) {
    expect_equal(round(power_icc(rho0 = s[1], rho1 = s[2], k = s[3], power = 0.8,
                                 alternative = g, method = "approximate")$n, 2),
                 s[4])
  }
})

test_that("exact power matches PASS", {
  specs <- list(c(50, 2, 0.18333), c(50, 3, 0.29534), c(50, 5, 0.45522),
                c(150, 4, 0.74538))
  for (s in specs) {
    expect_equal(power_icc(n = s[1], k = s[2], rho0 = 0.2, rho1 = 0.3,
                           alternative = g)$power, s[3], tolerance = 1e-4)
  }
  expect_equal(round(power_icc(n = 544, rho0 = 0.2, rho1 = 0.3,
                               alternative = g)$power, 4), 0.8003)
})

test_that("one-sided lower limit sample sizes match Zou (2012) and PASS", {
  approx <- power_icc(rho0 = 0.5, rho1 = 0.6, power = 0.8, alternative = g,
                      method = "approximate")
  exact <- power_icc(rho0 = 0.5, rho1 = 0.6, power = 0.8, alternative = g)
  expect_equal(ceiling(approx$n), 300)
  expect_equal(ceiling(exact$n), 301)
})

test_that("exact sample size matches Mondal et al. (2024, testing approach)", {
  expect_equal(ceiling(power_icc(rho0 = 0.7, rho1 = 0.8, power = 0.9,
                                 alternative = g)$n), 162)
})

# power_icc: exact power is the power of icc_test's F test --------------------------

test_that("exact power is the F-distribution probability of rejection", {
  n <- 20; k <- 3; r0 <- 0.4; r1 <- 0.7
  c0 <- (1 + (k - 1) * r0) / (1 - r0)
  c1 <- (1 + (k - 1) * r1) / (1 - r1)
  crit <- qf(0.95, n - 1, (n - 1) * (k - 1))
  expect_equal(power_icc(n = n, k = k, rho0 = r0, rho1 = r1, alternative = g,
                         model = "twoway")$power,
               pf(crit * c0 / c1, n - 1, (n - 1) * (k - 1), lower.tail = FALSE))
  # two-sided: both tails at alpha / 2
  two <- power_icc(n = n, k = k, rho0 = r0, rho1 = r1, model = "twoway")
  lo <- qf(0.025, n - 1, (n - 1) * (k - 1))
  hi <- qf(0.975, n - 1, (n - 1) * (k - 1))
  expect_equal(two$power,
               pf(hi * c0 / c1, n - 1, (n - 1) * (k - 1), lower.tail = FALSE) +
                 pf(lo * c0 / c1, n - 1, (n - 1) * (k - 1)))
  # under the null, power is the significance level
  expect_equal(power_icc(n = n, k = k, rho0 = r0, rho1 = r0 + 1e-12,
                         alternative = g)$power, 0.05, tolerance = 1e-6)
})

test_that("exact power matches a simulation of icc_test()", {
  skip_if_not(identical(Sys.getenv("SIMPLYAGREE_SLOW_TESTS"), "true"),
              "set SIMPLYAGREE_SLOW_TESTS=true to run simulation checks")
  set.seed(2026)
  n <- 15; k <- 3; r0 <- 0.4; r1 <- 0.75
  v_e <- (1 - r1) / r1
  rej <- replicate(2000, {
    Y <- outer(rnorm(n), rep(1, k)) + outer(rep(1, n), rnorm(k, 0, 0.3)) +
      matrix(rnorm(n * k, 0, sqrt(v_e)), n)
    d <- as.data.frame(Y)
    icc_test(col.names = names(d), data = d, null.value = r0,
             alternative = g)$p.value < 0.05
  })
  expect_equal(mean(rej), power_icc(n = n, k = k, rho0 = r0, rho1 = r1,
                                    alternative = g, model = "twoway")$power,
               tolerance = 0.03)
})

# power_icc: solving ---------------------------------------------------------------------

test_that("solving round-trips for every argument", {
  base <- power_icc(n = 40, k = 3, rho0 = 0.5, rho1 = 0.75, alternative = g)
  p <- base$power
  expect_equal(power_icc(k = 3, rho0 = 0.5, rho1 = 0.75, power = p,
                         alternative = g)$n, 40, tolerance = 1e-6)
  expect_equal(power_icc(n = 40, k = 3, rho0 = 0.5, power = p,
                         alternative = g)$rho1, 0.75, tolerance = 1e-6)
  expect_equal(power_icc(n = 40, k = 3, rho0 = 0.5, rho1 = 0.75, power = p,
                         sig.level = NULL, alternative = g)$sig.level, 0.05,
               tolerance = 1e-6)
  # k: smallest whole number reaching the target
  k_res <- power_icc(n = 40, k = NULL, rho0 = 0.5, rho1 = 0.75, power = p,
                     alternative = g)
  expect_equal(k_res$k, 3)
  expect_lt(power_icc(n = 40, k = 2, rho0 = 0.5, rho1 = 0.75,
                      alternative = g)$power, p)
  # approximate method round-trips too
  pa <- power_icc(n = 60, rho0 = 0.3, rho1 = 0.6, alternative = g,
                  method = "approximate")$power
  expect_equal(power_icc(rho0 = 0.3, rho1 = 0.6, power = pa, alternative = g,
                         method = "approximate")$n, 60, tolerance = 1e-6)
})

test_that("solving for rho1 handles each alternative", {
  two <- power_icc(n = 30, rho0 = 0.5, power = 0.8)
  expect_gt(two$rho1, 0.5)
  expect_match(two$note, "solution above rho0")
  less <- power_icc(n = 30, rho0 = 0.5, power = 0.8, alternative = "less")
  expect_lt(less$rho1, 0.5)
  expect_equal(power_icc(n = 30, rho0 = 0.5, rho1 = less$rho1,
                         alternative = "less")$power, 0.8, tolerance = 1e-6)
})

# power_icc: errors and printing -----------------------------------------------------

test_that("power_icc checks its arguments", {
  expect_error(power_icc(rho0 = 0.5, rho1 = 0.7), "Exactly one of")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.7, power = 0.8),
               "Exactly one of")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.3, alternative = g),
               "must be above")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.7, alternative = "less"),
               "must be below")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.5), "differ")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.7, model = "twoway",
                         method = "approximate"), "only available")
  expect_error(power_icc(n = 20, k = 2.5, rho1 = 0.7), "whole number")
  expect_error(power_icc(n = 1, rho1 = 0.7), "at least 2")
  expect_error(power_icc(n = 20, rho1 = 1.2), "between")
  expect_error(power_icc(n = 20, rho0 = 0.5, rho1 = 0.5001, power = 0.999,
                         k = NULL, alternative = g), "up to 1000")
})

test_that("power_icc returns a power.htest that prints", {
  res <- power_icc(rho0 = 0.6, rho1 = 0.8, power = 0.8, alternative = g)
  expect_s3_class(res, "power.htest")
  expect_named(res, c("n", "k", "rho0", "rho1", "sig.level", "power",
                      "alternative", "note", "method"))
  expect_match(res$method, "one-way ICC")
  expect_match(res$note, "round up")
  expect_output(print(res), "rho1 = 0.8")
  tw <- power_icc(n = 30, rho0 = 0.6, rho1 = 0.8, model = "twoway")
  expect_match(tw$method, "two-way consistency ICC")
})

# precision_icc --------------------------------------------------------------------------

test_that("approximate n matches Zou (2012) and PASS", {
  expect_equal(ceiling(precision_icc(rho = 0.6, width = 0.2, assurance = 0.8,
                                     method = "approximate")$n), 183)
  for (s in list(c(2, 196), c(4, 96), c(6, 77), c(8, 70))) {
    expect_equal(ceiling(precision_icc(rho = 0.6, k = s[1], width = 0.2,
                                       assurance = 0.9,
                                       method = "approximate")$n), s[2])
  }
})

test_that("exact n matches Mondal et al. (2024, Searle interval)", {
  # assurance approach (Table 4) and median width (Table 3); full width
  expect_equal(ceiling(precision_icc(rho = 0.7, width = 0.1,
                                     assurance = 0.9)$n), 473)
  expect_equal(ceiling(precision_icc(rho = 0.8, width = 0.2,
                                     assurance = 0.9)$n), 81)
  expect_equal(ceiling(precision_icc(rho = 0.7, width = 0.1)$n), 403)
})

test_that("exact assurance is the probability that the F interval is narrow enough", {
  n <- 25; k <- 3; rho <- 0.7; w <- 0.3
  a <- precision_icc(n = n, k = k, rho = rho, width = w, assurance = NULL,
                     model = "twoway")$assurance
  # direct numerical check over the distribution of the F ratio
  df1 <- n - 1; df2 <- (n - 1) * (k - 1)
  scale <- (1 + (k - 1) * rho) / (1 - rho)
  qa <- qf(0.975, df1, df2); qb <- qf(0.975, df2, df1)
  width_at <- function(Fr) {
    gg <- function(x) (x - 1) / (x + k - 1)
    gg(Fr * qb) - gg(Fr / qa)
  }
  u <- seq(1e-6, 1 - 1e-6, length.out = 200001)
  Fr <- scale * qf(u, df1, df2)
  expect_equal(a, mean(width_at(Fr) <= w), tolerance = 1e-3)
})

test_that("precision_icc agrees with simulated icc_test() interval widths", {
  skip_if_not(identical(Sys.getenv("SIMPLYAGREE_SLOW_TESTS"), "true"),
              "set SIMPLYAGREE_SLOW_TESTS=true to run simulation checks")
  set.seed(123)
  n <- 20; k <- 3; rho <- 0.8
  widths <- replicate(4000, {
    Y <- outer(rnorm(n), rep(1, k)) +
      matrix(rnorm(n * k, 0, sqrt((1 - rho) / rho)), n)
    d <- as.data.frame(Y)
    diff(as.numeric(icc_test(col.names = names(d), data = d,
                             model = "oneway")$conf.int))
  })
  med <- precision_icc(n = n, k = k, rho = rho, width = NULL)$width
  expect_equal(mean(widths <= med), 0.5, tolerance = 0.03)
})

test_that("precision_icc solving round-trips", {
  w <- precision_icc(n = 50, k = 3, rho = 0.75, width = NULL,
                     assurance = 0.8)$width
  expect_equal(precision_icc(k = 3, rho = 0.75, width = w,
                             assurance = 0.8)$n, 50, tolerance = 1e-5)
  expect_equal(precision_icc(n = 50, k = 3, rho = 0.75, width = w,
                             assurance = NULL)$assurance, 0.8, tolerance = 1e-6)
  k_res <- precision_icc(n = 50, k = NULL, rho = 0.75, width = w,
                         assurance = 0.8)
  expect_equal(k_res$k, 3)
  # approximate: assurance inverts Zou's formula
  na <- precision_icc(rho = 0.6, width = 0.2, assurance = 0.85,
                      method = "approximate")$n
  expect_equal(precision_icc(n = na, rho = 0.6, width = 0.2, assurance = NULL,
                             method = "approximate")$assurance, 0.85,
               tolerance = 1e-8)
  expect_equal(precision_icc(n = na, rho = 0.6, width = NULL, assurance = 0.85,
                             method = "approximate")$width, 0.2,
               tolerance = 1e-6)
})

test_that("precision_icc checks its arguments and prints", {
  expect_error(precision_icc(n = 30, width = 0.2), "'rho'")
  expect_error(precision_icc(rho = 0.7, width = 0.2, assurance = NULL),
               "Exactly one of")
  expect_error(precision_icc(rho = 0.7, width = -1), "positive")
  expect_error(precision_icc(rho = 0.7, width = 0.2, model = "twoway",
                             method = "approximate"), "only available")
  expect_error(precision_icc(rho = 0.7, width = 0.2, assurance = 0.3,
                             method = "approximate"), "at least 0.5")
  expect_warning(a <- precision_icc(n = 10, rho = 0.6, width = 0.2,
                                    assurance = NULL, method = "approximate"),
                 "below the median-width")
  expect_equal(a$assurance, 0.5)
  # a width wider than any interval can be is met with certainty
  expect_equal(precision_icc(n = 30, rho = 0.7, width = 1.99,
                             assurance = NULL)$assurance, 1)
  res <- precision_icc(rho = 0.8, k = 3, width = 0.2)
  expect_s3_class(res, "power.htest")
  expect_output(print(res), "width = 0.2")
  expect_match(res$note, "upper minus lower")
})
