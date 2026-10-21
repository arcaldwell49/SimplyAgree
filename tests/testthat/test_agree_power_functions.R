context("agreement power functions")

# Tests for power_agreement_exact() ------


test_that("power_agreement_exact validates input parameters correctly", {
  # Should stop if not exactly one parameter is NULL
  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, sigma = 0.044, power = 0.8),
    "Exactly one of 'n', 'delta', 'power', or 'sigma' must be NULL"
  )

  expect_error(
    power_agreement_exact(delta = 0.1, sigma = 0.044),
    "Exactly one of 'n', 'delta', 'power', or 'sigma' must be NULL"
  )

  # Should validate n
  expect_error(
    power_agreement_exact(n = 1.5, delta = 0.1, mu = 0, sigma = 0.044),
    "'n' must be an integer"
  )

  expect_error(
    power_agreement_exact(n = 1, delta = 0.1, mu = 0, sigma = 0.044),
    "'n' must be an integer >= 2"
  )

  # Should validate delta
  expect_error(
    power_agreement_exact(n = 15, delta = -0.1, mu = 0, sigma = 0.044),
    "'delta' must be positive"
  )

  expect_error(
    power_agreement_exact(n = 15, delta = 0, mu = 0, sigma = 0.044),
    "'delta' must be positive"
  )

  # Should validate sigma
  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, mu = 0, sigma = -0.044),
    "'sigma' must be positive"
  )

  # Should validate p0_star
  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, sigma = 0.044, p0_star = 0),
    "'p0_star' must be between 0 and 1"
  )

  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, sigma = 0.044, p0_star = 1),
    "'p0_star' must be between 0 and 1"
  )

  # Should validate alpha
  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, sigma = 0.044, alpha = 0),
    "'alpha' must be between 0 and 1"
  )

  expect_error(
    power_agreement_exact(n = 15, delta = 0.1, sigma = 0.044, alpha = 1.5),
    "'alpha' must be between 0 and 1"
  )

  # Should validate power
  expect_error(
    power_agreement_exact(delta = 0.1, sigma = 0.044, power = 0),
    "'power' must be between 0 and 1"
  )

  expect_error(
    power_agreement_exact(delta = 0.1, sigma = 0.044, power = 1.1),
    "'power' must be between 0 and 1"
  )
})

test_that("power_agreement_exact returns correct structure", {
  result <- power_agreement_exact(
    n = 15,
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  expect_s3_class(result, "power.htest")
  expect_named(result, c("n", "delta", "mu", "sigma", "p0_star", "p1_star",
                         "alpha", "power", "critical_value", "m", "icc", "n.obs",
                         "method", "note"))
  expect_type(result$n, "double")
  expect_type(result$delta, "double")
  expect_type(result$mu, "double")
  expect_type(result$sigma, "double")
  expect_type(result$p0_star, "double")
  expect_type(result$p1_star, "double")
  expect_type(result$alpha, "double")
  expect_type(result$power, "double")
  expect_type(result$critical_value, "double")
  expect_type(result$method, "character")
  expect_type(result$note, "character")
})

test_that("power_agreement_exact computes power correctly (Shieh 2019 validation)", {
  # Example from Shieh (2019) page 357 - MPI study
  # With N=15, delta=0.1, mu=0.011, sigma=0.044, p0_star=0.8, alpha=0.05
  # The paper reports power = 0.8315
  result <- power_agreement_exact(
    n = 15,
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  expect_equal(result$power, 0.8315, tolerance = 0.001)
  expect_equal(result$critical_value, 6.39, tolerance = 0.01)
})

test_that("power_agreement_exact computes sample size correctly (Shieh 2019 validation)", {
  # Example from Shieh (2019) Table 5
  # p0_star=0.8, p_star=0.9, delta=0.1, mu=0.011, sigma=0.044, power=0.8
  # Reports n=14
  result <- power_agreement_exact(
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    power = 0.80,
    alpha = 0.05
  )

  expect_equal(result$n, 14)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  # More
  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.5,
    p0_star = 0.95,
    power = 0.80,
    alpha = 0.05
  )

  expect_equal(result$n, 34)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.6,
    p0_star = 0.95,
    power = 0.80,
    alpha = 0.05
  )

  expect_equal(result$n, 45)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.7,
    p0_star = 0.95,
    power = 0.80,
    alpha = 0.05
  )

  expect_equal(result$n, 62)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.5,
    p0_star = 0.95,
    power = 0.90,
    alpha = 0.05
  )

  expect_equal(result$n, 48)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.6,
    p0_star = 0.95,
    power = 0.90,
    alpha = 0.05
  )

  expect_equal(result$n, 64)
  expect_gte(result$power, 0.80)  # Should achieve at least target power

  result <- power_agreement_exact(
    delta = 7,
    mu = 0.5,
    sigma = 2.7,
    p0_star = 0.95,
    power = 0.90,
    alpha = 0.05
  )

  expect_equal(result$n, 90)
  expect_gte(result$power, 0.80)  # Should achieve at least target power
})

test_that("power_agreement_exact sample size increases with power", {
  # Sample size should increase as target power increases
  result_80 <- power_agreement_exact(
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    power = 0.80,
    alpha = 0.05
  )

  result_90 <- power_agreement_exact(
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    power = 0.90,
    alpha = 0.05
  )

  expect_lt(result_80$n, result_90$n)
})

test_that("power_agreement_exact sample size decreases with larger delta", {
  # Sample size should decrease as acceptable difference increases
  # (delta = 0.05 would give p1_star < p0_star, so no n reaches the power)
  result_small <- power_agreement_exact(
    delta = 0.08,
    mu = 0,
    sigma = 0.044,
    p0_star = 0.80,
    power = 0.80,
    alpha = 0.05
  )

  result_large <- power_agreement_exact(
    delta = 0.15,
    mu = 0,
    sigma = 0.044,
    p0_star = 0.80,
    power = 0.80,
    alpha = 0.05
  )

  expect_gt(result_small$n, result_large$n)
})

test_that("power_agreement_exact computes delta correctly", {
  # When solving for delta, should return reasonable value
  result <- power_agreement_exact(
    n = 50,
    mu = 0,
    sigma = 2.5,
    p0_star = 0.95,
    power = 0.90,
    alpha = 0.05
  )

  expect_gt(result$delta, 0)
  expect_gte(result$power, 0.90)

  # Verify by computing power with this delta
  verify <- power_agreement_exact(
    n = result$n,
    delta = result$delta,
    mu = result$mu,
    sigma = result$sigma,
    p0_star = result$p0_star,
    alpha = result$alpha
  )

  expect_equal(verify$power, result$power, tolerance = 0.001)
})

test_that("power_agreement_exact computes sigma correctly", {
  # When solving for sigma, should return reasonable value
  result <- power_agreement_exact(
    n = 50,
    delta = 7,
    mu = 0.5,
    p0_star = 0.95,
    power = 0.80,
    alpha = 0.05
  )

  expect_gt(result$sigma, 0)
  expect_gte(result$power, 0.80)
})

test_that("power_agreement_exact handles mu = 0 correctly", {
  # Default mu should be 0
  result1 <- power_agreement_exact(
    n = 15,
    delta = 0.1,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  result2 <- power_agreement_exact(
    n = 15,
    delta = 0.1,
    mu = 0,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  expect_equal(result1$power, result2$power)
  expect_equal(result1$mu, 0)
})

test_that("power_agreement_exact p1_star calculation is correct", {
  result <- power_agreement_exact(
    n = 15,
    delta = 0.1,
    mu = 0.011,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  # p1_star should be proportion of distribution within [-delta, delta]
  expected_p1 <- pnorm((0.1 - 0.011) / 0.044) - pnorm((-0.1 - 0.011) / 0.044)
  expect_equal(result$p1_star, expected_p1, tolerance = 1e-6)

  # p1_star should be greater than p0_star for positive power
  expect_gt(result$p1_star, result$p0_star)
})


# Tests for agree_expected_half() ---------


test_that("agree_expected_half validates input parameters correctly", {
  # Should require exactly one of n and delta to be NULL
  expect_error(
    agree_expected_half(conf.level = 0.95, pstar = 0.95, sigma = 1),
    "one of 'n' or 'delta' must be specified"
  )

  expect_error(
    agree_expected_half(n = 50, delta = 2, conf.level = 0.95, pstar = 0.95, sigma = 1),
    "exactly one of 'n' and 'delta' must be NULL"
  )

  # Should validate conf.level
  expect_error(
    agree_expected_half(delta = 2, conf.level = 0, pstar = 0.95, sigma = 1),
    "'conf.level' must be between 0 and 1"
  )

  expect_error(
    agree_expected_half(delta = 2, conf.level = 1.5, pstar = 0.95, sigma = 1),
    "'conf.level' must be between 0 and 1"
  )

  # Should validate pstar
  expect_error(
    agree_expected_half(delta = 2, conf.level = 0.95, pstar = 0, sigma = 1),
    "'pstar' must be between 0 and 1"
  )

  expect_error(
    agree_expected_half(delta = 2, conf.level = 0.95, pstar = 1, sigma = 1),
    "'pstar' must be between 0 and 1"
  )

  # Should validate sigma
  expect_error(
    agree_expected_half(delta = 2, conf.level = 0.95, pstar = 0.95, sigma = 0),
    "'sigma' must be positive"
  )

  expect_error(
    agree_expected_half(delta = 2, conf.level = 0.95, pstar = 0.95, sigma = -1),
    "'sigma' must be positive"
  )
})

test_that("agree_expected_half returns correct structure", {
  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.25 * 19.61,
    pstar = 0.95,
    sigma = 19.61
  )

  expect_s3_class(result, "power.htest")
  expect_named(result, c("n", "conf.level", "target.delta", "actual.delta",
                         "pstar", "sigma", "g", "c", "zp", "m", "icc", "n.obs",
                         "method", "note"))
  expect_type(result$n, "double")
  expect_type(result$conf.level, "double")
  expect_type(result$pstar, "double")
  expect_type(result$sigma, "double")
})

test_that("agree_expected_half computes sample size correctly (Jan & Shieh 2018 validation)", {
  # Example from Jan & Shieh (2018) Table 4, page 251
  # delta = 2.5*19.61, sigma = 19.61, pstar = 0.95, conf.level = 0.95
  # Reports n = 52
  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5 * 19.61,
    pstar = 0.95,
    sigma = 19.61
  )

  expect_equal(result$n, 52)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.2,
    pstar = 0.9,
    sigma = 1
  )

  expect_equal(result$n, 42)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.2,
    pstar = 0.95,
    sigma = 1
  )

  expect_equal(result$n, 219)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.3,
    pstar = 0.9,
    sigma = 1
  )

  expect_equal(result$n, 32)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.3,
    pstar = 0.95,
    sigma = 1
  )

  expect_equal(result$n, 116)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.4,
    pstar = 0.9,
    sigma = 1
  )

  expect_equal(result$n, 26)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.4,
    pstar = 0.95,
    sigma = 1
  )

  expect_equal(result$n, 74)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.9,
    sigma = 1
  )

  expect_equal(result$n, 21)
  expect_lte(result$actual.delta, result$target.delta)

  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_equal(result$n, 52)
  expect_lte(result$actual.delta, result$target.delta)
})

test_that("agree_expected_half sample size increases with smaller delta", {
  # Smaller acceptable width requires larger sample size
  result_large <- agree_expected_half(
    conf.level = 0.95,
    delta = 3.0,
    pstar = 0.95,
    sigma = 1
  )

  result_small <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_lt(result_large$n, result_small$n)
})

test_that("agree_expected_half sample size increases with higher pstar", {
  # Higher coverage proportion requires larger sample size
  result_low <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.80,
    sigma = 1
  )

  result_high <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_lt(result_low$n, result_high$n)
})

test_that("agree_expected_half computes delta when n is specified", {
  # When n is given, should compute achievable delta
  result <- agree_expected_half(
    n = 100,
    conf.level = 0.95,
    pstar = 0.95,
    sigma = 1
  )

  expect_gt(result$actual.delta, 0)
  expect_equal(result$target.delta, result$actual.delta)
})

test_that("agree_expected_half g factor is positive and reasonable", {
  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_gt(result$g, 0)
  # g should be larger than the z-quantile
  expect_gt(result$g, result$zp )
})

test_that("agree_expected_half c factor is close to 1 for large n", {
  result <- agree_expected_half(
    n = 1000,
    conf.level = 0.95,
    pstar = 0.95,
    sigma = 1
  )

  # For large n, bias correction factor c should approach 1
  expect_equal(result$c, 1, tolerance = 0.01)
})

test_that("agree_expected_half expected half-width formula is correct", {
  result <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  # Verify that actual.delta = (g/c)*sigma
  expected_delta <- (result$g / result$c) * result$sigma
  expect_equal(result$actual.delta, expected_delta, tolerance = 1e-6)
})


# Tests for agree_assurance() ---------


test_that("agree_assurance validates input parameters correctly", {
  # Should require exactly one of n and omega to be NULL
  expect_error(
    agree_assurance(conf.level = 0.95, assurance = 0.90, pstar = 0.95, sigma = 1),
    "one of 'n' or 'omega' must be specified"
  )

  expect_error(
    agree_assurance(n = 50, omega = 2, conf.level = 0.95, assurance = 0.90,
                    pstar = 0.95, sigma = 1),
    "exactly one of 'n' and 'omega' must be NULL"
  )

  # Should validate conf.level
  expect_error(
    agree_assurance(omega = 2, conf.level = 0, assurance = 0.90, pstar = 0.95, sigma = 1),
    "'conf.level' must be between 0 and 1"
  )

  # Should validate assurance
  expect_error(
    agree_assurance(omega = 2, conf.level = 0.95, assurance = 0, pstar = 0.95, sigma = 1),
    "'assurance' must be between 0 and 1"
  )

  expect_error(
    agree_assurance(omega = 2, conf.level = 0.95, assurance = 1.1, pstar = 0.95, sigma = 1),
    "'assurance' must be between 0 and 1"
  )

  # Should validate pstar
  expect_error(
    agree_assurance(omega = 2, conf.level = 0.95, assurance = 0.90, pstar = 1.5, sigma = 1),
    "'pstar' must be between 0 and 1"
  )

  # Should validate sigma
  expect_error(
    agree_assurance(omega = 2, conf.level = 0.95, assurance = 0.90, pstar = 0.95, sigma = -1),
    "'sigma' must be positive"
  )
})

test_that("agree_assurance returns correct structure", {
  result <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_s3_class(result, "power.htest")
  expect_named(result, c("n", "conf.level", "assurance", "actual.assurance",
                         "omega", "pstar", "sigma", "g", "zp", "m", "icc", "n.obs",
                         "method", "note"))
  expect_type(result$n, "double")
  expect_type(result$conf.level, "double")
  expect_type(result$assurance, "double")
  expect_type(result$actual.assurance, "double")
})

test_that("agree_assurance computes sample size correctly (Jan & Shieh 2018 validation)", {
  # Example from Jan & Shieh (2018) Table 4, page 251
  # omega = 2.5*19.61, sigma = 19.61, pstar = 0.95, assurance = 0.9
  # Reports n = 115
  result <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5 * 19.61,
    pstar = 0.95,
    sigma = 19.61
  )

  expect_equal(result$n, 115)
  expect_gte(result$actual.assurance, result$assurance)

  # Example from Jan & Shieh (2018)  page 251
  result <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.25,
    pstar = 0.95,
    sigma = 1
  )

  expect_equal(result$n, 354)
  expect_gte(result$actual.assurance, result$assurance)
})

test_that("agree_assurance sample size is larger than expected half-width", {
  # For same parameters, assurance should require larger n than expected width
  n_expected <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )$n

  n_assurance <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )$n

  expect_gt(n_assurance, n_expected)
})

test_that("agree_assurance sample size increases with higher assurance", {
  result_low <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.80,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  result_high <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.95,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  expect_lt(result_low$n, result_high$n)
})

test_that("agree_assurance sample size increases with smaller omega", {

  result_large <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 4,
    pstar = 0.95,
    sigma = 1
  )

  result_small <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 3,
    pstar = 0.95,
    sigma = 1
  )

  expect_lt(result_large$n, result_small$n)
})

test_that("agree_assurance computes omega when n is specified", {
  result <- agree_assurance(
    n = 100,
    conf.level = 0.95,
    assurance = 0.90,
    pstar = 0.95,
    sigma = 1
  )

  expect_gt(result$omega, 0)
  expect_gte(result$actual.assurance, result$assurance)
})

test_that("agree_assurance assurance probability formula is correct", {
  result <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  df <- result$n - 1
  # Verify assurance = P(chi-square <= eta)
  eta <- (df / result$g^2) * (result$omega / result$sigma)^2
  expected_assurance <- pchisq(eta, df)

  expect_equal(result$actual.assurance, expected_assurance, tolerance = 1e-6)
})

test_that("agree_assurance achieves target assurance", {
  result <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )

  # Actual assurance should meet or exceed target
  expect_gte(result$actual.assurance, result$assurance)
  # But shouldn't be too much higher (within 1%)
  expect_lte(result$actual.assurance, result$assurance + 0.01)
})


# Integration tests across functions -------

test_that("all functions produce consistent results for same scenario", {
  # All three functions should produce sensible, ordered sample sizes
  n_exact <- power_agreement_exact(
    delta = 2.5,
    mu = 0,
    sigma = 1,
    p0_star = 0.95,
    power = 0.80,
    alpha = 0.05
  )$n

  n_expected <- agree_expected_half(
    conf.level = 0.95,
    delta = 2.5,
    pstar = 0.95,
    sigma = 1
  )$n

  n_assurance <- agree_assurance(
    conf.level = 0.95,
    assurance = 0.90,
    omega = 2.5,
    pstar = 0.95,
    sigma = 1
  )$n

  # All should be positive integers
  expect_gt(n_exact, 0)
  expect_gt(n_expected, 0)
  expect_gt(n_assurance, 0)

  # Assurance should require more than expected width
  expect_gt(n_assurance, n_expected)
})

test_that("functions handle edge cases for pstar", {
  # Test with pstar = 0.80 (lower coverage)
  result1 <- power_agreement_exact(
    n = 20,
    delta = 0.1,
    sigma = 0.044,
    p0_star = 0.80,
    alpha = 0.05
  )

  # Test with pstar = 0.99 (higher coverage)
  result2 <- power_agreement_exact(
    n = 20,
    delta = 0.1,
    sigma = 0.044,
    p0_star = 0.99,
    alpha = 0.05
  )

  # Higher coverage should have lower power for same n
  expect_lt(result2$power, result1$power)
})

test_that("functions are numerically stable", {
  # Test with very small sigma
  result_small <- power_agreement_exact(
    n = 50,
    delta = 0.01,
    sigma = 0.001,
    p0_star = 0.95,
    alpha = 0.05
  )

  expect_true(is.finite(result_small$power))
  expect_gte(result_small$power, 0)
  expect_lte(result_small$power, 1)

  # Test with very large sigma
  result_large <- power_agreement_exact(
    n = 50,
    delta = 100,
    sigma = 10,
    p0_star = 0.95,
    alpha = 0.05
  )

  expect_true(is.finite(result_large$power))
  expect_gte(result_large$power, 0)
  expect_lte(result_large$power, 1)
})


# Input checks and edge cases fixed in 0.3.0 -------

test_that("n is validated in agree_expected_half and agree_assurance", {
  expect_error(agree_expected_half(n = 1), "'n' must be an integer >= 2")
  expect_error(agree_expected_half(n = 10.5), "'n' must be an integer >= 2")
  expect_error(agree_assurance(n = 1), "'n' must be an integer >= 2")
})

test_that("clustering arguments are validated", {
  expect_error(agree_expected_half(delta = 2.5, m = 0), "'m' must be")
  expect_error(agree_expected_half(delta = 2.5, m = 1.5), "'m' must be")
  expect_error(agree_assurance(omega = 2.5, icc = 1), "'icc' must be")
  expect_error(power_agreement_exact(n = 10, delta = 7, sigma = 2.5, icc = -0.1),
               "'icc' must be")
  expect_error(blandPowerCurve(SD = 1, delta = 3, m = 0), "'m' must be")
})

test_that("power_agreement_exact stops when power cannot exceed alpha", {
  expect_error(
    power_agreement_exact(delta = 0.05, mu = 0, sigma = 0.044,
                          p0_star = 0.80, power = 0.80),
    "p1_star"
  )
})

test_that("power_agreement_exact warns when the n search limit is reached", {
  # p1_star is barely above p0_star, so n > 10000 is needed
  expect_warning(
    res <- power_agreement_exact(delta = qnorm(0.9005), mu = 0, sigma = 1,
                                 p0_star = 0.80, power = 0.80),
    "Target not reached"
  )
  expect_equal(res$n, 10000)
})

test_that("power_agreement_exact solves delta and sigma on any scale", {
  big <- power_agreement_exact(n = 50, mu = 0, sigma = 2.5,
                               p0_star = 0.95, power = 0.90)
  small <- power_agreement_exact(n = 50, mu = 0, sigma = 2.5e-5,
                                 p0_star = 0.95, power = 0.90)
  expect_equal(small$delta / small$sigma, big$delta / big$sigma,
               tolerance = 1e-5)
  expect_gte(small$power, 0.90)

  s_small <- power_agreement_exact(n = 50, delta = 7e-5, mu = 5e-6,
                                   p0_star = 0.95, power = 0.80)
  s_big <- power_agreement_exact(n = 50, delta = 7, mu = 0.5,
                                 p0_star = 0.95, power = 0.80)
  expect_equal(s_small$sigma * 1e5, s_big$sigma, tolerance = 1e-5)
  expect_gte(s_small$power, 0.80)
})

test_that("agree_assurance omega is not capped for small n", {
  # the previous bisection stopped at 10 * sigma
  res <- agree_assurance(n = 5, conf.level = 0.99, assurance = 0.99,
                         pstar = 0.99, sigma = 1)
  expect_gt(res$omega, 10)
  expect_gte(res$actual.assurance, 0.99)
  expect_equal(res$actual.assurance, 0.99, tolerance = 1e-6)
})

test_that("sample size searches can return n below 5", {
  expect_lt(agree_expected_half(delta = 20, pstar = 0.9)$n, 5)
  expect_lt(power_agreement_exact(delta = 7, mu = 0, sigma = 0.5,
                                  p0_star = 0.8, power = 0.8)$n, 5)
})

# Clustered designs (m, icc) -------

test_that("m = 1 reproduces the default (independent pairs) results", {
  expect_identical(
    agree_expected_half(delta = 2.5, m = 1, icc = 0.4)[c("n", "g", "c")],
    agree_expected_half(delta = 2.5)[c("n", "g", "c")]
  )
  expect_identical(
    agree_assurance(omega = 2.5, m = 1)$n,
    agree_assurance(omega = 2.5)$n
  )
  expect_identical(
    power_agreement_exact(n = 15, delta = 0.1, mu = 0.011, sigma = 0.044,
                          p0_star = 0.80, m = 1)$power,
    power_agreement_exact(n = 15, delta = 0.1, mu = 0.011, sigma = 0.044,
                          p0_star = 0.80)$power
  )
})

test_that("icc = 0 with m pairs per subject is close to n * m independent pairs", {
  clus <- agree_expected_half(n = 20, m = 3, icc = 0)
  iid <- agree_expected_half(n = 60)
  expect_equal(clus$actual.delta, iid$actual.delta, tolerance = 1e-3)
  expect_equal(clus$n.obs, 60)

  clus <- power_agreement_exact(n = 20, delta = 7, mu = 0.5, sigma = 2.5,
                                m = 3, icc = 0)
  iid <- power_agreement_exact(n = 60, delta = 7, mu = 0.5, sigma = 2.5)
  expect_equal(clus$power, iid$power, tolerance = 0.01)

  clus <- agree_assurance(n = 20, m = 3, icc = 0)
  iid <- agree_assurance(n = 60)
  expect_equal(clus$omega, iid$omega, tolerance = 1e-3)
})

test_that("clustering widens intervals and lowers power", {
  w0 <- agree_assurance(n = 20, m = 3, icc = 0.1)$omega
  w1 <- agree_assurance(n = 20, m = 3, icc = 0.5)$omega
  expect_lt(w0, w1)

  p0 <- power_agreement_exact(n = 20, delta = 7, mu = 0.5, sigma = 2.5,
                              m = 3, icc = 0.1)$power
  p1 <- power_agreement_exact(n = 20, delta = 7, mu = 0.5, sigma = 2.5,
                              m = 3, icc = 0.5)$power
  expect_gt(p0, p1)
})

test_that("clustered sample size lies between the iid and the simple design effect answers", {
  res <- power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                               p0_star = 0.95, power = 0.80,
                               m = 3, icc = 0.15)
  # 34 independent pairs; 1 + (m - 1) * icc design effect gives 15 subjects
  expect_gte(res$n.obs, 34)
  expect_lte(res$n, 15)
  expect_gte(res$power, 0.80)
})

test_that("clustered calculations match simulation (balanced compound symmetry)", {
  skip_on_cran()
  set.seed(4202)
  n <- 20; m <- 3; icc <- 0.3; R <- 4000
  eh <- agree_expected_half(n = n, m = m, icc = icc)
  as <- agree_assurance(n = n, assurance = 0.9, m = m, icc = icc)
  pe <- power_agreement_exact(n = n, delta = 3, mu = 0.3, sigma = 1,
                              m = m, icc = icc)
  zp <- qnorm(0.975)

  u <- matrix(rnorm(R * n, 0, sqrt(icc)), R)
  e <- array(rnorm(R * n * m, 0, sqrt(1 - icc)), c(R, n, m))
  ebar <- apply(e, c(1, 2), mean)
  cm <- 0.3 + u + ebar
  xb <- rowMeans(cm)
  msb <- m * apply(cm, 1, var)
  msw <- apply(sweep(e, c(1, 2), ebar)^2, 1, sum) / (n * (m - 1))
  S <- sqrt(msb / m + (m - 1) / m * msw)

  se_p <- function(p) 4 * sqrt(p * (1 - p) / R)
  # expected half-width and tolerance coverage
  expect_equal(mean(eh$g * S), eh$actual.delta, tolerance = 0.01)
  expect_lt(abs(mean(abs(xb - 0.3) < eh$g * S - zp) - 0.95), se_p(0.95))
  # assurance
  expect_lt(abs(mean(as$g * S <= as$omega) - 0.9), se_p(0.9))
  # power of the exact test
  deff <- 1 + (m - 1) * icc
  rej <- abs(xb) + pe$critical_value * S * sqrt(deff / (n * m)) < 3
  expect_lt(abs(mean(rej) - pe$power), se_p(pe$power))
})
