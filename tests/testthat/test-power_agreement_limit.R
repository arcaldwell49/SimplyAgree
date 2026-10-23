test_that("sample size matches PASS 427 Example 1", {
  pass <- data.frame(sigma = rep(c(2.5, 2.6, 2.7), 2),
                     power = rep(c(0.8, 0.9), each = 3),
                     n = c(60, 82, 118, 78, 108, 156),
                     achieved = c(0.8059, 0.8019, 0.8024,
                                  0.9014, 0.9003, 0.9002))
  for (i in seq_len(nrow(pass))) {
    res <- power_agreement_limit(delta = 7, mu = 0.5, sigma = pass$sigma[i],
                                 power = pass$power[i])
    expect_equal(res$n, pass$n[i])
    expect_equal(round(res$power, 4), pass$achieved[i])
  }
  expect_s3_class(res, "power.htest")
})

test_that("power matches PASS 427 Example 2", {
  pow <- vapply(200:203, function(n) {
    power_agreement_limit(n = n, delta = 2.5, mu = 0.2, sigma = 1)$power
  }, numeric(1))
  expect_equal(round(pow, 4), c(0.7983, 0.8003, 0.8023, 0.8042))
  expect_equal(power_agreement_limit(delta = 2.5, mu = 0.2, sigma = 1,
                                     power = 0.8)$n, 201)
})

test_that("power agrees with blandPowerCurve", {
  settings <- list(
    list(mu = 0.5, sigma = 2.5, delta = 7, conf.level = 0.95,
         agree.level = 0.95, m = 1, icc = 0),
    list(mu = -0.2, sigma = 1, delta = 2, conf.level = 0.90,
         agree.level = 0.80, m = 1, icc = 0),
    list(mu = 0.5, sigma = 2.5, delta = 7, conf.level = 0.95,
         agree.level = 0.95, m = 3, icc = 0.2)
  )
  for (s in settings) {
    n <- c(10, 37, 90)
    pc <- blandPowerCurve(samplesizes = n, mu = s$mu, SD = s$sigma,
                          delta = s$delta, conf.level = s$conf.level,
                          agree.level = s$agree.level, m = s$m, icc = s$icc)
    pw <- vapply(n, function(nn) {
      power_agreement_limit(n = nn, delta = s$delta, mu = s$mu,
                            sigma = s$sigma, conf.level = s$conf.level,
                            agree.level = s$agree.level, m = s$m,
                            icc = s$icc)$power
    }, numeric(1))
    expect_equal(pw, pc$power, tolerance = 1e-12)
  }
})

test_that("solved n is the smallest reaching the target", {
  res <- power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.85,
                               m = 3, icc = 0.3)
  expect_gte(res$power, 0.85)
  prev <- power_agreement_limit(n = res$n - 1, delta = 7, mu = 0.5,
                                sigma = 2.5, m = 3, icc = 0.3)
  expect_lt(prev$power, 0.85)
  expect_equal(res$n.obs, res$n * 3)
})

test_that("solving for delta and sigma round-trips", {
  d <- power_agreement_limit(n = 80, mu = 0.5, sigma = 2.5, power = 0.9)
  expect_equal(power_agreement_limit(n = 80, delta = d$delta, mu = 0.5,
                                     sigma = 2.5)$power, 0.9,
               tolerance = 1e-6)

  s <- power_agreement_limit(n = 80, delta = 7, mu = 0.5, power = 0.9)
  expect_equal(power_agreement_limit(n = 80, delta = 7, mu = 0.5,
                                     sigma = s$sigma)$power, 0.9,
               tolerance = 1e-6)

  # small-scale data
  s2 <- power_agreement_limit(n = 79, delta = 0.004, mu = 0.001167,
                              power = 0.8)
  expect_equal(power_agreement_limit(n = 79, delta = 0.004, mu = 0.001167,
                                     sigma = s2$sigma)$power, 0.8,
               tolerance = 1e-6)
})

test_that("input validation", {
  expect_error(power_agreement_limit(delta = 7, sigma = 2.5),
               "Exactly one")
  expect_error(power_agreement_limit(n = 10, delta = 7, sigma = 2.5,
                                     power = 0.8), "Exactly one")
  expect_error(power_agreement_limit(n = 1.5, delta = 7, sigma = 2.5),
               "'n'")
  expect_error(power_agreement_limit(n = 10, delta = -1, sigma = 2.5),
               "'delta'")
  expect_error(power_agreement_limit(n = 10, delta = 7, sigma = 0),
               "'sigma'")
  expect_error(power_agreement_limit(delta = 7, sigma = 2.5, power = 1),
               "'power'")
  expect_error(power_agreement_limit(n = 10, delta = 7, sigma = 2.5,
                                     conf.level = 1.2), "'conf.level'")
  expect_error(power_agreement_limit(n = 10, delta = 7, sigma = 2.5,
                                     agree.level = 0), "'agree.level'")
  expect_error(power_agreement_limit(n = 10, delta = 7, mu = c(0, 1),
                                     sigma = 2.5), "'mu'")
  expect_error(power_agreement_limit(n = 10, delta = 7, sigma = 2.5,
                                     m = 2, icc = 1), "'icc'")
  # true limits outside the bounds
  expect_error(power_agreement_limit(delta = 5, mu = 0.5, sigma = 2.5,
                                     power = 0.8), "must lie within")
  # mu outside the bounds when solving for sigma
  expect_error(power_agreement_limit(n = 50, delta = 1, mu = 2, power = 0.8),
               "'mu' must lie within")
})
