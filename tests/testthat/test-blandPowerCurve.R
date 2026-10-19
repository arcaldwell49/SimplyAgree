context("powerCurve")
testthat::test_that("errors", {

  testthat::expect_error(
    blandPowerCurve(
      samplesizes = seq(10, 100, 1),
      mu = c(0,.5),
      SD = 2.5,
      delta = 7,
      conf.level = .95,
      agree.level = .95
    )
  )

  testthat::expect_error(
    blandPowerCurve(
      samplesizes = seq(10, 100, 1),
      mu = 0,
      SD = c(2.5,2.7),
      delta = 7,
      conf.level = .95,
      agree.level = .95
    )
  )

})

testthat::test_that("basic runs", {
  powerCurve <- blandPowerCurve(samplesizes = seq(10, 100, 1),
                                mu = 0.5,
                                SD = 2.5,
                                delta = 7,
                                conf.level = .95,
                                agree.level = .95)
  p = plot(powerCurve)

  Ntest1 = find_n(powerCurve, power = .8)
  Ntest2 = find_n(powerCurve, power = .9)
  testthat::expect_equivalent(Ntest1$N,
                              60,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest1$power,
                              .806,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$N,
                              78,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$power,
                              .901,
                              tolerance = 0.001)

  powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
                                mu = 0.5,
                                SD = 2.6,
                                delta = 7,
                                conf.level = .95,
                                agree.level = .95)
  p = plot(powerCurve)

  Ntest1 = find_n(powerCurve, power = .8)
  Ntest2 = find_n(powerCurve, power = .9)
  testthat::expect_equivalent(Ntest1$N,
                              82,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest1$power,
                              .801,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$N,
                              108,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$power,
                              .900,
                              tolerance = 0.001)

  powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
                                mu = 0.5,
                                SD = 2.7,
                                delta = 7,
                                conf.level = .95,
                                agree.level = .95)
  p = plot(powerCurve)

  Ntest1 = find_n(powerCurve, power = .8)
  Ntest2 = find_n(powerCurve, power = .9)
  testthat::expect_equivalent(Ntest1$N,
                              118,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest1$power,
                              .802,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$N,
                              156,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest2$power,
                              .900,
                              tolerance = 0.001)

  # issue on github
  powerCurve <- blandPowerCurve(
    samplesizes = seq(2, 200, 1),
    mu = 0.001167,
    SD = 0.001129,
    delta = 0.004,
    conf.level = 0.95,
    agree.level = 0.95
  )
  found_n <- find_n(powerCurve, power = 0.80)
  n <- found_n[["N"]]
  power <- found_n[["power"]]

  expect_equal(n, 79)
  expect_gte(power, 0.80)


})

testthat::test_that("find_n returns the smallest N reaching the target", {
  pc <- blandPowerCurve(samplesizes = seq(10, 100, 1), mu = 0.5, SD = 2.5,
                        delta = 7)
  res <- find_n(pc, power = 0.8)
  testthat::expect_gte(res$power, 0.8)
  testthat::expect_lt(pc$power[pc$N == res$N - 1], 0.8)
})

testthat::test_that("find_n gives NA and a warning when power is not reached", {
  pc <- blandPowerCurve(samplesizes = seq(10, 30, 1), mu = 0.5, SD = 2.5,
                        delta = c(7, 8))
  testthat::expect_warning(res <- find_n(pc, power = 0.8),
                           "range of sample sizes must be expanded")
  testthat::expect_true(is.na(res$N[res$delta == 7]))
  testthat::expect_equal(res$N[res$delta == 8], 26)
})

testthat::test_that("blandPowerCurve handles clustered designs", {
  pc1 <- blandPowerCurve(samplesizes = 10:40, mu = 0.5, SD = 2.5, delta = 7)
  pcm1 <- blandPowerCurve(samplesizes = 10:40, mu = 0.5, SD = 2.5, delta = 7,
                          m = 1, icc = 0.5)
  testthat::expect_identical(pc1$power, pcm1$power)

  # icc = 0: n subjects x 3 pairs ~ 3n independent pairs
  pcm <- blandPowerCurve(samplesizes = 20, mu = 0.5, SD = 2.5, delta = 7,
                         m = 3, icc = 0)
  pci <- blandPowerCurve(samplesizes = 60, mu = 0.5, SD = 2.5, delta = 7)
  testthat::expect_equal(pcm$power, pci$power, tolerance = 0.01)

  # more clustering, more subjects
  n0 <- find_n(blandPowerCurve(samplesizes = 10:80, mu = 0.5, SD = 2.5,
                               delta = 7, m = 3, icc = 0.1))$N
  n1 <- find_n(blandPowerCurve(samplesizes = 10:80, mu = 0.5, SD = 2.5,
                               delta = 7, m = 3, icc = 0.5))$N
  testthat::expect_lt(n0, n1)
  testthat::expect_true(all(c("m", "icc") %in% names(pcm)))
})
