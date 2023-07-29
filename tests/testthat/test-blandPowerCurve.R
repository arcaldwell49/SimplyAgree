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
                              59,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest1$power,
                              .799,
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
                              117,
                              tolerance = 0.001)
  testthat::expect_equivalent(Ntest1$power,
                              .7999,
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

  print(c(n, power))  # [1] 79.0000000  0.8022956


})
