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

testthat::test_that("matches PASS 427 Example 1", {
  # PASS Chapter 427, Example 1: delta = 7, mu = 0.5, SD = 2.5, 2.6, 2.7
  pass <- data.frame(SD = c(2.5, 2.6, 2.7),
                     N80 = c(60, 82, 118), pow80 = c(0.8059, 0.8019, 0.8024),
                     N90 = c(78, 108, 156), pow90 = c(0.9014, 0.9003, 0.9002))
  for (i in seq_len(nrow(pass))) {
    powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
                                  mu = 0.5,
                                  SD = pass$SD[i],
                                  delta = 7,
                                  conf.level = .95,
                                  agree.level = .95)
    Ntest1 = find_n(powerCurve, power = .8)
    Ntest2 = find_n(powerCurve, power = .9)
    testthat::expect_equal(Ntest1$N, pass$N80[i])
    testthat::expect_equal(round(Ntest1$power, 4), pass$pow80[i])
    testthat::expect_equal(Ntest2$N, pass$N90[i])
    testthat::expect_equal(round(Ntest2$power, 4), pass$pow90[i])
  }
  testthat::expect_s3_class(plot(powerCurve), "ggplot")
})

testthat::test_that("matches PASS 427 Example 2 (Lu et al. 2016 Table 1)", {
  # Lu et al. report n = 203; PASS shows n = 201 already reaches 0.8
  pc <- blandPowerCurve(samplesizes = 200:203, mu = 0.2, SD = 1, delta = 2.5)
  testthat::expect_equal(round(pc$power, 4),
                         c(0.7983, 0.8003, 0.8023, 0.8042))
  testthat::expect_equal(find_n(pc, power = 0.8)$N, 201)
})

testthat::test_that("power matches Lu et al. (2016) eq. 5 in closed form", {
  n <- c(15, 40, 120)
  mu <- -0.3; SD <- 1.2; delta <- 3.5
  conf.level <- 0.90; agree.level <- 0.80
  z <- qnorm(1 - (1 - agree.level) / 2)
  se <- SD * sqrt(1 / n + z^2 / (2 * (n - 1)))
  tcrit <- qt(1 - (1 - conf.level) / 2, n - 1)
  pow <- 1 - pt(tcrit, n - 1, (delta - mu - z * SD) / se) -
    pt(tcrit, n - 1, (delta + mu - z * SD) / se)
  pc <- blandPowerCurve(samplesizes = n, mu = mu, SD = SD, delta = delta,
                        conf.level = conf.level, agree.level = agree.level)
  testthat::expect_equal(pc$power, pow, tolerance = 1e-10)
  testthat::expect_equal(names(pc),
                         c("N", "mu", "SD", "delta", "power", "agree.level",
                           "conf.level", "m", "icc"))

  # power is truncated at 0 when the true limits exceed delta
  pc0 <- blandPowerCurve(samplesizes = 5:10, mu = 0, SD = 3, delta = 2)
  testthat::expect_true(all(pc0$power == 0))
})

testthat::test_that("input validation", {
  testthat::expect_error(blandPowerCurve(SD = 0, delta = 7), "'SD' must be positive")
  testthat::expect_error(blandPowerCurve(SD = 1, delta = 7, conf.level = 1),
                         "'conf.level'")
  testthat::expect_error(blandPowerCurve(SD = 1, delta = 7, agree.level = 0),
                         "'agree.level'")
  testthat::expect_error(blandPowerCurve(samplesizes = c(1, 10), SD = 1,
                                         delta = 7), "'samplesizes'")
  testthat::expect_error(blandPowerCurve(samplesizes = 10.5, SD = 1,
                                         delta = 7), "'samplesizes'")
})

testthat::test_that("small-scale data (GitHub issue)", {
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
                           "not reached .*delta = 7.*power_agreement_limit")
  testthat::expect_true(is.na(res$N[res$delta == 7]))
  testthat::expect_equal(res$power[res$delta == 7],
                         max(pc$power[pc$delta == 7]))
  testthat::expect_equal(res$N[res$delta == 8], 26)
})

testthat::test_that("find_n gives NA when the minimum is below the range", {
  curve <- function(n) blandPowerCurve(samplesizes = n, mu = 0, SD = 25,
                                       delta = 60)
  pc <- curve(200:100)
  result <- find_n(pc)
  testthat::expect_equal(result$N, 162)
  testthat::expect_gte(result$power, 0.8)
  testthat::expect_lt(pc$power[pc$N == 161], 0.8)

  testthat::expect_warning(high <- find_n(curve(200:220)),
                           "below the range .*power_agreement_limit")
  testthat::expect_true(is.na(high$N))
  testthat::expect_true(is.na(high$power))

  # the minimum itself as the first N is not flagged
  boundary <- curve(162:170)
  testthat::expect_equal(find_n(boundary)$N, 162)
  testthat::expect_equal(find_n(boundary, power = boundary$power[1])$N, 162)
  testthat::expect_equal(find_n(curve(2:4), power = 0)$N, 2)

  # powerCurve objects without the m and icc columns
  legacy <- boundary[, !names(boundary) %in% c("m", "icc")]
  testthat::expect_equal(find_n(legacy)$N, 162)
  testthat::expect_warning(high <- find_n(legacy[legacy$N > 162, ]),
                           "below the range")
  testthat::expect_true(is.na(high$N))
})

testthat::test_that("find_n warns separately for each end of the range", {
  pc <- blandPowerCurve(samplesizes = 50:55, mu = 0.5, SD = 2.5,
                        delta = c(6, 8))
  w <- testthat::capture_warnings(result <- find_n(pc))
  testthat::expect_length(w, 2)
  testthat::expect_match(w[1], "not reached .*delta = 6")
  testthat::expect_match(w[2], "below the range .*delta = 8")
  testthat::expect_true(all(is.na(result$N)))
})

testthat::test_that("find_n matches power_agreement_limit", {
  pc <- blandPowerCurve(samplesizes = 10:120, mu = 0.5, SD = 2.5, delta = 7)
  testthat::expect_equal(find_n(pc)$N,
                         power_agreement_limit(delta = 7, mu = 0.5,
                                               sigma = 2.5, power = 0.8)$n)

  clustered <- blandPowerCurve(samplesizes = 20:80, mu = 0.5, SD = 2.5,
                               delta = 7, m = 3, icc = 0.5)
  optimum <- find_n(clustered)$N
  testthat::expect_equal(optimum,
                         power_agreement_limit(delta = 7, mu = 0.5,
                                               sigma = 2.5, power = 0.8,
                                               m = 3, icc = 0.5)$n)
  testthat::expect_equal(find_n(clustered[clustered$N >= optimum, ])$N,
                         optimum)
  testthat::expect_warning(result <- find_n(clustered[clustered$N > optimum, ]),
                           "below the range")
  testthat::expect_true(is.na(result$N))
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
