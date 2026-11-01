context("agree_np")


testthat::test_that("Simple Use Run Through", {

    data("reps")
    agree1 = suppressWarnings(agree_np(x = "x",
                        y = "y",
                      data = reps,
                        delta = 2.5))

    agree2 = agree_np(x = "x",
                      y = "y",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5)

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)

    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = FALSE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = FALSE)

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)

    expect_error(agree_np(x = "x",
                                       y = "y",
                                       data = reps,
                                       TOST = FALSE))


    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       id = "id",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = FALSE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      id = "id",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = FALSE)

    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       id = "id",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = TRUE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      id = "id",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = TRUE)

    expect_message(agree_np(x = "x",
                            y = "y",
                            id = "id",
                            prop_bias = TRUE,
                            data = reps,
                            delta = 10,
                            TOST = TRUE))

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)
    print(agree1)
    print(agree2)
    expect_error(check(agree1))


    p1 = plot(agree1,
              geom = "geom_point")

    p1 = plot(agree1,
              geom = "geom_bin2d")

    p1 = plot(agree1,
              geom = "geom_density_2d")

    p1 = plot(agree1,
              geom = "geom_density_2d_filled")

    p1 = plot(agree1,
              geom = "stat_density_2d")


})

# GitHub issue #50 data: x1 is 0 for 23 of 31 subjects, so those points lie
# exactly on delta = -2 * mean. quantreg's bootstrap (simplex fits) cycled
# without end on some resamples of these data, e.g. `bad` below.
df50 = data.frame(
  x1 = c(0, 0, 0, 0, -0.15, 0, 0, 0, 0, 0, -0.185714286, 0, 0, 0, 0, -0.05,
         0.028571429, 0, 0.028571429, 0, 0, 0, 0, 0, 0, 0, -0.05, 0, -0.3,
         0, 0),
  x2 = c(-0.255255255, -0.092972973, -0.421428571, -0.001228501,
         -0.027777778, -0.103070175, 0.042857143, -0.005555556, 0.048178613,
         -0.236842105, -0.092436975, 0.117777778, 0.08168643, -0.090909091,
         -0.082309582, -0.038461538, 0.058823529, -0.03968254, -0.008009153,
         0.052287582, -0.001349528, 0, -0.016746411, 0.078947368,
         -0.031400966, -0.161616162, 0.057142857, 0.051578947, -0.117647059,
         -0.169642857, -0.057017544)
)
d50 = data.frame(delta = df50$x1 - df50$x2, mean = (df50$x1 + df50$x2) / 2)

testthat::test_that("bootstrap fits a resample on which quantreg's cycled", {
  bad = c(21, 2, 6, 12, 21, 16, 12, 8, 4, 7, 29, 21, 25, 30, 1, 14, 20, 1,
          9, 4, 26, 29, 26, 1, 2, 9, 29, 19, 6, 6, 30)
  q50 = quantreg::rq(delta ~ mean, data = d50, tau = c(0.025, 0.5, 0.975))
  V = rq_boot_vcov(q50, idx = cbind(bad, bad, seq_len(31)))
  expect_length(V, 3)
  for (v in V) {
    expect_equal(dim(v), c(2, 2))
    expect_true(all(is.finite(v)))
  }
})

testthat::test_that("bootstrap helpers give rq estimates with t intervals", {
  data("reps")
  df = na.omit(data.frame(delta = reps$x - reps$y,
                          mean = (reps$x + reps$y) / 2))
  qm = quantreg::rq(delta ~ mean, data = df, tau = c(0.025, 0.5, 0.975))
  set.seed(1)
  V = rq_boot_vcov(qm, R = 50)
  at = c(3, 5, 7)
  pr = predict(qm, newdata = data.frame(mean = at))
  for (j in 1:3) {
    ci = rq_boot_ci(qm, V, tau = qm$tau[j], at = at, conf.level = 0.9)
    expect_equal(ci$emmean, unname(pr[, j]))
    expect_equal(ci$df, rep(nrow(df) - 2, 3))
    X = cbind(1, at)
    expect_equal(ci$SE, sqrt(diag(X %*% V[[j]] %*% t(X))))
    expect_equal(ci$upper.CL - ci$emmean, qt(0.95, nrow(df) - 2) * ci$SE)
  }
  co = rq_boot_coef(qm, V, conf.level = 0.9)
  expect_equal(co$estimate, as.vector(qm$coefficients))
  expect_equal(co$std.error, unlist(lapply(V, function(v) sqrt(diag(v)))))
  expect_equal(co$p.value,
               2 * pt(-abs(co$estimate / co$std.error), nrow(df) - 2))
  expect_equal(co$tau, rep(qm$tau, each = 2))
})

testthat::test_that("bootstrap SEs agree with quantreg's xy bootstrap", {
  skip_on_cran()
  data("reps")
  df = na.omit(data.frame(delta = reps$x - reps$y,
                          mean = (reps$x + reps$y) / 2))
  qm = quantreg::rq(delta ~ mean, data = df, tau = 0.5)
  set.seed(1)
  se_ours = sqrt(diag(rq_boot_vcov(qm, R = 4000)[[1]]))
  set.seed(2)
  se_qr = suppressWarnings(
    summary(qm, se = "boot", R = 4000, covariance = TRUE))$coefficients[, 2]
  expect_equal(se_ours, unname(se_qr), tolerance = 0.05)
})

testthat::test_that("agree_np and its plot finish on the issue #50 data", {
  skip_on_cran()
  for (s in 1:3) {
    set.seed(s)
    for (pb in c(FALSE, TRUE)) {
      res = suppressWarnings(agree_np(x = "x1", y = "x2", data = df50,
                                      delta = log(1.10), prop_bias = pb))
      expect_true(all(is.finite(as.matrix(res$loa[, 1:3]))))
      expect_s3_class(plot(res), "ggplot")
    }
  }
})
