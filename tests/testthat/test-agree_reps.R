context("agree_reps")


testthat::test_that("examples from Zou", {
 data("reps")

  reps_test = agree_reps(x="x",y="y",
                         id = "id",
                         data = reps,
                         TOST = FALSE)

  reps_test_new  = agreement_limit(x="x",y="y",
                              id = "id",
                              data = reps,
                              data_type = "reps",
                              alpha = .025,
                              prop_bias = FALSE)
  reps_test_new2  = agreement_limit(x="x",y="y",
                                   id = "id",
                                   data = reps,
                                   data_type = "reps",
                                   alpha = .025,
                                   prop_bias = TRUE)
  print(reps_test_new2)
  testthat::expect_equal(round(reps_test_new$loa$bias,2),
                         round(reps_test$loa$estimate[1],2),
                         tolerance = .01)
  reps_test_newlog  = agreement_limit(x="x",y="y",
                                   id = "id",
                                   data = reps,
                                   data_type = "reps",
                                   alpha = .025,
                                   log_tf = TRUE)
  reps_test_blan = agreement_limit(x="x",y="y",
                                      id = "id",
                                      data = reps,
                                      data_type = "reps",
                                   loa_calc = "b",
                                      alpha = .025,
                                      log_tf = TRUE)
  print(reps_test_blan)
  print(reps_test_newlog)
  print(reps_test_new)
  reps_test2 = agree_reps(x="x",y="y",
                         id = "id",
                         data = reps,
                         TOST = FALSE,
                         prop_bias = TRUE)
  ptest = plot(reps_test2)

  reps_test3 = agree_reps(x="x",y="y",
                          id = "id",
                          data = reps,
                          TOST = FALSE,
                          delta = 5,
                          prop_bias = TRUE)
  ptest = plot(reps_test2)
  ptest = plot(reps_test2,
               delta = 5)
  ptest = plot(reps_test2,
               geom = "geom_bin2d")
  ptest = plot(reps_test2,
               geom = "geom_density_2d")
  ptest = plot(reps_test2,
               geom = "geom_density_2d_filled")
  ptest = plot(reps_test2,
               geom = "stat_density_2d")

  print(reps_test3)
  ptest = plot(reps_test3)
  ptest = plot(reps_test3,
               delta = 5)
  ptest = plot(reps_test3,
               geom = "geom_bin2d")
  ptest = plot(reps_test3,
               geom = "geom_density_2d")
  ptest = plot(reps_test3,
               geom = "geom_density_2d_filled")
  ptest = plot(reps_test3,
               geom = "stat_density_2d")

  testthat::expect_equivalent(reps_test$loa$estimate,
                              c(.7125,-2.23,3.66),
                              tolerance = 0.001)
  testthat::expect_equivalent(reps_test$loa$lower.ci[2:3],
                              c(-9.76,1.91),
                              tolerance = 0.01)
  testthat::expect_equivalent(reps_test$loa$upper.ci[2:3],
                              c(-.48,11.20),
                              tolerance = 0.01)
  reps_test2 = agree_reps(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = reps
  )

  reps_test3 = agree_reps(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = reps,
    agree.level = .8,
    conf.level = .75
  )

  reps_test4 = agree_reps(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = reps,
    agree.level = .75,
    conf.level = .8
  )

  pr_test = print(reps_test)
  p = plot(reps_test, type = 1,
           smooth_method = "lm")
  p = plot(reps_test, type = 1,
           smooth_method = "loess")
  expect_error(plot(reps_test, type = 1,
           smooth_method = "gam"))
  p = plot(reps_test, type = 1,
           smooth_method = "lm",
           smooth_se = FALSE)
  p = plot(reps_test, type = 1,
           smooth_method = "loess",
           smooth_se = FALSE)
  expect_error(plot(reps_test, type = 1,
           smooth_method = "gam",
           smooth_se = FALSE))
  p = plot(reps_test, type = 2)
  c1 = check(reps_test)$p_norm
  c2 = check(reps_test)$p_het
  c3 = check(reps_test)$p_bias

  c1 = check(reps_test)$p_norm
  c2 = check(reps_test)$p_het
  c3 = check(reps_test)$p_bias

  pr_test = print(reps_test2)
  p = plot(reps_test2, type = 1)
  p = plot(reps_test2, type = 2)

  pr_test = print(reps_test3)
  p = plot(reps_test3, type = 1)
  p = plot(reps_test3, type = 2)

  pr_test = print(reps_test3)
  p = plot(reps_test3, type = 1)
  p = plot(reps_test3, type = 2)

  reps_test = agree_reps(x="x",y="y",
                         id = "id",
                         data = reps)

  test_jmv = jmvagreemulti(
    data = reps,
    method1 = "x",
    method2 = "y",
    id = "id",
    valEq = TRUE
  )

  #test_jmv$blandtab$asDF
  #nest_test$loa

  testthat::expect_equivalent(test_jmv$blandtab$asDF$estimate,
                              reps_test$loa$estimate)

  testthat::expect_equivalent(test_jmv$blandtab$asDF$lowerci,
                              reps_test$loa$lower.ci)

  testthat::expect_equivalent(test_jmv$blandtab$asDF$upperci,
                              reps_test$loa$upper.ci)

  reps_test = agree_reps(x="x",y="y",
                         id = "id",
                         data = reps,
                         ccc = FALSE)
})
