context("agree_nest")


testthat::test_that("examples from Zou", {
 data("reps")

  reps2 = reps

  reps2$y[5] = 6.35
  reps2$y[9] = 4.09

  nest_test = agree_nest(x="x",y="y",
                         id = "id",
                         data = reps2,
                         TOST = FALSE)

  nest_test2 = agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE)
  nest_test_new = agreement_limit(x="x",y="y",
                         id = "id",
                         data = reps2,
                         data_type = "nest",
                         alpha = .025)
  expect_error(agreement_limit(x="x",y="y",
                               #id = "id",
                               data = reps2,
                               data_type = "nest",
                               alpha = .025))
  nest_test_new2 = agreement_limit(x="x",y="y",
                                  id = "id",
                                  data = reps2,
                                  data_type = "nest",
                                  alpha = .025,
                                  prop_bias = TRUE)
  testthat::expect_equal(nest_test_new$loa$bias,0.7234979,
                         tolerance = .0001)
  ch1 = check(nest_test_new)
  nest_test_newlog = agreement_limit(x="x",y="y",
                                  id = "id",
                                  data = reps2,
                                  data_type = "nest",
                                  log_tf =TRUE)
  nest_test_bland = agreement_limit(x="x",y="y",
                                     id = "id",
                                     data = reps2,
                                     data_type = "nest",
                                     log_tf =TRUE,
                                    loa_calc = "b")
  print(nest_test_bland)
  agree_test_newlog = agreement_limit(x="x",y="y",
                                     id = "id",
                                     data = reps2,
                                     data_type = "simple",
                                     log_tf =TRUE)

  ptest = plot(nest_test_new)
  ptest = plot(nest_test_new,
               delta = 5)
  ptest = plot(nest_test_new,
               geom = "geom_bin2d")
  ptest = plot(nest_test_new,
               geom = "geom_density_2d")
  ptest = plot(nest_test_new,
               geom = "geom_density_2d_filled")
  ptest = plot(nest_test_new,
               geom = "stat_density_2d")

  print(nest_test_new2)
  ptest = plot(nest_test_new2)
  ptest = plot(nest_test_new2,
               delta = 5)
  ptest = plot(nest_test_new2,
               geom = "geom_bin2d")
  ptest = plot(nest_test_new2,
               geom = "geom_density_2d")
  ptest = plot(nest_test_new2,
               geom = "geom_density_2d_filled")
  ptest = plot(nest_test_new2,
               geom = "stat_density_2d")


  # Ensure emmeans working appropriately
  testthat::expect_equal(agree_test_newlog$loa$lower.CL,-0.02185209,
                         tolerance = .0001)
  testthat::expect_equal(nest_test_newlog$loa$df,3,
                         tolerance = .01)
  testthat::expect_equal(!is.null(nest_test_newlog$loa), TRUE)
  testthat::expect_equal(nrow(nest_test_newlog$loa), 1)
  testthat::expect_equal(nest_test_newlog$loa$lower.CL,-0.319,
                         tolerance = .001)
  testthat::expect_equal(nest_test_newlog$loa$bias,0.1138129,
                         tolerance = .0001)
  testthat::expect_equal(nest_test_newlog$loa$lme,1.030285,
                         tolerance = .0001)
  testthat::expect_equal(class(nest_test_newlog), "loa")
  pr1 = print(nest_test_newlog)
  nest_test2_new = agreement_limit(x="x",y="y",
                          id = "id",
                          data = reps2,
                          prop_bias = TRUE,
                          data_type = "nest")

  ptest = plot(nest_test2)
  ptest = plot(nest_test2_new)
  ptest = plot(nest_test_new)

  nest_test3_ccc = agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE,
                          ccc = FALSE)

  nest_test3 = agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE)
  ptest = plot(nest_test3)


  nest_test3 = agree_nest(x="x",y="y",
                          id = "id",
                          data = reps,
                          TOST = FALSE,
                          delta = 5,
                          prop_bias = TRUE)
  ptest = plot(nest_test3)

  testthat::expect_equivalent(nest_test$loa$estimate,
                              c(.7255,-2.14,3.59),
                              tolerance = 0.001)
  testthat::expect_equivalent(c(nest_test_new$loa$bias,
                                nest_test_new$loa$lower_loa,
                                nest_test_new$loa$upper_loa),
                              c(.7255,-2.14,3.59),
                              tolerance = 0.005)
  testthat::expect_equivalent(c(nest_test_new$loa$lower_loa_ci,
                                nest_test_new$loa$upper_loa_ci),
                              c(-9.8,11.2),
                              tolerance = 0.01)
  testthat::expect_equivalent(nest_test$loa$lower.ci[2:3],
                              c(-9.83,1.77),
                              tolerance = 0.01)
  testthat::expect_equivalent(nest_test$loa$upper.ci[2:3],
                              c(-.33,11.27),
                              tolerance = 0.01)
  nest_test = agree_nest(x="x",y="y",
                         delta = 2,
                         id = "id",
                         data = reps2)
  pr_test = print(nest_test)
  p = plot(nest_test, type = 1)
  p = plot(nest_test, type = 2)
  #expect_warning(check(nest_test)$p_norm)
  #expect_warning(check(nest_test)$p_het)

  nest = reps2
  nest_test2 = agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest
  )

  nest_test3 = agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest,
    agree.level = .8,
    conf.level = .75
  )

  nest_test4 = agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest,
    agree.level = .75,
    conf.level = .8
  )

  pr_test = print(nest_test)
  p = plot(nest_test, type = 1)
  p = plot(nest_test, type = 2)

  pr_test = print(nest_test2)
  p = plot(nest_test2, type = 1)
  p = plot(nest_test2, type = 2)

  pr_test = print(nest_test3)
  p = plot(nest_test3, type = 1)
  p = plot(nest_test3, type = 2)

  pr_test = print(nest_test3)
  p = plot(nest_test3, type = 1)
  p = plot(nest_test3, type = 1,
           smooth_method = "lm")
  p = plot(nest_test3, type = 1,
           smooth_method = "loess")
  expect_error(plot(nest_test3, type = 1,
           smooth_method = "gam"))
  p = plot(nest_test3, type = 1)
  p = plot(nest_test3, type = 2)

  test_jmv = jmvagreemulti(
    data = nest,
    method1 = "x",
    method2 = "y",
    id = "id"
  )

  #test_jmv$blandtab$asDF
  #nest_test$loa

  testthat::expect_equivalent(test_jmv$blandtab$asDF$estimate,
                              nest_test$loa$estimate)

  testthat::expect_equivalent(test_jmv$blandtab$asDF$lowerci,
                              nest_test$loa$lower.ci)

  testthat::expect_equivalent(test_jmv$blandtab$asDF$upperci,
                              nest_test$loa$upper.ci)


})


testthat::test_that("example from error message",{
  set.seed(637234)
  data2 = data.frame(IDNum = c(1,1,1,
                               2,2,
                               3,3,3,
                               4,4,4,
                               5,5,5,5,
                               6,6,
                               7,7,
                               8,8,
                               9,9,
                               10,10,10),
                    ACT_Sleep_Time = c(rnorm(3),
                                       2,NA,
                                       rnorm(3),
                                       rnorm(3),
                                       rnorm(4),
                                       rnorm(2),
                                       rnorm(2),
                                       rnorm(2),
                                       rnorm(2),
                                       rnorm(3)),
                    E_TST1_minutes = c(rnorm(3),
                                       rnorm(2),
                                       rnorm(3),
                                       rnorm(3),
                                       rnorm(4),
                                       rnorm(2),
                                       rnorm(2),
                                       rnorm(2),
                                       -1,NA,
                                       rnorm(3))
                    )

  TSTnest = agree_nest(x = "ACT_Sleep_Time",
                       y = "E_TST1_minutes",
                       id = "IDNum",
                       #delta = 100,
                       data = data2,
                       agree.level = .8,
                       TOST = FALSE)

  testthat::expect_equivalent(TSTnest$loa$estimate,
                              c(.597,-.568,1.761),
                              tolerance = 0.001)
  testthat::expect_equivalent(TSTnest$loa$upper.ci,
                              c(1.0301,-0.0741,2.5673),
                              tolerance = 0.001)
  testthat::expect_equivalent(TSTnest$loa$lower.ci,
                              c(.164,-1.373,1.268),
                              tolerance = 0.001)

  TSTrep = agree_reps(x = "ACT_Sleep_Time",
                       y = "E_TST1_minutes",
                       id = "IDNum",
                       #delta = 100,
                       data = data2,
                       agree.level = .8,
                      TOST = FALSE)

  testthat::expect_equivalent(TSTrep$loa$estimate,
                              c(.717,-.665,2.099),
                              tolerance = 0.001)
  testthat::expect_equivalent(TSTrep$loa$upper.ci,
                              c(1.185,-.142,2.936),
                              tolerance = 0.001)
  testthat::expect_equivalent(TSTrep$loa$lower.ci,
                              c(.249,-1.502,1.576),
                              tolerance = 0.001)



})



