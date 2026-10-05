# Extracted from test-agree_nest.R:230

# prequel ----------------------------------------------------------------------
context("agree_nest")

# test -------------------------------------------------------------------------
data("reps")
reps2 = reps
reps2$y[5] = 6.35
reps2$y[9] = 4.09
nest_test = suppressWarnings(agree_nest(x="x",y="y",
                         id = "id",
                         data = reps2,
                         TOST = FALSE))
nest_test2 = suppressWarnings(agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE))
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
nest_test_new_asym = agreement_limit(x="x",y="y",
                                   id = "id",
                                   data = reps2,
                                   data_type = "nest",
                                   alpha = .025,
                                   prop_bias = TRUE,
                                   lmer_df = "a")
testthat::expect_equal(nest_test_new$loa$bias,0.7234979,
                         tolerance = .0001)
ch1 = check(nest_test_new)
nest_test_newlog = agreement_limit(x="x",y="y",
                                  id = "id",
                                  data = reps2,
                                  data_type = "nest",
                                  log_tf = TRUE)
nest_test_newlog2 = agreement_limit(x="x",y="y",
                                     id = "id",
                                     data = reps2,
                                     data_type = "nest",
                                     log_tf =TRUE,
                                     log_tf_display = "sym")
nest_test_bland = agreement_limit(x="x",y="y",
                                     id = "id",
                                     data = reps2,
                                     data_type = "nest",
                                     log_tf =TRUE,
                                    loa_calc = "b")
nest_test_bland1 = agreement_limit(x="x",y="y",
                                    id = "id",
                                    data = reps2,
                                    data_type = "nest",
                                    log_tf =TRUE,
                                    loa_calc = "b",
                                    log_tf_display = "sym")
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
nest_test3_ccc = suppressWarnings(agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE,
                          ccc = FALSE))
nest_test3 = suppressWarnings(agree_nest(x="x",y="y",
                          id = "id",
                          data = reps2,
                          TOST = FALSE,
                          prop_bias = TRUE))
ptest = plot(nest_test3)
nest_test3 = suppressWarnings(agree_nest(x="x",y="y",
                          id = "id",
                          data = reps,
                          TOST = FALSE,
                          delta = 5,
                          prop_bias = TRUE))
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
nest_test = suppressWarnings(agree_nest(x="x",y="y",
                         delta = 2,
                         id = "id",
                         data = reps2))
pr_test = print(nest_test)
p = plot(nest_test, type = 1)
p = plot(nest_test, type = 2)
nest = reps2
nest_test2 = suppressWarnings(agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest
  ))
nest_test3 = suppressWarnings(agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest,
    agree.level = .8,
    conf.level = .75
  ))
nest_test4 = suppressWarnings(agree_nest(
    x = "x",
    y = "y",
    delta = 2.5,
    id = "id",
    data = nest,
    agree.level = .75,
    conf.level = .8
  ))
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
