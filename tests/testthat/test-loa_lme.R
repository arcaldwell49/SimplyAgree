context("loa_lme")


testthat::test_that("basic runs", {
  expect_error(loa_lme(
    diff = "diff",
    avg = "avg",
    #condition = "condition",
    id = "id",
    data = df,
    replicates = 10,
    het_var = TRUE
  ))
  data('recpre_long')
  df = recpre_long
  df$avg = (recpre_long$AM + recpre_long$PM) / 2
  df$condition = df$trial_condition
  test1 = suppressWarnings(loa_lme(diff = "diff",
                  avg = "avg",
                  #condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10))
  test2 = suppressWarnings(loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10))
  test3 = suppressWarnings(loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  prop_bias = TRUE))
  test4 = suppressWarnings(loa_lme(diff = "diff",
                  avg = "avg",
                  #condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  prop_bias = FALSE))

  test5 =loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  het_var = TRUE)

  test6 = (loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  het_var = TRUE,
                  prop_bias = TRUE))

  test7 =loa_lme(diff = "diff",
                 avg = "avg",
                 condition = "condition",
                 id = "id",
                 data = df,
                 replicates = 10,
                 het_var = TRUE,
                 prop_bias = TRUE)

  testprint = print(test6)
  testplot = plot(test6)
  testplot = plot(test5)
  testplot= plot(test3)
  testplot = plot(test1)
  check1 = check(test1)
  check2 = check(test6)

  testplot = plot(test6,
                  smooth_method = "lm")
  testplot = plot(test5,
                  smooth_method = "lm")
  testplot= plot(test3,
                 smooth_method = "lm")
  testplot = plot(test1,
                  smooth_method = "lm")

  testplot = plot(test6,
                  smooth_method = "loess")
  testplot = plot(test5,
                  smooth_method = "loess")
  testplot= plot(test3,
                 smooth_method = "loess")
  testplot = plot(test1,
                  smooth_method = "loess")


  testplot = plot(test6,
                  geom = "geom_bin2d")
  testplot = plot(test5,
                  geom = "geom_bin2d")
  testplot= plot(test3,
                 geom = "geom_bin2d")
  testplot = plot(test1,
                  geom = "geom_bin2d")

  testplot = plot(test6,
                  geom = "geom_density_2d")
  testplot = plot(test5,
                  geom = "geom_density_2d")
  testplot= plot(test3,
                 geom = "geom_density_2d")
  testplot = plot(test1,
                  geom = "geom_density_2d")


  testplot = plot(test6,
                  geom = "geom_density_2d_filled")
  testplot = plot(test5,
                  geom = "geom_density_2d_filled")
  testplot= plot(test3,
                 geom = "geom_density_2d_filled")
  testplot = plot(test1,
                  geom = "geom_density_2d_filled")


  testplot = plot(test6,
                  geom = "stat_density_2d")
  testplot = plot(test5,
                  geom = "stat_density_2d")
  testplot= plot(test3,
                 geom = "stat_density_2d")
  testplot = plot(test1,
                  geom = "stat_density_2d")
})
