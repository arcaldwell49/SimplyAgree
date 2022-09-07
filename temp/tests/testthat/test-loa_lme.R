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

  test5 = (loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  het_var = TRUE))

  test6 = (loa_lme(diff = "diff",
                  avg = "avg",
                  condition = "condition",
                  id = "id",
                  data = df,
                  replicates = 10,
                  het_var = TRUE,
                  prop_bias = TRUE))

  testprint = print(test6)
  testplot = plot(test6)
  testplot = plot(test5)
  testplot= plot(test3)
  testplot = plot(test1)
  check1 = check(test1)
  check2 = check(test6)

})
