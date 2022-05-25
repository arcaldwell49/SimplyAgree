context("loa_mixed")


testthat::test_that("basic runs", {
  estimates1 = c(0.2510000,-0.1887407,
                 0.6907407)

  estimates2 = c(0.1675079,
                 0.1492624,
                 0.2243616)

  #skip_on_cran()
  set.seed(117)
  test_bca = suppressWarnings({
    loa_mixed(
      diff = "diff",
      condition = "trial_condition",
      id = "id",
      data = recpre_long,
      delta = 2,
      plot.xaxis = "AM",
      conf.level = .95,
      agree.level = .95,
      replicates = 99,
      type = "bca"
    )
    })
  testthat::expect_equivalent(test_bca$loa$estimate,
                              estimates1,
                              tolerance = 0.001)
  testthat::expect_equivalent(test_bca$var_comp$estimate,
                              estimates2,
                              tolerance = 0.001)
  pr_test = print(test_bca)
  p = plot(test_bca)
  test_perc = suppressWarnings(loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "perc"
  ))
  testthat::expect_equivalent(test_perc$loa$estimate,
                              estimates1,
                              tolerance = 0.001)
  testthat::expect_equivalent(test_perc$var_comp$estimate,
                              estimates2,
                              tolerance = 0.001)
  test_basic = suppressWarnings({loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "basic"
  )})
  testthat::expect_equivalent(test_basic$loa$estimate,
                              estimates1,
                              tolerance = 0.001)
  testthat::expect_equivalent(test_basic$var_comp$estimate,
                              estimates2,
                              tolerance = 0.001)
  test_norm = suppressWarnings({ loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "norm"
  ) })
  testthat::expect_equivalent(test_norm$loa$estimate,
                              estimates1,
                              tolerance = 0.001)
  testthat::expect_equivalent(test_norm$var_comp$estimate,
                              estimates2,
                              tolerance = 0.001)
})
