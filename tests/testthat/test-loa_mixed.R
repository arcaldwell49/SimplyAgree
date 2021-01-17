context("loa_mixed")


testthat::test_that("basic runs", {
  estimates = c(0.2510000,
                -0.1887407,
                0.6907407,
                0.1675079,
                0.1492624,
                0.2243616)
  #skip('skip')
  test_bca = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "bca"
  )
  testthat::expect_equivalent(test_bca$bs_tab$estimate,
                              estimates,
                              tolerance = 0.001)
  test_perc = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "perc"
  )
  testthat::expect_equivalent(test_perc$bs_tab$estimate,
                              estimates,
                              tolerance = 0.001)
  test_basic = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "basic"
  )
  testthat::expect_equivalent(test_basic$bs_tab$estimate,
                              estimates,
                              tolerance = 0.001)
  test_norm = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 99,
    type = "norm"
  )
  testthat::expect_equivalent(test_norm$bs_tab$estimate,
                              estimates,
                              tolerance = 0.001)
})
