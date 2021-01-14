context("loa_mixed")


testthat::test_that("basic runs", {
  estimates = c(0.2510000,
                0.1675079,
                0.1492624,
                0.2243616,
                -0.1887407,
                0.6907407)
  test_bca = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 199,
    type = "bca"
  )
  test_perc = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 199,
    type = "perc"
  )
  test_basic = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 199,
    type = "basic"
  )
  test_norm = loa_mixed(
    diff = "diff",
    condition = "trial_condition",
    id = "id",
    data = recpre_long,
    conf.level = .95,
    agree.level = .95,
    replicates = 199,
    type = "norm"
  )
})
