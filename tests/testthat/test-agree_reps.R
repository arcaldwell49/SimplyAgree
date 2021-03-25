context("agree_reps")


testthat::test_that("examples from Zou", {
 data("reps")

  reps_test = agree_reps(x="x",y="y",
                         id = "id",
                         data = reps)

  testthat::expect_equivalent(reps_test$loa$estimate,
                              c(.7125,-2.23,3.66),
                              tolerance = 0.001)
  testthat::expect_equivalent(reps_test$loa$lower.ci[2:3],
                              c(-9.76,1.91),
                              tolerance = 0.01)
  testthat::expect_equivalent(reps_test$loa$upper.ci[2:3],
                              c(-.48,11.20),
                              tolerance = 0.01)
  reps_test = agree_reps(x="x",y="y",
                         delta = 2.5,
                         id = "id",
                         data = reps)

  pr_test = print(reps_test)
  p = plot(reps_test, type = 1)
  p = plot(reps_test, type = 2)
})
