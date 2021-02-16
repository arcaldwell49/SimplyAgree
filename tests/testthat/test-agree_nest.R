context("agree_nest")


testthat::test_that("examples from Zou", {
 data("reps")

  reps2 = reps

  reps2$y[5] = 6.35
  reps2$y[9] = 4.09

  nest_test = agree_nest(x="x",y="y",
                         id = "id",
                         data = reps2)
  testthat::expect_equivalent(nest_test$loa$estimate,
                              c(.7255,-2.14,3.59),
                              tolerance = 0.001)
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


})
