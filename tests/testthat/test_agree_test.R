context("agree_test")


testthat::test_that("Simple Use Run Through", {
  data("reps")
  agree1 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5)
  print(agree1)

  p = plot(agree1,type=1)
  p = plot(agree1,type=2)

  testthat::expect_equal(agree1$ccc.xy$est.ccc,
                         .479,
                         tolerance = .0001)
  testthat::expect_equal(agree1$h0_test,
                        "don't reject h0")

  testthat::expect_equivalent(agree1$loa$estimate,
                              c(0.4383333, -1.9470156, 2.8236823),
                              tolerance = 0.001)
  testthat::expect_equivalent(agree1$loa$lower.ci[2:3],
                              c(-2.150644, 2.620054),
                              tolerance = 0.01)
  testthat::expect_equivalent(agree1$loa$upper.ci[2:3],
                              c(-1.743387, 3.027311),
                              tolerance = 0.01)

  agree2 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      agree.level = .8)
  testthat::expect_equal(agree2$h0_test,
                        "reject h0")

  agree3 = agree_test(x = reps$x,
                      y = reps$y,
                      agree.level = .8)
  testthat::expect_equal(agree3$h0_test,
                        "No Hypothesis Test")

})
