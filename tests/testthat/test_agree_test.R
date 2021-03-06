context("agree_test")


testthat::test_that("Simple Use Run Through", {
  data("reps")
  agree1 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5)
  jmvagree1 = jmvagree(data = reps,
                       method1 = "x",
                       method2 = "y",
                       CCC = TRUE,
                       plotbland = TRUE,
                       plotcon = TRUE)
  jmvp = jmvagree1$plotba
  jmvp = jmvagree1$plotcon
  pr_test = print(agree1)

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
  testthat::expect_equivalent(agree1$loa$estimate,
                              jmvagree1$blandtab$asDF$estimate)

  testthat::expect_equivalent(agree1$loa$lower.ci[2:3],
                              c(-2.810938, 1.959760),
                              tolerance = 0.01)
  testthat::expect_equivalent(agree1$loa$lower.ci,
                              jmvagree1$blandtab$asDF$lowerci)

  testthat::expect_equivalent(agree1$loa$upper.ci[2:3],
                              c(-1.083094, 3.687604),
                              tolerance = 0.01)
  testthat::expect_equivalent(agree1$loa$upper.ci,
                              jmvagree1$blandtab$asDF$upperci)

  agree2 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      agree.level = .8)
  jmvagree2 = jmvagree(data = reps,
                       method1 = "x",
                       method2 = "y",
                       CCC = FALSE,
                       plotbland = FALSE,
                       plotcon = FALSE)
  testthat::expect_equal(agree2$h0_test,
                        "reject h0")

  agree3 = agree_test(x = reps$x,
                      y = reps$y,
                      agree.level = .8)
  jmvagree3 = jmvagree(data = reps,
                       method1 = "x",
                       method2 = "y",
                       CCC = FALSE,
                       plotbland = FALSE,
                       plotcon = FALSE)
  testthat::expect_equal(agree3$h0_test,
                        "No Hypothesis Test")



})
