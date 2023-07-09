context("agree_test")


testthat::test_that("Simple Use Run Through", {
  data("reps")
  data("ba1986")
  agree1 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      TOST = TRUE)

  agree_pr = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      TOST = TRUE,
                      prop_bias = TRUE)
  p1 = plot(agree_pr)

  p1 = plot(agree_pr,
       geom = "geom_point")

  p1 = plot(agree_pr,
       geom = "geom_bin2d")

  p1 = plot(agree_pr,
       geom = "geom_density_2d")

  p1 = plot(agree_pr,
       geom = "geom_density_2d_filled")

  p1 = plot(agree_pr,
       geom = "stat_density_2d")

  agree2 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      TOST = FALSE)

  agree2_new = agreement_limit(x = "x",
                               y = "y",
                               data = reps,
                               loa_calc = "b")

  agree2_new2 = agreement_limit(x = "x",
                               y = "y",
                               data = reps,
                               loa_calc = "m")
  agree2_new3 = agreement_limit(y = "wright1",
                                x = "mini1",
                                data = ba1986,
                                loa_calc = "m",
                                alpha = .025)
  agree2_new4 = agreement_limit(x = "wright1",
                                y = "mini1",
                                data = ba1986,
                                prop_bias = TRUE,
                                alpha = .025,
                                loa_calc = "m")

  agree2_bland = agreement_limit(x = "wright1",
                                y = "mini1",
                                data = ba1986,
                                prop_bias = FALSE,
                                alpha = .025,
                                loa_calc = "b")
  testthat::expect_gt(agree2_new2$loa$lme,agree2_new$loa$lme)
  testthat::expect_equal(round(agree2_new3$loa$upper_loa_ci,0), 122)
  testthat::expect_equal(round(agree2_bland$loa$upper_loa_ci,0), 109)

  ch1_new = check(agree2_bland)
  testthat::expect_equivalent(c(agree2$loa$estimate),
                              c(agree2_new$loa$bias,
                                agree2_new$loa$lower_loa,
                                agree2_new$loa$upper_loa))

  p1 = plot(agree2,
            geom = "geom_point")

  p1 = plot(agree2,
            geom = "geom_bin2d")

  p1 = plot(agree2,
            geom = "geom_density_2d")

  p1 = plot(agree2,
            geom = "geom_density_2d_filled")

  p1 = plot(agree2_new,
            geom = "stat_density_2d")

  p1 = plot(agree2_new,
            geom = "geom_point")

  p1 = plot(agree2_new,
            geom = "geom_bin2d")

  p1 = plot(agree2_new,
            geom = "geom_density_2d")

  p1 = plot(agree2_new,
            geom = "geom_density_2d_filled")

  p1 = plot(agree2_new,
            geom = "stat_density_2d")

  jmvagree1 = jmvagree(data = reps,
                       method1 = "x",
                       method2 = "y",
                       CCC = TRUE,
                       plotbland = FALSE,
                       plotcon = TRUE,
                       testValue= 2.5,
                       ciWidth = 95,
                       agreeWidth = 95)

  jmvp = jmvagree1$plotba
  jmvp = jmvagree1$plotcon
  pr_test = print(agree1)

  p = plot(agree1,type=1)
  p = plot(agree1,type=1,
           smooth_method = "lm")
  p = plot(agree1,type=1,
           smooth_method = "loess")
  p = plot(agree1,type=1,
           smooth_method = "gam")
  p = plot(agree1,type=2)
  c1 = check(agree1)$p_norm
  c2 = check(agree1)$p_het

  testthat::expect_equal(agree2$ccc.xy$est.ccc,
                         .479,
                         tolerance = .0001)
  testthat::expect_equal(agree2$h0_test,
                        "don't reject h0")

  testthat::expect_equivalent(agree2$loa$estimate,
                              c(0.4383333, -1.9470156, 2.8236823),
                              tolerance = 0.001)
  testthat::expect_equivalent(agree1$loa$estimate,
                              jmvagree1$blandtab$asDF$estimate)

  testthat::expect_equivalent(agree2$loa$lower.ci[2:3],
                              c(-3.001158,  1.769540),
                              tolerance = 0.01)
  testthat::expect_equivalent(agree1$loa$lower.ci,
                              jmvagree1$blandtab$asDF$lowerci)

  testthat::expect_equivalent(agree2$loa$upper.ci[2:3],
                              c(-0.8928731,  3.8778249),
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


testthat::test_that("Added plotting coverage",{

  data("reps")
  data("ba1986")
  agree1 = agree_test(x = reps$x,
                      y = reps$y,
                      delta = 2.5,
                      TOST = FALSE,
                      prop_bias = TRUE)
  testthat::expect_equal(class(agree1), "simple_agree")

  checker = check(agree1)
  p1 = plot(agree1)
  p2 = plot(agree1,
            geom = "geom_bin2d")
  p3 = plot(agree1,
            geom = "geom_density_2d")
  p4 = plot(agree1,
            geom = "geom_density_2d_filled")
  p5 = plot(agree1,
            geom = "stat_density_2d")


})
