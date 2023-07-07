context("agree_test")


testthat::test_that("Simple Use Run Through", {

  sf <- matrix(
    c(9,    2,   5,    8,
      6,    1,   3,    2,
      8,    4,   6,    8,
      7,    1,   2,    6,
      10,   5,   6,    9,
      6,   2,   4,    7),
    ncol = 4,
    byrow = TRUE
  )

  ratermat1 = ("Rater1 Rater2 Rater3 Rater4
1       1      1     NA      1
2       2      2      3      2
3       3      3      3      3
4       3      3      3      3
5       2      2      2      2
6       1      2      3      4
7       4      4      4      4
8       1      1      2      1
9       2      2      2      2
10     NA      5      5      5
11     NA     NA      1      1
12     NA     NA      3     NA")

  ratermat2 = as.matrix(read.table(textConnection(ratermat1),
                       header=TRUE,
                       row.names=1))

  expect_error(agree_coef(data = ratermat2,
                          wide = TRUE,
                          weighted = TRUE))

  expect_error(agree_coef(data = ratermat2,
             wide = FALSE,
             weighted = TRUE,
             col.names = c("Rater1", "Rater2", "Rater3", "Rater4")))

  irr1w = agree_coef(data = ratermat2,
                    wide = TRUE,
                    weighted = TRUE,
                    col.names = c("Rater1", "Rater2", "Rater3", "Rater4"))

  expect_equivalent(c(0.9753788, .9140007, 0.8649351, 0.8491071),
                    irr1w$est,
                    tolerance = .00001)
  expect_equivalent(c(1, 1, 1, 1),
                    irr1w$upper.ci,
                    tolerance = .01)
  expect_equivalent(c(.775, .685, .543, .561),
                    irr1w$lower.ci,
                    tolerance = .01)

  irr1 = agree_coef(data = ratermat2,
                     wide = TRUE,
                     weighted = FALSE,
                     col.names = c("Rater1", "Rater2", "Rater3", "Rater4"))

  expect_equivalent(c(.818,.775,.7611,.743),
                    irr1$est,
                    tolerance = .001)
  expect_equivalent(c(1, 1, 1, 1),
                    irr1$upper.ci,
                    tolerance = .01)
  expect_equivalent(c(.5417,.4608,.4243,.4192),
                    irr1$lower.ci,
                    tolerance = .01)

  colnames(sf) <- paste("J", 1:4, sep = "")
  rownames(sf) <- paste("S", 1:6, sep = "")
  #sf  #example from Shrout and Fleiss (1979)
  dat = as.data.frame(sf)

  test1 = reli_stats(data = dat,
                     wide = TRUE,
                     se_type = "ICC1",
                     col.names = c("J1", "J2", "J3", "J4"))
  test1 = reli_stats(data = dat,
                     wide = TRUE,
                     cv_calc = "SEM",
                     col.names = c("J1", "J2", "J3", "J4"))
  test1 = reli_stats(data = dat,
                     wide = TRUE,
                     cv_calc = "residuals",
                     col.names = c("J1", "J2", "J3", "J4"))

  test1 = reli_stats(data = dat,
                     wide = TRUE,
                     col.names = c("J1", "J2", "J3", "J4"))

  test1_aov = reli_aov(data = dat,
                         wide = TRUE,
                         col.names = c("J1", "J2", "J3", "J4"))

  expect_equivalent(test1$icc$icc, test1$icc$icc)
  checktest1 = check(test1)
  checktest2 = check(test1_aov)
  ptest = print(test1_aov)
  ptest2 = plot(test1_aov)
  jmvtest2 = jmvreli(data = dat,
                     vars =c("J1", "J2", "J3", "J4"),
                     desc = TRUE,
                     plots = TRUE)

  testthat::expect_equivalent(jmvtest2$icctab$asDF$icc,
                              test1$icc$icc)
  testthat::expect_equivalent(jmvtest2$icctab$asDF$lower.ci,
                              test1$icc$lower.ci)
  testthat::expect_equivalent(jmvtest2$icctab$asDF$upper.ci,
                              test1$icc$upper.ci)
  pr_test = print(test1)
  p = plot(test1)

  df = data.frame(id = c(1,1,1,1,
                         2,2,2,2,
                         3,3,3,3,
                         4,4,4,4,
                         5,5,5,5,
                         6,6,6,6),
                  it = c(1,2,3,4,
                         1,2,3,4,
                         1,2,3,4,
                         1,2,3,4,
                         1,2,3,4,
                         1,2,3,4),
                  va = c(9,2,5,8,
                         6,1,3,2,
                         8,4,6,8,
                         7,1,2,6,
                         10,5,6,9,
                         6,2,4,7))

  coeftest = agree_coef(wide = FALSE,
                        data = df,
                        measure = "va",
                        item = "it",
                        id = "id",
                        weighted = TRUE)
  test2 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id")
  test2_aov = reli_aov(data = df,
                     measure = "va",
                     item = "it",
                     id = "id")

  expect_equivalent(round(test2$icc$icc,4), round(test2_aov$icc$icc,4))
  expect_equivalent(test1_aov$icc$icc, test2_aov$icc$icc)
  test3_chi = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     other_ci = TRUE,
                     type = "chi")

  test3_chi = reli_stats(data = df,
                         measure = "va",
                         item = "it",
                         id = "id",
                         other_ci = TRUE,
                         type = "chi",
                         cv_calc = "resid")

  test3_chi = reli_stats(data = df,
                         measure = "va",
                         item = "it",
                         id = "id",
                         other_ci = TRUE,
                         type = "chi",
                         cv_calc = "SEM")

  test3 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     other_ci = TRUE,
                     type = "perc",
                     replicates = 49)


  test3_aov = reli_aov(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     other_ci = TRUE,
                     type = "perc",
                     replicates = 49)
  testthat::expect_equivalent(round(test3$icc$icc,4),
                              round(test3_aov$icc$icc,4))
  testthat::expect_equivalent(round(test3$var_comp$percent,4),
                              round(test3_aov$var_comp$percent,4))
  test3_aov = reli_aov(data = df,
                       measure = "va",
                       item = "it",
                       id = "id",
                       other_ci = TRUE,
                       type = "chi")

  test3_aov = reli_aov(data = df,
                       measure = "va",
                       item = "it",
                       id = "id",
                       other_ci = TRUE,
                       type = "chi",
                       cv_calc = "resid")

  test3_aov = reli_aov(data = df,
                       measure = "va",
                       item = "it",
                       id = "id",
                       other_ci = TRUE,
                       type = "chi",
                       cv_calc = "SEM")



  pr_test = print(test3)
  p = plot(test3)
  pr_test = print(test2)
  p = plot(test2)

  testthat::expect_equal(test1$icc,
                         test2$icc)

  testthat::expect_equal(test1$anova,
                         test2$anova)

  testthat::expect_equal(test1$var_comp,
                         test2$var_comp)

  testthat::expect_equal(test1$cv,
                         test2$cv)

  testthat::expect_equal(test1$SEM,
                         test2$SEM)

  testthat::expect_equal(test1$SEE,
                         test2$SEE)

  testthat::expect_equal(test1$SEP,
                         test2$SEP)

  df2 =  data.frame(
    id = c(
      1,1,
      3,3,
      4,4,
      5,5,
      6,6,
      7,7,
      9,9,
      10,10,
      12,12,
      13,13,
      14,14
    ),
    time = c(
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1,
      0,1
    ),
    fam_trial =
      c(1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2,
        1,2),
    dyspnea = c(
      7,8,
      7,7,
      4,4,
      8,6,
      5,3,
      7,5,
      3,3,
      5,6,
      1,3,
      3,4,
      4,8
    ),
    headache = c(
      0,0,
      8,7.5,
      0.5,4,
      2.5,3.5,
      0,0,
      1.5,1,
      0,1,
      1.5,1,
      0.5,0,
      0,0,
      4.5,6.5
    )
  )

  df2$id = as.factor(df2$id)
  df2$fam_trial = as.factor(df2$fam_trial)

  coeftest2 = agree_coef(wide = FALSE,
                        data = df2,
                        measure = "dyspnea",
                        item = "fam_trial",
                        id = "id",
                        weighted = TRUE)

  coeftest3 = agree_coef(wide = FALSE,
                         data = df2,
                         measure = "headache",
                         item = "fam_trial",
                         id = "id",
                         weighted = TRUE)

  ### increase coverage calcs ----

  test3 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     cv_calc = "SEM",
                     other_ci = TRUE,
                     type = "perc",
                     replicates = 59)
  test3 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     cv_calc = "residuals",
                     other_ci = TRUE,
                     type = "perc",
                     replicates = 59)

  test3 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id",
                     se_type = "ICC2",
                     other_ci = TRUE,
                     type = "perc",
                     replicates = 59)



})

test_that("Weir dataset A",{
  df_a = data.frame(
    x = c(146, 148, 170, 90, 157, 156, 176, 205),
    y = c(140, 152, 152, 99, 145, 153, 167, 218)
  )

  test_a = reli_aov(data = df_a,
                    col.names = c("x", "y"),
                    wide = TRUE)

  #testlmer = reli_stats(data = df_a,
  #                    col.names = c("x", "y"),
  #                    wide = TRUE)

  expect_equal(round(test_a$SEM$estimate,1),
               7.6) # page 236 paragraph 1

  test_a = reli_aov(data = df_a,
                    col.names = c("x", "y"),
                    wide = TRUE,
                    se_type = "ICC3")

  expect_equal(round(test_a$SEP$estimate,1),
               10.2) # page 236 2nd column

  expect_equal(round(test_a$SEE$estimate,1),
               7.1) # page 236 2nd column

})
