# Moved unchanged from test-reli_stats.R (phase 6 of the reliability redesign).

testthat::test_that("agree_coef matches published values", {

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
})

testthat::test_that("agree_coef runs on long data", {
  df = data.frame(id = rep(1:6, each = 4),
                  it = rep(1:4, times = 6),
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
  expect_true(is.data.frame(coeftest))

  df2 =  data.frame(
    id = factor(rep(c(1, 3, 4, 5, 6, 7, 9, 10, 12, 13, 14), each = 2)),
    fam_trial = factor(rep(1:2, times = 11)),
    dyspnea = c(7,8, 7,7, 4,4, 8,6, 5,3, 7,5, 3,3, 5,6, 1,3, 3,4, 4,8),
    headache = c(0,0, 8,7.5, 0.5,4, 2.5,3.5, 0,0, 1.5,1, 0,1, 1.5,1,
                 0.5,0, 0,0, 4.5,6.5)
  )

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
  expect_true(is.data.frame(coeftest2))
  expect_true(is.data.frame(coeftest3))
})

testthat::test_that("agree_coef bug fixes", {
  r <- data.frame(R1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
                  R2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
                  R3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
                  R4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
  cn <- c("R1", "R2", "R3", "R4")
  res <- agree_coef(data = r, col.names = cn)
  expect_equal(rownames(res), c("Percent Agreement", "Gwet's AC1",
                                "Fleiss' Kappa", "Krippendorff's Alpha"))
  # same engine as cac_test()
  expect_equal(res["Fleiss' Kappa", "se"],
               cac_test(col.names = cn, data = r, coef = "fleiss")$stderr)
  # weights and categories are passed through (irrCAC values)
  expect_equal(agree_coef(data = r, col.names = cn, weights = "linear")$est[2],
               0.85874, tolerance = 1e-4)
  expect_equal(agree_coef(data = r, col.names = cn, weighted = TRUE,
                          categories = 1:6)$est[2],
               0.94741, tolerance = 1e-4)
  # a single category no longer crashes
  same <- data.frame(a = rep(2, 4), b = rep(2, 4))
  expect_warning(one <- agree_coef(data = same, col.names = c("a", "b")),
                 "one category")
  expect_equal(one$est[1], 1)
  expect_true(all(is.na(one$est[-1])))
  # replicate measurements are no longer silently dropped
  dup <- data.frame(id = c(1, 1, 1, 2, 2), r = c("a", "a", "b", "a", "b"),
                    y = c(1, 2, 1, 2, 2))
  expect_error(agree_coef(wide = FALSE, data = dup, measure = "y", item = "r",
                          id = "id"), "at most one measurement")
})
