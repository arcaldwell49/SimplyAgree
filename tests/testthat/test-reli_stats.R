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

  colnames(sf) <- paste("J", 1:4, sep = "")
  rownames(sf) <- paste("S", 1:6, sep = "")
  #sf  #example from Shrout and Fleiss (1979)
  dat = as.data.frame(sf)

  test1 = reli_stats(data = dat,
                     wide = TRUE,
                     col.names = c("J1", "J2", "J3", "J4"))
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

  test2 = reli_stats(data = df,
                     measure = "va",
                     item = "it",
                     id = "id")
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


})
