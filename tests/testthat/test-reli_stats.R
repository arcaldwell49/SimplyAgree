context("agree_test")


testthat::test_that("Simple Use Run Through", {

  sf <- matrix(c(9,    2,   5,    8,
                 6,    1,   3,    2,
                 8,    4,   6,    8,
                 7,    1,   2,    6,
                 10,   5,   6,    9,
                 6,   2,   4,    7),ncol=4,byrow=TRUE)
  colnames(sf) <- paste("J",1:4,sep="")
  rownames(sf) <- paste("S",1:6,sep="")
  sf  #example from Shrout and Fleiss (1979)

  test = reli_stats(data = sf,
             wide = TRUE,
             col.names=c("J1","J2","J3","J4"))

})
