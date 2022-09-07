## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimplyAgree)

## -----------------------------------------------------------------------------

  # Example from Shrout and Fleiss (1979), pg. 423
  dat = data.frame(judge1 = 
                     c(9,6,8,7,10,6),
                   judge2 = 
                     c(2,1,4,1,5,2),
                   judge3 = 
                     c(5,3,6,2,6,4),
                   judge4 = 
                     c(8,2,8,6,9,7))

## -----------------------------------------------------------------------------
test1 = reli_stats(
  data = dat,
  wide = TRUE,
  col.names = c("judge1", "judge2", "judge3", "judge4")
)

## -----------------------------------------------------------------------------
print(test1)

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(test1)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
agree_coef(data = ratermat2,
                     wide = TRUE,
                     weighted = FALSE,
                     col.names = c("Rater1", "Rater2", "Rater3", "Rater4"))

## -----------------------------------------------------------------------------
agree_coef(data = ratermat2,
                    wide = TRUE,
                    weighted = TRUE,
                    col.names = c("Rater1", "Rater2", "Rater3", "Rater4"))

