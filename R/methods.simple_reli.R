#' Methods for simple_reli objects
#'
#' Methods defined for objects returned from the agree functions.
#'
#' @param x object of class \code{simple_reli} as returned from the reli_stats function
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{reli_stats}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the data points used in the reliability analysis}
#' }
#'
#' @name simple_reli-methods


### methods for simple_reli objects

#' @rdname simple_reli-methods
#' @method print simple_reli
#' @export

print.simple_reli <- function(x,...){
  cat("\n")
  cat("Coefficient of Variation (%): ",round(x$cv*100,2))
  cat("\n")
  cat("Standard Error of Measurement (SEM): ",round(x$SEM,4))
  cat("\n")
  cat("Standard Error of the Estimate (SEE): ",round(x$SEE,4))
  cat("\n")
  cat("Standard Error of Prediction (SEP): ",round(x$SEP,4))
  cat("\n")
  cat("\n")
  cat("Intraclass Correlation Coefficients")
  cat("\n")
  print(x$icc,digits=4)
  cat("\n")
}

#' @rdname simple_reli-methods
#' @method plot simple_reli
#' @import ggplot2
#' @export

plot.simple_reli <- function(x,  ...){

  return(x$plot.reliability)

}


#' @rdname simple_reli-methods
#' @method check simple_reli
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid
#' @export

check.simple_reli <- function(x) {
  check_reli(x)
}
