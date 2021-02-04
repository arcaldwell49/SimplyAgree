#' Methods for pwr_agree objects
#'
#' Methods defined for objects returned from the loa_mixed functions.
#'
#' @param x object of class \code{pwr_agree} as returned from \code{loa_mixed}
#' @param type Type of plot to output. Default (1) is Bland-Altman plot while type=2 will produce a line-of-identity plot.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{agree_test}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement (type = 1) or concordance plot (type = 2)}
#' }
#'
#' @name pwr_agree-methods


### methods for pwr_agree
#' @rdname pwr_agree-methods
#' @method print pwr_agree
#' @export

print.pwr_agree <- function(x,...){
  cat("Power for Shieh Hypothesis Test for Absolute Agreement")
  cat("\n")
  cat("Bounds for Agreement:", x$delta)
  cat("\n")
  cat("Agreement Level: ", round(x$agree.level*100,2), "% | Confidence Level: ",round(x$conf.level*100,2), "%",sep="")
  cat("\n")
  print(c('gam, power, n'))
  print(c(x$gam, x$power, x$n), digits = 4)

}


