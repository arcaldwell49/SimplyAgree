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
  print(x$icc)
}

#' @rdname simple_reli-methods
#' @method plot simple_reli
#' @import ggplot2
#' @export

plot.simple_reli <- function(x,  ...){

  return(x$plot.reliability)

}
