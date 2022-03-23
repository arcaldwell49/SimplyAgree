#' Methods for simple_deming objects
#'
#' Methods defined for objects returned from the agree functions.
#'
#' @param x object of class \code{simple_deming} as returned from a function starting with 'agree'
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{agree_test}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement (type = 1) or concordance plot (type = 2)}
#'   \item{\code{check}}{Returns 2 plots, p_norm and p_het, testing the assumptions of a Bland-Altman analysis. P-values for the normality and heteroskedascity tests are provided as captions to the plot.}
#' }
#'
#' @name simple_deming-methods


### methods for simple_deming objects

#' @rdname simple_deming-methods
#' @method print simple_deming
#' @export

print.simple_deming <- function(x,...){

  print(x$model, digits = 4)
}

#' @rdname simple_deming-methods
#' @method plot simple_deming
#' @import ggplot2
#' @export

plot.simple_deming <- function(x,
                               ...){

  print("test")

}


