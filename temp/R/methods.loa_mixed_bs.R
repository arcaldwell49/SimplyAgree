#' Methods for loa_mixed_bs objects
#'
#' Methods defined for objects returned from the loa_mixed functions.
#'
#' @param x object of class \code{loa_mixed_bs} as returned from \code{loa_mixed}
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{loa_mixed}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement}
#' }
#'
#' @name loa_mixed_bs-methods


### methods for loa_mixed_bs (created by loa_mixed)

#' @rdname loa_mixed_bs-methods
#' @method print loa_mixed_bs
#' @export

print.loa_mixed_bs <- function(x,...){
  agree = paste0(x$agree.level*100)
  conf = paste0(x$conf.level*100)
  title = paste0(agree,"% Limits of Agreement with Bootstrap ",conf, "% Confidence Intervals \n")
  cat(title)
  print(x$loa)
}

#' @rdname loa_mixed_bs-methods
#' @method plot loa_mixed_bs
#' @import ggplot2
#' @export

plot.loa_mixed_bs <- function(x,...){
  if(is.null(x$bland_alt.plot)){
    stop("No plot provided from this object. Be sure to set the plot.xaxis argument")
  }

  return(x$bland_alt.plot)

}
