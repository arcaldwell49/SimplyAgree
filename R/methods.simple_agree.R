#' Methods for simple_agree objects
#'
#' Methods defined for objects returned from the loa_mixed functions.
#'
#' @param x object of class \code{simple_agree} as returned from \code{loa_mixed}
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{loa_mixed}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement (type = 1) or concordance plot (type = 2)}
#' }
#'
#' @name simple_agree-methods


### methods for simple_agree (created by loa_mixed)

#' @rdname simple_agree-methods
#' @method print simple_agree
#' @export

print.simple_agree <- function(x,...){

  cat("Limit of Agreement = ", prop0*100, "%",  sep = "")
  cat("\n")
  cat("alpha =", alpha, "|", (1 - alpha)*100,"% Confidence Interval")
  cat("\n")
  cat("### Shieh TOST Results ###")
  cat("\n")
  cat("Exact C.I.:"," [",round(el,4),", ",round(eu, 4), "]", sep = "")
  cat("\n")
  cat("test: ",rej_text, sep = "")
  cat("\n")
  cat("### Bland-Altman Limits of Agreement (LoA) ###")
  cat("\n")
  cat("Mean Bias:",ccc_res$delta$d,"[",ccc_res$delta$d.lci,", ",ccc_res$delta$d.uci,"]")
  cat("\n")
  cat("Lower LoA:",ccc_res$delta$l.loa,"[",ccc_res$delta$lower.lci,", ",ccc_res$delta$lower.uci,"]")
  cat("\n")
  cat("Upper LoA:",ccc_res$delta$u.loa,"[",ccc_res$delta$upper.lci,", ",ccc_res$delta$upper.uci,"]")
  cat("\n")
  cat("### Concordance Correlation Coefficient (CCC) ###")
  cat("\n")
  cat("CCC: ",round(ccc_res$rho.c$est.ccc,4),", ",100*conf.level,"% C.I. ","[",round(ccc_res$rho.c$lower.ci,4),", ",round(ccc_res$rho.c$upper.ci,4),"]",sep = "")
  cat("\n")

}

#' @rdname simple_agree-methods
#' @method plot simple_agree
#' @import ggplot2
#' @export

plot.simple_agree <- function(x,type=1){
  if(type == 1){
    return(x$bland_alt.plot)
  } else if (type == 2){
    return(x$identity.plot)
  } else{
   stop("please select type = 1 for a Bland Altman plot or type = 2 for an identity plot")
  }

}
