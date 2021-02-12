#' Methods for simple_agree objects
#'
#' Methods defined for objects returned from the agree functions.
#'
#' @param x object of class \code{simple_agree} as returned from a fucntion starting with 'agree'
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
#' @name simple_agree-methods


### methods for simple_agree objects

#' @rdname simple_agree-methods
#' @method print simple_agree
#' @export

print.simple_agree <- function(x,...){
  if(x$class == "simple") {
  cat("Limit of Agreement = ", x$shieh_test$prop0*100, "%",  sep = "")
  cat("\n")
  cat("alpha =", (1-x$conf.level), "|", x$conf.level*100,"% Confidence Interval")
  cat("\n")
  cat("\n")
  cat("###- Shieh TOST Results -###")
  cat("\n")
  cat("Exact C.I.:"," [",round(x$shieh_test$lower.ci,4),", ",round(x$shieh_test$upper.ci, 4), "]", sep = "")
  cat("\n")
  cat("Hypothesis Test: ",x$shieh_test$h0_test, sep = "")
  cat("\n")
  cat("\n")
  cat("###- Bland-Altman Limits of Agreement (LoA) -###")
  cat("\n")
  cat("Mean Bias: ",x$loa$d," [",x$loa$d.lci,", ",x$loa$d.uci,"]", sep="")
  cat("\n")
  cat("Lower LoA: ",x$loa$d-x$loa$d.sd*qnorm(1-(1-x$agree.level)/2)," [",x$loa$lower.lci,", ",x$loa$lower.uci,"]", sep = "")
  cat("\n")
  cat("Upper LoA: ",x$loa$d+x$loa$d.sd*qnorm(1-(1-x$agree.level)/2)," [",x$loa$upper.lci,", ",x$loa$upper.uci,"]", sep = "")
  cat("\n")
  cat("\n")
  cat("###- Concordance Correlation Coefficient (CCC) -###")
  cat("\n")
  cat("CCC: ",round(x$ccc.xy$est.ccc,4),", ",100*x$conf.level,"% C.I. ","[",round(x$ccc.xy$lower.ci,4),", ",round(x$ccc.xy$upper.ci,4),"]",sep = "")
  cat("\n")
  } else if(x$class == "replicates"){
    cat("Limit of Agreement = ", x$agree.level*100, "%",  sep = "")
    cat("\n")
    cat("alpha =", (1-x$conf.level), "|", x$conf.level*100,"% Confidence Interval")
    cat("\n")
    cat("\n")
    cat("Hypothesis Test: ",x$h0_test, sep = "")
    cat("\n")
    cat("\n")
    cat("###- Bland-Altman Limits of Agreement (LoA) -###")
    cat("\n")
    cat("Mean Bias: ",x$loa$estimate[1]," [",x$loa$lower.ci[1],", ",x$loa$lower.ci[1],"]", sep = "")
    cat("\n")
    cat("Lower LoA: ",x$loa$estimate[2]," [",x$loa$lower.ci[2],", ",x$loa$lower.ci[2],"]", sep = "")
    cat("\n")
    cat("Upper LoA: ",x$loa$estimate[3]," [",x$loa$lower.ci[3],", ",x$loa$lower.ci[3],"]", sep = "")
    cat("\n")
  }

}

#' @rdname simple_agree-methods
#' @method plot simple_agree
#' @import ggplot2
#' @export

plot.simple_agree <- function(x, type = 1, ...){

  if(type == 1){
    return(x$bland_alt.plot)
  } else if (type == 2){
    return(x$identity.plot)
  } else{
   stop("please select type = 1 for a Bland Altman plot or type = 2 for an identity plot")
  }

}
