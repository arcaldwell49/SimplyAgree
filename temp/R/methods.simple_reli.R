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
  if(x$call$other_ci == TRUE){
    cat(paste0("Coefficient of Variation (%): ",
        signif(x$cv$estimate*100,3), " ",
        100*x$call$conf.level, "% C.I. [",
        signif(x$cv$lower.ci*100,3), ", ",
        signif(x$cv$upper.ci*100,3),
        "]"))
    cat("\n")
    cat(paste0("Standard Error of Measurement (SEM): ",signif(x$SEM$estimate,3), " ",
        100*x$call$conf.level, "% C.I. [",
        signif(x$SEM$lower.ci,3), ", ",
        signif(x$SEM$upper.ci,3),
        "]"))
    cat("\n")
    cat(paste0("Standard Error of the Estimate (SEE): ",signif(x$SEE$estimate,3), " ",
        100*x$call$conf.level, "% C.I. [",
        signif(x$SEE$lower.ci,3), ", ",
        signif(x$SEE$upper.ci,3),
        "]"))
    cat("\n")
    cat(paste0("Standard Error of Prediction (SEP): ",signif(x$SEP$estimate,3), " ",
        100*x$call$conf.level, "% C.I. [",
        signif(x$SEP$lower.ci,3), ", ",
        signif(x$SEP$upper.ci,3),
        "]"))
    cat("\n")
    cat("\n")
    cat("Intraclass Correlation Coefficients")
  } else {
  cat("Coefficient of Variation (%): ",signif(x$cv$estimate*100,3))
  cat("\n")
  cat("Standard Error of Measurement (SEM): ",signif(x$SEM$estimate,3))
  cat("\n")
  cat("Standard Error of the Estimate (SEE): ",signif(x$SEE$estimate,3))
  cat("\n")
  cat("Standard Error of Prediction (SEP): ",signif(x$SEP$estimate,3))
  cat("\n")
  cat("\n")
  cat("Intraclass Correlation Coefficients with ",
      100*x$call$conf.level, "% C.I.")
  }
  cat("\n")
  df_icc = x$icc
  colnames(df_icc) = c("Model", "Measures", "Type", "ICC", "Lower CI", "Upper CI")
  print(df_icc,digits=4)
  cat("\n")
}

#' @rdname simple_reli-methods
#' @method plot simple_reli
#' @import ggplot2
#' @export

plot.simple_reli <- function(x,  ...){

  df = model.frame(x$call$lm_mod)
  colnames(df) = c("values", "id", "items")
  plot.reliability = ggplot(df,
                            aes(
                              x = items,
                              y = values,
                              color = id,
                              group = id
                            )) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_line(color = "black",
              alpha = .2,
              position = position_dodge(width = 0.2)) +
    labs(y = "Measurement",
         x = "Item",
         color = "id") +
    scale_color_viridis_d()+
    theme_bw()
  return(plot.reliability)

}


#' @rdname simple_reli-methods
#' @method check simple_reli
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid fitted
#' @export

check.simple_reli <- function(x) {

  check_reli(x)
}
