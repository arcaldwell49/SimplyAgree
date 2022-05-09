#' Methods for loa_mermod objects
#'
#' Methods defined for objects returned from the loa_mixed functions.
#'
#' @param x object of class \code{loa_mermod}.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{loa_mixed}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement}
#' }
#'
#' @name loa_mermod-methods


### methods for loa_mermod (created by loa_mixed)

#' @rdname loa_mermod-methods
#' @method print loa_mermod
#' @export

print.loa_mermod <- function(x,...){
  agree = paste0(x$call$agree.level*100)
  conf = paste0(x$call$conf.level*100)
  title = paste0(agree,"% Limits of Agreement with Boostrapped ", conf, "% Confidence Intervals \n")
  cat(title)
  df = x$loa
  if("avg" %in% colnames(df) && "condition" %in% colnames(df) ){
    df = df %>%
      select(avg, condition, term, estimate, lower.ci, upper.ci)%>%
      rename(Average = avg,
             Condition = condition,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  } else if("avg" %in% colnames(df)){
    df = df %>%
      select(avg, term, estimate, lower.ci, upper.ci)%>%
      rename(Average = avg,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  }else if("condition" %in% colnames(df)){
    df = df %>%
      select(condition, term, estimate, lower.ci, upper.ci)%>%
      rename(Condition = condition,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  } else {
    df = df %>%
      select(term, estimate, lower.ci, upper.ci)%>%
      rename(Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  }
  #colnames(df) = c("Measures", "Estimate", "Lower CI", "Upper CI")
  print(df, digits = 4)
}

#' @rdname loa_mermod-methods
#' @method plot loa_mermod
#' @import ggplot2
#' @export

plot.loa_mermod <- function(x,
                            x_label = "Average of Both Methods",
                            y_label = "Difference Between Methods",
                            geom = "geom_point",
                            smooth_method = NULL,
                            smooth_se = TRUE,
                            ...){
  if(x$call$prop_bias != TRUE){
    simple_mix_plot(x,
                   x_label,
                   y_label,
                   geom,
                   smooth_method,
                   smooth_se)
  } else {
    bias_mix_plot(x,
                 x_label,
                 y_label,
                 geom,
                 smooth_se)
  }



}
