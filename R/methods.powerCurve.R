#' Methods for powerCurve objects
#'
#' Methods defined for objects returned from the powerCurve function.
#'
#' @param x object of class \code{powerCurve}
#' @param power Level of power (value between 0 and 1) for find_n to find the sample size.
#' @param tolerance Degree of tolerance for find_n to find the nearest observed power to the desired power.
#' @param type Type of plot to output. Default (1) is power curve plot while type=2 will produce a confidence interval width plot.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{blandPowerCurve}}.
#' @return
#' \describe{
#'   \item{\code{plot}}{Returns a plot of the limits of agreement (type = 1) or concordance plot (type = 2)}
#'   \item{\code{find_n}}{Find sample size at which desired power is achieved}
#' }
#'
#' @name powerCurve-methods


### methods for powerCurve objects

#' @rdname powerCurve-methods
#' @importFrom stringr str_split
#' @export

find_n <- function(x, power = 0.8, tolerance = 0.01){
  if(!"powerCurve" %in% class(x)) warning("input is not a powerCurve object")
  if (min(abs(x$power.power - power)) > tolerance)
    warning("Achieved power not found -- try more levels in samplesize or a wider samplesize range")
  n <- x$CI.n[which.min(abs(x$power.power - power))]
  ceiling(n)
}


#' @rdname powerCurve-methods
#' @method plot powerCurve
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @export

plot.powerCurve <- function(x, type = 1, ...){

  if(type ==1 ){
    plotdf = x
    plot_out = ggplot(plotdf) +
      aes(x = CI.n, y = power.power) +
      geom_line() +
      xlab("Sample Size (N pairs)") +
      ylab(expression("Power (1-"~beta~")")) +
      theme_bw()
    return(plot_out)
  }
  if(type == 2){
    features <- c("LOA.mu",
                  "LOA.upperLOA",
                  "LOA.lowerLOA",
                  "CI.lowerLOA_upperCI",
                  "CI.lowerLOA_lowerCI",
                  "CI.upperLOA_lowerCI",
                  "CI.upperLOA_upperCI")

    class(x) <- "data.frame"
    plotdf <- x %>%
      select(c("CI.n", "beta.delta", features)) %>%
      pivot_longer(cols = features)

    plotdf <- plotdf %>%
      mutate(feature = sapply(stringr::str_split(plotdf$name, "[.]"), function(x) x[1]))

    plot_out = plotdf %>%
      ggplot() +
      aes(x = CI.n,
          y = value,
          color = feature,
          group = name) +
      geom_line() +
      #geom_hline(yintercept = c(-beta.delta,beta.delta), lty = 2) +
      xlab("Sample Size (N pairs)") +
      ylab("Difference between Methods") +
      labs(color = "") +
      ggtitle("Confidence Intervals") +
      theme_bw()
    return(plot_out)
  }



}
