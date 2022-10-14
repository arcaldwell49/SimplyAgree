#' Methods for powerCurve objects
#'
#' Methods defined for objects returned from the powerCurve function.
#'
#' @param x object of class \code{powerCurve}
#' @param power Level of power (value between 0 and 1) for find_n to find the sample size.
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
#' @importFrom magrittr "%>%"
#' @export

find_n <- function(x, power = 0.8){
  if(!inherits(x,"powerCurve")) warning("input is not a powerCurve object")
  powtest = power

  test = x %>%
    as.data.frame() %>%
    group_by(delta,conf.level,agree.level) %>%
    summarise(power = nth(power, which.min(abs(power-powtest))),
              .groups = 'drop')
  test$N = NA


  for(i in 1:nrow(test)){
    val = unlist(x[which(
      x$delta == test$delta[i] &
        x$agree.level == test$agree.level[i] &
        x$conf.level == test$conf.level[i]  & x$power == test$power[i]
    ), ]$N)

    test$N[i] = val
  }

  return(test)
}


#' @rdname powerCurve-methods
#' @method plot powerCurve
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate select
#' @export

plot.powerCurve <- function(x, ...){
    plotdf = x
    plot_out = ggplot(plotdf) +
      aes(x = N,
          y = power,
          color = as.factor(delta)) +
      geom_line() +
      xlab("Sample Size (N pairs)") +
      ylab(expression("Power (1-"~beta~")")) +
      theme_bw() +
      facet_grid(agree.level~conf.level,
                 labeller = label_both) +
      labs(color = "Delta") +
      scale_color_viridis_d() +
      theme(
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        #panel.grid.major = element_blank(), #remove major gridlines
        #panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent') #transparent legend panel
      )
    return(plot_out)

}
