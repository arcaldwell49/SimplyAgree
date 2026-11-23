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
#'   \item{\code{find_n}}{Find the smallest sample size at which the desired
#'   power is achieved, for each combination of settings. If the power is not
#'   reached, or a smaller sample size already reaches it, `N` is `NA` (with a
#'   warning) and the range of sample sizes must be expanded.}
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
  .data = NULL
  powtest = power
  grp = intersect(c("delta", "conf.level", "agree.level", "m", "icc"),
                  names(x))
  has_m = "m" %in% names(x)
  has_icc = "icc" %in% names(x)

  # smallest N at which the target power is reached
  test = x %>%
    as.data.frame() %>%
    group_by(across(all_of(grp))) %>%
    arrange(N, .by_group = TRUE) %>%
    mutate(.below_range = if (N[1] > 2 && power[1] >= powtest) {
      .lu_power(n = N[1] - 1, delta = delta[1], mu = .data$mu[1], sigma = SD[1],
                conf.level = conf.level[1], agree.level = agree.level[1],
                m = if (has_m) m[1] else 1,
                icc = if (has_icc) .data$icc[1] else 0) >= powtest
    } else FALSE) %>%
    summarise(N = if (any(power >= powtest) && !.data$.below_range[1])
                    N[which(power >= powtest)[1]]
                  else NA_real_,
              power = if (any(power >= powtest)) power[which(power >= powtest)[1]]
                      else max(power),
              .groups = 'drop') %>%
    relocate(power, .before = N)

  if (any(is.na(test$N))) {
    warning("Desired sample size is outside the evaluated range for some settings (N is NA); ",
            "the range of sample sizes must be expanded.")
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
