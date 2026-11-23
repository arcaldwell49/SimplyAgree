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
#'   \item{\code{find_n}}{A data frame with, for each combination of settings,
#'   the smallest sample size evaluated at which the desired power is achieved
#'   (`N`) and the power at that sample size. `N` is `NA`, with a warning, when
#'   the power is not reached at the largest sample size evaluated (`power` is
#'   then the highest power evaluated), or when the valid sample size immediately
#'   below the smallest evaluated size also reaches the target (`power` is then
#'   also `NA`). With gaps between the sample sizes, `N` can be larger
#'   than the exact minimum. Use [power_agreement_limit()] (with `sigma` in
#'   place of `SD`) to find the exact sample size directly.}
#' }
#'
#' @seealso [blandPowerCurve()], [power_agreement_limit()]
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
  grp = intersect(c("delta", "conf.level", "agree.level", "m", "icc"),
                  names(x))
  x = as.data.frame(x)
  # powerCurve objects from before the clustered designs lack m and icc
  if (!"m" %in% names(x)) x$m = 1
  if (!"icc" %in% names(x)) x$icc = 0

  # Smallest N at which the target power is reached. Power increases with N,
  # so when the first N evaluated already reaches the target, N - 1 is checked
  # to tell whether the minimum lies below the range.
  test = x %>%
    group_by(across(all_of(grp))) %>%
    arrange(N, .by_group = TRUE) %>%
    summarise(
      reached = any(power >= powtest),
      below = reached && N[1] > 2 && power[1] >= powtest &&
        .lu_power(n = N[1] - 1, delta = delta[1], mu = mu[1],
                  sigma = SD[1], conf.level = conf.level[1],
                  agree.level = agree.level[1], m = m[1],
                  icc = icc[1]) >= powtest,
      N = if (reached && !below) N[which(power >= powtest)[1]] else NA_real_,
      power = if (!reached) max(power)
              else if (below) NA_real_
              else power[which(power >= powtest)[1]],
      .groups = 'drop')

  settings = function(rows) {
    vals = as.data.frame(test[rows, grp, drop = FALSE])
    paste(apply(vals, 1, function(r) paste(grp, "=", trimws(r),
                                             collapse = ", ")),
          collapse = "; ")
  }
  if (any(!test$reached)) {
    warning("Desired power not reached within the sample sizes evaluated ",
            "(N is NA) for: ", settings(!test$reached), ". Increase the ",
            "largest sample size, or use power_agreement_limit() to find ",
            "N directly.", call. = FALSE)
  }
  if (any(test$below)) {
    warning("The smallest sample size reaching the desired power is below ",
            "the range evaluated (N is NA) for: ", settings(test$below),
            ". Start the sample sizes lower, or use power_agreement_limit() ",
            "to find N directly.", call. = FALSE)
  }

  test %>%
    select(-"reached", -"below") %>%
    relocate(power, .before = N)
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
