#' Tests for Absolute Agreement with Nested Data
#' @param x Name of column with first measurement
#' @param y Name of other column with first measurement
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals using U-statistics. Warning: if underlying value varies this estimate will be inaccurate.}
#'   \item{\code{"conf.level"}}{Returned as input.}
#'   \item{\code{"agree.level"}}{Returned as input.}
#'
#' }

#' @examples
#' data('reps')
#' agree_nest(x = "x", y = "y", id = "id", data = reps, delta = 2)
#' @section References:
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#'
#' King, TS and Chinchilli, VM. (2001). A generalized concordance correlation coefficient for continuous and categorical data. Statistics in Medicine, 20, 2131:2147.
#'
#' King, TS; Chinchilli, VM; Carrasco, JL. (2007). A repeated measures concordance correlation coefficient. Statistics in Medicine, 26, 3095:3113.
#'
#' Carrasco, JL; Phillips, BR; Puig-Martinez, J; King, TS; Chinchilli, VM. (2013). Estimation of the concordance correlation coefficient for repeated measures using SAS and R. Computer Methods and Programs in Biomedicine, 109, 293-304.
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom cccrm cccUst
#' @import dplyr
#' @import ggplot2
#' @export

agree_nest <- function(x,
                       y,
                       id,
                       data,
                       delta,
                       agree.level = .95,
                       conf.level = .95){

  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  alpha.l = 1 - (1 - conf.level) / 2
  alpha.u = (1 - conf.level) / 2

  df = data %>%
    select(all_of(id),all_of(x),all_of(y)) %>%
    rename(id = all_of(id),
           x = all_of(x),
           y = all_of(y)) %>%
    select(id,x,y) %>%
    drop_na()

  df_long = df %>%
    pivot_longer(!id,
                 names_to = "method",
                 values_to = "measure")

  ccc_nest = cccUst(dataset = df_long,
                    ry = "measure",
                    rmet = "method",
                    cl = conf.level)

  ccc.xy = data.frame(est.ccc = ccc_nest[1],
                      lower.ci = ccc_nest[2],
                      upper.ci = ccc_nest[3],
                      SE = ccc_nest[4])

  df2 = df %>%
    group_by(id) %>%
    summarize(m = n(),
              x_bar = mean(x, na.rm=TRUE),
              x_var = var(x, na.rm=TRUE),
              y_bar = mean(y, na.rm=TRUE),
              y_var = var(y, na.rm=TRUE),
              d = mean(x-y),
              d_var = var(x-y),
              .groups = "drop") %>%
    mutate(both_avg = (x_bar+y_bar)/2)

  d_bar = mean(df2$d)
  d_var = var(df2$d)
  sdw2 = sum((df2$m-1)/(nrow(df)-nrow(df2))*df2$d_var)
  mh = nrow(df2)/sum(1/df2$m)
  d_lo = d_bar - confq*sqrt(d_var)/sqrt(nrow(df2))
  d_hi = d_bar + confq*sqrt(d_var)/sqrt(nrow(df2))

  var_tot = d_var + (1-1/mh) * sdw2
  loa_l = d_bar - agreeq*sqrt(var_tot)
  loa_u = d_bar + agreeq*sqrt(var_tot)

  move.l.1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha.l,nrow(df2)-1))))^2
  move.l.2 = ((1-1/mh)*sdw2*(1-(nrow(df)-nrow(df2))/(qchisq(alpha.l,nrow(df)-nrow(df2)))))^2

  move.l = var_tot - sqrt(move.l.1+move.l.2)

  move.u.1 = (d_var*((nrow(df2)-1)/(qchisq(alpha.u,nrow(df2)-1))-1))^2
  move.u.2 = ((1-1/mh)*sdw2*((nrow(df)-nrow(df2))/(qchisq(alpha.u,nrow(df)-nrow(df2)))-1))^2

  move.u = var_tot + sqrt(move.u.1+move.u.2)

  LME = sqrt(confq^2*(d_var/nrow(df2))+agreeq^2*(sqrt(move.u)-sqrt(var_tot))^2)
  RME = sqrt(confq^2*(d_var/nrow(df2))+agreeq^2*(sqrt(var_tot)-sqrt(move.l))^2)

  loa_l.l = loa_l - LME
  loa_l.u = loa_l + RME

  loa_u.l = loa_u - RME
  loa_u.u = loa_u + LME

  df_loa = data.frame(
    estimate = c(d_bar, loa_l, loa_u),
    lower.ci = c(d_lo, loa_l.l, loa_u.l),
    upper.ci = c(d_hi, loa_l.u, loa_u.u),
    row.names = c("Difference","Lower LoA","Upper LoA")
  )
  if (!missing(delta)) {
  rej <- (-delta < loa_l.l) * (loa_u.l < delta)
  rej_text = "don't reject h0"
  if (rej == 1) {
    rej_text = "reject h0"
  }} else {
    rej_text = "No Hypothesis Test"
  }

  ### Plots ----

  z <- lm(y_bar ~ x_bar, df2)
  the_int <- summary(z)$coefficients[1,1]
  the_slope <-  summary(z)$coefficients[2,1]
  tmp.lm <- data.frame(the_int, the_slope)
  scalemin = min(c(min(df$x),min(df$y)))
  scalemax = max(c(max(df$x),max(df$y)))
  df = df %>%
    mutate(id = as.factor(id))
  identity.plot = ggplot(df,
                         aes(x = x, y = y,color=id)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab("Method: x") +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab("Method: y") +
    coord_fixed(ratio = 1 / 1) +
    theme_bw() +
    scale_color_viridis_d()
  df = df %>%
    mutate(d = x-y,
           avg_both = (x+y)/2)
  bland_alt.plot =  ggplot(df,
                           aes(x = avg_both, y = d)) +
    geom_point(na.rm = TRUE) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = df_loa$lower.ci[2],
             ymax = df_loa$upper.ci[2],
             alpha = .5,
             fill = "#D55E00") +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = df_loa$lower.ci[3],
             ymax = df_loa$upper.ci[3],
             alpha = .5,
             fill = "#D55E00") +
    geom_hline(aes(yintercept = d_bar),
               linetype = 1) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = df_loa$lower.ci[1],
             ymax = df_loa$upper.ci[1],
             alpha = .5,
             fill = "gray") +
    xlab("Average of Method x and Method y") +
    ylab("Average Difference between Methods") +
    theme_bw() +
    theme(legend.position = "none")
  #######################
  # Return Results ----
  #######################

  structure(list(loa = df_loa,
                 h0_test = rej_text,
                 bland_alt.plot = bland_alt.plot,
                 identity.plot = identity.plot,
                 conf.level = conf.level,
                 agree.level = agree.level,
                 ccc.xy = ccc.xy,
                 class = "nested"),
            class = "simple_agree")

}
