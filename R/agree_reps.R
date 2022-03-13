#' Tests for Absolute Agreement with Replicates
#' @description agree_nest produces an absolute agreement analysis for data where there is multiple observations per subject but the mean does not vary within subjects as described by Zou (2013). Output mirrors that of agree_test but CCC is calculated via U-statistics.
#' @param x Name of column with first measurement
#' @param y Name of other column with first measurement
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param x_lab Label for x values (first measurement)
#' @param y_lab Label for y values (second measurement)
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals using U-statistics.}
#'   \item{\code{"conf.level"}}{Returned as input.}
#'   \item{\code{"agree.level"}}{Returned as input.}
#'
#' }

#' @examples
#' data('reps')
#' agree_reps(x = "x", y = "y", id = "id", data = reps, delta = 2)
#' @section References:
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#'
#' King, TS and Chinchilli, VM. (2001). A generalized concordance correlation coefficient for continuous and categorical data. Statistics in Medicine, 20, 2131:2147.
#'
#' King, TS; Chinchilli, VM; Carrasco, JL. (2007). A repeated measures concordance correlation coefficient. Statistics in Medicine, 26, 3095:3113.
#'
#' Carrasco, JL; Phillips, BR; Puig-Martinez, J; King, TS; Chinchilli, VM. (2013). Estimation of the concordance correlation coefficient for repeated measures using SAS and R. Computer Methods and Programs in Biomedicine, 109, 293-304.
#' @importFrom stats pnorm qnorm lm anova dchisq qchisq sd var
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @import dplyr
#' @import ggplot2
#' @export

agree_reps <- function(x,
                       y,
                       id,
                       data,
                       delta,
                       agree.level = .95,
                       conf.level = .95,
                       x_lab = "x",
                       y_lab = "y"){

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
    select(id,x,y)

  df_long = df %>%
    pivot_longer(!id,
                 names_to = "method",
                 values_to = "measure") %>%
    drop_na()

  ccc_reps = cccUst(dataset = df_long,
                    ry = "measure",
                    rmet = "method",
                    cl = conf.level)

  ccc.xy = data.frame(est.ccc = ccc_reps[1],
                      lower.ci = ccc_reps[2],
                      upper.ci = ccc_reps[3],
                      SE = ccc_reps[4])

  df2 = df %>%
    group_by(id) %>%
    summarize(mxi = sum(!is.na(x)),
              myi = sum(!is.na(y)),
              x_bar = mean(x, na.rm=TRUE),
              x_var = var(x, na.rm=TRUE),
              y_bar = mean(y, na.rm=TRUE),
              y_var = var(y, na.rm=TRUE),
              .groups = "drop") %>%
    mutate(d = x_bar-y_bar,
           both_avg = (x_bar+y_bar)/2)

  df3 = df2 %>%
    drop_na()

  Nx = sum(df2$mxi)
  Ny = sum(df2$myi)
  mxh = nrow(df2)/sum(1/df2$mxi)
  myh = nrow(df2)/sum(1/df2$myi)


  sxw2 = sum((df3$mxi-1)/(Nx-nrow(df3))*df3$x_var)
  syw2 = sum((df3$myi-1)/(Ny-nrow(df3))*df3$y_var)
  d_bar = mean(df2$d, na.rm = TRUE)
  d_var = var(df2$d, na.rm = TRUE)
  d_lo = d_bar - confq*sqrt(d_var)/sqrt(nrow(df2))
  d_hi = d_bar + confq*sqrt(d_var)/sqrt(nrow(df2))

  tot_var = d_var + (1-1/mxh)*sxw2 + (1-1/myh)*syw2

  loa_l = d_bar - agreeq*sqrt(tot_var)

  loa_u = d_bar + agreeq*sqrt(tot_var)

  move.l.1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha.l,nrow(df2)-1))))^2
  move.l.2 = ((1-1/mxh)*sxw2*(1-(Nx-nrow(df2))/(qchisq(alpha.l,Nx-nrow(df2)))))^2
  move.l.3 = ((1-1/myh)*syw2*(1-(Ny-nrow(df2))/(qchisq(alpha.l,Ny-nrow(df2)))))^2
  move.l = tot_var - sqrt(move.l.1+move.l.2+move.l.3)

  move.u.1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha.u,nrow(df2)-1))))^2
  move.u.2 = ((1-1/mxh)*sxw2*(1-(Nx-nrow(df2))/(qchisq(alpha.u,Nx-nrow(df2)))))^2
  move.u.3 = ((1-1/myh)*syw2*(1-(Ny-nrow(df2))/(qchisq(alpha.u,Ny-nrow(df2)))))^2
  move.u = tot_var + sqrt(move.u.1+move.u.2+move.u.3)

  LME = sqrt(confq^2*(d_var/nrow(df2))+agreeq^2*(sqrt(move.u)-sqrt(tot_var))^2)
  RME = sqrt(confq^2*(d_var/nrow(df2))+agreeq^2*(sqrt(tot_var)-sqrt(move.l))^2)

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
    }
  } else {
    rej_text = "No Hypothesis Test"
  }

  ### Plots ----

  z <- lm(y_bar ~ x_bar, df2)
  the_int <- summary(z)$coefficients[1,1]
  the_slope <-  summary(z)$coefficients[2,1]
  tmp.lm <- data.frame(the_int, the_slope)
  scalemin = min(c(min(df2$x_bar),min(df2$y_bar)))
  scalemax = max(c(max(df2$x_bar),max(df2$y_bar)))

  df_loa2 = df_loa
  df_loa2$x = scalemin
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))
  pd2 = position_dodge2(.03*(scalemax-scalemin))

  identity.plot = ggplot(df2,
                         aes(x = x_bar,
                             y = y_bar)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab("Method: Average of x") +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab("Method: Average of y") +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  bland_alt.plot =  ggplot(df2,
                           aes(x = both_avg, y = d)) +
  geom_point(na.rm = TRUE) +
    geom_pointrange(data = df_loa2,
                    aes(
                      x = x,
                      y = estimate,
                      ymin = lower.ci,
                      ymax = upper.ci,
                      color = text),
                    #width = .03*(scalemax-scalemin),
                    position = pd2,
                    inherit.aes = FALSE)+
    labs(x = paste0("Average of ", x_lab ," & ", y_lab),
         y = paste0("Difference between Methods ",x_lab ," & ", y_lab),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")
  if (!missing(delta)){
    df_delta = data.frame(y1 = c(delta, -1*delta))
    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = c(delta, -1*delta),
        name = "Maximal Allowable Difference"))
  }


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
                 class = "replicates"),
            class = "simple_agree")


}
