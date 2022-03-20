#' Tests for Absolute Agreement with Nested Data
#' @description agree_nest produces an absolute agreement analysis for data where there is multiple observations per subject but the mean varies within subjects as described by Zou (2013). Output mirrors that of agree_test but CCC is calculated via U-statistics.
#' @param x Name of column with first measurement
#' @param y Name of other column with first measurement
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param TOST Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests for the limits of agreement. Default is TRUE.
#'
#' @return Returns single simple_agree class object with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals using U-statistics. Warning: if underlying value varies this estimate will be inaccurate.}
#'   \item{\code{"call"}}{the matched call}
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
#' @import dplyr
#' @import ggplot2
#' @export

agree_nest <- function(x,
                       y,
                       id,
                       data,
                       delta,
                       agree.level = .95,
                       conf.level = .95,
                       TOST = TRUE,
                       prop_bias = FALSE){

  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  if(TOST == TRUE){
    confq2 = qnorm(1 - (1 - conf.level) )
    alpha.l = 1 - (1 - conf.level)
    alpha.u = (1 - conf.level)
  } else {
    confq2 = qnorm(1 - (1 - conf.level) / 2)
    alpha.l = 1 - (1 - conf.level) / 2
    alpha.u = (1 - conf.level) / 2
  }

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
  df_lmer = df %>%
    mutate(mean = (x+y)/2,
           delta = x - y)
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

  df3 = df2 %>%
    drop_na()
  d_varl = c()
  if(prop_bias == TRUE){
    form1 = as.formula(delta ~ mean)
  } else{
    form1 = as.formula(delta ~ 1)
  }
  # Get variance per id
  for(i in 1:nrow(df3)){
    idtemp = df3[i,]$id
    dftemp = df %>% filter(id == idtemp) %>%
      mutate(mean = (x+y)/2,
             delta = x - y)
    d_varl[i] = sigma(lm(delta ~ form1,
                         data = dftemp))^2
  }

  if(prop_bias == FALSE){
    d_bar = mean(df2$d)
    d_var = var(df2$d)
    d_lo = d_bar - confq*sqrt(d_var)/sqrt(nrow(df2))
    d_hi = d_bar + confq*sqrt(d_var)/sqrt(nrow(df2))
  } else{
    lmer_d = lme4::lmer(delta ~ mean + (1 | id),
                        data = df_lmer)
    d_var = as.data.frame(VarCorr(lmer_d))$vcov[1]
    d_bar = as.data.frame(emmeans(lmer_d, ~1))$emmean
    d_lo = as.data.frame(emmeans(lmer_d, ~1,
                                 level = conf.level))$lower.CL
    d_hi = as.data.frame(emmeans(lmer_d, ~1,
                                 level = conf.level))$upper.CL
  }

  sdw2 = sum((df3$m-1)/(nrow(df)-nrow(df3))*d_varl)
  mh = nrow(df2)/sum(1/df2$m)

  var_tot = d_var + (1-1/mh) * sdw2
  loa_l = d_bar - agreeq*sqrt(var_tot)
  loa_u = d_bar + agreeq*sqrt(var_tot)

  move.l.1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha.l,nrow(df2)-1))))^2
  move.l.2 = ((1-1/mh)*sdw2*(1-(nrow(df)-nrow(df2))/(qchisq(alpha.l,nrow(df)-nrow(df2)))))^2

  move.l = var_tot - sqrt(move.l.1+move.l.2)

  move.u.1 = (d_var*((nrow(df2)-1)/(qchisq(alpha.u,nrow(df2)-1))-1))^2
  move.u.2 = ((1-1/mh)*sdw2*((nrow(df)-nrow(df2))/(qchisq(alpha.u,nrow(df)-nrow(df2)))-1))^2

  move.u = var_tot + sqrt(move.u.1+move.u.2)

  LME = sqrt(confq2^2*(d_var/nrow(df2))+agreeq^2*(sqrt(move.u)-sqrt(var_tot))^2)
  RME = sqrt(confq2^2*(d_var/nrow(df2))+agreeq^2*(sqrt(var_tot)-sqrt(move.l))^2)

  loa_l.l = loa_l - LME
  loa_l.u = loa_l + RME

  loa_u.l = loa_u - RME
  loa_u.u = loa_u + LME

  df_loa = data.frame(
    estimate = c(d_bar, loa_l, loa_u),
    lower.ci = c(d_lo, loa_l.l, loa_u.l),
    upper.ci = c(d_hi, loa_l.u, loa_u.u),
    row.names = c("Bias","Lower LoA","Upper LoA")
  )
  if (!missing(delta)) {
  rej <- (-delta < loa_l.l) * (loa_u.l < delta)
  rej_text = "don't reject h0"
  if (rej == 1) {
    rej_text = "reject h0"
  }} else {
    rej_text = "No Hypothesis Test"
  }


  # Return Results ----
  if(missing(delta)){
    delta = NULL
  }

  lm_mod = list(call = list(formula = as.formula(df$x ~ df$y +
                                                   df$id)))
  call2 = match.call()
  if(is.null(call2$agree.level)){
    call2$agree.level = agree.level
  }

  if(is.null(call2$conf.level)){
    call2$conf.level = conf.level
  }
  call2$lm_mod = lm_mod


  structure(list(loa = df_loa,
                 h0_test = rej_text,
                 ccc.xy = ccc.xy,
                 call = call2,
                 var_comp = list(LME = LME,
                                 RME = RME,
                                 tot_var = var_tot,
                                 sdw2 = sdw2),
                 class = "nested"),
            class = "simple_agree")

}
