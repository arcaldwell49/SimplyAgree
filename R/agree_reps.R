#' Tests for Absolute Agreement with Replicates
#' @description agree_nest produces an absolute agreement analysis for data where there is multiple observations per subject but the mean does not vary within subjects as described by Zou (2013). Output mirrors that of agree_test but CCC is calculated via U-statistics.
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
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals using U-statistics.}
#'   \item{\code{"call"}}{The matched call.}
#'   \item{\code{"var_comp"}}{Table of Variance Components.}
#'   \item{\code{"class"}}{The type of simple_agree analysis.}
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
#' @importFrom stats pnorm qnorm lm anova dchisq qchisq sd var model.frame
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
                       prop_bias = FALSE,
                       TOST = TRUE){

  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  conf1 = conf.level
  if(TOST == TRUE){
    confq2 = qnorm(1 - (1 - conf.level) )
    alpha_l = 1 - (1 - conf.level)
    alpha_u = (1 - conf.level)
    conf2 = 1 - (1 - conf.level) * 2
  } else {
    confq2 = qnorm(1 - (1 - conf.level) / 2)
    alpha_l = 1 - (1 - conf.level) / 2
    alpha_u = (1 - conf.level) / 2
    conf2 = conf.level
  }


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

  # Calculate CCC ----
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
    summarize(mxi = base::sum(!is.na(x)),
              myi = base::sum(!is.na(y)),
              x_bar = base::mean(x, na.rm=TRUE),
              x_var = var(x, na.rm=TRUE),
              y_bar = base::mean(y, na.rm=TRUE),
              y_var = var(y, na.rm=TRUE),
              .groups = "drop") %>%
    mutate(d = x_bar-y_bar,
           both_avg = (x_bar+y_bar)/2)

  df3 = df2 %>%
    drop_na()

  df$mean = (df$x + df$y)/2
  df$delta = (df$x - df$y)
  #lmer_mod = lme4::lmer(delta ~ 1 + (1|id),
  #                data = df)
 # base::sum(as.data.frame(VarCorr(lmer_mod))$vcov)
  Nx = base::sum(df2$mxi)
  Ny = base::sum(df2$myi)
  mxh = nrow(df2)/base::sum(1/df2$mxi)
  myh = nrow(df2)/base::sum(1/df2$myi)

  if(prop_bias == FALSE){
    sxw2 = base::sum((df3$mxi-1)/(Nx-nrow(df3))*df3$x_var)
    syw2 = base::sum((df3$myi-1)/(Ny-nrow(df3))*df3$y_var)
    d_bar = base::mean(df2$d, na.rm = TRUE)
    d_var = var(df2$d, na.rm = TRUE)
    d_lo = d_bar - confq*sqrt(d_var)/sqrt(nrow(df2))
    d_hi = d_bar + confq*sqrt(d_var)/sqrt(nrow(df2))

    tot_var = d_var + (1-1/mxh)*sxw2 + (1-1/myh)*syw2
  } else {
    lmer_x = lmer(x ~ 1 + (1|id),
                  data = df)
    mxh_l = as.data.frame(emmeans(lmer_x, ~1))$emmean
    lmer_y = lmer(y ~ 1 + (1|id),
                  data = df)
    myh_l = as.data.frame(emmeans(lmer_y, ~1))$emmean
    lmer_d = lmer(delta ~ 1 + (1|id),
                  data = df)

    d_var = as.data.frame(VarCorr(lmer_d))$vcov[1]
    em_frame = summary(emmeans(lmer_d, ~1,
                       level = conf.level))
    colnames(em_frame) = c("overall", "emmean", "se", "df", "lower", "upper")
    d_bar = em_frame$emmean
    d_lo = em_frame$lower
    d_hi = em_frame$upper
    #d_bar = as.data.frame(emmeans(lmer_d, ~1))$emmean
    #d_lo = as.data.frame(emmeans(lmer_d, ~1,
    #                             level = conf.level))$lower.CL
    #d_hi = as.data.frame(emmeans(lmer_d, ~1,
    #                             level = conf.level))$upper.CL
    sxw2 = as.data.frame(VarCorr(lmer_x))$vcov[2]
    syw2 = as.data.frame(VarCorr(lmer_y))$vcov[2]
    tot_var = d_var + (1-1/mxh_l)*sxw2 + (1-1/myh_l)*syw2
  }


  loa_l = d_bar - agreeq*sqrt(tot_var)

  loa_u = d_bar + agreeq*sqrt(tot_var)

  move_l_1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha_l,nrow(df2)-1))))^2
  move_l_2 = ((1-1/mxh)*sxw2*(1-(Nx-nrow(df2))/(qchisq(alpha_l,Nx-nrow(df2)))))^2
  move_l_3 = ((1-1/myh)*syw2*(1-(Ny-nrow(df2))/(qchisq(alpha_l,Ny-nrow(df2)))))^2
  move_l = tot_var - sqrt(move_l_1+move_l_2+move_l_3)

  move_u_1 = (d_var*(1-(nrow(df2)-1)/(qchisq(alpha_u,nrow(df2)-1))))^2
  move_u_2 = ((1-1/mxh)*sxw2*(1-(Nx-nrow(df2))/(qchisq(alpha_u,Nx-nrow(df2)))))^2
  move_u_3 = ((1-1/myh)*syw2*(1-(Ny-nrow(df2))/(qchisq(alpha_u,Ny-nrow(df2)))))^2
  move_u = tot_var + sqrt(move_u_1+move_u_2+move_u_3)

  LME = sqrt(confq2^2*(d_var/nrow(df2))+agreeq^2*(sqrt(move_u)-sqrt(tot_var))^2)
  RME = sqrt(confq2^2*(d_var/nrow(df2))+agreeq^2*(sqrt(tot_var)-sqrt(move_l))^2)

  loa_l_l = loa_l - LME
  loa_l_u = loa_l + RME

  loa_u_l = loa_u - RME
  loa_u_u = loa_u + LME

  if(prop_bias == TRUE){
    message("prop_bias set to TRUE. Hypothesis test may be bogus. Check plots.")
  }

  ## Save LoA ----
  df_loa = data.frame(
    estimate = c(d_bar, loa_l, loa_u),
    lower.ci = c(d_lo, loa_l_l, loa_u_l),
    upper.ci = c(d_hi, loa_l_u, loa_u_u),
    ci.level = c(conf1, conf2, conf2)#,
    #row.names = c("Bias","Lower LoA","Upper LoA")
  )

  if (!missing(delta)) {
    rej <- (-delta < loa_l_l) * (loa_u_l < delta)
    rej_text = "don't reject h0"
    if (rej == 1) {
      rej_text = "reject h0"
    }
  } else {
    rej_text = "No Hypothesis Test"
  }

  # Save call----

  lm_mod = list(call = list(formula = as.formula(df$y ~ df$x +
                                                   df$id)))
  call2 = match.call()
  if(is.null(call2$agree.level)){
    call2$agree.level = agree.level
  }

  if(is.null(call2$conf.level)){
    call2$conf.level = conf.level
  }

  if(is.null(call2$TOST)){
    call2$TOST = TOST
  }
  if(is.null(call2$prop_bias)){
    call2$prop_bias = prop_bias
  }
  call2$lm_mod = lm_mod
  # Return Results ----

  structure(list(loa = df_loa,
                 h0_test = rej_text,
                 ccc.xy = ccc.xy,
                 call = call2,
                 var_comp = list(var_tot = tot_var,
                                 var_y = syw2,
                                 var_x = sxw2,
                                 LME = LME,
                                 RME = RME),
                 class = "replicates"),
            class = "simple_agree")

}
