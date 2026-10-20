#' Tests for Absolute Agreement with Replicates
#'
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' Development on `agree_reps()` is complete, and for new code we recommend
#' switching to `agreement_limit()`, which is easier to use, has more features,
#' and still under active development.
#'
#' agree_nest produces an absolute agreement analysis for data where there is multiple observations per subject but the mean does not vary within subjects as described by Zou (2013). Output mirrors that of agree_test but the CCC is estimated from variance components (Carrasco & Jover, 2003) with a delete-one-subject jackknife standard error; see [ccc_test()].
#'
#' @inheritParams agree_nest
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#'   - `loa`: a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.
#'   - `h0_test`: Decision from hypothesis test.
#'   - `ccc.xy`: The concordance correlation coefficient (variance components) and confidence intervals, as in `ccc_test(data_type = "reps")`.
#'   - `call`: The matched call.
#'   - `var_comp`: Table of Variance Components.
#'   - `class`: The type of simple_agree analysis.
#'
#' @examples
#' data('reps')
#' agree_reps(x = "x", y = "y", id = "id", data = reps, delta = 2)
#' @references
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#'
#' Carrasco, JL & Jover, L. (2003). Estimating the generalized concordance correlation coefficient through variance components. Biometrics, 59(4), 849-858.
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
                       TOST = TRUE,
                       ccc = TRUE){
  lifecycle::deprecate_soft("0.2.0", "agree_reps()", "agreement_limit()")

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

  # Calculate CCC ----
  if(ccc){
  ccc_reps = .ccc_clustered(df, "reps")
  ccc_ci = .ccc_ci(ccc_reps$estimate, ccc_reps$se_z, conf.level, "two.sided",
                   df = ccc_reps$n - 1)

  ccc.xy = data.frame(est.ccc = ccc_reps$estimate,
                      lower.ci = ccc_ci[1],
                      upper.ci = ccc_ci[2],
                      SE = ccc_reps$se)
  } else{
    ccc.xy = data.frame(est.ccc = NA,
                        lower.ci = NA,
                        upper.ci = NA,
                        SE = NA)
  }

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
    ci.level = c(conf1, conf2, conf2),
    row.names = c("Bias","Lower LoA","Upper LoA")
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

  lm_mod = plot_frame(y = df$y, x = df$x, id = df$id)
  call2 = match.call()
  call2$agree.level = agree.level
  call2$conf.level = conf.level
  call2$TOST = TOST
  call2$prop_bias = prop_bias
  if (!missing(delta)) {
    call2$delta = delta
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
