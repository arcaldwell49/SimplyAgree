#' @title Nonparametric Test for Limits of Agreement
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' `agree_np` A non-parametric approach to limits of agreement.
#' The hypothesis test is based on binomial proportions within the maximal allowable differences, and the limits are calculated with quantile regression.
#'
#' @param x Name of column with first measurement.
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier with samples are taken in replicates.
#' @param data Data frame with all data.
#' @param conf.level the confidence level required. Default is 95%.
#' @param agree.level the agreement level required. Default is 95%. The proportion of data that should lie between the thresholds, for 95% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent and this argument is required. Equivalence Bound for Agreement or Maximal Allowable Difference.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param TOST Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests for the limits of agreement. Default is TRUE.
#'
#' @return Returns simple_agree object with the results of the agreement analysis.
#'
#'   - `loa`: A data frame of the limits of agreement.
#'   - `agree`: A data frame of the binomial proportion of results in agreement.
#'   - `h0_test`: Decision from hypothesis test.
#'   - `qr_mod`: The quantile regression model.
#'   - `call`: The matched call
#'
#' @examples
#' data('reps')
#' agree_np(x = "x", y = "y", id = "id", data = reps, delta = 2)
#' @section References:
#' Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method comparison studies. In Statistical Methods in Medical Research (Vol. 8, Issue 2, pp. 135–160).
#' SAGE Publications.
#' \doi{10.1177/096228029900800204}
#' @importFrom stats binom.test binomial confint glm median setNames quantile
#' @import quantreg
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @import dplyr
#' @import ggplot2
#' @export

agree_np <- function(x,
                     y,
                     id = NULL,
                     data,
                     delta = NULL,
                     prop_bias = FALSE,
                     TOST = TRUE,
                     agree.level = .95,
                     conf.level = .95){
  if(TOST == TRUE) {
    conf2 = 1 - (1 - conf.level) * 2
  } else {
    conf2 = conf.level
  }
  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2

  stopifnot(!is.null(delta) && is.numeric(delta) && delta > 0)

  if(!is.null(id)){
    df = data %>%
      select(all_of(id),all_of(x),all_of(y)) %>%
      rename(id = all_of(id),
             x = all_of(x),
             y = all_of(y)) %>%
      select(id,x,y) %>%
      drop_na()
  } else {
    df = data %>%
      select(all_of(x),all_of(y)) %>%
      rename(x = all_of(x),
             y = all_of(y)) %>%
      select(x,y)
    df$id = as.factor(1)
    df = df %>%
      drop_na()

  }

  df$mean = (df$x+df$y)/2
  df$delta = (df$x-df$y)
  dbound = delta
  if(!is.null(id)){
    df_glm1 = df
    df_glm1 = df_glm1 %>%
      mutate(success = ifelse(abs(delta) <= dbound,
                              1,0),
             failure = ifelse(abs(delta) > dbound,
                              1,0))

    df_glm = df_glm1 %>%
      group_by(id) %>%
      summarize(x = mean(x),
                y = mean(y),
                success = sum(success),
                failure = sum(failure))

  }else{
    df_glm = df
    df_glm = df_glm %>%
      mutate(success = ifelse(abs(delta) <= dbound,
             1,0),
             failure = ifelse(abs(delta) > dbound,
                                        1,0))
  }

  if(sum(df_glm$failure) == 0){
    message("All values within delta bounds. Confidence intervals hypothesis test likely bogus.")
    test = binom.test(sum(df_glm$success), sum(df_glm$success))
    df_agree = data.frame(
      row.names = paste0("% within ", dbound),
      agreement = test$estimate,
      lower.ci = test$conf.int[1],
      upper.ci = test$conf.int[2]
    )
    rej <- df_agree$lower.ci >= agree.level
    rej_text = "don't reject h0"
    if (rej == TRUE) {
      rej_text = "reject h0"
    }
  } else{

  glm_mod = glm(cbind(success,failure) ~ 1,
                data = df_glm,
                family = binomial)
  glm_emm = as.data.frame(confint(
    suppressWarnings(emmeans(glm_mod, ~1,
                             type = "response",
                             data = df_glm)),
    level = conf.level))
  df_agree = data.frame(agreement = glm_emm$prob,
                        lower.ci = glm_emm$asymp.LCL,
                        upper.ci = glm_emm$asymp.UCL,
                        row.names = paste0("% within ", dbound))
  rej <- glm_emm$asymp.LCL >= agree.level
  rej_text = "don't reject h0"
  if (rej == TRUE) {
    rej_text = "reject h0"
  }
  }



  # Quantile reg ------
  if(prop_bias == FALSE){
    quan_mod = suppressWarnings({rq(formula =  delta ~ 1,
                                     data = df,
                                     tau = c(agree.u,.5,agree.l))})
    quan_mod2 = rq(formula =  delta ~ mean,
                  data = df,
                  tau = c(agree.u,.5,agree.l))
    # bootstrap standard errors (see rq_boot_vcov())
    df_test = rq_boot_coef(quan_mod2, rq_boot_vcov(quan_mod2))
    df_test = df_test[c(4),]
    if(df_test$p.value < (1-conf.level)){
      warning("Evidence of proportional bias. Consider setting prop_bias to TRUE.")
    }

    df_coef = rq_boot_coef(quan_mod, rq_boot_vcov(quan_mod),
                           conf.level = conf.level)
    # TODO: the bounds of the limits are at conf.level, although ci.level
    # reports conf2 (as in earlier versions).
    df_coeff = data.frame(estimate = df_coef$estimate,
                         lower.ci = df_coef$conf.low,
                         upper.ci = df_coef$conf.high,
                         ci.level = c(conf2,
                                      conf.level,
                                      conf2))
    rownames(df_coeff) = c("Lower LoA", "Bias", "Upper LoA")
  } else {
    quan_mod = rq(formula =  delta ~ mean,
                   data = df,
                   tau = c(agree.u,.5,agree.l))
    minavg = min(df$mean)
    medavg = median(df$mean)
    maxavg = max(df$mean)
    # bootstrap standard errors (see rq_boot_vcov())
    V = rq_boot_vcov(quan_mod)
    quan_emm_med = rq_boot_ci(quan_mod, V, tau = .5,
                              at = c(minavg,medavg,maxavg),
                              conf.level = conf.level)
    quan_emm_med$at = paste0("Bias @ ",signif(quan_emm_med$mean,3))
    df_coef_med = data.frame(row.names = quan_emm_med$at,
                             estimate = quan_emm_med$emmean,
                             lower.ci = quan_emm_med$lower.CL,
                             upper.ci = quan_emm_med$upper.CL)
    df_coef_med$ci.level = conf.level
    quan_emm_lloa = rq_boot_ci(quan_mod, V, tau = agree.u,
                               at = c(minavg,medavg,maxavg),
                               conf.level = conf2)
    quan_emm_lloa$at = paste0("Lower LoA @ ", signif(quan_emm_lloa$mean,3))
    df_coef_lloa = data.frame(row.names = quan_emm_lloa$at,
                             estimate = quan_emm_lloa$emmean,
                             lower.ci = quan_emm_lloa$lower.CL,
                             upper.ci = quan_emm_lloa$upper.CL)
    df_coef_lloa$ci.level = conf2

    quan_emm_uloa = rq_boot_ci(quan_mod, V, tau = agree.l,
                               at = c(minavg,medavg,maxavg),
                               conf.level = conf2)
    quan_emm_uloa$at = paste0("Upper LoA @ ", signif(quan_emm_uloa$mean,3))
    df_coef_uloa = data.frame(row.names = quan_emm_uloa$at,
                              estimate = quan_emm_uloa$emmean,
                              lower.ci = quan_emm_uloa$lower.CL,
                              upper.ci = quan_emm_uloa$upper.CL)
    df_coef_uloa$ci.level = conf2
    df_coeff = rbind(df_coef_lloa,
                     df_coef_med,
                     df_coef_uloa)

  }

  ## Save LoA ----
  df_loa = df_coeff

  # Save call----
  # function name will be: as.character(call2[[1]])
  lm_mod = plot_frame(y = df$y, x = df$x)
  call2 = match.call()
  call2$agree.level = agree.level
  call2$conf.level = conf.level
  call2$prop_bias = prop_bias
  call2$TOST = TOST
  call2$delta = delta
  call2$lm_mod = lm_mod
  # Return Results ----

  structure(list(loa = df_loa,
                 agree = df_agree,
                 h0_test = rej_text,
                 qr_mod = quan_mod,
                 call = call2),
            class = "simple_agree")

}
