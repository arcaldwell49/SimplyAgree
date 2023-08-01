#' @title Limits of Agreement
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' A function for calculating for Bland-Altman limits of agreement based on the difference between two measurements (difference = x-y).
#' Please note that the package developer recommends reporting/using tolerance limits (see \code{"tolerance_limit"} function).
#' @param x Name of column with first measurement
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier. Default is "id" if no entry is provided.
#' @param data Data frame with all data.
#' @param agree.level the agreement level required. Default is 95%. The proportion of data that should lie between the thresholds, for 95% limits of agreement this should be 0.95.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param alpha The alpha-level for confidence levels.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param data_type The type of data structure. Options include "simple" (all independent data points), "nest" (nested data) and "reps" (replicated data points).
#' @param loa_calc The method by which the limits of agreement confidence intervals are calculated. Options are "mover" (Methods of Recovering Variances method) or "blandlatman" (Bland-Altman method).
#' @return Returns single loa class object with the results of the agreement analysis.
#'
#'   - `loa`: A data frame containing the Limits of Agreement.
#'   - `call`:The matched call.
#' @details The limits of agreement (LoA) are calculated in this function are based on the method originally detailed by Bland & Atlman (1986 & 1999).
#' The `loa_calc` allow users to specify the calculative method for the LoA which can be based on Bland-Altman (1999) (`loa_calc = "blandaltman"`),
#' or by the more accurate MOVER method of Zou (2013) and Donner & Zou (2012) (`loa_calc = "mover"`).
#'
#' @examples
#' data('reps')
#'
#' # Simple
#' agreement_limit(x = "x", y ="y", data = reps)
#'
#' # Replicates
#' agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "rep")
#'
#' # Nested
#' agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "nest")
#'
#' @references
#'
#' MOVER methods:
#'
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#'
#' Donner, A., & Zou, G. Y. (2012). Closed-form confidence intervals for functions of the normal mean and standard deviation. Statistical Methods in Medical Research, 21(4), 347-359.
#'
#' Bland & Altman methods:
#'
#' Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The Lancet, 327(8476), 307-310.
#'
#' Bland, J. M., & Altman, D. (1999). Measuring agreement in method comparison studies. Statistical methods in medical research, 8(2), 135-160.
#'
#' Bland, J. M., & Altman, D. G. (1996). Statistics notes: measurement error proportional to the mean. BMJ, 313(7049), 106.
#'
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom dplyr rename mutate
#' @import ggplot2
#' @export

agreement_limit = function(x,
                           y,
                           id = NULL,
                           data,
                           data_type = c("simple","nest","reps"),
                           loa_calc = c("mover","blandaltman"),
                           agree.level = 0.95,
                           alpha = 0.05,
                           prop_bias = FALSE,
                           log_tf = FALSE){
  data_type = match.arg(data_type)
  loa_calc = match.arg(loa_calc)
  conf.level = 1- alpha

  call2 = match.call()
  call2$data_type = data_type
  call2$loa_calc = loa_calc
  call2$agree.level = agree.level
  call2$conf.level = conf.level
  call2$alpha = alpha
  call2$id = id
  call2$prop_bias  = prop_bias
  call2$log_tf = log_tf

  df = loa_data_org(
    data = data,
    x = x,
    y = y,
    id = id,
    data_type = data_type,
    log_tf = log_tf
  )

  if(ncol(df) != 5){
    stop("incorrect df dimensions. internal error.")
  }

  # Get LoA ----
  ## simple -----
  if(data_type == "simple"){
    df_loa = calc_loa_simple(
      df = df,
      conf.level = conf.level,
      agree.level = agree.level,
      loa_calc = loa_calc,
      prop_bias = prop_bias)

  }
  ## reps -----
  if(data_type == "reps"){
    df_loa = calc_loa_reps(
      df = df,
      conf.level = conf.level,
      agree.level = agree.level,
      loa_calc = loa_calc,
      prop_bias = prop_bias
    )
  }
  ## nest -----
  if(data_type == "nest"){
    df_loa = calc_loa_nest(
      df = df,
      conf.level = conf.level,
      agree.level = agree.level,
      loa_calc = loa_calc,
      prop_bias = prop_bias
    )
  }

  if(!is.data.frame(df_loa)){
    stop("Internal error. LoA data frame not created.")
  }

  # Save data

  lm_mod = list(call = list(formula = as.formula(df$y ~ df$x +
                                                   df$id + df$avg + df$delta)))
  call2$lm_mod = lm_mod
  res = structure(list(loa = df_loa,
                       call = call2),
                 class = "loa")
  return(res)
}

loa_data_org = function(data,
                        x,
                        y,
                        id,
                        data_type,
                        log_tf = FALSE){
  if(data_type == "simple"){
    df = data %>%
      select(all_of(x),all_of(y)) %>%
      rename(x = all_of(x),
             y = all_of(y)) %>%
      select(x,y) %>%
      drop_na()
    df$id = 1:nrow(df)
  } else {
    if(is.null(id)){
      stop("id must be provided if data_type != \'simple\'.")
    }
    df = data %>%
      select(all_of(id),all_of(x),all_of(y)) %>%
      rename(id = all_of(id),
             x = all_of(x),
             y = all_of(y)) %>%
      select(id,x,y)
  }

  df = df %>%
    mutate(avg = (x+y)/2)
  if(log_tf == TRUE){
    df = df %>%
      mutate(x = log(x),
             y = log(y))
  }
    df = df %>%
      mutate(delta = x-y)

  return(df)
}

calc_loa_simple = function(df,
                            conf.level = .95,
                            agree.level = .95,
                            loa_calc,
                            prop_bias = FALSE) {
  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree_l = 1 - (1 - agree.level) / 2
  agree_u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  conf1 = 1-((1-conf.level)/2)

  confq2 = qnorm(1 - (1 - conf.level))
  alpha.l = 1 - (1 - conf.level)
  alpha.u = (1 - conf.level)
  conf2 = conf.level

  k <- nrow(df)
  yb <- mean(df$y, na.rm = TRUE)
  sy2 <- var(df$y, na.rm = TRUE) * (k - 1) / k
  sd1 <- sd(df$y, na.rm = TRUE)
  xb <- mean(df$x, na.rm = TRUE)
  sx2 <- var(df$x, na.rm = TRUE) * (k - 1) / k
  sd2 <- sd(df$x, na.rm = TRUE)
  r <- cor(df$x, df$y)
  sl <- r * sd1 / sd2
  sxy <- r * sqrt(sx2 * sy2)

  if (prop_bias == FALSE) {
    # sqrt(var(delta, na.rm = TRUE))
    model = lm(formula = delta ~ 1,
               data = df)
    bias_values = emmeans(model, ~1) %>%
      confint(level = conf.level) %>%
      as.data.frame() %>%
      rename(bias = emmean,
             avg = `1`)
  } else {
    model <- lm(formula = delta ~ avg,
                         data = df)

    bias_values = ref_grid(model,
                           at = list(
                             avg = c(
                               min(df$avg, na.rm = TRUE),
                               mean(df$avg, na.rm = TRUE),
                               max(df$avg, na.rm = TRUE)
                             )
                           )) %>%
      emmeans(~avg) %>%
      confint(level = conf.level) %>%
      as.data.frame() %>%
      rename(bias = emmean)
  }
  delta.sd <- sigma(model)
  dfs = df.residual(model)

  if(loa_calc == "blandaltman"){
    df_loa = bias_values %>%
      mutate(
        sd_delta = delta.sd,
        var_loa = (delta.sd*sqrt(1/k + agreeq^2 / (2*(k-1))) )^2,
        agree_int = agreeq * sd_delta,
        lme = qt(conf.level, df) * sqrt(var_loa)
        ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme)
      )

    #lower_loa = bias - agreeq * delta.sd
    #lower_loa_ci = (lower_loa - qt(conf2, dfs) * sqrt(var_loa))
    #upper_loa = bias + agreeq * delta.sd
    #upper_loa_ci = (upper_loa  + qt(conf2, dfs) * sqrt(var_loa))
  }

  if(loa_calc == "mover"){
    #a_val = sqrt(dfs/qchisq(conf1,dfs))
    #b_bal = sqrt(dfs/qchisq(1-conf1,dfs))
    df_loa = bias_values %>%
      mutate(
        sd_delta = delta.sd,
        var_loa = (delta.sd*sqrt(1/k + agreeq^2 / (2*(k-1))) )^2,
        agree_int = agreeq * sd_delta,
        lme =  sd_delta * sqrt(confq2^2/k + agreeq^2 * (sqrt(dfs/qchisq(1-conf2,dfs))-1)^2)
      ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme)
      )
  }


  return(df_loa)
}

calc_loa_reps = function(df,
                            conf.level = .95,
                            agree.level = .95,
                            loa_calc,
                            prop_bias = FALSE) {
  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree_l = 1 - (1 - agree.level) / 2
  agree_u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  conf1 = 1-((1-conf.level)/2)

  confq2 = qnorm(1 - (1 - conf.level))
  alpha_l = 1 - (1 - conf.level)
  alpha_u = (1 - conf.level)
  conf2 = 1 - (1 - conf.level) * 2

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
           avg = (x_bar+y_bar)/2)

  df3 = df2 %>%
    drop_na()

  Nx = base::sum(df2$mxi)
  Ny = base::sum(df2$myi)
  mxh = nrow(df2)/base::sum(1/df2$mxi)
  myh = nrow(df2)/base::sum(1/df2$myi)

  if(prop_bias == TRUE){
    form1 = as.formula(d ~ avg)
    model = lm(form1, data = df2)
    bias_values = ref_grid(model,
                           at = list(
                             avg = c(
                               min(df$avg,na.rm=TRUE),
                               mean(df$avg,na.rm=TRUE),
                               max(df$avg,na.rm=TRUE)
                             )
                           )) %>%
      emmeans(~avg) %>%
      confint(level = conf.level) %>%
      as.data.frame() %>%
      rename(bias = emmean)

    sxw2 = base::sum((df3$mxi-1)/(Nx-nrow(df3))*df3$x_var)
    syw2 = base::sum((df3$myi-1)/(Ny-nrow(df3))*df3$y_var)
    d_bar = base::mean(df2$d, na.rm = TRUE)
    d_var = sigma(model)^2
    between_variance = d_var
    total_variance = d_var + (1-1/mxh)*sxw2 + (1-1/myh)*syw2

  } else{
    form1 = as.formula(d ~ 1 )
    model = lm(form1, data = df2)
    sxw2 = base::sum((df3$mxi-1)/(Nx-nrow(df3))*df3$x_var)
    syw2 = base::sum((df3$myi-1)/(Ny-nrow(df3))*df3$y_var)
    d_var = sigma(model)^2
    between_variance = d_var
    total_variance = d_var + (1-1/mxh)*sxw2 + (1-1/myh)*syw2

    bias_values = emmeans(model, ~1) %>%
      confint(level = conf.level) %>%
      as.data.frame() %>%
      rename(bias = emmean,
             avg = `1`)
  }
  # Calc variances -----

  n_sub = nrow(df2)
  n_obs = nrow(df)

  # LoA Variance ----
  loa_var = (between_variance/n_sub) + agreeq^2 / (2*total_variance) *
    ((between_variance)^2/(n_sub-1) + (1 - 1/mxh)^2 *
       (sxw2)^2/(Nx-n_sub) + (1 - 1/myh)^2 *
       (syw2)^2/(Ny-n_sub))

  # LoA -------
  if(loa_calc == "blandaltman"){

    df_loa = bias_values %>%
      mutate(
        sd_delta = sqrt(total_variance),
        var_loa = loa_var,
        agree_int = agreeq * sd_delta,
        lme = confq2 * sqrt(var_loa)
      ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme),
        within_variance_x = sxw2,
        within_variance_y = syw2
      )

  }

  if(loa_calc == "mover"){

    # MOVER Components -----
    move_u_1 = (d_var*(1-(n_sub-1)/(qchisq(alpha_u,n_sub-1))))^2
    move_u_2 = ((1-1/mxh)*sxw2*(1-(Nx-n_sub)/(qchisq(alpha_u,Nx-n_sub))))^2
    move_u_3 = ((1-1/myh)*syw2*(1-(Ny-n_sub)/(qchisq(alpha_u,Ny-n_sub))))^2
    move_u = total_variance + sqrt(move_u_1+move_u_2+move_u_3)

    # LME -----
    LME = sqrt(confq2^2*(d_var/nrow(df2))+agreeq^2*(sqrt(move_u)-sqrt(total_variance))^2)

    df_loa = bias_values %>%
      mutate(
        sd_delta = sqrt(total_variance),
        var_loa = loa_var,
        agree_int = agreeq * sd_delta,
        lme = LME
      ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme),
        within_variance_x = sxw2,
        within_variance_y = syw2
      )
  }

  return(df_loa)

}


calc_loa_nest = function(df,
                          conf.level = .95,
                          agree.level = .95,
                          loa_calc,
                          prop_bias = FALSE){
  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree_l = 1 - (1 - agree.level) / 2
  agree_u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  conf1 = 1-((1-conf.level)/2)

  confq2 = qnorm(1 - (1 - conf.level))
  alpha.l = alpha_l = 1 - (1 - conf.level)
  alpha.u = alpha_u = (1 - conf.level)
  conf2 = 1 - (1 - conf.level) * 2

  df2 = df %>%
    group_by(id) %>%
    summarize(m = n(),
              x_bar = mean(x, na.rm= TRUE),
              x_var = var(x, na.rm= TRUE),
              y_bar = mean(y, na.rm= TRUE),
              y_var = var(y, na.rm= TRUE),
              d = mean(x-y, na.rm = TRUE),
              d_var = var(x-y, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(avg = (x_bar+y_bar)/2)

  df3 = df2 %>%
    drop_na()

  if(prop_bias == TRUE){
    form1 = as.formula(delta ~ avg + (1|id))
  } else{
    form1 = as.formula(delta ~ 1 + (1|id))
  }
  # Model ----
  model = lmer(form1,
               data = df,
               REML = TRUE)

  df_var = as.data.frame(VarCorr(model))
  total_variance = sum(df_var$vcov)
  within_variance = subset(df_var, grp == "Residual")$vcov
  between_variance = total_variance-subset(df_var, grp == "Residual")$vcov
  sd_w = sqrt(within_variance)
  sd_b = sqrt(between_variance)
  mh = nrow(df3)/sum(1/df3$m)
  n_sub = nrow(df3)
  n_obs = nrow(df)

  # LoA Variance ----
  loa_var = (between_variance/n_sub) + agreeq^2 / (2*total_variance) *
    ((between_variance)^2/(n_sub-1) + (1 - 1/mh)^2 * (within_variance)^2/(n_obs-n_sub))

  if (prop_bias == FALSE) {
    bias_values = emmeans(model, ~1, lmer.df = "satterthwaite") %>%
      as.data.frame() %>%
      rename(bias = emmean) %>%
      mutate(avg = "overall") %>%
      select(avg, bias, SE, df, lower.CL, upper.CL)

  } else {
    bias_values = ref_grid(model,
                           at = list(
                             avg = c(
                               min(df$avg, na.rm = TRUE),
                               mean(df$avg, na.rm = TRUE),
                               max(df$avg, na.rm = TRUE)
                             )
                           )) %>%
      emmeans(~avg, lmer.df = "satterthwaite") %>%
      as.data.frame() %>%
      rename(bias = emmean) %>%
      select(avg, bias, SE, df, lower.CL, upper.CL)

  }

  if(loa_calc == "blandaltman"){

    df_loa <- bias_values %>%
      mutate(
        sd_delta = sqrt(total_variance),
        var_loa = loa_var,
        agree_int = agreeq * sd_delta,
        lme = confq2 * sqrt(var_loa)
      ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme)
      )

  }

  if(loa_calc == "mover"){

    # MOVER Components -----
    move_u_1 = (between_variance*((n_sub-1)/(qchisq(alpha.u,n_sub-1))-1))^2
    move_u_2 = ((1-1/mh)*within_variance*((n_obs-n_sub)/(qchisq(alpha.u,n_obs-n_sub))-1))^2
    move_u = total_variance + sqrt(move_u_1+move_u_2)

    # LME -----
    LME = sqrt(confq2^2*(between_variance/n_sub)+agreeq^2*(sqrt(move_u)-sqrt(total_variance))^2)

    df_loa <- bias_values %>%
      mutate(
        sd_delta = sqrt(total_variance),
        var_loa = loa_var,
        agree_int = agreeq * sd_delta,
        lme = LME
      ) %>%
      mutate(
        lower_loa = bias - agree_int,
        lower_loa_ci = (lower_loa - lme),
        upper_loa = bias + agree_int,
        upper_loa_ci = (upper_loa  + lme)
      )
  }

  return(df_loa)
}
