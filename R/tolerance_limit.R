#' @title Tolerance Limits from an Agreement Study
#'
#' @description `r lifecycle::badge('maturing')`
#'
#' A function for calculating tolerance limits for the difference between two measurements (difference = x-y).
#' This is a procedure that should produce results similar to the Bland-Altman limits of agreement.
#' See vignettes for more details.
#'
#' @param data A data frame containing the variables.
#' @param x Name of the column for the first measurement.
#' @param y Name of the column for the second measurement.
#' @param id Name of the column for the subject ID.
#' @param condition Name of the column indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param time Name of the column indicating the time points. Only necessary if the data is from time series or repeated measures collection.
#' @param pred_level Prediction level for the prediction interval. Default is 95%.
#' @param tol_level Tolerance level for the tolerance limit (i.e., the CI of the prediction limit). Default is 95%.
#' @param tol_method Method for calculating the tolerance interval. Options are "approx" for a chi-square based approximation and "perc" for a parametric percentile bootstrap method.
#' @param prop_bias Whether to include a proportional bias term in the model. Determines whether proportional bias should be considered for the prediction/tolerance limits calculations.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param cor_type The type of correlation structure. "sym" is for Compound Symmetry, "car1" is for continuous autocorrelation structure of order 1, or "ar1" for autocorrelation structure of order 1.
#' @param correlation an optional corStruct object describing the within-group correlation structure that overrides the default setting. See the documentation of corClasses for a description of the available corStruct classes. If a grouping variable is to be used, it must be specified in the form argument to the corStruct constructor. Defaults to NULL.
#' @param weights an optional varFunc object or one-sided formula describing the within-group heteroskedasticity structure that overrides the default setting. If given as a formula, it is used as the argument to varFixed, corresponding to fixed variance weights. See the documentation on varClasses for a description of the available varFunc classes.
#' @param keep_model Logical indicator to retain the GLS model. Useful when working with large data and the model is very large.
#' @inheritParams loa_lme
#' @details The tolerance limits calculated in this function are based on the papers by Francq & Govaerts (2016), Francq, et al. (2019), and Francq, et al. (2020).
#' When `tol_method` is set to "approx", the tolerance limits are calculated using the approximation detailed in Francq et al. (2020).
#' However, these are only an approximation and conservative.
#' Therefore, as suggested by Francq, et al. (2019), a parametric bootstrap approach can be utilized to calculate percentile tolerance limits (`tol_method = "perc"`).
#'
#' @return Returns single `tolerance_delta` class object with the results of the agreement analysis with a prediction interval and tolerance limits.
#'
#'   - `limits`: A data frame containing the prediction/tolerance limits.
#'   - `model`: The GLS model; NULL if keep_model set to FALSE.
#'   - `call`: The matched call.
#' @examples
#' data('reps')
#'
#' # Simple
#' tolerance_limit(x = "x", y ="y", data = reps)
#'
#' # Nested
#' tolerance_limit(x = "x", y ="y", data = reps, id = "id")
#'
#' @references
#'
#' Francq, B. G., & Govaerts, B. (2016). How to regress and predict in a Bland–Altman plot? Review and contribution based on tolerance intervals and correlated‐errors‐in‐variables models. Statistics in mMdicine, 35(14), 2328-2358.
#'
#' Francq, B. G., Lin, D., & Hoyer, W. (2019). Confidence, prediction, and tolerance in linear mixed models. Statistics in Medicine, 38(30), 5603-5622.
#'
#' Francq, B. G., Berger, M., & Boachie, C. (2020). To tolerate or to agree: A tutorial on tolerance intervals in method comparison studies with BivRegBLS R Package. Statistics in Medicine, 39(28), 4334-4349.
#'
#' @importFrom nlme gls  corCompSymm corAR1 corCAR1 varIdent
#' @importFrom stats vcov model.matrix formula na.fail update
#' @importFrom emmeans ref_grid
#' @importFrom dplyr inner_join join_by
#' @importFrom MASS mvrnorm
#' @importFrom Matrix chol Matrix bdiag
#' @export

tolerance_limit = function(data,
                               x,
                               y,
                               id = NULL,
                               condition = NULL,
                               time = NULL,
                               pred_level = 0.95,
                               tol_level = 0.95,
                               tol_method = c("approx","perc"),
                               prop_bias = FALSE,
                               log_tf = FALSE,
                               cor_type = c("sym", "car1", "ar1", "none"),
                               correlation = NULL,
                               weights = NULL,
                               keep_model = TRUE,
                               replicates = 999){
  alpha = 1 - tol_level
  alpha.pred=1-pred_level
  # match args -----
  cor_type = match.arg(cor_type)
  tol_method = match.arg(tol_method)
  # set call ----
  call2 = match.call()
  call2$id = id
  call2$condition = condition
  call2$pred_level = pred_level
  call2$tol_level = tol_level
  call2$prop_bias = prop_bias
  call2$log_tf = log_tf
  call2$cor_type = cor_type
  call2$correlation = correlation
  call2$weights = weights

  # organize data -----
  temp_frame = data[c(x,y,id,condition,time)]
  names(temp_frame)[names(temp_frame) == x] <- "x"
  names(temp_frame)[names(temp_frame) == y] <- "y"
  names(temp_frame)[names(temp_frame) == id] <- "id"
  names(temp_frame)[names(temp_frame) == condition] <- "condition"
  names(temp_frame)[names(temp_frame) == time] <- "time"
  #colnames(temp_frame) = c(x,y,id,condition,time)

  temp_frame = na.omit(temp_frame)
  temp_frame$avg = (temp_frame$x + temp_frame$y)/2
  avg_vals = c(min(temp_frame$avg),
               median(temp_frame$avg),
               max(temp_frame$avg))
  if(log_tf){
    temp_frame$x = log(temp_frame$x)
    temp_frame$y = log(temp_frame$y)
  }
  temp_frame$delta = temp_frame$x - temp_frame$y

  if(!("id" %in% colnames(temp_frame))){
    temp_frame$id = 1:nrow(temp_frame)
  }
  deg_of_freedom = length(unique(temp_frame$id)) -1
  # MODEL ----
  model = gls(delta ~ 1, data = temp_frame)
  # Set to null for when not used
  var1 = NULL
  cor1 = NULL
  ## Update model with condition -----
  if(!is.null(condition)){
    var1 = varIdent(form=~1|condition)
    model = update(model,
                   . ~ . + condition,
                   weights = var1)

  }

  ## Update model for prop bias ----
  if(prop_bias){
    model = update(model,
                   . ~ . + avg)
  }

  ## Correlation -----
  if(!is.null(id) && cor_type != "none"){

    if(!is.null(time)){
      cor1 = switch(cor_type,
                    sym = nlme::corCompSymm(form = ~time|id),
                    car1 = nlme::corCAR1(form = ~time|id),
                    ar1 = nlme::corAR1(form= ~time|id))
    } else {
      cor1 = switch(cor_type,
                    sym = nlme::corCompSymm(form = ~1|id),
                    car1 = nlme::corCAR1(form = ~1|id),
                    ar1 = nlme::corAR1(form= ~1|id))
    }

    model = update(model,
                   correlation = cor1)
  }

  ## Custom model input -----

  if(!is.null(weights)){
    var1 = weights
    model = update(model,
           weights = var1)

  }

  if(!is.null(correlation)){
    cor1 = correlation
    model = update(model,
           correlation = cor1)

  }

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1

  res_emm = gls_emm_delta(model = model,
                          temp_frame = temp_frame,
                          avg_vals = avg_vals)

  if(tol_method == "approx"){
    emm_df = as.data.frame(res_emm) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP,
             lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
             upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df))) %>%
      rename(bias = emmean)
  }

  if(tol_method == "perc"){
    emm_df = as.data.frame(res_emm) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)

    res_sum_df = boot_delta_gls(model = model,
                                temp_frame = temp_frame,
                                avg_vals = avg_vals,
                                res_emm = res_emm,
                                tol_level = tol_level,
                                alpha.pred = alpha.pred,
                                replicates = replicates,
                                cor1 = cor1,
                                var1 = var1)

    emm_df = suppressMessages({ full_join(emm_df, res_sum_df) %>%
      rename(bias = emmean)
    })
  }


  # Save model -----
  model <- if(keep_model){
    model
  } else{
    NULL
  }

  if(!("condition" %in% colnames(emm_df))){
    emm_df$condition = NA
  }

  if(!("avg" %in% colnames(emm_df))){
    emm_df$avg = NA
  }

  if(!("condition" %in% colnames(temp_frame))){
    temp_frame$condition = 1:nrow(temp_frame)
  }

  if(!("id" %in% colnames(temp_frame))){
    temp_frame$id = 1:nrow(temp_frame)
  }

  if(!("time" %in% colnames(temp_frame))){
    temp_frame$time = 1:nrow(temp_frame)
  }

  df = as.data.frame(temp_frame)
  lm_mod = list(call = list(formula = as.formula(df$y ~ df$x + df$id + df$avg + df$delta + df$condition + df$time)))
  call2$lm_mod = lm_mod

  res = structure(list(limits = emm_df,
             emmeans = res_emm,
             model = model,
             call = call2),
             class = "tolerance_delta")

  return(res)
}


# get emmeans -----

gls_emm_delta = function(model,
                         temp_frame,
                         avg_vals){
  # "avg" %in% paste0(nlme::getCovariateFormula(model))

  if(grepl("avg", paste0(nlme::getCovariateFormula(model))[2])){
    if(grepl("condition",paste0(nlme::getCovariateFormula(model))[2])){

      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals),
                                 data = temp_frame),
                        ~ condition + avg,
                        mode = "satterthwaite",
                        data = temp_frame)
    } else{
      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals),
                                 data = temp_frame),
                        ~ avg,
                        mode = "satterthwaite",
                        data = temp_frame)
    }

  } else {
    if(grepl("condition",paste0(nlme::getCovariateFormula(model))[2])){
      res_emm = emmeans(model,
                        ~ condition ,
                        mode = "satterthwaite",
                        data = temp_frame)
    } else{
      res_emm = emmeans(model,
                        ~ 1 ,
                        mode = "satterthwaite",
                        data = temp_frame)
    }
  }
  return(res_emm)
}

# bootstrap ----
boot_delta_gls = function(model,
                          temp_frame,
                          avg_vals,
                          res_emm,
                          tol_level,
                          alpha.pred,
                          replicates,
                          cor1,
                          var1){
  emm_df = as.data.frame(res_emm) %>%
    rename(SEM = SE) %>%
    mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
    mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
           upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)

  res_df = data.frame()

  for(i in 1:replicates){
    dat2 = r_gen(dat = temp_frame,
                 mle = model)
    res_i = update(model, data = dat2)

    emm1 = gls_emm_delta(model = res_i,
                         temp_frame = dat2,
                         avg_vals = avg_vals)
    emm_df1 = as.data.frame(emm1) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(res_i)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)
    class(emm_df1) = "data.frame"
    emm_df1$boot_n = i

    res_df = rbind(res_df,emm_df1)
  }

  if(grepl("avg",paste0(nlme::getCovariateFormula(model))[2])){
    if(grepl("condition",paste0(nlme::getCovariateFormula(model))[2])){
      sum_res_df = res_df %>%
        group_by(avg, condition) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(condition, avg))
    } else{
      sum_res_df = res_df %>%
        group_by(avg) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(avg))
    }

  } else {
    if(grepl("condition",paste0(nlme::getCovariateFormula(model))[2])){
      sum_res_df = res_df %>%
        group_by(condition) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(condition))
    } else{
      sum_res_df = res_df %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>% cbind(emm_df, .)
    }
  }

  return(sum_res_df)

}

r_gen <- function(dat, mle) {
  out <- dat
  out$delta <- sim_gls(mle, data = dat)
  return(out)
}

