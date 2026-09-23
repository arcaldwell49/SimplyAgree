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
#' @param conf_level Confidence level for the confidence interval of the bias (`lower.CL`/`upper.CL`). Default is 95%. This does not affect the prediction or tolerance limits. For a two one-sided tests (TOST) procedure on the bias at level alpha, use `conf_level = 1 - 2 * alpha` (e.g., 0.90).
#' @param tol_method Method for calculating the tolerance interval. Options are "approx" for a chi-square based approximation and "perc" for a parametric percentile bootstrap method.
#' @param prop_bias Whether to include a proportional bias term in the model. Determines whether proportional bias should be considered for the prediction/tolerance limits calculations.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param log_tf_display The type of presentation for log-transformed results. The differences between methods can be displayed as a "ratio" or "sympercent".
#' @param cor_type The type of correlation structure. "sym" is for Compound Symmetry, "car1" is for continuous autocorrelation structure of order 1, or "ar1" for autocorrelation structure of order 1.
#' @param correlation an optional corStruct object describing the within-group correlation structure that overrides the default setting. See the documentation of corClasses for a description of the available corStruct classes. If a grouping variable is to be used, it must be specified in the form argument to the corStruct constructor. Defaults to NULL.
#' @param weights an optional varFunc object or one-sided formula describing the within-group heteroskedasticity structure that overrides the default setting. If given as a formula, it is used as the argument to varFixed, corresponding to fixed variance weights. See the documentation on varClasses for a description of the available varFunc classes. Variance covariates must use the internal column names (`avg`, `condition`, `x`, `y`, `time`) or `fitted(.)`. Currently `varIdent`, `varFixed`, `varExp`, and `varPower` are supported. If the variance depends on `avg`, the limits are reported at the minimum, median, and maximum of `avg` even when `prop_bias = FALSE`.
#' @param keep_model Logical indicator to retain the GLS model. Useful when working with large data and the model is very large.
#' @inheritParams loa_lme
#' @details The tolerance limits calculated in this function are based on the papers by Francq & Govaerts (2016), Francq, et al. (2019), and Francq, et al. (2020).
#' When `tol_method` is set to "approx", the tolerance limits are calculated using the approximation detailed in Francq et al. (2020).
#' However, these are only an approximation and conservative.
#' Therefore, as suggested by Francq, et al. (2019), a parametric bootstrap approach can be utilized to calculate percentile tolerance limits (`tol_method = "perc"`).
#'
#' @return Returns single `tolerance_delta` class object with the results of the agreement analysis with a prediction interval and tolerance limits.
#'
#'   - `limits`: A data frame containing the prediction/tolerance limits. Columns include `bias` (estimated mean difference), `SEM` (standard error of the bias), `SD` (residual standard deviation of a single difference at that row, from the variance function if one is in the model), `SEP` (standard error of prediction, `sqrt(SD^2 + SEM^2)`), `lower.CL`/`upper.CL` (confidence limits for the bias), `lower.PL`/`upper.PL` (prediction limits), and `lower.TL`/`upper.TL` (tolerance limits).
#'   - `model`: The GLS model; NULL if keep_model set to FALSE. The data (with the internal column names `x`, `y`, `delta`, `avg`, `id` (the row number if not supplied), and, if supplied, `condition` and `time`), correlation structure, and variance function are stored with the model, so `update()` and `nlme::getData()` can be used on it directly.
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
                               conf_level = 0.95,
                               tol_method = c("approx","perc"),
                               prop_bias = FALSE,
                               log_tf = FALSE,
                           log_tf_display = c("ratio", "sympercent"),
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
  log_tf_display = match.arg(log_tf_display)
  # set call ----
  call2 = match.call()
  call2$id = id
  call2$condition = condition
  call2$pred_level = pred_level
  call2$tol_level = tol_level
  call2$conf_level = conf_level
  call2$prop_bias = prop_bias
  call2$log_tf = log_tf
  call2$cor_type = cor_type
  call2$correlation = correlation
  call2$weights = weights
  call2$log_tf_display = log_tf_display
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

  model = gls_self_contained(model = model,
                             data = temp_frame,
                             cor1 = cor1,
                             var1 = var1)

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1

  res_emm = gls_emm_delta(model = model,
                          temp_frame = temp_frame,
                          avg_vals = avg_vals,
                          conf_level = conf_level)

  emm_df = tol_grid(res_emm = res_emm,
                    model = model,
                    avg_vals = avg_vals) %>%
    add_pred_limits(model = model,
                    alpha.pred = alpha.pred)

  if(tol_method == "approx"){
    emm_df = emm_df %>%
      mutate(lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
             upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df))) %>%
      rename(bias = emmean)
  }

  if(tol_method == "perc"){
    emm_df = boot_delta_gls(model = model,
                            temp_frame = temp_frame,
                            avg_vals = avg_vals,
                            emm_df = emm_df,
                            tol_level = tol_level,
                            alpha.pred = alpha.pred,
                            replicates = replicates) %>%
      rename(bias = emmean)
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
  lm_mod = plot_frame(y = df$y, x = df$x, id = df$id,
                      avg = df$avg, delta = df$delta,
                      condition = df$condition, time = df$time)
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
                         avg_vals,
                         conf_level = 0.95){
  # emmeans passes the gls call's weights (a varFunc) to model.frame when
  # recovering the data, which errors for intercept-only models. The varFunc
  # is not needed from the call (vcov and apVar come from the fitted object),
  # so drop it from this local copy.
  model$call$weights = NULL

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
  res_emm = update(res_emm, level = conf_level)
  return(res_emm)
}

# self-contained model ----
# The gls call built above refers to objects that only exist inside
# tolerance_limit() (temp_frame, cor1, var1), so update() and getData() on
# the returned model would fail. Replace them with the objects themselves.
# The data are stored in a small environment rather than inline so that
# print(model) does not deparse the whole data frame.
gls_self_contained = function(model,
                              data,
                              cor1,
                              var1){
  data_env = new.env(parent = baseenv())
  assign("tol_data", data, envir = data_env)

  model$call[[1]] = quote(nlme::gls)
  model$call$data = call("get", "tol_data", envir = data_env)
  if(!is.null(cor1)){
    model$call$correlation = cor1
  }
  if(!is.null(var1)){
    model$call$weights = var1
  }

  model
}

# bootstrap ----
boot_delta_gls = function(model,
                          temp_frame,
                          avg_vals,
                          emm_df,
                          tol_level,
                          alpha.pred,
                          replicates){
  res_df = data.frame()

  for(i in 1:replicates){
    dat2 = r_gen(dat = temp_frame,
                 mle = model)
    res_i = update(model, data = dat2)

    emm1 = gls_emm_delta(model = res_i,
                         temp_frame = dat2,
                         avg_vals = avg_vals)
    emm_df1 = tol_grid(res_emm = emm1,
                       model = res_i,
                       avg_vals = avg_vals) %>%
      add_pred_limits(model = res_i,
                      alpha.pred = alpha.pred)
    class(emm_df1) = "data.frame"
    emm_df1$boot_n = i

    res_df = rbind(res_df,emm_df1)
  }

  grp_vars = intersect(c("condition", "avg"), colnames(emm_df))

  sum_res_df = res_df %>%
    group_by(across(all_of(grp_vars))) %>%
    summarize(
      lower.TL = quantile(lower.PL, 1 - tol_level),
      upper.TL = quantile(upper.PL, tol_level),
      .groups = "drop"
    )

  if(length(grp_vars) > 0){
    sum_res_df = left_join(emm_df,
                           sum_res_df,
                           by = grp_vars)
  } else {
    sum_res_df = cbind(emm_df, sum_res_df)
  }

  return(sum_res_df)

}

# prediction grid ----
# Data frame of the emmeans grid. If the variance function depends on avg
# but the mean model does not, the grid is expanded over avg_vals so that
# the limits can vary with avg even though the bias does not.
tol_grid = function(res_emm,
                    model,
                    avg_vals){
  grid = as.data.frame(res_emm)
  class(grid) = "data.frame"
  vrSt = model$modelStruct$varStruct

  if(!is.null(vrSt) && !("avg" %in% colnames(grid))){
    var_covs = all.vars(nlme::getCovariateFormula(vrSt))
    if("avg" %in% var_covs){
      grid = merge(grid,
                   data.frame(avg = avg_vals),
                   by = NULL)
    }
  }

  return(grid)
}

# residual SD for each row of the prediction grid ----
# sigma(model) is only the SD at the reference level of the variance
# function, so the variance function has to be evaluated at each grid row.
resid_sd_grid = function(model,
                         grid){
  vrSt = model$modelStruct$varStruct

  if(is.null(vrSt)){
    return(rep(sigma(model), nrow(grid)))
  }

  # varIdent without a grouping factor is a constant variance
  if(inherits(vrSt, "varIdent") && is.null(nlme::getGroupsFormula(vrSt))){
    return(rep(sigma(model), nrow(grid)))
  }

  predict_varFunc(model, newdata = grid)
}

# prediction limits ----
add_pred_limits = function(grid,
                           model,
                           alpha.pred){
  grid %>%
    rename(SEM = SE) %>%
    mutate(SD = resid_sd_grid(model, grid),
           SEP = sqrt(SD^2 + SEM^2),
           lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
           upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)
}

r_gen <- function(dat, mle) {
  out <- dat
  out$delta <- sim_gls(mle, data = dat)
  return(out)
}

