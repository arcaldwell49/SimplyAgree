#' Calculate the Tolerance Limits from a Agreement Study
#' @description A function for calculating tolerance limits for the difference between two measurements. Approximately the same procedure as Bland-Altman limits of agreement.
#' @param x Name of column with first measurement
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier. Default is "id" if no entry is provided.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param time A column naming/numbering the time point. Only necessary if the data is from time series collection.
#' @param data Data frame with all data.
#' @param pred_level The prediction level. Default is 95\%.
#' @param tol_level The tolerance level related to the prediction level. Default is 95\%.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the prediction/tolerance limits calculations.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param cor_type The type of correlation structure. "sym" is for Compound Symmetry, "car1" is for continuous autocorrelation structure of order 1, or "ar1" for autocorrelation structure of order 1.
#' @param correlation an optional corStruct object describing the within-group correlation structure that overrides the default setting. See the documentation of corClasses for a description of the available corStruct classes. If a grouping variable is to be used, it must be specified in the form argument to the corStruct constructor. Defaults to NULL
#' @param weights an optional varFunc object or one-sided formula describing the within-group heteroscedasticity structure that overrides the default setting. If given as a formula, it is used as the argument to varFixed, corresponding to fixed variance weights. See the documentation on varClasses for a description of the available varFunc classes.
#' @param keep_model Logical indicator to retain the GLS model. Useful when working with large datasets and the model is very large.
#'
#' @return Returns single tolerance_delta class object with the results of the agreement analysis with prediction/tolerance limits..
#'
#' \describe{
#'   \item{\code{"limits"}}{A data frame containing the prediction/tolerance limits.}
#'   \item{\code{"model"}}{The GLS model; NULL if keep_model set to FALSE.}
#'   \item{\code{"call"}}{The matched call.}
#' }

#' @examples
#' data('reps')
#'
#' @section References:
#'
#' @importFrom nlme gls  corCompSymm corAR1 corCAR1 varIdent
#' @export

tolerance_delta_gls = function(data,
                               x,
                               y,
                               id = NULL,
                               condition = NULL,
                               time = NULL,
                               pred_level = 0.95,
                               tol_level = 0.95,
                               prop_bias = FALSE,
                               log_tf = FALSE,
                               cor_type = c("sym", "car1", "ar1", "none"),
                               correlation = NULL,
                               weights = NULL,
                               keep_model = TRUE){
  alpha = 1 - tol_level
  alpha.pred=1-pred_level
  # match args -----
  cor_type = match.arg(cor_type)
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

  if(!is.null(condition)){
    model = update(model,
                   . ~ . + condition,
                   weights = varIdent(form=~1|condition))

  }

  if(prop_bias){
    model = update(model,
                   . ~ . + avg)
  }

  if(!is.null(id) && cor_type != "none"){

    if(!is.null(time)){
      cor1 = switch(cor_type,
                    sym = corCompSymm(form = ~time|id),
                    car1 = corCAR1(form = ~time|id),
                    ar1 = corAR1(form= ~time|id))
    } else {
      cor1 = switch(cor_type,
                    sym = corCompSymm(form = ~1|id),
                    car1 = corCAR1(form = ~1|id),
                    ar1 = corAR1(form= ~1|id))
    }

    model = update(model,
                   correlation = cor1)
  }

  if(!is.null(weights)){
    model = update(model,
           weights = weights)
  }

  if(!is.null(correlation)){
    model = update(model,
           correlation = correlation)
  }

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1
  if(prop_bias){
    if(!is.null(condition)){

      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = c(
                                   min(temp_frame$avg),
                                   median(temp_frame$avg),
                                   max(temp_frame$avg)
                                 ))),
                        ~ condition + avg,
                        mode = "satterthwaite")
    } else{
      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = c(
                                   min(temp_frame$avg),
                                   median(temp_frame$avg),
                                   max(temp_frame$avg)
                                 ))),
                        ~ avg,
                        mode = "satterthwaite")
    }

  } else {
    if(!is.null(condition)){
      res_emm = emmeans(model,
                        ~ condition ,
                        mode = "satterthwaite")
    } else{
      res_emm = emmeans(model,
                        ~ 1 ,
                        mode = "satterthwaite")
    }
  }



  emm_df = as.data.frame(res_emm) %>%
    rename(SEM = SE) %>%
    mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
    mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
           upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP,
           lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
           upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df))) %>%
    rename(bias = emmean)
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



