
#' @importFrom nlme gls corCompSymm corAR1 corCAR1 varIdent
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
           upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)))
  model <- if(keep_model){
    model
  } else{
    NULL
  }

  res = list(limits = emm_df,
             model = model,
             call = call2)
  return(res)
}


data(reps)

test1 = tolerance_delta_gls(
  data = reps,
  x = "x",
  y = "y"
)
test1
test1$call$lm_mod
model.frame(test1$call$lm_mod)
test2 = agreement_limit(
  data = reps,
  x = "x",
  y = "y"
)
test1$call$lm_mod
model.frame(test1$call$lm_mod)
x = test1

data(temps)
test3 = tolerance_delta_gls(
  data = temps,
  x = "trec_pre",
  y = "teso_pre",
  id = "id",
  condition = "tod"
)

