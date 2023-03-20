
#' @importFrom nlme gls corCompSymm corAR1 corCAR1 varIdent
tolerance_delta_gls = function(data,
                               x,
                               y,
                               id = NULL,
                               condition = NULL,
                               time = NULL,
                               tol.level = 0.95,
                               alpha = 0.05,
                               prop_bias = FALSE,
                               log_tf = FALSE,
                               cor_type = c("sym", "car1", "ar1", "none"),
                               correlation = NULL,
                               weights = NULL){
  # match args -----
  cor_type = match.arg(cor_type)
  # set call ----
  call2 = match.call()
  call2$id = id
  call2$condition = condition
  call2$tol.level = tol.level
  call2$alpha = alpha
  call2$prop_bias = prop_bias
  call2$log_tf = log_tf
  call2$cor_type = cor_type
  call2$correlation = correlation
  call2$weights = weights

  # organize data -----
  df = data[c(x,y,id,condition,time)]
  colnames(df) = c(x,y,id,condition,time)
  df = na.omit(df)
  df$avg = (df$x + df$y)/2
  if(log_tf){
    df$x = log(df$x)
    df$y = log(df$y)
  }
  df$delta = df$x - df$y

  # MODEL ----
  model = gls(delta ~ 1, data = df)

  if(!is.null(condition)){
    model = update(model,
                   . ~ . + condition,
                   weights = varIdent(form=~1|condition))

  }

  if(!is.null(prop_bias)){
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

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1
  if(prop_bias){
    if(!is.null(condition)){

    }

  } else {
    if(!is.null(condition)){

    }
  }
}
