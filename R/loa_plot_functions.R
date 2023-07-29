
simple_loa_plot = function(x,
                           geom = "geom_point",
                           delta = NULL) {
  call2 = x$call
  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y","x","id","mean","delta")

  df_loa = x$loa
  if(x$call$log){
    df_loa = exp(df_loa)
    df$delta = exp(df$delta)
  }
  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  pd2 = position_dodge2(.03*(scalemax-scalemin))
  x_lab = "x"
  y_lab = "y"
  df_loa2 = data.frame(
    text = factor(c("Upper LoA", "Bias", "Lower LoA"),
                  levels = c("Upper LoA", "Bias", "Lower LoA"),
                  ordered  = TRUE),
    estimate = c(df_loa$upper_loa, df_loa$bias, df_loa$lower_loa),
    lower.ci = c(df_loa$upper_loa, df_loa$lower.CL, df_loa$lower_loa_ci),
    upper.ci = c(df_loa$upper_loa_ci, df_loa$upper.CL, df_loa$lower_loa)
  )

  df_loa2$x = scalemin

  conf.level = get_call(x$call$conf.level)
  agree.level = get_call(x$call$agree.level)
  confq = qnorm(1 - (1 - get_call(x$call$conf.level)) / 2)
  delta = delta
  #smooth_method = x$smooths$smooth_method
  #smooth_se = x$smooths$smooth_se
  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      stat_density_2d(na.rm = TRUE,
                      geom = "polygon",
                      contour = TRUE,
                      aes(fill = after_stat(level)),
                      contour_var = "ndensity",
                      colour = "black",) +
      scale_fill_distiller(palette = "Blues", direction = 1)
  }  else {
    stop("geom option not supported")
  }


  cap1 = paste0(
    "Agreement = ",
    agree.level * 100,
    "% \n",
    "Confidence Level = ",
    conf.level * 100,
    "% (Bias) & ",
    (1 - (1 - conf.level) * 2) * 100,
    "% (LoA)"
  )



  bland_alt.plot = bland_alt.plot +
    geom_pointrange(data = df_loa2,
                    aes(
                      x = x,
                      y = estimate,
                      ymin = lower.ci,
                      ymax = upper.ci,
                      color = text),
                    #width = .03*(scalemax-scalemin),
                    position = pd2,
                    inherit.aes = FALSE)+
    labs(x = paste0("Average of ", x_lab ," & ", y_lab),
         y = ifelse(call2$log,
                    paste0("Ratio of Methods (x/y)"),
                    paste0("Difference between Methods (x - y)")),
         caption = cap1,
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")
  if (!is.null(delta)){
    #delta = get_call(x$call$delta)
    if(length(delta) == 1){
      df_delta = data.frame(y1 = c(delta, -1*delta))
    } else{
      df_delta = data.frame(y1 = c(delta[1], delta[2]))
    }

    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = df_delta$y1,
        name = "Maximal Allowable Difference"))
  }


  return(bland_alt.plot)

}


bias_loa_plot = function(x,
                         geom = "geom_point",
                         delta = NULL){
  call2 = x$call
  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y","x","id","mean","delta")
  df_loa = x$loa
  if(x$call$log){
    df_loa = exp(df_loa)
    df$delta = exp(df$delta)
  }
  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  pd2 = position_dodge2(.03*(scalemax-scalemin))
  x_lab = "x"
  x_name = "x"
  y_name = "y"
  y_lab = "y"
  df_loa2 = data.frame(
    text = factor(c(rep("Upper LoA",3),
                    rep("Bias",3),
                    rep("Lower LoA",3)),
                  levels = c("Upper LoA", "Bias", "Lower LoA"),
                  ordered  = TRUE),
    estimate = c(df_loa$upper_loa, df_loa$bias, df_loa$lower_loa),
    lower.ci = c(df_loa$upper_loa, df_loa$lower.CL, df_loa$lower_loa_ci),
    upper.ci = c(df_loa$upper_loa_ci, df_loa$upper.CL, df_loa$lower_loa)
  )
  df_loa2$x = df_loa$avg

  #df_loa2$x = scalemin

  conf.level = get_call(x$call$conf.level)
  agree.level = get_call(x$call$agree.level)
  confq = qnorm(1 - (1 - get_call(x$call$conf.level)) / 2)
  delta = delta

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      stat_density_2d(na.rm = TRUE,
                      geom = "polygon",
                      contour = TRUE,
                      aes(fill = after_stat(level)),
                      contour_var = "ndensity",
                      colour = "black",) +
      scale_fill_distiller(palette = "Blues", direction = 1)
  }  else {
    stop("geom option not supported")
  }

  cap1 = paste0("Agreement = ",
                agree.level * 100,
                "% \n",
                "Confidence Level = ",
                conf.level * 100,
                "%")

    bland_alt.plot = bland_alt.plot +
      geom_ribbon(inherit.aes = FALSE,
                  alpha = .2,
                data = df_loa2,
                aes(y=estimate,
                    ymin = lower.ci,
                    ymax = upper.ci,
                    x=x,
                    fill=text)) +
      geom_line(inherit.aes = FALSE,
                data = df_loa2,
                aes(y=estimate,
                    x=x,
                    color=text)) +
      scale_color_viridis_d(option = "C", end = .8) +
      scale_fill_viridis_d(option = "C", end = .8) +
      labs(x = paste0("Average of ", x_name ," & ", y_name),
           y = ifelse(call2$log,
                      paste0("Ratio of Methods (x/y)"),
                      paste0("Difference between Methods (x - y)")),
           caption = cap1,
           guides = "") +
      theme_bw() +
      theme(legend.position = "left",
            legend.title = element_blank())
    if(!is.null(delta)) {

      if(length(delta) == 1){
        df_delta = data.frame(y1 = c(delta, -1*delta))
      } else{
        df_delta = data.frame(y1 = c(delta[1], delta[2]))
      }
      bland_alt.plot = bland_alt.plot +
        geom_hline(data = df_delta,
                   #inherit.aes = FALSE,
                   aes(yintercept = y1),
                   linetype = 2) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = c(delta, -1*delta),
          name = "Maximal Allowable Difference"))
    }


  return(bland_alt.plot)

}
