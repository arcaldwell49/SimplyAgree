
simple_ident_plot = function(x,
                             x_name = "x",
                             y_name = "y",
                             smooth_method = NULL,
                             smooth_se = TRUE) {
  if(as.character(x$call[1]) %in% c("agree_nest", "agree_reps",
                                    "SimplyAgree::agree_nest", "SimplyAgree::agree_reps")){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x","id")
    if(as.character(x$call[1]) == "agree_reps" | as.character(x$call[1]) == "SimplyAgree::agree_reps"){
    df = df %>%
      group_by(id) %>%
      summarize(mxi = sum(!is.na(x)),
                myi = sum(!is.na(y)),
                x = mean(x, na.rm=TRUE),
                x_var = var(x, na.rm=TRUE),
                y = mean(y, na.rm=TRUE),
                y_var = var(y, na.rm=TRUE),
                .groups = "drop")
    }

  } else{
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y", "x")
  }

  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  x_lab = x_name
  y_lab = y_name

  dems <- calc_dem(df$x, df$y, w_i = rep(1, length(df$y)), error.ratio = 1)

  slp <- dems$b1
  int <- dems$b0
  tmp.lm <- data.frame(the_int = int, the_slope = slp)

  p1 = ggplot(df,aes(x = x,
                y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0,
                slope = 1) +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab(paste0("Method: ",x_lab)) +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab(paste0("Method: ",y_lab)) +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  return(p1)
}


simple_ba_plot = function(x,
                          x_name = "x",
                          y_name = "y",
                          geom = "geom_point",
                          smooth_method = NULL,
                          smooth_se = TRUE) {

  if(as.character(x$call[1]) %in% c("agree_nest", "agree_reps",
                                    "SimplyAgree::agree_nest", "SimplyAgree::agree_reps")){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x","id")

  } else{
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x")
  }
  df_loa = x$loa

  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  pd2 = position_dodge2(.03*(scalemax-scalemin))
  x_lab = x_name
  y_lab = y_name
  df_loa2 = df_loa
  df_loa2$x = scalemin
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                       levels = c("Upper LoA", "Bias", "Lower LoA"))
  df$mean = (df$x + df$y)/2
  df$delta = df$x - df$y
  conf.level = get_call(x$call$conf.level)
  agree.level = get_call(x$call$agree.level)
  confq = qnorm(1 - (1 - get_call(x$call$conf.level)) / 2)
  delta = get_call(x$call$delta)
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

  if(x$call$TOST){
    cap1 = paste0("Agreement = ",
                  agree.level * 100,
                  "% \n",
                  "Confidence Level = ",
                  conf.level * 100, "% (Bias) & ",
                  (1-(1-conf.level)*2) * 100, "% (LoA)")
  } else {
    cap1 = paste0("Agreement = ",
                  agree.level * 100,
                  "% \n",
                  "Confidence Level = ",
                  conf.level * 100,
                  "%")
  }


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
         y = paste0("Difference between Methods ",x_lab ," & ", y_lab),
         caption = cap1,
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")
  if (!is.null(get_call(x$call$delta))){
    delta = get_call(x$call$delta)
    df_delta = data.frame(y1 = c(delta, -1*delta))
    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = c(delta, -1*delta),
        name = "Maximal Allowable Difference"))
  }
  if (!is.null(smooth_method)){
    if (!(smooth_method %in% c("loess", "lm", "gam"))){
      stop("Only lm, loess, and gam are supported as smooth_method at this time.")
    }
    if(smooth_method == "gam"){
      if (requireNamespace(c("mgcv","ggeffects"), quietly = TRUE)) {

        if(as.character(x$call[1]) %in% c("agree_test", "agree_np",
                                          "SimplyAgree::agree_test", "SimplyAgree::agree_np") ){
          gam1 = mgcv::gam(data = df,
                           delta ~ s(mean))
        } else {
          gam1 = mgcv::gam(data = df,
                           delta ~ s(mean, bs = "tp") + s(id, bs="re"))
        }

        df2 = data.frame(mean = seq(min(df$mean, na.rm=TRUE),
                                        max(df$mean, na.rm=TRUE),
                                        length.out = 100))
        df2 = as.data.frame(ggeffects::ggemmeans(gam1, "mean"))

        if(smooth_se){
          bland_alt.plot = bland_alt.plot +
            geom_ribbon(#inherit.aes = FALSE,
                        data = df2,
                        alpha = .2,
                        aes(x=x, ymin=conf.low,ymax=conf.high))
        }
        bland_alt.plot = bland_alt.plot +
          geom_line(inherit.aes = FALSE,
                    color = "#3aaf85",
                    data = df2,
                    aes(x=x,y=predicted))
      } else {
        message("For gam smooths, the mgcv & ggeffects package must be installed.")
      }
    } else if(smooth_method == "lm"){
      if (requireNamespace("ggeffects", quietly = TRUE)) {
        if(!(as.character(x$call[1]) %in%
           c("agree_test", "agree_np",
             "SimplyAgree::agree_test", "SimplyAgree::agree_np"))){
          lm1 = lme4::lmer(data = df,
                           delta ~ mean + (1|id))
        } else {
          lm1 = lm(data = df,
                   delta ~ mean)
        }

        df2 = as.data.frame(ggeffects::ggemmeans(lm1, "mean"))
        if(smooth_se){
          bland_alt.plot = bland_alt.plot +
            geom_ribbon(#inherit.aes = FALSE,
                        data = df2,
                        alpha = .2,
                        aes(x=x, ymin=conf.low,ymax=conf.high))
        }
        bland_alt.plot = bland_alt.plot +
          geom_line(inherit.aes = FALSE,
                    color = "#3aaf85",
                    data = df2,
                    aes(x=x,y=predicted))
      }else {
        message("For lm smooths, the ggeffects package must be installed")
      }
    } else if(smooth_method == "loess") {
      bland_alt.plot = bland_alt.plot +
        stat_smooth(
          method = "loess",
          se = smooth_se,
          level = conf.level,
          alpha = 0.2,
          formula = y ~ x,
          size = 0.8,
          colour = "#3aaf85"
        )
    }

  }

  return(bland_alt.plot)

}


bias_ba_plot = function(x,
                        x_name = "x",
                        y_name = "y",
                        geom = "geom_point",
                        smooth_method = NULL,
                        smooth_se = TRUE){

  if(as.character(x$call[1]) %in% c("agree_nest", "agree_reps", "SimplyAgree::agree_nest", "SimplyAgree::agree_reps")){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x","id")

  } else{
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x")
  }

  agree.level = all_of(x$call$agree.level)

  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2

  df$mean = (df$x + df$y)/2
  df$delta = df$x - df$y
  conf.level = get_call(x$call$conf.level)

  confq = qnorm(1 - (1 - conf.level) / 2)
  delta = get_call(x$call$delta)

  if(as.character(x$call[1]) == "agree_np" | as.character(x$call[1]) == "SimplyAgree::agree_np"){
    quan_mod = rq(formula =  delta ~ mean,
                  data = df,
                  tau = c(agree.u,.5,agree.l))
    emm = get_qemm(quan_mod,
                   df,
                   agree.l,
                   agree.u,
                   conf.level)

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

    if(smooth_se == TRUE) {

    bland_alt.plot = bland_alt.plot +
      geom_ribbon(data = emm,
                  alpha = .2,
                  aes(y=estimate,
                      ymax=upper.ci,
                      ymin=lower.ci,
                      x= at,
                      fill=text)) +
      scale_fill_viridis_d(option = "C", end = .8)
    }

    if(x$call$TOST){
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100, "% (Bias) & ",
                    (1-(1-conf.level)*2) * 100, "% (LoA)")
    } else {
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100,
                    "%")
    }
    bland_alt.plot = bland_alt.plot +
      geom_line(inherit.aes = FALSE,
                data = emm,
                aes(y=estimate,
                    x= at,
                    color=text)) +

      scale_color_viridis_d(option = "C", end = .8) +
      labs(x = paste0("Average of ", x_name ," & ", y_name),
           y = paste0("Difference between Methods ",x_name ," & ", y_name),
           caption = cap1,
           guides = "") +
      theme_bw() +
      theme(legend.position = "left",
            legend.title = element_blank())

    delta = get_call(x$call$delta)
    df_delta = data.frame(y1 = c(delta, -1*delta))
    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 #inherit.aes = FALSE,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = c(delta, -1*delta),
        name = "Maximal Allowable Difference"))
  } else if(as.character(x$call[1]) == "agree_test" | as.character(x$call[1]) == "SimplyAgree::agree_test"){
    lm_mod = lm(delta ~ mean,
                data = df)
    emm = simple_emm(lm_mod,
                     df,
                     agree.level,
                     conf.level,
                     TOST = get_call(x$call$TOST),
                     var_comp = x$var_comp)

    bland_alt.plot = ggplot(df,
                            aes(x=mean,
                                y=delta)) +
      geom_point()
    if(smooth_se == TRUE){
    bland_alt.plot = bland_alt.plot +
      geom_ribbon(data = emm,
                  alpha = .2,
                  aes(y=estimate,
                      ymax=upper.ci,
                      ymin=lower.ci,
                      x= at,
                      fill=text)) +
      scale_fill_viridis_d(option = "C", end = .8)
    }

    if(x$call$TOST){
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100, "% (Bias) & ",
                    (1-(1-conf.level)*2) * 100, "% (LoA)")
    } else {
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100,
                    "%")
    }
    bland_alt.plot = bland_alt.plot +
      geom_line(inherit.aes = FALSE,
                data = emm,
                aes(y=estimate,
                    x= at,
                    color=text)) +
      scale_color_viridis_d(option = "C", end = .8) +
      labs(x = paste0("Average of ", x_name ," & ", y_name),
           y = paste0("Difference between Methods ",x_name ," & ", y_name),
           caption = cap1,
           guides = "") +
      theme_bw() +
      theme(legend.position = "left",
            legend.title = element_blank())
    if(!is.null(x$call$delta)) {
      delta = all_of(x$call$delta)
      df_delta = data.frame(y1 = c(delta, -1*delta))
      bland_alt.plot = bland_alt.plot +
        geom_hline(data = df_delta,
                   #inherit.aes = FALSE,
                   aes(yintercept = y1),
                   linetype = 2) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = c(delta, -1*delta),
          name = "Maximal Allowable Difference"))
    }

  } else {

    lmer_mod = lmer(delta ~ mean + (1 | id),
                    data = df)
    emm = mover_emm(lmer_mod,
                    df,
                    agree.level,
                    conf.level,
                    TOST = x$call$TOST,
                    var_comp = x$var_comp)

    bland_alt.plot = ggplot(df,
                            aes(x=mean,
                                y=delta)) +
      geom_point()
    if(smooth_se == TRUE){
      bland_alt.plot = bland_alt.plot +
        geom_ribbon(data = emm,
                    alpha = .2,
                    aes(y=estimate,
                        ymax=upper.ci,
                        ymin=lower.ci,
                        x= at,
                        fill=text)) +
        scale_fill_viridis_d(option = "C", end = .8)
    }

    if(x$call$TOST){
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100, "% (Bias) & ",
                    (1-(1-conf.level)*2) * 100, "% (LoA)")
    } else {
      cap1 = paste0("Agreement = ",
                    agree.level * 100,
                    "% \n",
                    "Confidence Level = ",
                    conf.level * 100,
                    "%")
    }
    bland_alt.plot = bland_alt.plot +
      geom_line(inherit.aes = FALSE,
                data = emm,
                aes(y=estimate,
                    x= at,
                    color=text)) +
      scale_color_viridis_d(option = "C", end = .8) +
      labs(x = paste0("Average of ", x_name ," & ", y_name),
           y = paste0("Difference between Methods ",x_name ," & ", y_name),
           caption = cap1,
           guides = "") +
      theme_bw() +
      theme(legend.position = "left",
            legend.title = element_blank())
    if(!is.null(all_of(x$call$delta))) {
      delta = all_of(x$call$delta)
      df_delta = data.frame(y1 = c(delta, -1*delta))
      bland_alt.plot = bland_alt.plot +
        geom_hline(data = df_delta,
                   #inherit.aes = FALSE,
                   aes(yintercept = y1),
                   linetype = 2) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = c(delta, -1*delta),
          name = "Maximal Allowable Difference"))
    }
    }

  return(bland_alt.plot)

}
