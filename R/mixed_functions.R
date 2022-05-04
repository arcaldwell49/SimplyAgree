
# Simple BA plot from loa_mixed -------
simple_mix_plot = function(x,
                           x_label = "Average of Both Methods",
                           y_label = "Difference Between Methods",
                           geom = "geom_point",
                           smooth_method = NULL,
                           smooth_se = TRUE){
  df_plt = model.frame(x$call$lm_mod)
  colnames(df_plt) = c("diff", "X", "id")
  df = df_plt
  colnames(df) = c("delta", "mean", "id")
  df_loa = x$loa

  scalemin = min(df_plt$X)
  scalemax = max(df_plt$X)
  pd2 = position_dodge2(.03 * (scalemax - scalemin))

  df_loa2 = df_loa
  df_loa2$x = scalemin - (.03 * (scalemax - scalemin))
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))

  conf.level = x$call$conf.level
  agree.level = x$call$agree.level

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
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
    labs(x = x_label,
         y = y_label,
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")

  if (!is.null(x$call$delta)){
    delta = x$call$delta
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


          gam1 = mgcv::gam(data = df,
                           delta ~ s(mean, bs = "tp") + s(id, bs="re"))


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

          lm1 = lme4::lmer(data = df,
                           delta ~ mean + (1|id))

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
      } else {
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

# Prop bias BA plot from loa_mixed ------

bias_mix_plot = function(x,
                         x_label = "Average of Both Methods",
                         y_label = "Difference Between Methods",
                         geom = "geom_point",
                         smooth_method = NULL,
                         smooth_se = TRUE){
  df_plt = model.frame(x$call$lm_mod)
  colnames(df_plt) = c("diff", "X", "id")

  df_loa = x$loa

  scalemin = min(df_plt$X)
  scalemax = max(df_plt$X)
  pd2 = position_dodge2(.03 * (scalemax - scalemin))

  df_loa2 = df_loa
  df_loa2$x = scalemin - (.03 * (scalemax - scalemin))
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))

  conf.level = x$call$conf.level
  agree.level = x$call$agree.level

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = X, y = diff)) +
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
    labs(x = x_label,
         y = y_label,
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")

  return(bland_alt.plot)
}

# Miscellaneous for loa_mixed functions

loa_bs = function(diff,
                  condition,
                  id,
                  data,
                  formula = NULL,
                  conf.level,
                  agree.level,
                  indices){

  limits = qnorm(1 - (1 - conf.level) / 2)
  agree.lim = qnorm(1 - (1 - agree.level) / 2)

  if(is.null(formula)){
    formula1 = as.formula(paste0(diff,"~",condition,"+(1|",id,")"))
  } else {
    formula1 = formula
  }


  datboot <- data[indices,] # allows boot to select sample

  res3 = lmer(formula1,
              data = datboot,
              weights = NULL,
              subset = NULL,
              offset = NULL,
              na.action = na.omit)

  mean = as.data.frame(emmeans(res3, ~1))$emmean
  #se = as.data.frame(emmeans(res3, ~1))$SE
  vartab = as.data.frame(VarCorr(res3))
  withinsd = vartab$sdcor[2]
  betweensd <- vartab$sdcor[1]
  totalsd <- sqrt(vartab$vcov[1] + vartab$vcov[2])

  # 95% Limits of agreement
  low <- mean - agree.lim * totalsd
  upper <- mean + agree.lim * totalsd
  # cat(cl*100,"% LoA are from",low,"to",upper,"\n")
  c(bias = mean,
    low_loa = low,
    upper_loa = upper,
    within_sd = withinsd,
    between_sd = betweensd,
    total_sd = totalsd)
}

loa_bstab = function(bsls,
                     type ,
                     conf.level){
  if (type == "bca" || type == "basic" || type == "perc") {
    l_cl = 4
    h_cl = 5
  } else if (type == "norm"){
    l_cl = 2
    h_cl = 3
  }

  if (type == "bca"){
    conf_bias = bsls$boot_bias$bca[l_cl:h_cl]
    conf_within_sd = bsls$boot_within_sd$bca[l_cl:h_cl]
    conf_between_sd = bsls$boot_between_sd$bca[l_cl:h_cl]
    conf_total_sd = bsls$boot_total_sd$bca[l_cl:h_cl]
    conf_low_loa = bsls$boot_low_loa$bca[l_cl:h_cl]
    conf_upper_loa = bsls$boot_upper_loa$bca[l_cl:h_cl]
  } else if(type == "norm"){
    conf_bias = bsls$boot_bias$norm[l_cl:h_cl]
    conf_within_sd = bsls$boot_within_sd$norm[l_cl:h_cl]
    conf_between_sd = bsls$boot_between_sd$norm[l_cl:h_cl]
    conf_total_sd = bsls$boot_total_sd$norm[l_cl:h_cl]
    conf_low_loa = bsls$boot_low_loa$norm[l_cl:h_cl]
    conf_upper_loa = bsls$boot_upper_loa$norm[l_cl:h_cl]
  } else if(type == "perc"){
    conf_bias = bsls$boot_bias$perc[l_cl:h_cl]
    conf_within_sd = bsls$boot_within_sd$perc[l_cl:h_cl]
    conf_between_sd = bsls$boot_between_sd$perc[l_cl:h_cl]
    conf_total_sd = bsls$boot_total_sd$perc[l_cl:h_cl]
    conf_low_loa = bsls$boot_low_loa$perc[l_cl:h_cl]
    conf_upper_loa = bsls$boot_upper_loa$perc[l_cl:h_cl]
  } else if (type == "basic"){
    conf_bias = bsls$boot_bias$basic[l_cl:h_cl]
    conf_within_sd = bsls$boot_within_sd$basic[l_cl:h_cl]
    conf_between_sd = bsls$boot_between_sd$basic[l_cl:h_cl]
    conf_total_sd = bsls$boot_total_sd$basic[l_cl:h_cl]
    conf_low_loa = bsls$boot_low_loa$basic[l_cl:h_cl]
    conf_upper_loa = bsls$boot_upper_loa$basic[l_cl:h_cl]
  }
  conf_dat = data.frame(row.names = c(
    "Mean Bias",
    "Lower LoA",
    "Upper LoA",
    "Within SD",
    "Between SD",
    "Total SD"
  ),
  estimate = c(
    bsls$boot_bias$t0,
    bsls$boot_low_loa$t0,
    bsls$boot_upper_loa$t0,
    bsls$boot_within_sd$t0,
    bsls$boot_between_sd$t0,
    bsls$boot_total_sd$t0
  ),
  lower.ci = c(
    conf_bias[1],
    conf_low_loa[1],
    conf_upper_loa[1],
    conf_within_sd[1],
    conf_between_sd[1],
    conf_total_sd[1]
  ),
  upper.ci = c(
    conf_bias[2],
    conf_low_loa[2],
    conf_upper_loa[2],
    conf_within_sd[2],
    conf_between_sd[2],
    conf_total_sd[2]
  ))
}


# For prop bias or condtions ------

loa_bs = function(diff,
                  condition,
                  id,
                  data,
                  formula = NULL,
                  conf.level,
                  agree.level,
                  indices){

  limits = qnorm(1 - (1 - conf.level) / 2)
  agree.lim = qnorm(1 - (1 - agree.level) / 2)

  if(is.null(formula)){
    formula1 = as.formula(paste0(diff,"~",condition,"+(1|",id,")"))
  } else {
    formula1 = formula
  }


  datboot <- data[indices,] # allows boot to select sample

  res3 = lmer(formula1,
              data = datboot,
              weights = NULL,
              subset = NULL,
              offset = NULL,
              na.action = na.omit)

  mean = as.data.frame(emmeans(res3, ~1))$emmean
  #se = as.data.frame(emmeans(res3, ~1))$SE
  vartab = as.data.frame(VarCorr(res3))
  withinsd = vartab$sdcor[2]
  betweensd <- vartab$sdcor[1]
  totalsd <- sqrt(vartab$vcov[1] + vartab$vcov[2])

  # 95% Limits of agreement
  low <- mean - agree.lim * totalsd
  upper <- mean + agree.lim * totalsd
  # cat(cl*100,"% LoA are from",low,"to",upper,"\n")
  c(bias = mean,
    low_loa = low,
    upper_loa = upper,
    within_sd = withinsd,
    between_sd = betweensd,
    total_sd = totalsd)
}
