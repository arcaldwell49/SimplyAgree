
simple_ident_plot = function(x,
                             x_name = "x",
                             y_name = "y",
                             smooth_method = NULL,
                             smooth_se = TRUE) {
  if(x$class != "simple"){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("x","y","id")
    if(x$class == "replicates"){
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
  }

  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  x_lab = x_name
  y_lab = y_name

  pca <- prcomp(~x+y, data = df, retx = FALSE)

  slp <- with(pca, rotation[2,1] / rotation[1,1])
  int <- with(pca, center[2] - slp*center[1])
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
                          smooth_method = NULL,
                          smooth_se = TRUE) {
  if(x$class != "simple"){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("x","y","id")

  } else{
    df = model.frame(x$call$lm_mod)
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
  conf.level = x$call$conf.level
  agree.level = x$call$agree.level
  confq = qnorm(1 - (1 - conf.level) / 2)
  delta = x$call$delta
  #smooth_method = x$smooths$smooth_method
  #smooth_se = x$smooths$smooth_se
  bland_alt.plot = ggplot(df,
         aes(x = mean, y = delta)) +
    geom_point(na.rm = TRUE) +
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

        if(x$class == "simple"){
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
            geom_ribbon(inherit.aes = FALSE,
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
        if(x$class !=
           "simple"){
          lm1 = lme4::lmer(data = df,
                           delta ~ mean + (1|id))
        } else {
          lm1 = lm(data = df,
                   delta ~ mean)
        }

        df2 = as.data.frame(ggeffects::ggemmeans(lm1, "mean"))
        if(smooth_se){
          bland_alt.plot = bland_alt.plot +
            geom_ribbon(inherit.aes = FALSE,
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
