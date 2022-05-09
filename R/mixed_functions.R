
# Simple BA plot from loa_mixed -------
simple_mix_plot = function(x,
                           x_label = "Average of Both Methods",
                           y_label = "Difference Between Methods",
                           geom = "geom_point",
                           smooth_method = NULL,
                           smooth_se = TRUE){

  if(x$call$condition == 1){
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id")
    df_loa = x$loa
  } else {
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id", "condition")

    df_loa = x$loa
  }


  scalemin = min(df_plt$avg)
  scalemax = max(df_plt$avg)
  pd2 = position_dodge2(.03 * (scalemax - scalemin))

  df_loa2 = df_loa
  df_loa2$x = scalemin - (.03 * (scalemax - scalemin))
  df_loa2$term = factor(df_loa2$term,
                        levels = c("Upper LoA", "Bias", "Lower LoA"),
                        ordered = TRUE)
  conf.level = x$call$conf.level
  agree.level = x$call$agree.level

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
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

  if(x$call$condition == 1){
    bland_alt.plot = bland_alt.plot +
      geom_pointrange(data = df_loa2,
                      aes(
                        x = x,
                        y = estimate,
                        ymin = lower.ci,
                        ymax = upper.ci,
                        color = term),
                      #width = .03*(scalemax-scalemin),
                      position = pd2,
                      inherit.aes = FALSE)
  } else{
    bland_alt.plot = bland_alt.plot +
      facet_wrap(~condition) +
      geom_pointrange(data = df_loa2,
                      aes(
                        x = x,
                        y = estimate,
                        ymin = lower.ci,
                        ymax = upper.ci,
                        color = term),
                      #width = .03*(scalemax-scalemin),
                      position = pd2)
  }
  bland_alt.plot = bland_alt.plot +
    labs(x = x_label,
         y = y_label,
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")

  #if (!is.null(x$call$delta)){
  #  delta = x$call$delta
  #  df_delta = data.frame(y1 = c(delta, -1*delta))
  #  bland_alt.plot = bland_alt.plot +
  #    geom_hline(data = df_delta,
  #               aes(yintercept = y1),
  #               linetype = 2) +
  #    scale_y_continuous(sec.axis = dup_axis(
  #      breaks = c(delta, -1*delta),
  #      name = "Maximal Allowable Difference"))
  #}
  if (!is.null(smooth_method)){
    if (!(smooth_method %in% c("loess", "lm", "gam"))){
      stop("Only lm, loess, and gam are supported as smooth_method at this time.")
    }
    if(smooth_method == "gam"){
      if (requireNamespace(c("mgcv","ggeffects"), quietly = TRUE)) {


          gam1 = mgcv::gam(data = df_plt,
                           delta ~ s(avg, bs = "tp") + s(id, bs="re"))


        df2 = data.frame(mean = seq(min(df$avg, na.rm=TRUE),
                                    max(df$avg, na.rm=TRUE),
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

          lm1 = lme4::lmer(data = df_plt,
                           delta ~ avg + (1|id))

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

bias_mix_plot = function(x,
                         x_label = "Average of Both Methods",
                         y_label = "Difference Between Methods",
                         geom = "geom_point",
                         smooth_se = TRUE){
  if(x$call$condition == 1){
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id")
    df_loa = x$loa
  } else {
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id", "condition")

    df_loa = x$loa
  }


  scalemin = min(df_plt$avg)
  scalemax = max(df_plt$avg)
  pd2 = position_dodge2(.03 * (scalemax - scalemin))

  df_loa2 = df_loa
  df_loa2$x = scalemin - (.03 * (scalemax - scalemin))
  df_loa2$term = factor(df_loa2$term,
                        levels = c("Upper LoA", "Bias", "Lower LoA"),
                        ordered = TRUE)

  conf.level = x$call$conf.level
  agree.level = x$call$agree.level

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df_plt,
                            aes(x = avg, y = diff)) +
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

  if(x$call$condition != 1){
    bland_alt.plot = bland_alt.plot +
      facet_wrap(~condition)
  }

  if(smooth_se == TRUE){
    bland_alt.plot = bland_alt.plot +
      geom_ribbon(
        data = df_loa2,
        alpha = .2,
        aes(
          y = estimate,
          ymax = upper.ci,
          ymin = lower.ci,
          x = avg,
          fill = term
        )
      ) +
      scale_fill_viridis_d(option = "C", end = .8)
  }

  bland_alt.plot = bland_alt.plot +
    geom_line(inherit.aes = FALSE,
              data = df_loa2,
              aes(x = avg,
                  y = estimate,
                  #ymin = lower.ci,
                  #ymax = upper.ci,
                  color = term)) +
    scale_color_viridis_d(option = "C", end = .8) +
    labs(x = x_label,
         y = y_label,
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         guides = "") +
    theme_bw() +
    theme(legend.position = "left",
          legend.title = element_blank())

  return(bland_alt.plot)
}

# Miscellaneous for loa_mixed functions --------

loa_bs = function(diff,
                  condition,
                  id,
                  data,
                  conf.level,
                  agree.level,
                  indices){

  limits = qnorm(1 - (1 - conf.level) / 2)
  agree.lim = qnorm(1 - (1 - agree.level) / 2)

  formula = as.formula(paste0(diff,"~",condition,"+(1|",id,")"))

  datboot <- data[indices,] # allows boot to select sample

  res3 = lmer(formula,
              data = datboot,
              weights = NULL,
              subset = NULL,
              offset = NULL,
              na.action = na.omit)

  mean = as.data.frame(emmeans(res3, ~1))$emmean
  se = as.data.frame(emmeans(res3, ~1))$SE
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

loa_bs2 = function(diff,
                   condition = NULL,
                   id,
                   avg,
                   data,
                   avgvals,
                   conf.level = .95,
                   agree.level = .95,
                   prop_bias = FALSE){

  limits = qnorm(1 - (1 - conf.level) / 2)
  agree.lim = qnorm(1 - (1 - agree.level) / 2)
  avg_vals = avgvals
  df = data[c(diff,avg,condition,id)]
  if(is.null(condition) || condition == 1){
    condition = 1
  }
  colnames(df) = c("diff", "avg", "condition", "id")
  if(prop_bias){
    #avg_vals = c(min(df$avg, na.rm = TRUE),
    #             median(df$avg, na.rm = TRUE),
    #             max(df$avg, na.rm = TRUE))
    if(condition != 1){
      formula1 = as.formula(diff ~ avg + condition +(1|id))
      specs1 = c("avg","condition")
    } else{
      formula1 = as.formula(diff ~ avg +(1|id))
      specs1 = c("avg", 1)
    }
    at_list = list(avg = avgvals)

  } else {
    if(condition != 1){
      formula1 = as.formula(diff ~ condition + (1|id))
    }

    specs1 = c("condition")
  }

  res3 = lmer(formula1,
              data = df,
              weights = NULL,
              subset = NULL,
              offset = NULL,
              na.action = na.omit)

  vartab = as.data.frame(VarCorr(res3))
  withinsd = vartab$sdcor[2]
  betweensd <- vartab$sdcor[1]
  totalsd <- sqrt(vartab$vcov[1] + vartab$vcov[2])

  if(prop_bias) {
    emm_tab = emmeans(res3,
                      specs=specs1,
                      at=at_list) %>%
      as.data.frame()
    colnames(emm_tab) = c("avg", "condition", "mean", "se", "df", "lower.CL", "upper.CL")
    emm_tab = emm_tab %>%
      select(avg, condition,mmean) %>%
      mutate(withinsd = withinsd,
             betweensd = betweensd,
             totalsd = totalsd)
  } else {
    emm_tab = emmeans(res3,
                      specs=specs1) %>%
      as.data.frame()
    colnames(emm_tab) = c("condition", "mean", "se", "df", "lower.CL", "upper.CL")
    emm_tab$avg = avg_vals
    emm_tab = emm_tab %>%
      select(avg, condition, mean) %>%
      mutate(withinsd = withinsd,
             betweensd = betweensd,
             totalsd = totalsd,
             low = mean - agree.lim * totalsd,
             high = mean + agree.lim * totalsd)
  }

  #mean = as.data.frame(emmeans(res3, ~1))$emmean
  # 95% Limits of agreement
  #emm_tab$low <- mean - agree.lim * totalsd
  #emm_tab$upper <- mean + agree.lim * totalsd
  # cat(cl*100,"% LoA are from",low,"to",upper,"\n")
  return(emm_tab)
}

# Secondary bootstrap function ----

boot_loa_mix = function(x.df,
                        nboot = 499,
                        condition = NULL,
                        id,
                        diff,
                        avg,
                        conf.level,
                        agree.level,
                        prop_bias = FALSE){
  if(is.null(condition)){
    condition = 1
  }
  len = nrow(x.df)
  if(prop_bias){
    avgvals = c(min(x.df$avg, na.rm = TRUE),
                 median(x.df$avg, na.rm = TRUE),
                 max(x.df$avg, na.rm = TRUE))
  } else{
    avgvals = c(median(x.df$avg, na.rm = TRUE))
  }
  mod1 = loa_bs2(diff,
                 condition,
                 id,
                 avg,
                 data = df2,
                 avgvals,
                 conf.level,
                 agree.level,
                 prop_bias)
  u <- list()
  for (i in 1:nboot) {
    mysample = sample(1:len,replace=TRUE)
    df2 = x.df[mysample,]
    u <- append(u,list(loa_bs2(diff,
                               condition,
                               id,
                               avg,
                               data = df2,
                               avgvals,
                               conf.level,
                               agree.level,
                               prop_bias)))
  }
  lconf = (1 - conf.level)/2
  uconf = 1-(1 - conf.level)/2
  df_boot = bind_rows(u, .id = "nboot")


  df_var = df_boot %>%
      select(nboot, avg, condition, withinsd, betweensd, totalsd)

  df_var_within = df_var %>%
    group_by(avg, condition) %>%
    summarize(boot_est = quantile(withinsd, .5, na.rm = TRUE),
              lower.ci = quantile(withinsd, lconf, na.rm = TRUE),
              upper.ci = quantile(withinsd, uconf, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(value = "SD within") %>%
    .[1,]  %>%
    mutate(est = mod1$withinsd[1]) %>%
    select(value,est, boot_est, lower.ci, upper.ci)

  df_var_between = df_var %>%
    group_by(avg, condition) %>%
    summarize(boot_est = quantile(betweensd, .5, na.rm = TRUE),
              lower.ci = quantile(betweensd, lconf, na.rm = TRUE),
              upper.ci = quantile(betweensd, uconf, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(value = "SD between") %>%
    .[1,]  %>%
    mutate(est = mod1$betweensd[1]) %>%
    select(value,est, boot_est, lower.ci, upper.ci)

  df_var_total = df_var %>%
    group_by(avg, condition) %>%
    summarize(boot_est = quantile(totalsd, .5, na.rm = TRUE),
              lower.ci = quantile(totalsd, lconf, na.rm = TRUE),
              upper.ci = quantile(totalsd, uconf, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(value = "SD total") %>%
    .[1,]  %>%
    mutate(est = mod1$totalsd[1]) %>%
    select(value, est, boot_est, lower.ci, upper.ci)

  df_var_all = bind_rows(df_var_within,
                         df_var_between,
                         df_var_total) %>%
    select(value, est, boot_est, lower.ci, upper.ci) %>%
    column_to_rownames("value")

  df_loa = df_boot %>%
    select(nboot, avg, condition, mean, low, high)

  df_loa_bias = df_loa %>%
    group_by(avg, condition) %>%
    summarize(boot_est = quantile(mean, .5, na.rm = TRUE),
              lower.ci = quantile(mean, lconf, na.rm = TRUE),
              upper.ci = quantile(mean, uconf, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(value = "Bias") %>%
    merge(x= ., y = mod1 %>% select(avg,condition,mean),
          by = c("avg","condition")) %>%
    rename(est = mean)

  df_loa_low = df_loa %>%
    group_by(avg, condition) %>%
    summarize(
              boot_est = quantile(low, .5, na.rm = TRUE),
              lower.ci = quantile(low, lconf, na.rm = TRUE),
              upper.ci = quantile(low, uconf, na.rm = TRUE),
              .groups = 'drop')%>%
    mutate(value = "Lower LoA") %>%
    merge(x= ., y = mod1 %>% select(avg,condition,low),
          by = c("avg","condition"))%>%
    rename(est = low)

  df_loa_hi = df_loa %>%
    group_by(avg, condition) %>%
    summarize(
              boot_est = quantile(high, .5, na.rm = TRUE),
              lower.ci = quantile(high, lconf, na.rm = TRUE),
              upper.ci = quantile(high, uconf, na.rm = TRUE),
              .groups = 'drop')%>%
    mutate(value = "Upper LoA")%>%
    merge(x= ., y = mod1 %>% select(avg,condition,high),
          by = c("avg","condition")) %>%
    rename(est = high)

  df_loa_all = bind_rows(df_loa_bias,
                        df_loa_low,
                        df_loa_hi) %>%
    select(avg, condition, value, est, boot_est, lower.ci, upper.ci) %>%
    arrange(avg, condition)

  res = list(
    loa = df_loa_all,
    var = df_var_all
  )
  return(res)
}


# Parametric bootstrap functions ---------



pred_bias = function(mod1, newdata){
  predict(mod1, re.form = NA, newdata = newdata)
}

pred_lloa = function(mod1, newdata, agree.level){
  agree.lim = qnorm(1 - (1 - agree.level) / 2)
  means = predict(mod1, re.form = NA, newdata = newdata)
  totalsd = sqrt(sigma(mod1)^2 + unlist(VarCorr(mod1)))
  res = means - agree.lim * totalsd
  return(res)
}

pred_uloa = function(mod1, newdata, agree.level){
  agree.lim = qnorm(1 - (1 - agree.level) / 2)
  means = predict(mod1, re.form = NA, newdata = newdata)
  totalsd = sqrt(sigma(mod1)^2 + unlist(VarCorr(mod1)))
  res = means + agree.lim * totalsd
  return(res)
}

tidy_boot <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      conf.method = c("perc", "bca", "basic", "norm"),
                      ...) {
  conf.method <- rlang::arg_match(conf.method)

  # calculate the bias and standard error
  # this is an adapted version of the code in print.boot, where the bias
  # and standard error are calculated
  boot.out <- x
  index <- 1:ncol(boot.out$t)
  t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
  allNA <- apply(t, 2L, function(t) all(is.na(t)))
  index <- index[!allNA]
  t <- matrix(t[, !allNA], nrow = nrow(t))

  if (is.null(t0 <- boot.out$t0)) {
    if (is.null(boot.out$call$weights)) {
      op <- cbind(
        apply(t, 2L, mean, na.rm = TRUE),
        sqrt(apply(t, 2L, function(t.st) var(t.st[!is.na(t.st)])))
      )
    } else {
      op <- NULL
      for (i in index) op <- rbind(op, boot::imp.moments(boot.out, index = i)$rat)
      op[, 2L] <- sqrt(op[, 2])
    }
    colnames(op) <- c("estimate", "std.error")
  } else {
    t0 <- boot.out$t0[index]
    if (is.null(boot.out$call$weights)) {
      op <- cbind(t0, apply(t, 2L, mean, na.rm = TRUE) -
                    t0, sqrt(apply(t, 2L, function(t.st) var(t.st[!is.na(t.st)]))))
      colnames(op) <- c("statistic", "bias", "std.error")
    }
    else {
      op <- NULL
      for (i in index) {
        op <- rbind(op, boot::imp.moments(boot.out,
                                          index = i
        )$rat)
      }
      op <- cbind(t0, op[, 1L] - t0, sqrt(op[, 2L]), apply(t,
                                                           2L, mean,
                                                           na.rm = TRUE
      ))
      colnames(op) <- c("statistic", "bias", "std.error", "estimate")
    }
  }

  # bring in rownames as "term" column, and turn into a data.frame
  ret <- as_tidy_tibble(op)

  if (conf.int) {
    ci.list <- lapply(seq_along(x$t0),
                      boot::boot.ci,
                      boot.out = x,
                      conf = conf.level, type = conf.method
    )

    # boot.ci uses c("norm", "basic", "perc", "stud") for types
    # stores them with longer names
    ci.pos <- pmatch(conf.method, names(ci.list[[1]]))

    if (conf.method == "norm") {
      ci.tab <- cbind(ci.list[[1]][ci.pos][[1]][2:3], ci.list[[2]][ci.pos][[1]][2:3])
    } else {
      ci.tab <- t(sapply(ci.list, function(x) x[[ci.pos]][4:5]))
    }

    colnames(ci.tab) <- c("conf.low", "conf.high")
    ret <- cbind(ret, ci.tab)
  }
  as_tibble(ret)
}

as_tidy_tibble = function (x, new_names = NULL, new_column = "term")
{
  if (!is.null(new_names) && length(new_names) != ncol(x)) {
    stop("newnames must be NULL or have length equal to number of columns")
  }
  ret <- x
  if (!is.null(new_names)) {
    if (inherits(x, "data.frame")) {
      ret <- setNames(x, new_names)
    }
    else {
      colnames(ret) <- new_names
    }
  }
  if (all(rownames(x) == seq_len(nrow(x)))) {
    tibble::as_tibble(ret)
  }
  else {
    dplyr::bind_cols(`:=`(!!new_column, rownames(x)), tibble::as_tibble(ret))
  }
}
