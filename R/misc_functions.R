# Miscellaneous functions

loa_bs = function(diff = "diff",
                  condition = "trial_condition",
                  id = "id",
                  data = df_rec.pre,
                  conf.level = .95,
                  agree.level = .95,
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

  mean = as.data.frame(emmeans::emmeans(res3, ~1))$emmean
  se = as.data.frame(emmeans::emmeans(res3, ~1))$SE
  withinsd = vartab$sdcor[2]
  betweensd <- vartab$sdcor[1]
  totalsd <- sqrt(vartab$vcov[1] + vartab$vcov[2])

  # 95% Limits of agreement
  low <- mean - agree.lim * totalsd
  upper <- mean + agree.lim * totalsd
  # cat(cl*100,"% LoA are from",low,"to",upper,"\n")
  c(bias = mean,
    within_sd = withinsd,
    between_sd = betweensd,
    total_sd = totalsd,
    low_loa = low,
    upper_loa = upper)
}

loa_bstab = function(bsls,
                     type = "bca",
                     conf.level = .95){
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
  Estimate = c(
    boot_bias$t0,
    boot_within_sd$t0,
    boot_between_sd$t0,
    boot_total_sd$t0,
    boot_low_loa$t0,
    boot_upper_loa$t0
  ),
  lower.ci = c(
    conf_bias[1],
    conf_within_sd[1],
    conf_between_sd[1],
    conf_total_sd[1],
    conf_low_loa[1],
    conf_upper_loa[1]
  ),
  upper.ci = c(
    conf_bias[2],
    conf_within_sd[2],
    conf_between_sd[2],
    conf_total_sd[2],
    conf_low_loa[2],
    conf_upper_loa[2]
  ))
}
