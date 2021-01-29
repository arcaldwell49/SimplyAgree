# for agree_test
ccc.xy <- function(x, y, conf.level,agree.level) {
  N. <- 1 - ((1 - conf.level) / 2)
  zv <- qnorm(N., mean = 0, sd = 1)
  dat <- data.frame(x, y)
  id <- complete.cases(dat)
  nmissing <- sum(!complete.cases(dat))
  dat <- dat[id,]
  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1) / k
  sd1 <- sd(dat$y)
  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1) / k
  sd2 <- sd(dat$x)
  r <- cor(dat$x, dat$y)
  sl <- r * sd1 / sd2
  sxy <- r * sqrt(sx2 * sy2)
  p <- 2 * sxy / (sx2 + sy2 + (yb - xb) ^ 2)

  delta <- (dat$x - dat$y)
  rmean <- apply(dat, MARGIN = 1, FUN = mean)
  blalt <- data.frame(x = dat$x,
                      y = dat$y,
                      mean = rmean,
                      delta)
  v <- sd1 / sd2
  u <- (yb - xb) / ((sx2 * sy2) ^ 0.25)
  C.b <- p / r
  sep <- sqrt(((1 - ((r) ^ 2)) * (p) ^ 2 * (1 - ((p) ^ 2)) / (r) ^ 2 +
                 (2 * (p) ^ 3 * (1 - p) * (u) ^ 2 / r) - 0.5 * (p) ^ 4 * (u) ^
                 4 / (r) ^ 2
  ) / (k -
         2))
  ll <- p - (zv * sep)
  ul <- p + (zv * sep)
  t <- log((1 + p) / (1 - p)) / 2
  set = sep / (1 - ((p) ^ 2))
  llt = t - (zv * set)
  ult = t + (zv * set)
  llt = (exp(2 * llt) - 1) / (exp(2 * llt) + 1)
  ult = (exp(2 * ult) - 1) / (exp(2 * ult) + 1)
  delta.sd <- sqrt(var(delta, na.rm = TRUE))
  var.d = (delta.sd)^2/k
  var.dlim = (1/k+zv/(2*(k-1)))*var.d

  ba.p <- mean(delta)
  pct <- 1 - (1 - agree.level) / 2
  agreelim = qnorm(pct)
  l.loa = ba.p - agreelim*delta.sd
  u.loa = ba.p + agreelim*delta.sd
  # Calculate Bland Altman Limits
  sblalt <- data.frame(d = ba.p,
                       d.lci = (ba.p - qt(N.,k-1)*sqrt(var.d)),
                       d.uci = (ba.p + qt(N.,k-1)*sqrt(var.d)),
                       d.sd = delta.sd,
                       var.d = var.d,
                       var.loa = var.dlim,
                       lower.lci = (l.loa - qt(N.,k-1)*sqrt(var.dlim)),
                       lower.uci = (l.loa + qt(N.,k-1)*sqrt(var.dlim)),
                       upper.lci = (u.loa - qt(N.,k-1)*sqrt(var.dlim)),
                       upper.uci = (u.loa + qt(N.,k-1)*sqrt(var.dlim)))

  rho.c <- data.frame(p, llt, ult)
  names(rho.c) <- c("est.ccc", "lower.ci", "upper.ci")

  res_list <- list(
    rho.c = rho.c,
    s.shift = v,
    l.shift = u,
    bias = C.b,
    df_diff = blalt,
    delta = sblalt,
    nmissing = nmissing
  )
  #s.shift = scale (variance) shift
  #l.shift = change in mean
  #bias = bias correction (1 = perfect)
  #mean.dlt = mean and delta pairs data frame


}

# Miscellaneous for loa_mixed functions

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

  mean = as.data.frame(emmeans::emmeans(res3, ~1))$emmean
  se = as.data.frame(emmeans::emmeans(res3, ~1))$SE
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

