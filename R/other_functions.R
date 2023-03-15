#' @importFrom dplyr rename
#'

# CV function

# CCC calcs for nest and reps functions
cccUst = function (dataset, ry, rmet, rtime = NULL, Dmat = NULL, delta = 1,
                   cl = 0.95)
{
  dades <- data.frame(dataset)
  if (length(rtime) == 0)
    dades <- dades %>% rename(y = all_of(ry), met = all_of(rmet))
  if (length(rtime) > 0)
    dades <- dades %>% rename(y = all_of(ry), met = all_of(rmet),
                                     time = all_of(rtime))
  catmet <- unique(dades$met)
  if (length(rtime) == 0) {
    valtime <- "1"
    ntime = 1
  }
  else {
    valtime <- unique(dades$time)
    ntime <- length(valtime)
  }
  ns <- length(dades$y)/(2 * ntime)
  if (length(Dmat) == 0) {
    Dmat <- diag(rep(1, ntime))
  }
  if (sum(dim(Dmat) == c(ntime, ntime)) != 2) {
    stop("Invalid dimensions in weigth matrix")
  }
  if (length(rtime) > 0) {
    dades <- dades[order(dades$time), ]
  }
  Y <- array(dades[dades$met == catmet[1], ]$y, c(ns, ntime))
  X <- array(dades[dades$met == catmet[2], ]$y, c(ns, ntime))
  colnames(Y) <- valtime
  colnames(X) <- valtime
  phimat <- array(NA, c(ns * ns, 4))
  cont <- 0
  for (i in 1:ns) {
    for (j in 1:ns) {
      cont <- cont + 1
      phimat[cont, c(3, 4)] <- phi(X[i, ], Y[i, ], X[j,
      ], Y[j, ], Dmat, delta)
      phimat[cont, c(1, 2)] <- c(i, j)
    }
  }
  colnames(phimat) <- c("i", "j", "phi1", "phi2")
  if(NA %in% phimat){
    phimat = na.omit(phimat)
  }
  U <- sum((phimat[phimat[, 1] != phimat[, 2], 3]))/(ns *
                                                       (ns - 1))
  V <- sum((phimat[phimat[, 1] != phimat[, 2], 4]))/(ns *
                                                       (ns - 1))
  CCC <- ((ns - 1) * (V - U))/(U + (ns - 1) * V)
  phimat1 <- phimat[phimat[, 1] != phimat[, 2], ]
  phi1 <- tapply(phimat1[, 3], phimat1[, 1], sum)/(ns - 1)
  phi2 <- tapply(phimat1[, 4], phimat1[, 1], sum)/(ns - 1)
  phiv <- cbind(phi1, phi2)
  UV <- cbind(U, V)
  Saux <- array(0, c(2, 2))
  C <- array(c(1, 0, 0, 2), c(2, 2))
  for (i in 1:ns) {
    Saux <- Saux + t(phiv[i, ] - UV) %*% (phiv[i, ] - UV)
  }
  Smat <- C %*% (Saux/(ns^2)) %*% C
  dev <- array(NA, c(1, 2))
  dev[1, 1] <- ((-1) * ns * (ns - 1) * V)/((U + (ns - 1) *
                                              V)^2)
  dev[1, 2] <- (ns * (ns - 1) * U)/((U + (ns - 1) * V)^2)
  VarCCC <- dev %*% Smat %*% t(dev)
  alpha = 1 - cl
  Z <- 0.5 * (log((1 + CCC)/(1 - CCC)))
  VarZ <- VarCCC/(((1 - CCC)^2) * ((1 + CCC)^2))
  ic.z = Z + c(-1, 1) * qnorm(1 - alpha/2) * c(sqrt(VarZ))
  ic.ccc = (exp(2 * ic.z) - 1)/(exp(2 * ic.z) + 1)
  result <- c(CCC, ic.ccc, sqrt(VarCCC), Z, sqrt(VarZ))
  conf.lab = paste((1 - alpha) * 100, "%", sep = "")
  names(result) <- c("CCC", paste("LL CI", conf.lab), paste("UL CI",
                                                            conf.lab), "SE CCC", "Z", "SE Z")
  class(result) <- "cccUst"
  result
}

phi <- function (X1, Y1, X2, Y2, Dmat, delta)
{
  if (delta != 0) {
    phi1 <- 0.5 * (((abs(X1 - Y1))^delta) %*% Dmat %*% ((abs(X1 -
                                                               Y1))^delta) + ((abs(X2 - Y2))^delta) %*% Dmat %*%
                     ((abs(X2 - Y2))^delta))
    phi2 <- 0.5 * (((abs(X1 - Y2))^delta) %*% Dmat %*% ((abs(X1 -
                                                               Y2))^delta) + ((abs(X2 - Y1))^delta) %*% Dmat %*%
                     ((abs(X2 - Y1))^delta))
    return(c(phi1, phi2))
  }
  if (delta == 0) {
    phi1 <- 0.5 * (((abs(X1 - Y1)) != 0) %*% Dmat %*% ((abs(X1 -
                                                              Y1)) != 0) + ((abs(X2 - Y2)) != 0) %*% Dmat %*%
                     ((abs(X2 - Y2)) != 0))
    phi2 <- 0.5 * (((abs(X1 - Y2)) != 0) %*% Dmat %*% ((abs(X1 -
                                                              Y2)) != 0) + ((abs(X2 - Y1)) != 0) %*% Dmat %*%
                     ((abs(X2 - Y1)) != 0))
    return(c(phi1, phi2))
  }
}

# for agree_test
ccc.xy <- function(x, y,
                   conf.level,
                   agree.level,
                   TOST = TRUE,
                   prop_bias = FALSE) {
  N. <- 1 - ((1 - conf.level) / 2)
  N.2 = if(TOST == FALSE) {
    1 - ((1 - conf.level) / 2)
  } else {
    conf.level
  }
  zv <- qnorm(N., mean = 0, sd = 1)
  zv2 = qnorm(N.2, mean = 0, sd = 1)
  pct <- 1 - (1 - agree.level) / 2
  agreelim = qnorm(pct)
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
  # LoA
  if(prop_bias == FALSE){
   # sqrt(var(delta, na.rm = TRUE))
    delta.sd <- sigma(lm(formula = delta ~ 1))
    dfs = df.residual(lm(formula = delta ~ 1))
  } else {
    delta.sd <- sigma(lm(formula = delta ~ rmean))
    dfs = df.residual(lm(formula = delta ~ rmean))
    }
  var.d = (delta.sd)^2/k
  # var.dlim = (1/k+zv2/(2*(k-1)))*(delta.sd)^2
  var.dlim = (delta.sd*sqrt(1/k + agreelim^2 / (2*(k-1))) )^2

  ba.p <- mean(delta)

  l.loa = ba.p - agreelim*delta.sd
  u.loa = ba.p + agreelim*delta.sd

  # Calculate Bland Altman Limits
  sblalt <- data.frame(d = ba.p,
                       d.lci = (ba.p - qt(N.,dfs)*sqrt(var.d)),
                       d.uci = (ba.p + qt(N.,dfs)*sqrt(var.d)),
                       d.sd = delta.sd,
                       var.d = var.d,
                       var.loa = var.dlim,
                       lower.loa = l.loa,
                       lower.lci = (l.loa - qt(N.2,dfs)*sqrt(var.dlim)),
                       lower.uci = (l.loa + qt(N.2,dfs)*sqrt(var.dlim)),
                       upper.loa = u.loa,
                       upper.lci = (u.loa - qt(N.2,dfs)*sqrt(var.dlim)),
                       upper.uci = (u.loa + qt(N.2,dfs)*sqrt(var.dlim))
                       )

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


plot_het = function (x,
                     x_lab = "Mean of both methods",
                     y_lab = expression(sqrt("|Std. residuals|")),
                     size_point = 2,
                     size_line = .8,
                     alpha_level = 0.2,
                     colors = c("#3aaf85","#0077B5","#cd201f"),
                     dot_alpha_level = 0.8)
{
  ggplot(x, aes(x = .data$x, .data$y)) +
    geom_point(colour = colors[2], stroke = 0, shape = 16,
                size = size_point,
                alpha = dot_alpha_level) +
    stat_smooth(
      method = "loess",
      se = TRUE,
      alpha = alpha_level,
      formula = y ~ x,
      linewidth = size_line,
      colour = colors[1]
    ) +
    labs(
      title = "Homogeneity of Variance",
      subtitle = "Reference line should be flat and horizontal",
      y = y_lab,
      x = x_lab
    ) +
    theme_bw()
}


plot_qq = function (x,
                    size_line = .8,
                    size_point = 2,
                    alpha_level = 0.2,
                    colors = c("#3aaf85","#0077B5","#cd201f"))
{

  qq_stuff <- list(
    geom_qq(
      shape = 16,
      stroke = 0,
      size = size_point,
      colour = colors[2]
    ),
    geom_qq_line(linewidth = size_line,
                          colour = colors[1])
  )
  y_lab <- "Sample Quantiles"

  ggplot(x, aes(sample = .data$y)) + qq_stuff +
    labs(
      title = "Normality of Residuals",
      subtitle = "Dots should fall along the line",
      y = y_lab,
      x = "Standard Normal Distribution Quantiles"
    ) +
    theme_bw()
}

plot_bias = function(x,
                     size_line = .8,
                     size_point = 2,
                     alpha_level = 0.2,
                     colors = c("#3aaf85","#0077B5","#cd201f"),
                     dot_alpha_level = .8){
  ggplot(x, aes(x = mean,
                y = resid)) +
    geom_point(colour = colors[2], stroke = 0, shape = 16,
               size = size_point,
               alpha = dot_alpha_level) +
    stat_smooth(
      method = "lm",
      se = TRUE,
      alpha = alpha_level,
      formula = y ~ x,
      linewidth = size_line,
      colour = colors[1]
    ) +
    labs(
      title = "Proportional Bias",
      subtitle = "Reference line should be horizontal",
      y = "Residuals",
      x = "Mean of both methods"
    ) +
    theme_bw()+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
}

# Check agree_test -----

# Check reli_stats -----

check_reli = function(x){

  dat = x$plot.reliability$data

  ## Heteroskedasticity -------
  mod_check = x$lmer
  stan_res = residuals(mod_check, type = "pearson")
  df_het = df.residual(mod_check)
  sum_het_res = sum(!is.na(stan_res))
  sigma_het = sigma(mod_check)

  s_sq = df_het * sigma_het^2 / sum_het_res

  u_het = stan_res^2 / s_sq

  mod <- lm(u_het ~ fitted(mod_check))

  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  ### Breusch-Pagan Test
  p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)

  rstan_het =  residuals(mod_check, scaled = TRUE)
  dat_het <- data.frame(
    x = na.omit(fitted(mod_check)),
    y = na.omit(sqrt(abs(rstan_het)))
  )
  p_het = plot_het(dat_het,
                   x_lab = "Fitted values") +
    labs(caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ",
                          signif(p_val_het,4)))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )


  ## Normality ------------

  mod_res = residuals(mod_check)
  if(length(mod_res) < 5000){
    norm_test = shapiro.test(mod_res)
    norm_text = "Shapiro-Wilk Test"
  } else {
    norm_test = ks.test(mod_res, y = "pnorm",
                        alternative = "two.sided")
    norm_text = "Kolmogorov-Smirnov Test"
  }

  rstan_norm = sort(rstudent(mod_check), na.last = NA)
  dat_norm <- na.omit(data.frame(y = rstan_norm))
  p_norm = plot_qq(
    x = dat_norm
  ) +
    labs(caption = paste0("Normality", " \n",
                          norm_text, ": p = ",
                          signif(norm_test$p.value,4))) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  all_checks = wrap_plots(p_norm, p_het, ncol = 2)& plot_annotation(
    theme = theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    ))
  return(all_checks)
}

# Deming ----
calc_dem = function(X,Y, w_i, error.ratio){
  x_w = sum(w_i*X)/sum(w_i)
  y_w = sum(w_i*Y)/sum(w_i)
  p_w = sum(w_i * (X - x_w)*(Y - y_w))
  u_w = sum(w_i * (X - x_w)^2)
  q_w = sum(w_i * (Y - y_w)^2)
  b1_w <- ((error.ratio * q_w - u_w) + sqrt((u_w - error.ratio * q_w)^2 +
                                              4 * error.ratio * p_w^2))/(2 * error.ratio * p_w)
  b0_w <- y_w- b1_w * x_w
  return(list(b0 = b0_w, b1 = b1_w))
}
jack_dem = function(X,Y, w_i, error.ratio){
  len <- length(X)
  u <- list()
  for (i in 1:len) {
    u <- append(u,list(calc_dem(X[-i], Y[-i], w_i[-i],
                                error.ratio)))
  }
  b0 = rep(0,length(u))
  b1 = rep(0,length(u))
  for (j in 1:length(u)){
    b0[j] = u[[j]]$b0
    b1[j] = u[[j]]$b1
  }
  theta_b0 <- calc_dem(X,Y, w_i, error.ratio)$b0
  theta_b1 <- calc_dem(X,Y, w_i, error.ratio)$b1
  b0_bias <- (len - 1) * (mean(b0) - theta_b0)
  b1_bias <- (len - 1) * (mean(b1) - theta_b1)
  b0_se <- sqrt(((len - 1)/len) * sum((b0 - mean(b0))^2))
  b1_se <- sqrt(((len - 1)/len) * sum((b1 - mean(b1))^2))
  res = data.frame(row.names = c("Intercept","Slope"),
                   coef = c(theta_b0, theta_b1),
                   bias = c(b0_bias, b1_bias),
                   se = c(b0_se, b1_se))
  return(list(df = res,
              jacks = list(b0 = b0,
                           b1 = b1)))
}


# LoA prop bias -----

# MOVERS ------

mover_emm = function(lmer_mod,
                     df,
                     agree.level,
                     conf.level,
                     TOST,
                     var_comp) {
  pct <- 1 - (1 - agree.level) / 2
  LME = var_comp$LME
  RME = var_comp$RME
  var_tot = var_comp$var_tot
  agreeq = qnorm(pct)
  confq = qnorm(1 - (1 - conf.level) / 2)
  if (TOST == TRUE) {
    confq2 = qnorm(1 - (1 - conf.level))
  } else {
    confq2 = qnorm(1 - (1 - conf.level) / 2)
  }

  ref_med = ref_grid(lmer_mod,
                     at = list(mean = seq(
                       min(df$mean, na.rm = TRUE),
                       max(df$mean, na.rm = TRUE),
                       length.out = 100
                     )))

  emm_med = summary(emmeans(ref_med,~ mean), level = conf.level)
  colnames(emm_med) = c("mean", "emmean", "se", "df", "lower", "upper")
  df_coef_med = data.frame(
    at = emm_med$mean,
    estimate = emm_med$emmean,
    lower.ci = emm_med$lower,
    upper.ci = emm_med$upper,
    text = "Bias"
  )

  df_coef_lloa = data.frame(at = emm_med$mean,
                            estimate = emm_med$emmean - agreeq * sqrt(var_tot))
  df_coef_lloa$lower.ci = df_coef_lloa$estimate - LME
  df_coef_lloa$upper.ci = df_coef_lloa$estimate + RME
  df_coef_lloa$text = "Lower LoA"

  df_coef_uloa = data.frame(at = emm_med$mean,
                            estimate = emm_med$emmean + agreeq * sqrt(var_tot))
  df_coef_uloa$lower.ci = df_coef_uloa$estimate - RME
  df_coef_uloa$upper.ci = df_coef_uloa$estimate + LME
  df_coef_uloa$text = "Upper LoA"

  df_emm = rbind(df_coef_uloa,
                 df_coef_med,
                 df_coef_lloa)

  df_emm$text = factor(df_emm$text,
                       levels = c("Upper LoA", "Bias", "Lower LoA"))
  return(df_emm)

}

# Simple emm -----


simple_emm = function(lm_mod,
                      df,
                      agree.level,
                      conf.level,
                      TOST,
                      var_comp) {
  pct <- 1 - (1 - agree.level) / 2
  agreeq = qnorm(pct)
  sd_loa = var_comp$sd_loa
  sd_delta = var_comp$sd_delta
  dfs = df.residual(lm_mod)
  conf = conf.level
  if (TOST == TRUE) {
    conf2 = 1 - (1 - conf.level)
  } else {
    conf2 = 1 - (1 - conf.level) / 2
  }

  ref_med = ref_grid(lm_mod,
                     at = list(mean = seq(
                       min(df$mean, na.rm = TRUE),
                       max(df$mean, na.rm = TRUE),
                       length.out = 100
                     )))
  emm_med = summary(emmeans(ref_med,~ mean), level = conf.level)
  colnames(emm_med) = c("mean", "emmean", "se", "df", "lower", "upper")
  #emm_med = confint(emmeans(ref_med,  ~ mean), level = conf)
  df_coef_med = data.frame(
    at = emm_med$mean,
    estimate = emm_med$emmean,
    lower.ci = emm_med$lower,
    upper.ci = emm_med$upper,
    text = "Bias"
  )

  df_coef_lloa = data.frame(at = emm_med$mean,
                            estimate = emm_med$emmean - agreeq * sd_delta)
  df_coef_lloa$lower.ci = df_coef_lloa$estimate - qt(conf2,dfs)*sd_loa
  df_coef_lloa$upper.ci = df_coef_lloa$estimate + qt(conf2,dfs)*sd_loa
  df_coef_lloa$text = "Lower LoA"

  df_coef_uloa = data.frame(at = emm_med$mean,
                            estimate = emm_med$emmean + agreeq * sd_delta)
  df_coef_uloa$lower.ci = df_coef_uloa$estimate - qt(conf2,dfs)*sd_loa
  df_coef_uloa$upper.ci = df_coef_uloa$estimate + qt(conf2,dfs)*sd_loa
  df_coef_uloa$text = "Upper LoA"

  df_emm = rbind(df_coef_uloa,
                 df_coef_med,
                 df_coef_lloa)

  df_emm$text = factor(df_emm$text,
                       levels = c("Upper LoA", "Bias", "Lower LoA"))
  return(df_emm)

}



# Get call item --------

get_call = function(x){
  if(inherits(x,"name")){
    get(x)
  } else {
    x
  }
}
