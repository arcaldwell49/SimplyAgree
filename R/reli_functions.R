reli2 = function(x.df, cv_calc = "MSE") {
  mod.lmer <- lmer(values ~ 1 + (1 | id) + (1 | items),
                   data = x.df,
                   na.action = na.omit)
  num_lvls = ngrps(mod.lmer)
  nj = num_lvls["items"]
  n.obs = num_lvls["id"]
  vc <- VarCorr(mod.lmer) # Get Variance Components
  MS_id <- vc$id[1, 1] # var by id
  MS_items <- vc$items[1, 1] # var by item
  MSE <- error <- MS_resid <- (attributes(vc)$sc) ^ 2
  # Create variance table
  MS.df <- data.frame(variance = c(MS_id, MS_items, MS_resid,
                                   NA))
  rownames(MS.df) <- c("ID", "Items", "Residual", "Total")
  MS.df["Total",] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["percent"] <- MS.df / MS.df["Total", 1]
  lmer.MS <- MS.df
  MSB <- nj * MS_id + error
  MSJ <- n.obs * MS_items + error
  MSW <- error + MS_items
  stats <- matrix(NA, ncol = 3, nrow = 5)
  stats[1, 1] <- dfB <- n.obs - 1
  stats[1, 2] <- dfJ <- nj - 1
  stats[1, 3] <- dfE <- (n.obs - 1) * (nj - 1)
  stats[2, 1] <- MSB * (n.obs - 1)
  stats[2, 2] <- MSJ * (nj - 1)
  stats[2, 3] <- MSE * (n.obs - 1) * (nj - 1)
  stats[3, 1] <- MSB
  stats[3, 2] <- MSJ
  stats[3, 3] <- MSE
  stats[4, 1] <- FB <- MSB / MSE
  stats[4, 2] <- FJ <- MSJ / MSE
  stats[5, 1] <- -expm1(pf(FB, dfB, dfE, log.p = TRUE))
  stats[5, 2] <- -expm1(pf(FJ, dfJ, dfE, log.p = TRUE))

  ICC3 <- (MSB - MSE)/(MSB + (nj - 1) * MSE)

  SEM = sqrt(MSE)
  sd_tots = sqrt(sum(stats[2,])/(n.obs-1))
  SEE = sd_tots*sqrt(ICC3*(1-ICC3))
  SEP = sd_tots*sqrt(1-ICC3^2)

  if(cv_calc == "residuals"){
    stddev <- sqrt(mean(residuals(mod.lmer)^2))
  } else if(cv_calc == "MSE"){
    stddev <- sqrt(MSE)
  } else if(cv_calc == "SEM"){
    stddev <- SEM
  } else {
    stop("cv_calc must be SEM, MSE, or residuals")
  }
  mw <- mean(x.df$values, na.rm = TRUE)
  cv_out = stddev/mw

  return(list(cv = cv_out,
              SEM = SEM,
              SEE = SEE,
              SEP = SEP))
}

boot_rel = function(x.df, cv_calc = "MSE",
                    nboot = 499,
                    conf.level){
  len = nrow(x.df)
  u <- list()
  for (i in 1:nboot) {
    mysample = sample(1:len,replace=TRUE)
    df2 = x.df[mysample,]
    u <- append(u,list(reli2(df2,
                             cv_calc)))
  }
  cv = SEM = SEP = SEE = rep(0,nboot)
  lconf = (1 - conf.level)/2
  uconf = 1-(1 - conf.level)/2
  for (j in 1:nboot){
    cv[j] = u[[j]]$cv
    SEM[j] = u[[j]]$SEM
    SEP[j] = u[[j]]$SEP
    SEE[j] = u[[j]]$SEE
  }

  theta_cv <- reli2(x.df,cv_calc)$cv
  theta_SEM <- reli2(x.df,cv_calc)$SEM
  theta_SEP <- reli2(x.df,cv_calc)$SEP
  theta_SEE <- reli2(x.df,cv_calc)$SEE

  cv_bias <- (mean(cv, na.rm = TRUE) - theta_cv)
  SEM_bias <- (mean(SEM, na.rm = TRUE) - theta_SEM)
  SEP_bias <- (mean(SEP, na.rm = TRUE) - theta_SEP)
  SEE_bias <- (mean(SEE, na.rm = TRUE) - theta_SEE)

  cv_se <- sd(cv, na.rm = TRUE)
  SEM_se <- sd(SEM, na.rm = TRUE)
  SEP_se <- sd(SEP, na.rm = TRUE)
  SEE_se <- sd(SEE, na.rm = TRUE)

  cv_lci = quantile(cv, lconf, na.rm = TRUE)
  SEM_lci = quantile(SEM, lconf, na.rm = TRUE)
  SEP_lci = quantile(SEP, lconf, na.rm = TRUE)
  SEE_lci = quantile(SEE, lconf, na.rm = TRUE)

  cv_uci = quantile(cv, uconf, na.rm = TRUE)
  SEM_uci = quantile(SEM, uconf, na.rm = TRUE)
  SEP_uci = quantile(SEP, uconf, na.rm = TRUE)
  SEE_uci = quantile(SEE, uconf, na.rm = TRUE)

  res = list(
    cv = list(
      est = theta_cv,
      bias = cv_bias,
      se = cv_se,
      lower.ci = cv_lci,
      upper.ci = cv_uci
    ),
    SEM = list(
      est = theta_SEM,
      bias = SEM_bias,
      se = SEM_se,
      lower.ci = SEM_lci,
      upper.ci = SEM_uci
    ),
    SEP = list(
      est = theta_SEP,
      bias = SEP_bias,
      se = SEP_se,
      lower.ci = SEP_lci,
      upper.ci = SEP_uci
    ),
    SEE = list(
      est = theta_SEE,
      bias = SEE_bias,
      se = SEE_se,
      lower.ci = SEE_lci,
      upper.ci = SEE_uci
    )
  )
  return(res)
}
