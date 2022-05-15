

reli_mod_mse = function(mod.lmer, cv_calc = "MSE") {

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
  mw <- mean(get_response(mod.lmer), na.rm = TRUE)
  cv_out = stddev/mw

  res_out = c(cv_out, SEM, SEE, SEP)
  names(res_out) = c("cv", "SEM", "SEE", "SEP")
  return(res_out)
}


