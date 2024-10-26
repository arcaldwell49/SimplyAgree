

reli_mod_mse = function(mod.lmer, cv_calc = "MSE",
                        se_type = "MSE") {

  num_lvls = ngrps(mod.lmer)
  nj = num_lvls["items"]
  n_items = nj
  n.obs = num_lvls["id"]
  n_id = n.obs
  n_obs = length(residuals(mod.lmer))
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
  colnames(stats) <- c("Subjects", "items", "Residual")
  rownames(stats) <- c("df", "SumSq", "MS", "F", "p")
  # transpose
  stat.final = t(stats)
  # Calculate ICCs ----
  ICC1 <- (MSB - MSW)/(MSB + (n_items - 1) * MSW)
  ICC2 <- (MSB - MSE)/(MSB + (n_items - 1) * MSE + n_items * (MSJ -
                                                                MSE)/n_id)
  ICC3 <- (MSB - MSE)/(MSB + (n_items - 1) * MSE)
  ICC_1_k <- (MSB - MSW)/(MSB)
  ICC_2_k <- (MSB - MSE)/(MSB + (MSJ - MSE)/n_id)
  ICC_3_k <- (MSB - MSE)/MSB
  F11 <- MSB/MSW
  df11n <- n_id - 1
  df11d <- n_id * (n_items - 1)
  p11 <- -expm1(pf(F11, df11n, df11d, log.p = TRUE))
  F21 <- MSB/MSE
  df21n <- n_id - 1
  df21d <- (n_id - 1) * (n_items - 1)
  p21 <- -expm1(pf(F21, df21n, df21d, log.p = TRUE))
  F31 <- F21
  results <- data.frame(matrix(NA, ncol = 6, nrow = 6))
  colnames(results) <- c("model","measures","type", "icc","lower.ci", "upper.ci")

  results$model = c("one-way random","two-way random", "two-way fixed",
                    "one-way random","two-way random", "two-way fixed")
  results$measures = c("Agreement", "Agreement", "Consistency",
                       "Avg. Agreement", "Avg. Agreement", "Avg. Consistency")
  results$type = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results$icc = c(ICC1,ICC2,ICC3,ICC_1_k,ICC_2_k,ICC_3_k)
  # Other stats -----
  sd_tots = sqrt(sum(stats[2,])/(n_obs-1))
  if(se_type == "MSE"){
    SEM = sqrt(MSE)
  } else{
    SEM = sd_tots * sqrt(1-subset(results, type == se_type)$icc)
  }

  if(se_type == "MSE"){
    ICC3 <- (MSB - MSE)/(MSB + (n_items - 1) * MSE)
    SEE = sd_tots*sqrt(ICC3*(1-ICC3))
    SEP = sd_tots*sqrt(1-ICC3^2)
  } else {
    SEE = sd_tots*sqrt(subset(results, type == se_type)$icc*(1-subset(results, type == se_type)$icc))
    SEP = sd_tots*sqrt(1-subset(results, type == se_type)$icc^2)
  }


  if(cv_calc == "residuals"){
    stddev <- sqrt(mean(residuals(mod.lmer)^2))
  }
  if(cv_calc == "MSE"){
    stddev <- sqrt(MSE)
  }
  if(cv_calc == "SEM"){
    stddev <- SEM
  }

  mw <- mean(get_response(mod.lmer), na.rm = TRUE)
  cv_out = stddev/mw

  res_out = c(cv_out, SEM, SEE, SEP)
  names(res_out) = c("cv", "SEM", "SEE", "SEP")
  return(res_out)
}

reli_mod_icc = function(mod.lmer, icc_type = "ICC3") {

  # Get estimates from model ----
  num_lvls = ngrps(mod.lmer)
  n_items = num_lvls["items"]
  n_id = num_lvls["id"]
  n_obs = nrow(na.omit(x.df))
  vc <- VarCorr(mod.lmer) # Get Variance Components
  MS_id <- vc$id[1, 1] # var by id
  MS_items <- vc$items[1, 1] # var by item
  MSE <- (attributes(vc)$sc)^2
  # Create variance table ------
  MS.df <- data.frame(variance = c(MS_id, MS_items, MSE,
                                   NA))
  rownames(MS.df) <- c("ID", "Items", "Residual", "Total")
  MS.df["Total", ] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["percent"] <- MS.df/MS.df["Total", 1]

  MSB <- n_items * MS_id + MSE
  MSJ <- n_id * MS_items + MSE
  MSW <- MSE + MS_items
  stats <- matrix(NA, ncol = 3, nrow = 5)
  stats[1, 1] <- dfB <- n_id - 1
  stats[1, 2] <- dfJ <- n_items - 1
  stats[1, 3] <- dfE <- (n_id - 1) * (n_items - 1)
  stats[2, 1] <- MSB * (n_id - 1)
  stats[2, 2] <- MSJ * (n_items - 1)
  stats[2, 3] <- MSE * (n_id - 1) * (n_items - 1)
  stats[3, 1] <- MSB
  stats[3, 2] <- MSJ
  stats[3, 3] <- MSE
  stats[4, 1] <- FB <- MSB/MSE
  stats[4, 2] <- FJ <- MSJ/MSE
  stats[5, 1] <- -expm1(pf(FB, dfB, dfE, log.p = TRUE))
  stats[5, 2] <- -expm1(pf(FJ, dfJ, dfE, log.p = TRUE))

  colnames(stats) <- c("Subjects", "items", "Residual")
  rownames(stats) <- c("df", "SumSq", "MS", "F", "p")
  # transpose
  stat.final = t(stats)
  # Calculate ICCs ----
  ICC1 <- (MSB - MSW)/(MSB + (n_items - 1) * MSW)
  ICC2 <- (MSB - MSE)/(MSB + (n_items - 1) * MSE + n_items * (MSJ -
                                                                MSE)/n_id)
  ICC3 <- (MSB - MSE)/(MSB + (n_items - 1) * MSE)
  ICC_1_k <- (MSB - MSW)/(MSB)
  ICC_2_k <- (MSB - MSE)/(MSB + (MSJ - MSE)/n_id)
  ICC_3_k <- (MSB - MSE)/MSB
  F11 <- MSB/MSW
  df11n <- n_id - 1
  df11d <- n_id * (n_items - 1)
  p11 <- -expm1(pf(F11, df11n, df11d, log.p = TRUE))
  F21 <- MSB/MSE
  df21n <- n_id - 1
  df21d <- (n_id - 1) * (n_items - 1)
  p21 <- -expm1(pf(F21, df21n, df21d, log.p = TRUE))
  F31 <- F21
  results <- data.frame(matrix(NA, ncol = 6, nrow = 6))
  colnames(results) <- c("model","measures","type", "icc","lower.ci", "upper.ci")

  results$model = c("one-way random","two-way random", "two-way fixed",
                    "one-way random","two-way random", "two-way fixed")
  results$measures = c("Agreement", "Agreement", "Consistency",
                       "Avg. Agreement", "Avg. Agreement", "Avg. Consistency")
  results$type = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results$icc = c(ICC1,ICC2,ICC3,ICC_1_k,ICC_2_k,ICC_3_k)

  F1L <- F11/qf(1 - alpha, df11n, df11d)
  F1U <- F11 * qf(1 - alpha, df11d, df11n)
  L1 <- (F1L - 1)/(F1L + (n_items - 1))
  U1 <- (F1U - 1)/(F1U + n_items - 1)
  F3L <- F31/qf(1 - alpha, df21n, df21d)
  F3U <- F31 * qf(1 - alpha, df21d, df21n)
  results[1, 5] <- L1
  results[1, 6] <- U1
  results[3, 5] <- (F3L - 1)/(F3L + n_items - 1)
  results[3, 6] <- (F3U - 1)/(F3U + n_items - 1)
  results[4, 5] <- 1 - 1/F1L
  results[4, 6] <- 1 - 1/F1U
  results[6, 5] <- 1 - 1/F3L
  results[6, 6] <- 1 - 1/F3U
  Fj <- MSJ/MSE
  vn <- (n_items - 1) * (n_id - 1) * ((n_items * ICC2 * Fj + n_id *
                                         (1 + (n_items - 1) * ICC2) - n_items * ICC2))^2
  vd <- (n_id - 1) * n_items^2 * ICC2^2 * Fj^2 + (n_id * (1 +
                                                            (n_items - 1) * ICC2) - n_items * ICC2)^2
  v <- vn/vd
  F3U <- qf(1 - alpha, n_id - 1, v)
  F3L <- qf(1 - alpha, v, n_id - 1)
  L3 <- n_id * (MSB - F3U * MSE)/(F3U * (n_items * MSJ + (n_items *
                                                            n_id - n_items - n_id) * MSE) + n_id * MSB)
  results[2, 5] <- L3
  U3 <- n_id * (F3L * MSB - MSE)/(n_items * MSJ + (n_items * n_id -
                                                     n_items - n_id) * MSE + n_id * F3L * MSB)
  results[2, 6] <- U3
  L3k <- L3 * n_items/(1 + L3 * (n_items - 1))
  U3k <- U3 * n_items/(1 + U3 * (n_items - 1))
  results[5, 5] <- L3k
  results[5, 6] <- U3k


  res_final = results %>%
    filter(type == icc_type)

  return(res_final$icc)
}

