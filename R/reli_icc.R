
reli_icc_lmer = function(
  measure,
  item,
  id,
  data,
  wide = FALSE,
  col.names = NULL,
  conf.level = .95,
  icc_type = c("ICC3","ICC1","ICC2","ICC1k","ICC2k","ICC3k"),
  type = c("Fdist", "perc", "norm", "basic"),
  replicates = 1999){
  icc_type = match.arg(icc_type, several.ok = FALSE)
  alpha = 1-conf.level
  # Organize Data ----
  x.df = reli_data_org(
    measure = measure,
    item = item,
    id = id,
    data = data,
    wide = wide,
    col.names = col.names
  )
  # Model ------
  mod.lmer <- lmer(values ~ 1 + (1 | id) + (1 | items),
                   data = x.df,
                   na.action = na.omit)
  if(length(unique(x.df$items)) == 2){
    message("Only 2 items in data. It is recommended to use aov instead of lmer methods.")
  }

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


  results %>%
    filter(type == icc_type)
}

reli_data_org = function(
    measure,
    item,
    id,
    data,
    wide = FALSE,
    col.names = NULL
    ){
  x = data
  if(wide == TRUE){
    if(is.null(col.names)){
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    x = x[,col.names]
    n_id <- dim(x)[1]
    n_items <- dim(x)[2]
    x.s <- stack(as.data.frame(x))
    x.df <- data.frame(x.s, subs = rep(paste("S", 1:n_id, sep = ""),
                                       n_items))
  } else {
    if(is.null(measure) || is.null(item) ||is.null(id)){
      stop("Must provide measure, item, and id column names if wide = FALSE")
    }
    x.df = x %>%
      select(all_of(measure),all_of(item),all_of(id))

    return(x.df)
  }

  colnames(x.df) <- c("values", "items", "id")

  return(x.df)
}
