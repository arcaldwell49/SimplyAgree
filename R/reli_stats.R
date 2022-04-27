#' Reliability Statistics
#' @description reli_stats produces reliability statistics desccribed by Weir (2005). This includes intraclass correlation coefficients, the coefficient of variation, and the standard error of meassurement.
#' @param measure Name of column containing the measurement of interest
#' @param item Name of column containing the items. If this is a test-retest reliability study then this would indicate the time point (e.g., time1,time2, time3, etc.)
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95\%.
#' @param cv_calc Coefficient of variation (CV) calculation. This function allows for 3 versions of the CV. "MSE" is the default.
#' @param other_ci Logical value (TRUE or FALSE) indicating whether to calculate confidence intervals for the CV, SEM, SEP, and SEE. Note: this will dramatically increase the computation time.
#' @param replicates 	the number of bootstrap replicates. Passed on to the boot function. Default is 500.
#' @details
#'
#' The CV calculation has 3 versions. The "MSE" uses the "mean squared error" or residual error from the linear mixed model used to calculate the ICCs. The "SEM" option instead uses the SEM calculation and expresses CV as a ratio of the SEM to the overall mean. The "residuals" option uses the sjstats R package approach which uses the model residuals to calculate the root mean square error.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"icc"}}{Table of ICC results}
#'   \item{\code{"lmer"}}{Linear mixed model from lme4}
#'   \item{\code{"anova"}}{Analysis of Variance table}
#'   \item{\code{"var_comp"}}{Table of Variance Components}
#'   \item{\code{"n.id"}}{Number of subjects/participants}
#'   \item{\code{"n.items"}}{Number of items/time points}
#'   \item{\code{"cv"}}{Coefficient of Variation}
#'   \item{\code{"SEM"}}{Standard Error of Measurement}
#'   \item{\code{"SEE"}}{Standard Error of the Estimate}
#'   \item{\code{"SEP"}}{Standard Error of Predicitions}
#'   \item{\code{"plot.reliability"}}{Plot of data points within subjects across items}
#'
#'
#' }

#' @examples
#' data('reps')
#' reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
#'
#' @section References:
#' Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM. The Journal of Strength & Conditioning Research, 19(1), 231-240.
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var residuals
#' @importFrom tidyselect all_of
#' @import dplyr
#' @import ggplot2
#' @import lme4
#' @export


reli_stats = function(measure,
                      item,
                      id,
                      data,
                      wide = FALSE,
                      col.names = NULL,
                      cv_calc = "MSE",
                      conf.level = .95,
                      other_ci = FALSE,
                      replicates = 500){
  alpha = 1-conf.level
  x = data
  if(wide == TRUE){
    if(is.null(col.names)){
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    x = x[,col.names]
    n.obs <- dim(x)[1]
    nj <- dim(x)[2]
    x.s <- stack(as.data.frame(x))
    x.df <- data.frame(x.s, subs = rep(paste("S", 1:n.obs, sep = ""),
                                       nj))
  } else {
    if(is.null(measure) || is.null(item) ||is.null(id)){
      stop("Must provide measure, item, and id column names if wide = FALSE")
    }
    x.df = x %>%
      select(all_of(measure),all_of(item),all_of(id))
  }

  colnames(x.df) <- c("values", "items", "id")

  mod.lmer <- lmer(values ~ 1 + (1 | id) + (1 | items),
                   data = x.df,
                   na.action = na.omit)
  num_lvls = ngrps(mod.lmer)
  nj = num_lvls["items"]
  n.obs = num_lvls["id"]
  vc <- VarCorr(mod.lmer) # Get Variance Components
  MS_id <- vc$id[1, 1] # var by id
  MS_items <- vc$items[1, 1] # var by item
  MSE <- error <- MS_resid <- (attributes(vc)$sc)^2
  # Create variance table
  MS.df <- data.frame(variance = c(MS_id, MS_items, MS_resid,
                                   NA))
  rownames(MS.df) <- c("ID", "Items", "Residual", "Total")
  MS.df["Total", ] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["percent"] <- MS.df/MS.df["Total", 1]
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
  stats[4, 1] <- FB <- MSB/MSE
  stats[4, 2] <- FJ <- MSJ/MSE
  stats[5, 1] <- -expm1(pf(FB, dfB, dfE, log.p = TRUE))
  stats[5, 2] <- -expm1(pf(FJ, dfJ, dfE, log.p = TRUE))

  colnames(stats) <- c("Subjects", "items", "Residual")
  rownames(stats) <- c("df", "SumSq", "MS", "F", "p")
  # transpose
  stat.final = t(stats)
  # Calculate ICCs
  ICC1 <- (MSB - MSW)/(MSB + (nj - 1) * MSW)
  ICC2 <- (MSB - MSE)/(MSB + (nj - 1) * MSE + nj * (MSJ -
                                                      MSE)/n.obs)
  ICC3 <- (MSB - MSE)/(MSB + (nj - 1) * MSE)
  ICC12 <- (MSB - MSW)/(MSB)
  ICC22 <- (MSB - MSE)/(MSB + (MSJ - MSE)/n.obs)
  ICC32 <- (MSB - MSE)/MSB
  F11 <- MSB/MSW
  df11n <- n.obs - 1
  df11d <- n.obs * (nj - 1)
  p11 <- -expm1(pf(F11, df11n, df11d, log.p = TRUE))
  F21 <- MSB/MSE
  df21n <- n.obs - 1
  df21d <- (n.obs - 1) * (nj - 1)
  p21 <- -expm1(pf(F21, df21n, df21d, log.p = TRUE))
  F31 <- F21
  results <- data.frame(matrix(NA, ncol = 6, nrow = 6))
  colnames(results) <- c("model","measures","type", "icc","lower.ci", "upper.ci")

  results$model = c("one-way random","two-way random", "two-way fixed",
                    "one-way random","two-way random", "two-way fixed")
  results$measures = c("Agreement", "Agreement", "Consistency",
                       "Avg. Agreement", "Avg. Agreement", "Avg. Consistency")
  results$type = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results$icc = c(ICC1,ICC2,ICC3,ICC12,ICC22,ICC32)

  F1L <- F11/qf(1 - alpha, df11n, df11d)
  F1U <- F11 * qf(1 - alpha, df11d, df11n)
  L1 <- (F1L - 1)/(F1L + (nj - 1))
  U1 <- (F1U - 1)/(F1U + nj - 1)
  F3L <- F31/qf(1 - alpha, df21n, df21d)
  F3U <- F31 * qf(1 - alpha, df21d, df21n)
  results[1, 5] <- L1
  results[1, 6] <- U1
  results[3, 5] <- (F3L - 1)/(F3L + nj - 1)
  results[3, 6] <- (F3U - 1)/(F3U + nj - 1)
  results[4, 5] <- 1 - 1/F1L
  results[4, 6] <- 1 - 1/F1U
  results[6, 5] <- 1 - 1/F3L
  results[6, 6] <- 1 - 1/F3U
  Fj <- MSJ/MSE
  vn <- (nj - 1) * (n.obs - 1) * ((nj * ICC2 * Fj + n.obs *
                                     (1 + (nj - 1) * ICC2) - nj * ICC2))^2
  vd <- (n.obs - 1) * nj^2 * ICC2^2 * Fj^2 + (n.obs * (1 +
                                                         (nj - 1) * ICC2) - nj * ICC2)^2
  v <- vn/vd
  F3U <- qf(1 - alpha, n.obs - 1, v)
  F3L <- qf(1 - alpha, v, n.obs - 1)
  L3 <- n.obs * (MSB - F3U * MSE)/(F3U * (nj * MSJ + (nj *
                                                        n.obs - nj - n.obs) * MSE) + n.obs * MSB)
  results[2, 5] <- L3
  U3 <- n.obs * (F3L * MSB - MSE)/(nj * MSJ + (nj * n.obs -
                                                 nj - n.obs) * MSE + n.obs * F3L * MSB)
  results[2, 6] <- U3
  L3k <- L3 * nj/(1 + L3 * (nj - 1))
  U3k <- U3 * nj/(1 + U3 * (nj - 1))
  results[5, 5] <- L3k
  results[5, 6] <- U3k



  if(other_ci == TRUE){
  res_other = boot_rel(x.df,
                       cv_calc = cv_calc,
                       nboot = replicates,
                       conf.level = conf.level)
  } else{
    SEM = sqrt(MSE)
    sd_tots = sqrt(sum(stats[2,])/(n.obs-1))
    SEE = sd_tots*sqrt(ICC3*(1-ICC3))
    SEP = sd_tots*sqrt(1-ICC3^2)

    mw <- mean(x.df$values, na.rm = TRUE)
    if(cv_calc == "residuals"){
      stddev <- sqrt(mean(residuals(mod.lmer)^2))
    } else if(cv_calc == "MSE"){
      stddev <- sqrt(MSE)
    } else if(cv_calc == "SEM"){
      stddev <- SEM
    } else {
      stop("cv_calc must be SEM, MSE, or residuals")
    }

    cv_out = stddev/mw
    res_other = list(cv = list(est = cv_out),
               SEM = list(est = SEM),
               SEP = list(est = SEP),
               SEE = list(est = SEE))
  }

  plot.reliability = ggplot(x.df,
                            aes(
                              x = items,
                              y = values,
                              color = id,
                              group = id
                            )) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_line(color = "black",
              alpha = .2,
              position = position_dodge(width = 0.2)) +
    labs(y = "Measurement",
         x = "Item",
         color = "id") +
    scale_color_viridis_d()+
    theme_bw()

  # Save call
  lm_mod = list(call = list(formula = as.formula(x.df$values ~ x.df$id + x.df$items)))
  call2 = match.call()

  call2$lm_mod = lm_mod
  if(is.null(call2$conf.level)){
    call2$conf.level = conf.level
  }

  if(is.null(call2$other_ci)){
    call2$other_ci = other_ci
  }

  result <- list(icc = results,
                 lmer = mod.lmer,
                 anova = stats,
                 var_comp = MS.df,
                 n.id = nrow(ranef(mod.lmer)$id),
                 n.item = nrow(ranef(mod.lmer)$item),
                 call = call2,
                 cv = res_other$cv,
                 SEM = res_other$SEM,
                 SEE = res_other$SEE,
                 SEP = res_other$SEP)

  structure(result,
            class = "simple_reli")
}
