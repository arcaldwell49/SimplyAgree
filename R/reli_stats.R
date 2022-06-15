#' Reliability Statistics
#' @description reli_stats produces reliability statistics described by Weir (2005). This includes intraclass correlation coefficients, the coefficient of variation, and the standard MSE of measurement.
#' @param measure Name of column containing the measurement of interest
#' @param item Name of column containing the items. If this is a test-retest reliability study then this would indicate the time point (e.g., time1,time2, time3, etc.)
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95\%.
#' @param cv_calc Coefficient of variation (CV) calculation. This function allows for 3 versions of the CV. "MSE" is the default.
#' @param other_ci Logical value (TRUE or FALSE) indicating whether to calculate confidence intervals for the CV, SEM, SEP, and SEE. Note: this will dramatically increase the computation time.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @param replicates 	The number of bootstrap replicates. Passed on to the boot function. Default is 1999.
#' @details
#'
#' The CV calculation has 3 versions. The "MSE" uses the "mean squared error" from the linear mixed model used to calculate the ICCs.
#' The "SEM" option instead uses the SEM calculation and expresses CV as a ratio of the SEM to the overall mean.
#' The "residuals" option uses the sjstats R package approach which uses the model residuals to calculate the root mean square error which is then divided by the grand mean.
#'
#' @details
#' This function returns intraclass correlation coefficients and other measures of reliability (CV, SEM, SEE, and SEP).
#' The estimates of variances for any of the measures are derived from linear mixed models.
#' The results may differ slightly from the results from ICC calculations derived from an analysis of variance.
#' When other_ci is set to TRUE, then a parametric bootstrap approach to calculating confidence intervals is used for the CV, SEM, SEE, and SEP.
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
#'   \item{\code{"SEM"}}{List with Standard MSE of Measurement estimate (est) }
#'   \item{\code{"SEE"}}{List with Standard MSE of the Estimate estimate (est)}
#'   \item{\code{"SEP"}}{List with Standard MSE of Predicitions (est)}
#'   \item{\code{"call"}}{the matched call}
#'
#' }

#' @examples
#' data('reps')
#' reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
#'
#' @section References:
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM. The Journal of Strength & Conditioning Research, 19(1), 231-240.
#'
#' Shrout, P.E. and Fleiss, J.L. (1976). Intraclass correlations: uses in assessing rater reliability. Psychological Bulletin, 86, 420-3428.
#'
#' McGraw, K. O. and Wong, S. P. (1996). Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1, 30-46. See errata on page 390 of same volume.
#'
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var residuals
#' @importFrom tidyselect all_of
#' @importFrom insight get_response
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
                      type = "perc",
                      replicates = 1999){
  alpha = 1-conf.level
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
  }

  colnames(x.df) <- c("values", "items", "id")

  mod.lmer <- lmer(values ~ 1 + (1 | id) + (1 | items),
                   data = x.df,
                   na.action = na.omit)
  num_lvls = ngrps(mod.lmer)
  n_items = num_lvls["items"]
  n_id = num_lvls["id"]
  vc <- VarCorr(mod.lmer) # Get Variance Components
  MS_id <- vc$id[1, 1] # var by id
  MS_items <- vc$items[1, 1] # var by item
  MSE <- (attributes(vc)$sc)^2
  # Create variance table
  MS.df <- data.frame(variance = c(MS_id, MS_items, MSE,
                                   NA))
  rownames(MS.df) <- c("ID", "Items", "Residual", "Total")
  MS.df["Total", ] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["percent"] <- MS.df/MS.df["Total", 1]
  lmer.MS <- MS.df
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
  # Calculate ICCs
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


  if(other_ci == TRUE){

    boot_reli <- function(.) {
      reli_mod_mse(., cv_calc = cv_calc)
    }

    boo2 <- bootMer(mod.lmer, boot_reli, nsim = replicates,
                    type = "parametric", use.u = FALSE)
    res_other = tidy_boot(boo2,
                         conf.int = TRUE,
                         conf.level = conf.level,
                         conf.method = type) %>%
      rename(estimate = statistic,
             se = std.error,
             lower.ci = conf.low,
             upper.ci = conf.high) %>%
      as.data.frame()
      row.names(res_other) = res_other$term
      res_other = res_other %>%
        select(estimate,bias,se,lower.ci,upper.ci)
  } else{
    SEM = sqrt(MSE)
    sd_tots = sqrt(sum(stats[2,])/(n_id-1))
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
    res_other = data.frame(
      estimate = c(cv_out, SEM, SEP, SEE),
      bias = NA,
      se = NA,
      lower.ci = NA,
      upper.ci = NA,
      row.names = c("cv", "SEM", "SEP", "SEE")
    )
  }


  # Save call
  lm_mod = list(call = list(formula = as.formula(x.df$values ~ x.df$id + x.df$items)))
  call2 = match.call()

  call2$lm_mod = lm_mod

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
                 cv = res_other["cv",],
                 SEM = res_other["SEM",],
                 SEE = res_other["SEE",],
                 SEP = res_other["SEP",])


  structure(result,
            class = "simple_reli")
}
