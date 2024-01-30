#' @title Reliability Statistics
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' The reli_stats and reli_aov functions produce reliability statistics described by Weir (2005).
#' This includes intraclass correlation coefficients, the coefficient of variation,
#'  and the standard MSE of measurement.
#'
#' @param measure Name of column containing the measurement of interest.
#' @param item Name of column containing the items. If this is a test-retest reliability study then this would indicate the time point (e.g., time1,time2, time3, etc.).
#' @param id Column with subject identifier.
#' @param data Data frame with all data.
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95%.
#' @param cv_calc Coefficient of variation (CV) calculation. This function allows for 3 versions of the CV. "MSE" is the default.
#' @param other_ci Logical value (TRUE or FALSE) indicating whether to calculate confidence intervals for the CV, SEM, SEP, and SEE. Note: this will dramatically increase the computation time.
#' @param se_type Type of standard error calculation. The default is to use the mean square error (MSE). Otherwise, the total sums of squares and the ICC are utilized to estimate the SEM, SEE, and SEP.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @param replicates 	The number of bootstrap replicates. Passed on to the boot function. Default is 1999.
#'
#' @details
#' These functions return intraclass correlation coefficients
#' and other measures of reliability (CV, SEM, SEE, and SEP).
#' The estimates of variances for any of the measures are derived from linear mixed models.
#' When other_ci is set to TRUE,
#' then a parametric bootstrap approach to calculating confidence intervals is used for the
#' CV, SEM, SEE, and SEP.
#'
#' reli_stats uses a linear mixed model to estimate variance components.
#' In some cases there are convergence issues.
#' When this occurs it is prudent to use reli_aov which instead utilizes sums of squares approach.
#' The results may differ slightly between the functions.
#' If reli_aov is used then rows with missing observations
#' (e.g., if a participant has a missing observation) will be dropped.
#'
#' The CV calculation has 3 versions. The "MSE" uses the "mean squared error"
#' from the linear mixed model used to calculate the ICCs.
#' The "SEM" option instead uses the SEM calculation and
#' expresses CV as a ratio of the SEM to the overall mean.
#' The "residuals" option u uses the model residuals to calculate the root mean square error which is then divided by the grand mean.
#'
#' The CV, SEM, SEE, and SEP values can have confidence intervals produced if the other_ci argument is set to TRUE.
#' For the CV, the default method (`type = "chisq"`) is Vangal's modification of the McKay approximation.
#' For the other measures, a simple chi-squared approximation is utilized (Hann & Meeker, 1991).
#' All other methods are bootstrapping based methods (see `?boot::boot`).
#' The reli_stats functions utilizes a parametric bootstrap while the reli_aov
#' function utilizes an ordinary (non-parametric) bootstrap method.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#'   - `icc`: Table of ICC results
#'   - `lmer`: Linear mixed model from lme4
#'   - `anova`: Analysis of Variance table
#'   - `var_comp`: Table of Variance Components
#'   - `n.id`: Number of subjects/participants
#'   - `n.items`: Number of items/time points
#'   - `cv`: Coefficient of Variation
#'   - `SEM`: List with Standard MSE of Measurement estimate (est)
#'   - `SEE`: List with Standard MSE of the Estimate estimate (est)
#'   - `SEP`: List with Standard MSE of Predictions (est)
#'   - `call`: the matched call

#' @examples
#' data('reps')
#' reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
#'
#' @references
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass correlation coefficient and the SEM. The Journal of Strength & Conditioning Research, 19(1), 231-240.
#'
#' Shrout, P.E. and Fleiss, J.L. (1976). Intraclass correlations: uses in assessing rater reliability. Psychological Bulletin, 86, 420-3428.
#'
#' McGraw, K. O. and Wong, S. P. (1996). Forming inferences about some intraclass correlation coefficients. Psychological Methods, 1, 30-46. See errata on page 390 of same volume.
#'
#' Hahn, G. J., & Meeker, W. Q. (2011). Statistical intervals: a guide for practitioners (Vol. 92). John Wiley & Sons. pp. 55-56.
#'
#' Vangel, M. G. (1996). Confidence intervals for a normal coefficient of variation. The American Statistician, 50(1), 21-26.
#'
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var residuals aov
#' @importFrom tidyselect all_of
#' @importFrom insight get_response
#' @importFrom tidyr pivot_wider
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
                      se_type = c("MSE","ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"),
                      cv_calc = c("MSE","residuals","SEM"),
                      conf.level = .95,
                      other_ci = FALSE,
                      type = c("chisq", "perc", "norm", "basic"),
                      replicates = 1999){
  se_type = match.arg(se_type)
  cv_calc = match.arg(cv_calc)
  type = match.arg(type)
  alpha = 1-conf.level
  # Organize Data ----
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
  if(length(unique(x.df$items)) == 2){
    message("Only 2 items in data. It is recommended to use reli_aov instead of reli_stats.")
  }
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

  mw <- mean(x.df$values, na.rm = TRUE)
  if(cv_calc == "residuals"){
    stddev <- sqrt(mean(residuals(mod.lmer)^2))
  }
  if(cv_calc == "MSE"){
    stddev <- sqrt(MSE)
  }
  if(cv_calc == "SEM"){
    stddev <- SEM
  }
  cv_out = stddev/mw
# Other CIs -----
  if(other_ci == TRUE){
    if(type != "chisq"){
    boot_reli <- function(.) {
      reli_mod_mse(., cv_calc = cv_calc,
                   se_type = se_type)
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
    } else {
        ## chisq ----
      cv_ci = cv_ci(cv_out,
                    df = dfE,
                    alpha = alpha)
      sem_ci = sigma_ci(SEM,
                        df = dfE,
                        alpha = alpha)
      sep_ci = sigma_ci(SEP,
                        df = dfE,
                        alpha = alpha)
      see_ci = sigma_ci(SEE,
                        df = dfE,
                        alpha = alpha)

      res_other = data.frame(
        estimate = c(cv_out, SEM, SEP, SEE),
        bias = NA,
        se = NA,
        lower.ci = c(cv_ci[1],sem_ci[1],
                     sep_ci[1],see_ci[1]),
        upper.ci = c(cv_ci[2],sem_ci[2],
                     sep_ci[2],see_ci[2]),
        row.names = c("cv", "SEM", "SEP", "SEE")
      )
      }

  } else{

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

#' @rdname reli_stats
#' @export
#'
reli_aov = function(measure,
                    item,
                    id,
                    data,
                    wide = FALSE,
                    col.names = NULL,
                    se_type = c("MSE","ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"),
                    cv_calc = c("MSE","residuals","SEM"),
                    conf.level = .95,
                    other_ci = FALSE,
                    type = c("chisq", "perc", "norm", "basic"),
                    replicates = 1999) {
  se_type = match.arg(se_type)
  cv_calc = match.arg(cv_calc)
  type = match.arg(type)

  # Organize Data ----
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
  na_df = subset(x.df, is.na(values))
  na_drops = unique(na_df$id)
  if(any(is.na(x.df$items)) || any(is.na(x.df$id))){
    stop("Missing values found in item label and/or id labels.")
  }
  x.df =  subset(x.df, !(x.df$id %in% na_drops))

  n_id <- length(unique(x.df$id))
  n_items <- length(unique(x.df$items))
  n_obs = nrow(x.df)

  aov.x <- aov(values~as.factor(id)+as.factor(items),data=x.df)
  s.aov <- summary(aov.x)

  stats <- matrix(unlist(s.aov),ncol=3,byrow=TRUE)
  MSB <- stats[3,1]
  MSW <- (stats[2,2] + stats[2,3])/(stats[1,2] + stats[1,3])
  MSJ <- stats[3,2]
  MSE <- stats[3,3]


  # Create variance table -----
  MS_items = MSW - MSE
  MS_id = (MSB-MSE)/n_items
  MS.df <- data.frame(variance = c(MS_id,
                                   ifelse(MS_items < 0 , 0,MS_items),
                                   MSE,
                                   NA))
  rownames(MS.df) <- c("ID",
                       "Items",
                       "Residual",
                       "Total")
  MS.df["Total", ] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["percent"] <- MS.df/MS.df["Total", 1]

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
  # Calculate ICCs-----
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

  # F-tests ------
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

  mw <- mean(x.df$values, na.rm = TRUE)
  if(cv_calc == "residuals"){
    stddev <- sqrt(mean(residuals(aov.x)^2))
  }
  if(cv_calc == "MSE"){
    stddev <- sqrt(MSE)
  }
  if(cv_calc == "SEM"){
    stddev <- SEM
  }
  cv_out = stddev/mw
  # Other CIs-----
  if(other_ci == TRUE){
    if(type != "chisq"){
      #stop("Bootstrapping for these statistics is currently not supported for this function.")

      wide_df = x.df %>%
        pivot_wider(values_from = values,
                    names_from = items,
                    names_prefix = "item_")

      boot_function <- function(data, indices,
                                cv_calc = "MSE") {
        d <- data[indices, ] # allows boot to select sample
        d$id = 1:nrow(d)

        x.df = d %>%
          pivot_longer(cols = starts_with("item"),
                       names_to = "items",
                       values_to = "values") %>%
          mutate(id = factor(id),
                 items = factor(items))

        n_id <- length(unique(x.df$id))
        n_items <- length(unique(x.df$items))

        aov.x <- aov(values~as.factor(id)+as.factor(items),data=x.df)
        s.aov <- summary(aov.x)
        stats <- matrix(unlist(s.aov),ncol=3,byrow=TRUE)
        MSB <- stats[3,1]
        MSW <- (stats[2,2] + stats[2,3])/(stats[1,2] + stats[1,3])
        MSJ <- stats[3,2]
        MSE <- stats[3,3]
        SEM = sqrt(MSE)
        sd_tots = sqrt(sum(stats[2,])/(n_id-1))
        SEE = sd_tots*sqrt(ICC3*(1-ICC3))
        SEP = sd_tots*sqrt(1-ICC3^2)

        mw <- mean(x.df$values, na.rm = TRUE)
        if(cv_calc == "residuals"){
          stddev <- sqrt(mean(residuals(aov.x)^2))
        } else if(cv_calc == "MSE"){
          stddev <- sqrt(MSE)
        } else if(cv_calc == "SEM"){
          stddev <- SEM
        } else {
          stop("cv_calc must be SEM, MSE, or residuals")
        }
        cv_out = stddev/mw

        res = c(
          cv = cv_out,
          SEM = SEM,
          SEE = SEE,
          SEP = SEP
        )
        return(res)
      }

      boo2 = boot::boot(wide_df, boot_function,  R = replicates)

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
      ## chisq ----
      cv_ci = cv_ci(cv_out,
                    df = dfE,
                    alpha = alpha)
      sem_ci = sigma_ci(SEM,
                        df = dfE,
                        alpha = alpha)
      sep_ci = sigma_ci(SEP,
                        df = dfE,
                        alpha = alpha)
      see_ci = sigma_ci(SEE,
                        df = dfE,
                        alpha = alpha)

      res_other = data.frame(
        estimate = c(cv_out, SEM, SEP, SEE),
        bias = NA,
        se = NA,
        lower.ci = c(cv_ci[1],sem_ci[1],
                     sep_ci[1],see_ci[1]),
        upper.ci = c(cv_ci[2],sem_ci[2],
                     sep_ci[2],see_ci[2]),
        row.names = c("cv", "SEM", "SEP", "SEE")
      )

    }

  } else{
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
                 lmer = aov.x,
                 anova = stats,
                 var_comp = MS.df,
                 n.id = n_id,
                 n.item = n_items,
                 call = call2,
                 cv = res_other["cv",],
                 SEM = res_other["SEM",],
                 SEE = res_other["SEE",],
                 SEP = res_other["SEP",])


  structure(result,
            class = "simple_reli")
}

# other functions ----

cv_ci = function(cv,
                 df,
                 alpha){
  u1 <- qchisq(1-alpha/2,df)
  u2 <- qchisq(alpha/2,df)
  u = c(u1,u2)
  #lower <- cv/sqrt(( (u1+2)/(df+1) -1)*cv*cv + u1/df)
  #upper <- cv/sqrt(( (u2+2)/(df+1) -1)*cv*cv + u2/df)

  res_vec = cv/sqrt(( (u+2)/(df+1) -1)*cv*cv + u/df)

  return(res_vec)
}

sigma_ci = function(sigma,
                    df,
                    alpha){

  lcl <-  sqrt( (df*sigma^2) / qchisq(p = 1-alpha/2, df = df) )
  ucl <-  sqrt( (df*sigma^2) / qchisq(p = alpha/2, df = df) )
  res_vec <- c(lcl,ucl)

  return(res_vec)
}



