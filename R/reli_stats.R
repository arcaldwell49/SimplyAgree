#' TReliability Statistics
#' @param x Name of column containing the measurement of interest
#' @param item Name of column containing the items. If this is a test-retest reliability study then this would indicate the time point (e.g., time1,time2, time3, etc.)
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95\%.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.
#'   \item{\code{"conf.level"}}{Returned as input.}
#'
#' }

#' @examples #to be added
#'
#' @section References:
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var mean
#' @importFrom tidyselect all_of
#' @import dplyr
#' @import ggplot2
#' @export


reli_stats = function(measure,
                      item,
                      id,
                      data,
                      wide = FALSE,
                      col.names = NULL,
                      conf.level = .95){
  alpha = 1-conf.level
  x = data
  if(wide == TRUE){
    if(is.null(col.names)){
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    x = x[,col.names]
    n.obs <- dim(x)[1]
    nj <- dim(x)[2]
    x.s <- stack(as.data.frame(sf))
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
  vc <- lme4::VarCorr(mod.lmer) # Get Variance Components
  MS_id <- vc$id[1, 1] # var by id
  MS_items <- vc$items[1, 1] # var by item
  MSE <- error <- MS_resid <- (attributes(vc)$sc)^2
  # Create variance table
  MS.df <- data.frame(variance = c(MS_id, MS_items, MS_resid,
                                   NA))
  rownames(MS.df) <- c("ID", "Items", "Residual", "Total")
  MS.df["Total", ] <- sum(MS.df[1:3, 1], na.rm = TRUE)
  MS.df["Percent"] <- MS.df/MS.df["Total", 1]
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

  cv = sjstats::cv(mod.lmer)
  SEM = sqrt(MSE)
  SEP = sqrt(MSE)*sqrt(1-ICC32^2)

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

  result <- list(results = results,
                 lmer = mod.lmer,
                 anova = stats.final,
                 var_comp = MS.df,
                 n.id = nrow(ranef(mod.lmer)$id),
                 n.item = nrow(ranef(mod.lmer)$item),
                 cv = cv,
                 SEM = SEM,
                 SEP = SEP,
                 plot.reliability)

  structure(result,
            class = "simple_agree")
}
