#' Tests for Absolute Agreement
#' @param x Criterion measurement, or first measurement if repeated measures
#' @param y Other measurement, or second measurement if repeated measures#'
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param verbose Option to print a summary of results to the console.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"shieh_test"}}{The TOST hypothesis test as described by Shieh.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals.}
#'   \item{\code{"s.shift"}}{Scale shift from x to y.}
#'   \item{\code{"l.shift"}}{Location shift from x to y.}
#'   \item{\code{"bias"}}{a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees. No deviation from the 45 degree line occurs when bias = 1. See Lin 1989, page 258.}
#'   \item{\code{"delta"}}{a data frame listing the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'
#' }

#' @examples #to be added
#'
#' @section References:
#' Gwowen Shieh (2019): Assessing Agreement Between Two Methods of Quantitative Measurements: Exact Test Procedure and Sample Size Calculation, Statistics in Biopharmaceutical Research, <https://doi.org/10.1080/19466315.2019.1677495>
#' @importFrom stats pnorm pt qnorm qt lm anova aov complete.cases cor dchisq qchisq sd var
#' @import ggplot2
#' @export

agree_test <- function(x,
                       y,
                       delta=.1,
                       conf.level = .95,
                       agree.level = .95,
                       verbose = FALSE) {
  est <- lower.ci <- upper.ci <- NULL
  if (agree.level >= 1 || agree.level <= 0) {

    stop("agree.level (Limit of Agreement) must be a value between 0 and 1")
  }

  if (conf.level >= 1 || conf.level <= 0) {

    stop("conf.level must be a value between 0 and 1")
  }
  #USER SPECIFICATIONS PORTION
  #alpha<-0.05 #DESIGNATED ALPHA
  #prop0<-0.8 #NULL CENTRAL PROPORTION or Limit of Agreement
  #delta<-0.1 #THRESHOLD
  #n<-15 #SAMPLE SIZE
  #xbar<-0.011 #SAMPLE MEAN
  #s<-0.044 #SAMPLE STANDARD DEVIATION
  #END OF SPECIFICATION
  prop0 = agree.level
  alpha = 1 - conf.level
  ccc_res = ccc.xy(x, y,
                   conf.level = conf.level,
                   agree.level = agree.level)
  #pull values from ccc function output
  xbar = ccc_res$delta$d #mean delta
  s = ccc_res$delta$d.sd #sd of delta
  n = nrow(ccc_res$df_diff)

  pct <- 1 - (1 - prop0) / 2
  zp <- qnorm(pct)
  df <- n - 1
  stdh <- s / sqrt(n)
  numint <- 1000
  coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  cl <- 1e-6
  cu <- qchisq(1 - cl, df)
  int <- cu - cl
  intl <- int / numint
  cvec <- cl + intl * (0:numint)
  wcpdf <- (intl / 3) * coevec * dchisq(cvec, df)
  gaml <- 0
  gamu <- 100
  loop <- 0
  dalpha <- 1
  while (abs(dalpha) > 1e-8 | dalpha < 0) {
    gam <- (gaml + gamu) / 2
    h <- zp * sqrt(n) - gam * sqrt(cvec / df)
    ht <- h * (cvec < n * df * (zp / gam) ^ 2)
    alphat <- sum(wcpdf * (2 * pnorm(ht) - 1))
    if (alphat > alpha)
      gaml <- gam
    else
      gamu <- gam
    loop <- loop + 1
    dalpha <- alphat - alpha
  }
  el <- xbar - gam * stdh
  eu <- xbar + gam * stdh
  rej <- (-delta < el) * (eu < delta)
  rej_text = "don't reject h0"
  if (rej == 1) {
    rej_text = "reject h0"
  }

  shieh_test = data.frame(prop0,el,eu,rej_text,gam)
  names(shieh_test) = c("prop0","lower.ci","upper.ci", "h0_test","test_statistic")

  #######################
  # Plot Results ----
  #######################

  z <- lm(y ~ x)
  the_int <- summary(z)$coefficients[1,1]
  the_slope <-  summary(z)$coefficients[2,1]
  tmp.lm <- data.frame(the_int, the_slope)
  scalemin = min(c(min(x),min(y)))
  scalemax = max(c(max(x),max(y)))

  identity.plot = ggplot(ccc_res$df_diff,
                         aes(x = x, y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab("Method: x") +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab("Method: y") +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  bor = delta
  neg.bor = -1*bor
  bland_alt.plot =  ggplot(ccc_res$df_diff,
                           aes(x = mean, y = delta)) +
    geom_point(na.rm = TRUE) +
    #geom_ribbon(aes(ymin = neg.bor,
    #                ymax = bor),
    #            alpha = .1) +
    #geom_hline(data = shieh_test,
    #           aes(yintercept = lower.ci), # Removing Shieh line for time being
    #           linetype = 2) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = ccc_res$delta$lower.lci,
             ymax = ccc_res$delta$lower.uci,
             alpha = .5,
             fill = "#D55E00") +
    #geom_hline(data = shieh_test,
    #           aes(yintercept = upper.ci),
    #           linetype = 2) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = ccc_res$delta$upper.lci,
             ymax = ccc_res$delta$upper.uci,
             alpha = .5,
             fill = "#D55E00") +
    geom_hline(data = ccc_res$delta,
               aes(yintercept = d),
               linetype = 1) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = ccc_res$delta$d.lci,
             ymax = ccc_res$delta$d.uci,
             alpha = .5,
             fill = "gray") +
    xlab("Average of Method x and Method y") +
    ylab("Difference between Methods x & y") +
    theme_bw() +
    theme(legend.position = "none")


  #######################
  # Return Results ----
  #######################

  if (verbose == TRUE) {
    # The section below should be blocked out when in Shiny

    cat("Limit of Agreement = ", prop0*100, "%",  sep = "")
    cat("\n")
    cat("alpha =", alpha, "|", (1 - alpha)*100,"% Confidence Interval")
    cat("\n")
    cat("### Shieh TOST Results ###")
    cat("\n")
    cat("Exact C.I.:"," [",round(el,4),", ",round(eu, 4), "]", sep = "")
    cat("\n")
    cat("test: ",rej_text, sep = "")
    cat("\n")
    cat("### Bland-Altman Limits of Agreement (LoA) ###")
    cat("\n")
    cat("Mean Bias:",ccc_res$delta$d,"[",ccc_res$delta$d.lci,", ",ccc_res$delta$d.uci,"]")
    cat("\n")
    cat("Lower LoA:",ccc_res$delta$l.loa,"[",ccc_res$delta$lower.lci,", ",ccc_res$delta$lower.uci,"]")
    cat("\n")
    cat("Upper LoA:",ccc_res$delta$u.loa,"[",ccc_res$delta$upper.lci,", ",ccc_res$delta$upper.uci,"]")
    cat("\n")
    cat("### Concordance Correlation Coefficient (CCC) ###")
    cat("\n")
    cat("CCC: ",round(ccc_res$rho.c$est.ccc,4),", ",100*conf.level,"% C.I. ","[",round(ccc_res$rho.c$lower.ci,4),", ",round(ccc_res$rho.c$upper.ci,4),"]",sep = "")

  }

  res_list <- list(shieh_test = shieh_test,
                   ccc.xy = ccc_res$rho.c,
                   s.shift = ccc_res$s.shift,
                   l.shift = ccc_res$l.shift,
                   bias = ccc_res$bias,
                   delta = ccc_res$delta,
                   bland_alt.plot = bland_alt.plot,
                   identity.plot = identity.plot)



}
