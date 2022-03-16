#' Tests for Absolute Agreement
#' @description The agree_test function calculates a variety of agreement statistics. The hypothesis test of agreement is calculated by the method described by Shieh (2019). Bland-Altman limits of agreement, and confidence intervals, are also provided (Bland & Altman 1999; Bland & Altman 1986). In addition, the concordance correlation coefficient (CCC; Lin 1989) is also provided.
#' @param x Vector with first measurement
#' @param y Vector with second measurement
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Often referred to as the "Equivalence Bound for Agreement" or "Maximal Allowable Difference".
#' @param x_lab Label for x values (first measurement)
#' @param y_lab Label for y values (second measurement)
#' @param smooth_method Smoothing method (function) to use, accepts either NULL or a character vector, e.g. "lm", "glm", "gam", "loess" or a function. Default is NULL, which will not include a trend line.
#' @param smooth_se Display confidence interval around smooth?
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"shieh_test"}}{The TOST hypothesis test as described by Shieh.}
#'   \item{\code{"ccc.xy"}}{Lin's concordance correlation coefficient and confidence intervals.}
#'   \item{\code{"s.shift"}}{Scale shift from x to y.}
#'   \item{\code{"l.shift"}}{Location shift from x to y.}
#'   \item{\code{"bias"}}{a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees. No deviation from the 45 degree line occurs when bias = 1. See Lin 1989, page 258.}
#'   \item{\code{"loa"}}{Data frame containing the limits of agreement calculations}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"identity.plot"}}{Plot of x and y with a line of identity with a linear regression line}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'
#' }

#' @examples
#' data('reps')
#' agree_test(x=reps$x, y=reps$y, delta = 2)
#'
#' @section References:
#' Shieh (2019). Assessing Agreement Between Two Methods of Quantitative Measurements: Exact Test Procedure and Sample Size Calculation, Statistics in Biopharmaceutical Research, <https://doi.org/10.1080/19466315.2019.1677495>
#'
#' Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method comparison studies. Statistical methods in medical research, 8(2), 135-160.
#'
#' Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The lancet, 327(8476), 307-310.
#'
#' Lawrence, I., & Lin, K. (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics, 255-268.
#' @importFrom stats pnorm pt qnorm qt lm anova aov complete.cases cor dchisq qchisq sd var prcomp
#' @importFrom graphics text
#' @import ggplot2
#' @export

agree_test <- function(x,
                       y,
                       delta,
                       conf.level = .95,
                       agree.level = .95,
                       x_lab = "x",
                       y_lab = "y",
                       smooth_method = NULL,
                       smooth_se = TRUE) {
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
  if (!missing(delta)){
  rej <- (-delta < el) * (eu < delta)
  rej_text = "don't reject h0"
  if (rej == 1) {
    rej_text = "reject h0"
  } } else {
    rej_text = "No Hypothesis Test"
  }

  shieh_test = data.frame(prop0,el,eu,rej_text,gam)
  names(shieh_test) = c("prop0","lower.ci","upper.ci", "h0_test","test_statistic")


  ### Save limits of agreement

  df_loa = data.frame(
    estimate = c(ccc_res$delta$d, ccc_res$delta$lower.loa, ccc_res$delta$upper.loa),
    lower.ci = c(ccc_res$delta$d.lci, ccc_res$delta$lower.lci, ccc_res$delta$upper.lci),
    upper.ci = c(ccc_res$delta$d.uci, ccc_res$delta$lower.uci, ccc_res$delta$upper.uci),
    row.names = c("Difference","Lower LoA","Upper LoA")
  )
  # Should I add this to the output?
  var_comp = data.frame(
    delta.sd = ccc_res$delta$d.sd,
    var.loa = ccc_res$delta$var.loa
  )

  #######################
  # Plot Results ----
  #######################

  scalemin = min(c(min(x, na.rm = TRUE),min(y, na.rm = TRUE)))
  scalemax = max(c(max(x, na.rm = TRUE),max(y, na.rm = TRUE)))

  df_loa2 = df_loa
  df_loa2$x = scalemin
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))

  #z <- lm(y ~ x)
  #the_int <- summary(z)$coefficients[1,1]
  #the_slope <-  summary(z)$coefficients[2,1]
  #tmp.lm <- data.frame(the_int, the_slope)
  pd2 = position_dodge2(.03*(scalemax-scalemin))

  # Deming Regression through PCA
  pca <- prcomp(~x+y, ccc_res$df_diff)
  slp <- with(pca, rotation[2,1] / rotation[1,1])
  int <- with(pca, center[2] - slp*center[1])
  tmp.lm <- data.frame(the_int = int, the_slope = slp)

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
    xlab(paste0("Method: ",x_lab)) +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab(paste0("Method: ",y_lab)) +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  bland_alt.plot =  ggplot(ccc_res$df_diff,
                           aes(x = mean, y = delta)) +
    geom_point(na.rm = TRUE) +
    geom_pointrange(data = df_loa2,
                    aes(
                      x = x,
                      y = estimate,
                      ymin = lower.ci,
                      ymax = upper.ci,
                      color = text),
                    #width = .03*(scalemax-scalemin),
                    position = pd2,
                    inherit.aes = FALSE)+
    labs(x = paste0("Average of ", x_lab ," & ", y_lab),
         y = paste0("Difference between Methods ",x_lab ," & ", y_lab),
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")
  if (!missing(delta)){
    df_delta = data.frame(y1 = c(delta, -1*delta))
    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = c(delta, -1*delta),
        name = "Maximal Allowable Difference"))
  }
  if (!is.null(smooth_method)){
    if (!(smooth_method %in% c("loess", "lm", "gam"))){
      stop("Only lm, loess, and gam are supported as smooth_method at this time.")
    }
    if(smooth_method != "gam"){
      bland_alt.plot = bland_alt.plot +
        stat_smooth(
          method = smooth_method,
          se = smooth_se,
          level = conf.level,
          alpha = 0.2,
          formula = y ~ x,
          size = 0.8,
          colour = "#3aaf85"
        )
    } else {
      bland_alt.plot = bland_alt.plot +
        stat_smooth(
          method = smooth_method,
          se = smooth_se,
          level = conf.level,
          alpha = 0.2,
          formula = y ~ s(x, bs = "tp"),
          size = 0.8,
          colour = "#3aaf85"
        )
    }

  }




  #######################
  # Return Results ----
  #######################

  structure(list(shieh_test = shieh_test,
                 ccc.xy = ccc_res$rho.c,
                 s.shift = ccc_res$s.shift,
                 l.shift = ccc_res$l.shift,
                 bias = ccc_res$bias,
                 loa = df_loa,
                 conf.level = conf.level,
                 agree.level = agree.level,
                 bland_alt.plot = bland_alt.plot,
                 identity.plot = identity.plot,
                 h0_test = rej_text,
                 class = "simple"),
            class = "simple_agree")



}
