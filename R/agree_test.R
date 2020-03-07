#' Tests for Absolute Agreement
#' @param x Criterion measurement, or first measurement if repeated measures
#' @param y Other measurement, or second measurement if repeated measures
#' @param LoA Limit of Agreement between 0 and 1; .95 = 95\% limit of agreement.
#' @param delta Equivalence Bound for Agreement.
#' @param rep.measure logical value; if TRUE then x & y are repeated measures across subjects
#' @param alpha Set the desired Type I error rate; default is .05
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
#'   \item{\code{"df_diff"}}{a data frame with 4 columns: x = data column x from arguments, y = data column y from agruments, mean = the mean of each pair of measurements, delta = vector y minus vector x.}
#'   \item{\code{"delta"}}{a data frame listing the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements. If rep.measure == TRUE the confidence interval of the difference is adjusted to account for repeated observations across individual subjects.}
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
#'


agree_test <- function(x,y,LoA=0.8,
                       delta=.1,
                       rep.measure=FALSE,
                       alpha=0.05,
                       verbose = FALSE) {
  if (LoA >= 1 || LoA <= 0) {

    stop("Limit of Agreement (LoA) must be a value between 0 and 1")
  }

  if (alpha >= 1 || alpha <= 0) {

    stop("alpha must be a value between 0 and 1")
  }
  #USER SPECIFICATIONS PORTION
  #alpha<-0.05 #DESIGNATED ALPHA
  #prop0<-0.8 #NULL CENTRAL PROPORTION or Limit of Agreement
  #delta<-0.1 #THRESHOLD
  #n<-15 #SAMPLE SIZE
  #xbar<-0.011 #SAMPLE MEAN
  #s<-0.044 #SAMPLE STANDARD DEVIATION
  #END OF SPECIFICATION
  prop0 = LoA
  conf.level=(1-alpha)
  ccc_res = ccc.xy(x,y,rep.measure=rep.measure, conf.level=(1-alpha))
  #pull values from ccc function output
  xbar = ccc_res$delta$est #mean delta
  s = ccc_res$delta$delta.sd #sd of delta
  n = nrow(ccc_res$df_diff)

  pct<-1-(1-prop0)/2
  zp<-qnorm(pct)
  df<-n-1
  stdh<-s/sqrt(n)
  numint<-1000
  coevec<-c(1,rep(c(4,2),numint/2-1),4,1)
  cl<-1e-6
  cu<-qchisq(1-cl,df)
  int<-cu-cl
  intl<-int/numint
  cvec<-cl+intl*(0:numint)
  wcpdf<-(intl/3)*coevec*dchisq(cvec,df)
  gaml<-0
  gamu<-100
  loop<-0
  dalpha<-1
  while(abs(dalpha)>1e-8 | dalpha<0){
    gam<-(gaml+gamu)/2
    h<-zp*sqrt(n)-gam*sqrt(cvec/df)
    ht<-h*(cvec<n*df*(zp/gam)^2)
    alphat<-sum(wcpdf*(2*pnorm(ht)-1))
    if (alphat>alpha) gaml<-gam else gamu<-gam
    loop<-loop+1
    dalpha<-alphat-alpha
  }
  el<-xbar-gam*stdh
  eu<-xbar+gam*stdh
  rej<-(-delta<el)*(eu<delta)
  rej_text = "don't reject h0"
  if(rej==1){
    rej_text = "reject h0"
  }

  shieh_test = data.frame(prop0,el,eu,rej_text,gam)
  names(shieh_test) = c("LoA","lower.ci","upper.ci", "h0_test","test_statistic")

  #######################
  # Plot Results ----
  #######################

  z <- lm(y ~ x)
  the_int <- summary(z)$coefficients[1,1]
  the_slope <-  summary(z)$coefficients[2,1]
  tmp.lm <- data.frame(alpha, beta)

  identity.plot = ggplot(ccc_res$df_diff, aes(x = x, y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab("Method: x") +
    ylab("Method: y") +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  bor = delta
  neg.bor = -1*bor
  bland_alt.plot =  ggplot(ccc_res$df_diff, aes(x = mean, y = delta)) +
    geom_point(na.rm = TRUE) +
    geom_ribbon(aes(ymin = neg.bor, ymax = bor),
                alpha = .1) +
    geom_hline(data = shieh_test,
               aes(yintercept = lower.ci, color = "red"),
               linetype = 2) +
    geom_hline(data = shieh_test,
               aes(yintercept = upper.ci, color = "red"),
               linetype = 2) +
    geom_hline(data = ccc_res$delta,
               aes(yintercept = est),
               linetype = 1) +
    xlab("Average of Method x and Method y") +
    ylab("Difference between Methods x & y") +
    theme_bw() +
    theme(legend.position = "none")


  #######################
  # Return Results ----
  #######################

  if (verbose == TRUE) {
    # The section below should be blocked out when in Shiny

    cat(100*LoA,"% Limits of Agreement |"," Null Central Proportion = ", LoA,  sep = "")
    cat("\n")
    cat("alpha =", alpha, "|", (1-alpha)*100,"% Confidence Interval")
    cat("\n")
    cat("### TOST Results ###")
    cat("\n")
    cat("Exact C.I.:"," [",round(el,4),", ",round(eu,4),"]",sep="")
    cat("\n")
    cat("test: ",rej_text,sep="")
    cat("\n")
    cat("### Concordance Correlation Coefficient (CCC) ###")
    cat("\n")
    cat("CCC: ",round(ccc_res$rho.c$est.ccc,4),", ",100*conf.level,"% C.I. ","[",round(ccc_res$rho.c$lower.ci,4),", ",round(ccc_res$rho.c$upper.ci,4),"]",sep="")

  }

  res_list <- list(shieh_test = shieh_test,
                   ccc.xy = ccc_res$rho.c,
                   s.shift = ccc_res$s.shift,
                   l.shift = ccc_res$l.shift,
                   bias = ccc_res$bias,
                   df_diff = ccc_res$df_diff,
                   delta = ccc_res$delta,
                   bland_alt.plot = bland_alt.plot,
                   identity.plot = identity.plot)



}
