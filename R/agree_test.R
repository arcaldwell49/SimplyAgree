#' Tests for Absolute Agreement
#'
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' Development on `agree_test()` is complete, and for new code we recommend
#' switching to `agreement_limit()`, which is easier to use, has more features,
#' and still under active development.
#'
#' The agree_test function calculates a variety of agreement statistics. The hypothesis test of agreement is calculated by the method described by Shieh (2019). Bland-Altman limits of agreement, and confidence intervals, are also provided (Bland & Altman 1999; Bland & Altman 1986).
#' In addition, the concordance correlation coefficient (CCC; Lin 1989) is additional part of the output.
#'
#' @param x Vector with first measurement
#' @param y Vector with second measurement
#' @param conf.level the confidence level required. Default is 95%.
#' @param agree.level the agreement level required. Default is 95%. The proportion of data that should lie between the thresholds, for 95% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Often referred to as the "Equivalence Bound for Agreement" or "Maximal Allowable Difference".
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param TOST Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests for the limits of agreement. Default is TRUE.
#'
#' @return Returns single list with the results of the agreement analysis.
#'
#'   - `shieh_test`: The TOST hypothesis test as described by Shieh.
#'   - `ccc.xy`: Lin's concordance correlation coefficient and confidence intervals.
#'   - `s.shift`: Scale shift from x to y.
#'   - `l.shift`: Location shift from x to y.
#'   - `bias`: a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees. No deviation from the 45 degree line occurs when bias = 1. See Lin 1989, page 258.
#'   - `loa`: Data frame containing the limits of agreement calculations
#'   - `h0_test`: Decision from hypothesis test.
#'   - `call`: the matched call
#'
#' @examples
#' data('reps')
#' agree_test(x=reps$x, y=reps$y, delta = 2)
#'
#' @references
#' Shieh (2019). Assessing Agreement Between Two Methods of Quantitative Measurements: Exact Test Procedure and Sample Size Calculation,
#' Statistics in Biopharmaceutical Research,
#' \doi{10.1080/19466315.2019.1677495}
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
                       TOST = TRUE,
                       prop_bias = FALSE) {
  lifecycle::deprecate_soft("0.2.0", "agree_test()", "agreement_limit()")
  est <- lower.ci <- upper.ci <- NULL
  if (agree.level >= 1 || agree.level <= 0) {

    stop("agree.level (Limit of Agreement) must be a value between 0 and 1")
  }

  if (conf.level >= 1 || conf.level <= 0) {

    stop("conf.level must be a value between 0 and 1")
  }
  # shieh test ----
  prop0 = agree.level
  if(TOST == TRUE) {
    alpha = (1 - conf.level)
    conf2 = 1 - (1 - conf.level) * 2
  } else {
    alpha = (1 - conf.level) / 2
    conf2 = conf.level
  }
  #alpha = 1 - conf.level
  # ccc calc ----
  if(prop_bias == TRUE){
    message("prop_bias set to TRUE. Hypothesis test may be bogus. Check plots.")
  }
  ccc_res = ccc.xy(x, y,
                   conf.level = conf.level,
                   agree.level = agree.level,
                   TOST = TOST,
                   prop_bias = prop_bias)
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

  # Save LoA ----

  df_loa = data.frame(
    estimate = c(ccc_res$delta$d, ccc_res$delta$lower.loa, ccc_res$delta$upper.loa),
    lower.ci = c(ccc_res$delta$d.lci, ccc_res$delta$lower.lci, ccc_res$delta$upper.lci),
    upper.ci = c(ccc_res$delta$d.uci, ccc_res$delta$lower.uci, ccc_res$delta$upper.uci),
    ci.level = c(conf.level, conf2, conf2),
    row.names = c("Bias","Lower LoA","Upper LoA")
  )
  # Should I add this to the output?
  var_comp = data.frame(
    sd_delta = ccc_res$delta$d.sd,
    sd_loa = sqrt(ccc_res$delta$var.loa)
  )

  # Save call -----
  lm_mod = list(call = list(formula = as.formula(y~x)))
  call2 = match.call()
  if(is.null(call2$agree.level)){
    call2$agree.level = agree.level
  }

  if(is.null(call2$conf.level)){
    call2$conf.level = conf.level
  }
  if(is.null(call2$TOST)){
    call2$TOST = TOST
  }
  if(is.null(call2$prop_bias)){
    call2$prop_bias = prop_bias
  }
  call2$lm_mod = lm_mod

  # Return Results ----

  structure(list(shieh_test = shieh_test,
                 ccc.xy = ccc_res$rho.c,
                 s.shift = ccc_res$s.shift,
                 l.shift = ccc_res$l.shift,
                 bias = ccc_res$bias,
                 loa = df_loa,
                 h0_test = rej_text,
                 var_comp = var_comp,
                 call = call2,
                 class = "simple"),
            class = "simple_agree")



}
