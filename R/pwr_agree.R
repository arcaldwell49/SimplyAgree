#' Power for Shieh Hypothesis Test for Absolute Agreement
#'
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param power Nominal power, the probability of rejecting the null-hypothesis when the alternative hypothesis, the specified null proportion of the data lies between the thresholds, is true.
#' @param mu Mean difference between methods, based on previous research or pilot work.
#' @param sigma SD of difference, based on previous research or pilot work. This SD can be determined from the limits of agreement.
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#'
#'
#' @return Returns single list with the results of the power analysis for Shieh equivalence bounds hypothesis test for agreement.
#'
#' \describe{
#'   \item{\code{"gam"}}{Gamma statistic.}
#'   \item{\code{"power"}}{Estimated power (1-Beta).}
#'   \item{\code{"conf.level"}}{Returned as input.}
#'   \item{\code{"agree.level"}}{Returned as input.}
#'   \item{\code{"n"}}{Sample Size Requirement.}
#'   \item{\code{"mu"}}{Specified mean difference between methods.}
#'   \item{\code{"sigma"}}{Specified SD of the difference.}
#'   \item{\code{"delta"}}{Proposed threshold of agreement.}
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


pwr_agree <- function(delta,
                      power = .8,
                      mu,
                      sigma,
                      conf.level = .95,
                      agree.level = .95) {
  #Need this to avoid "undefined" global error from occuring
  alpha = 1-conf.level
  prop0 = agree.level

  if (prop0 >= 1 || prop0 <= 0) {

    stop("Null central proportion must be a value between 0 and 1")
  }

  if (alpha >= 1 || alpha <= 0) {

    stop("alpha must be a value between 0 and 1")
  }

  if (power >= 1 || power <= 0) {

    stop("power must be a value between 0 and 1")
  }

  pct <- 1 - (1 - prop0) / 2
  zp <- qnorm(pct)
  l <- -delta
  u <- delta
  prop1 <- pnorm((u - mu) / sigma) - pnorm((l - mu) / sigma)
  sigsq <- sigma ^ 2
  thetal <- mu - zp * sigma
  thetau <- mu + zp * sigma
  numint <- 1000
  coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
  cl <- 1e-6

  n <- 5
  gpower <- 0
  while (gpower < power & n < 5000) {
    n <- n + 1
    df <- n - 1
    std <- sqrt(sigsq / n)
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
    hel <- (l - mu) / std + gam * sqrt(cvec / df)
    heu <- (u - mu) / std - gam * sqrt(cvec / df)
    ke <- (n * df * (u - l) ^ 2) / (4 * gam ^ 2 * sigsq)
    helt <- hel * (cvec < ke)
    heut <- heu * (cvec < ke)
    gpower <- sum(wcpdf * (pnorm(heut) - pnorm(helt)))
  }

  pwr_res = list(gam = gam,
                 power = gpower,
                 conf.level = conf.level,
                 agree.level = agree.level,
                 n = n,
                 mu = mu,
                 sigma = sigma,
                 delta = delta)

  structure(pwr_res,
            class = "pwr_agree")
}
