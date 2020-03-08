#' Oower for Absolute Agreement
#' @param prop0 Null central proportion: the propotion of data that should lie between the tresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The treshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param power Nominal power, the probability of rejecting the null-hypothesis when the alternative hypothesis, the specified null proportion of the data lies between the thresholds, is true.
#' @param mu Mean difference between methods, based on previous research or pilot work.
#' @param sigma SD of difference, based on previous research or pilot work. This SD can be determind from the limits of agreement.
#' @param alpha Desired Type 1 error rate. Probability of concluding the methods agree, the specified proportion of data falls between the tresholds, while they actually not agree. Default is .05
#' @param verbose Option to print a summary of results to the console.
#'
#' @return Returns single list with the results of the power analysis for agreement.
#'
#' \describe{
#'   \item{\code{"gam"}}{Gamma statistic.}
#'   \item{\code{"power"}}{Estimated power (1-Beta).}
#'   \item{\code{"alpha"}}{Desired Type 1 error rate.}
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


pwr_agree <- function(prop0 = 0.8,
                      delta = .1,
                      power = .8,
                      mu,
                      sigma,
                      alpha = 0.05,
                      verbose = FALSE) {
  #Need this to avoid "undefined" global error from occuring
  prop0 = prop0

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
if (verbose == TRUE) {
print(c('gam, power, n'))
print(c(gam, gpower, n), digits = 4)
}
  pwr_res = list(gam = gam,
                 power = gpower,
                 alpha = alpha,
                 n = n,
                 mu = mu,
                 sigma = sigma,
                 delta = delta)
}
