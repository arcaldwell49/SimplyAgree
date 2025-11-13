#' Sample Size for Bland-Altman Limits of Agreement (Expected Half-Width)
#'
#' Calculate the sample size needed for a confidence interval of the Bland-Altman
#' limits of agreement using the expected half-width criterion. Based on the exact
#' method of Jan and Shieh (2018).
#'
#' @param conf.level confidence level for the range of agreement (1 - alpha)
#' @param delta upper bound of expected half-width
#' @param pstar central proportion covered (P*)
#' @param sigma population standard deviation of paired differences
#' @param n sample size (if solving for another parameter)
#'
#' @return An object of class "power.htest" with the computed sample size and
#'   other parameters.
#'
#' @details
#' This function implements the exact method for determining sample size based on
#' expected half-width for Bland-Altman limits of agreement, as described in
#' Jan and Shieh (2018). The method ensures that the expected half-width of the
#' confidence interval is no more than delta.
#'
#' The method uses equal-tailed tolerance intervals based on the noncentral
#' t-distribution to construct exact confidence intervals for the range of
#' agreement. The expected half-width is E(H) = (g/c)*sigma, where g is the
#' tolerance interval factor and c is a bias correction factor.
#'
#' @references
#' Jan, S.L. and Shieh, G. (2018). The Bland-Altman range of agreement: Exact
#' interval procedure and sample size determination. Computers in Biology and
#' Medicine, 100, 247-252. \doi{10.1016/j.compbiomed.2018.06.020}
#'
#' @examples
#' # Example from Jan and Shieh (2018), page 251
#' agree_expected_half(conf.level = 0.95, delta = 2.25 * 19.61,
#'                pstar = 0.95, sigma = 19.61)
#'
#' @export
agree_expected_half <- function(conf.level = 0.95,
                           delta = NULL,
                           pstar = 0.95,
                           sigma = 1,
                           n = NULL) {

  # Input validation
  if (!is.null(n) && !is.null(delta)) {
    stop("exactly one of 'n' and 'delta' must be NULL")
  }

  if (is.null(n) && is.null(delta)) {
    stop("one of 'n' or 'delta' must be specified")
  }

  if (conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1")
  }

  if (pstar <= 0 || pstar >= 1) {
    stop("'pstar' must be between 0 and 1")
  }

  if (sigma <= 0) {
    stop("'sigma' must be positive")
  }

  alpha <- 1 - conf.level
  coverp <- conf.level

  # Calculate percentile
  pct <- (pstar + 1) / 2
  zp <- qnorm(pct)

  # Numerical integration setup
  numint <- 1000
  coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)

  # Function to calculate g factor (tolerance interval multiplier)
  gfun <- function(n, alpha, zp) {
    df <- n - 1
    cql <- 1e-8
    cqu <- qchisq(1 - cql, df)
    int <- cqu - cql
    intl <- int / numint
    cvec <- cql + intl * (0:numint)
    wcpdf <- (intl / 3) * coevec * dchisq(cvec, df)

    gl <- 0
    gu <- 100
    dd <- 1

    while (abs(dd) > 1e-9 | dd < 0) {
      g <- (gl + gu) / 2
      b <- sqrt(n) * (-zp + g * sqrt(cvec / df))
      cpt <- sum(wcpdf * ((2 * pnorm(b) - 1) * (cvec > df * (zp / g)^2)))

      if (cpt > coverp) {
        gu <- g
      } else {
        gl <- g
      }
      dd <- cpt - coverp
    }

    return(g)
  }

  # Function to calculate bias correction factor c
  calc_c <- function(n) {
    df <- n - 1
    logc <- log(sqrt(df / 2)) + lgamma(df / 2) - lgamma(n / 2)
    exp(logc)
  }

  # Solve for n
  if (is.null(n)) {
    n <- 4
    ehe <- Inf

    while (ehe > delta && n < 10000) {
      n <- n + 1
      df <- n - 1
      c <- calc_c(n)
      g <- gfun(n, alpha, zp)
      ehe <- (g / c) * sigma
    }

    if (n >= 10000) {
      warning("Maximum iterations reached; solution may not have converged")
    }

    # Calculate final values for output
    g_final <- gfun(n, alpha, zp)
    c_final <- calc_c(n)
    actual_delta <- (g_final / c_final) * sigma

  } else {
    # Solve for delta (less common case)
    df <- n - 1
    c <- calc_c(n)
    g <- gfun(n, alpha, zp)
    delta <- (g / c) * sigma

    g_final <- g
    c_final <- c
    actual_delta <- delta
  }

  # Create output object
  structure(
    list(
      n = n,
      conf.level = conf.level,
      target.delta = delta,
      actual.delta = actual_delta,
      pstar = pstar,
      sigma = sigma,
      g = g_final,
      c = c_final,
      zp = zp,
      method = "Sample size for Bland-Altman range of agreement (Expected Half-Width)"
    ),
    class = "power.htest"
  )
}
