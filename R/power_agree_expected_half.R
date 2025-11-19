#' Sample Size for Limits of Agreement Using Expected Half-Width
#'
#' `r lifecycle::badge('maturing')`
#'
#' Calculate the sample size necessary for a confidence interval of the Bland-Altman
#' range of agreement when the underlying data distribution is normal. This function
#' uses the expected half-width criterion to determine the optimum sample size,
#' based on the exact confidence interval method of Jan and Shieh (2018), which has
#' been shown to be superior to approximate methods.
#'
#' @param conf.level confidence level for the range of agreement (1 - alpha).
#'   The confidence level of the confidence interval of the range of agreement
#'   (tolerance interval). Default is 0.95.
#' @param delta target upper bound of expected half-width (delta). The sample size
#'   guarantees that the expected half-width of the confidence interval will be
#'   no more than this value. Can be specified in standard deviation units.
#' @param pstar central proportion of the data distribution covered (P*). It is
#'   the proportion of observations that fall between the limits. For example, a
#'   value of 0.95 indicates that 95% of the variable's values fall between the
#'   limits. Must be between 0 and 1. Common values are 0.90 or 0.95.
#' @param sigma population standard deviation of the paired differences. If the
#'   true value is unknown, delta can be specified in standard deviation units
#'   by setting sigma = 1.
#' @param n sample size (optional). If provided, the function will solve for a
#'   different parameter rather than sample size.
#'
#' @returns An object of class `"power.htest"` containing the following components:
#'
#' - `n`: The required sample size (number of subject pairs)
#' - `conf.level`: The confidence level (1 - alpha)
#' - `delta.target`: The target upper bound of expected half-width
#' - `delta.actual`: The actual upper bound of expected half-width achieved (may
#'   differ slightly from target due to discrete nature of n)
#' - `pstar`: The central proportion covered (P*)
#' - `sigma`: The population standard deviation
#' - `g.factor`: The Odeh-Owen factor (g'') used to construct the tolerance
#'   interval, tabulated in Odeh and Owen (1980)
#' - `c.factor`: The bias correction factor used in the expected half-width
#'   calculation
#' - `method`: Description of the method used
#' - `note`: Additional notes about the analysis
#'
#' @details
#' ## Overview
#'
#' This function implements the exact method for determining sample size based on
#' expected half-width for Bland-Altman limits of agreement, as described in
#' Jan and Shieh (2018). The expected half-width criterion determines an N that
#' guarantees that the expected half-width of the confidence interval is less than
#' a boundary value delta.
#'
#' ## Technical Details
#'
#' Suppose a study involves paired differences (X - Y) whose distribution is
#' approximately N(mu, sigma^2). The range of agreement is defined as a confidence
#' interval of the central portion of these differences, specifically the area
#' between the 100(1-p)th and 100p-th percentiles, where p* = 2p - 1.
#'
#' The exact two-sided, 100(1 - alpha)% confidence interval for the range of
#' agreement is defined as:
#'
#' Pr(theta_(1-p) < theta_hat_(1-p) and theta_hat_p < theta_p) = 1 - alpha
#'
#' The equal-tailed tolerance interval recommended by Jan and Shieh (2018) is:
#'
#' (X_bar - d, X_bar + d)
#'
#' where d = g * S, g is the Odeh-Owen tolerance factor (tabulated as g'' in
#' Odeh and Owen (1980) and Hahn and Meeker (1991)), and S is the sample
#' standard deviation.
#'
#' ## Sample Size Determination
#'
#' The sample size N is selected to satisfy: E(H) <= delta
#'
#' where H is the half-width of the confidence interval. This leads to the
#' expression:
#'
#' g(P*, 1-alpha, N-1) / c <= delta / sigma
#'
#' where c is a bias correction factor:
#'
#' c = (Gamma((N-1)/2) * sqrt((N-1)/2)) / Gamma(N/2)
#'
#' The expected half-width is E(H) = (g/c) * sigma. This accounts for the fact
#' that the sample standard deviation S is a biased estimator of sigma, requiring
#' correction when computing expected values.
#'
#' The method uses equal-tailed tolerance intervals based on the noncentral
#' t-distribution to construct exact confidence intervals for the range of
#' agreement. The tolerance factor g is calculated such that the interval
#' maintains the specified confidence level under normality.
#'
#' ## Comparison with Assurance Probability Method
#'
#' This function uses the expected half-width criterion, which ensures that
#' E(H) <= delta. An alternative approach is the assurance probability criterion
#' (see [agree_assurance()]), which ensures that Pr(H <= omega) >= 1 - gamma.
#'
#' The expected half-width criterion:
#' - Controls the average half-width across repeated sampling
#' - Generally requires smaller sample sizes than assurance probability
#' - Is appropriate when the average performance is of primary interest
#'
#' The assurance probability criterion:
#' - Provides a probability guarantee about the half-width
#' - Generally requires larger sample sizes
#' - Is appropriate when a stronger guarantee is needed for planning purposes
#'
#' ## Interpreting Results
#'
#' Each subject produces two measurements (one for each method being compared).
#' The sample size n returned is the number of subject pairs needed. The actual
#' expected half-width may differ slightly from the target due to the discrete
#' nature of sample size.
#'
#' For dropout considerations, inflate the sample size using: N' = N / (1 - dropout_rate),
#' always rounding up.
#'
#' @section Assumptions:
#'
#' - The paired differences are normally distributed
#' - The variance is constant across the range of measurement
#' - Pairs are independent
#'
#' @references
#' Jan, S.L. and Shieh, G. (2018). The Bland-Altman range of agreement: Exact
#' interval procedure and sample size determination. *Computers in Biology and Medicine*,
#' **100**, 247-252. \doi{10.1016/j.compbiomed.2018.06.020}
#'
#' Bland, J.M. and Altman, D.G. (1986). Statistical methods for assessing agreement
#' between two methods of clinical measurement. *The Lancet*, **327**(8476),
#' 307-310.
#'
#' Hahn, G.J. and Meeker, W.Q. (1991). *Statistical Intervals: A Guide for
#' Practitioners*. John Wiley & Sons, New York.
#'
#' Odeh, R.E. and Owen, D.B. (1980). *Tables for Normal Tolerance Limits,
#' Sampling Plans, and Screening*. Marcel Dekker, Inc., New York.
#'
#' @seealso
#' [agree_assurance()] for sample size determination using assurance
#' probability criterion, [power_agreement_exact()] for power analysis
#' of agreement tests.
#'
#' @examples
#' # Example 1: Reproduce Jan and Shieh (2018), page 251
#' # Expected half-width criterion with P* = 0.95
#' agree_expected_half(
#'   conf.level = 0.95,
#'   delta = 2.25 * 19.61,
#'   pstar = 0.95,
#'   sigma = 19.61
#' )
#' # Expected result: n = 155
#'
#' # Example 2: Planning a method comparison study
#' # Researchers want 95% confidence with expected half-width
#' # no more than 2.4 SD units, covering central 90% of differences
#' agree_expected_half(
#'   conf.level = 0.95,
#'   delta = 2.4,
#'   pstar = 0.90,
#'   sigma = 1
#' )
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
      method = "Expected half-width and sample size for limits of agreement"
    ),
    class = "power.htest"
  )
}
