#' Assurance Probability for Limits of Agreement
#'
#' `r lifecycle::badge('maturing')`
#'
#' Calculate the sample size necessary for a confidence interval of the Bland-Altman
#' range of agreement when the underlying data distribution is normal. This function
#' uses the assurance probability criterion to determine the optimum sample size,
#' based on the exact confidence interval method of Jan and Shieh (2018), which has
#' been shown to be superior to approximate methods.
#'
#' @param conf.level confidence level for the range of agreement (1 - alpha).
#'   The confidence level of the confidence interval of the range of agreement
#'   (tolerance interval). Default is 0.95.
#' @param assurance target lower bound of the assurance probability (1 - gamma).
#'   The assurance probability is the probability that the study half-width will
#'   be less than omega. Common values are 0.80, 0.90, or 0.95.
#' @param omega upper bound of assurance half-width. The sample size guarantees
#'   (assures) that 100(1 - gamma)% of interval half-widths will be less than
#'   this value. Can be specified in standard deviation units.
#' @param pstar central proportion of the data distribution covered (P*). It is
#'   the proportion of observations that fall between the limits. For example, a
#'   value of 0.95 indicates that 95% of the variable's values fall between the
#'   limits. Must be between 0 and 1. Common values are 0.90 or 0.95.
#' @param sigma population standard deviation of the paired differences. If the
#'   true value is unknown, omega can be specified in standard deviation units
#'   by setting sigma = 1.
#' @param n sample size (optional). If provided, the function will solve for a
#'   different parameter rather than sample size.
#'
#' @returns An object of class `"power.htest"` containing the following components:
#'
#' - `n`: The required sample size (number of subject pairs)
#' - `conf.level`: The confidence level (1 - alpha)
#' - `assurance`: The target assurance probability (1 - gamma)
#' - `actual.assurance`: The actual assurance probability achieved (may differ
#'   slightly from target due to discrete nature of n)
#' - `omega`: The upper bound of assurance half-width
#' - `pstar`: The central proportion covered (P*)
#' - `sigma`: The population standard deviation
#' - `g.factor`: The Odeh-Owen factor (g'') used to construct the tolerance
#'   interval, tabulated in Odeh and Owen (1980)
#' - `method`: Description of the method used
#' - `note`: Additional notes about the analysis
#'
#' @details
#' ## Overview
#'
#' This function implements the exact method for determining sample size based on
#' assurance probability for Bland-Altman limits of agreement, as described in
#' Jan and Shieh (2018). The assurance probability criterion determines an N that
#' guarantees with specified probability (1 - gamma) that the confidence interval
#' half-width will be no more than a boundary value omega.
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
#' Odeh and Owen (1980)), and S is the sample
#' standard deviation.
#'
#' ## Sample Size Determination
#'
#' The sample size N is selected to satisfy: Pr(H <= omega) >= 1 - gamma
#'
#' This leads to the expression:
#'
#' psi(eta) >= 1 - gamma
#'
#' where psi() is the CDF of a chi-square distribution with N-1 degrees of
#' freedom and eta = (N-1) \* (omega/(g\*sigma))^2.
#'
#' The method uses equal-tailed tolerance intervals based on the noncentral
#' t-distribution to construct exact confidence intervals for the range of
#' agreement. The tolerance factor g is calculated such that the interval
#' maintains the specified confidence level under normality.
#'
#' Jan and Shieh (2018) demonstrated through extensive simulations that this
#' exact method should be adopted rather than the classical Bland-Altman
#' approximate method.
#'
#' ## Interpreting Results
#'
#' Each subject produces two measurements (one for each method being compared).
#' The sample size n returned is the number of subject pairs needed. The actual
#' assurance probability may differ slightly from the target due to the discrete
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
#' interval procedure and sample size determination. *Computers in Biology and Medicine*, **100**,
#' 247-252. \doi{10.1016/j.compbiomed.2018.06.020}
#'
#' Odeh, R.E. and Owen, D.B. (1980). *Tables for Normal Tolerance Limits,
#' Sampling Plans, and Screening*. Marcel Dekker, Inc., New York.
#'
#' @seealso
#' [agree_expected_half()] for sample size determination using expected
#' half-width criterion, [power_agreement_exact()] for power analysis
#' of agreement tests.
#'
#' @examples
#' # Example: Planning a method comparison study
#' # Researchers want 95% confidence, 90% assurance that half-width
#' # will be within 2.5 SD units, covering central 95% of differences
#' agree_assurance(
#'   conf.level = 0.95,
#'   assurance = 0.90,
#'   omega = 2.5,
#'   pstar = 0.95,
#'   sigma = 1
#' )
#' @export
agree_assurance <- function(conf.level = 0.95,
                            assurance = 0.90,
                            omega = NULL,
                            pstar = 0.95,
                            sigma = 1,
                            n = NULL) {

  # Input validation
  if (!is.null(n) && !is.null(omega)) {
    stop("exactly one of 'n' and 'omega' must be NULL")
  }

  if (is.null(n) && is.null(omega)) {
    stop("one of 'n' or 'omega' must be specified")
  }


  if (conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1")
  }

  if (assurance <= 0 || assurance >= 1) {
    stop("'assurance' must be between 0 and 1")
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

  # Solve for n
  if (is.null(n)) {
    n <- 4
    ape <- 0

    while (ape < assurance && n < 20000) {
      n <- n + 1
      df <- n - 1
      g <- gfun(n, alpha, zp)
      ape <- pchisq((df / g^2) * (omega / sigma)^2, df)
    }

    if (n >= 20000) {
      warning("Maximum iterations reached; solution may not have converged")
    }

    # Calculate final g for output
    g_final <- gfun(n, alpha, zp)
    actual_assurance <- ape

  } else {
    # Solve for omega (less common case)
    df <- n - 1
    g <- gfun(n, alpha, zp)

    # Binary search for omega
    omega_low <- 0
    omega_high <- 10 * sigma  # Start with reasonable upper bound

    while (omega_high - omega_low > 1e-6) {
      omega_mid <- (omega_low + omega_high) / 2
      ape <- pchisq((df / g^2) * (omega_mid / sigma)^2, df)

      if (ape < assurance) {
        omega_low <- omega_mid
      } else {
        omega_high <- omega_mid
      }
    }

    omega <- omega_high
    g_final <- g
    actual_assurance <- pchisq((df / g^2) * (omega / sigma)^2, df)
  }

  # Create output object
  structure(
    list(
      n = n,
      conf.level = conf.level,
      assurance = assurance,
      actual.assurance = actual_assurance,
      omega = omega,
      pstar = pstar,
      sigma = sigma,
      g = g_final,
      zp = zp,
      method = "Assurance probability & sample size for Limits of Agreement"
    ),
    class = "power.htest"
  )
}
