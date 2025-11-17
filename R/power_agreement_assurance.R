#' Sample Size for Bland-Altman Limits of Agreement (Assurance Probability)
#'
#' Calculate the sample size needed for a confidence interval of the Bland-Altman
#' range of agreement using the assurance probability criterion. Based on the exact
#' method of Jan and Shieh (2018).
#'
#' @param conf.level confidence level for the range of agreement (1 - alpha)
#' @param assurance target assurance probability (1 - gamma)
#' @param omega upper bound of assurance half-width
#' @param pstar central proportion covered (P*)
#' @param sigma population standard deviation of paired differences
#' @param n sample size (if solving for another parameter)
#'
#' @return An object of class "power.htest" with the computed sample size and
#'   other parameters.
#'
#' @details
#' This function implements the exact method for determining sample size based on
#' assurance probability for Bland-Altman limits of agreement, as described in
#' Jan and Shieh (2018). The assurance probability is the probability that the
#' confidence interval half-width will be no more than omega.
#'
#' The method uses equal-tailed tolerance intervals based on the noncentral
#' t-distribution to construct exact confidence intervals for the range of
#' agreement.
#'
#' @references
#' Jan, S.L. and Shieh, G. (2018). The Bland-Altman range of agreement: Exact
#' interval procedure and sample size determination. Computers in Biology and
#' Medicine, 100, 247-252. \doi{10.1016/j.compbiomed.2018.06.020}
#'
#' @examples
#' # Example from Jan and Shieh (2018), page 251
#' agree_assurance(conf.level = 0.95, assurance = 0.90,
#'                 omega = 2.25, pstar = 0.95, sigma = 1)
#'
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
