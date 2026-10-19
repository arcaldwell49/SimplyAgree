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
#'   by setting sigma = 1. With clustered data (`m > 1`), this is the *total*
#'   SD of the differences (between- plus within-subject variance).
#' @param n sample size (optional): the number of pairs, or the number of
#'   subjects when `m > 1`. If provided, the function will solve for omega
#'   rather than sample size.
#' @inheritParams agree_expected_half
#'
#' @returns An object of class `"power.htest"` containing the following components:
#'
#' - `n`: The required sample size (number of pairs, or of subjects when `m > 1`)
#' - `conf.level`: The confidence level (1 - alpha)
#' - `assurance`: The target assurance probability (1 - gamma)
#' - `actual.assurance`: The actual assurance probability achieved (may differ
#'   slightly from target due to discrete nature of n)
#' - `omega`: The upper bound of assurance half-width
#' - `pstar`: The central proportion covered (P*)
#' - `sigma`: The population standard deviation
#' - `g`: The Odeh-Owen factor (g'') used to construct the tolerance
#'   interval, tabulated in Odeh and Owen (1980)
#' - `zp`: The normal quantile for the central proportion
#' - `m`, `icc`: The clustering inputs
#' - `n.obs`: The total number of pairs, `n * m`
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
#' @inheritSection agree_expected_half Clustered data
#'
#' @section Assumptions:
#'
#' - The paired differences are normally distributed
#' - The variance is constant across the range of measurement
#' - Pairs are independent, or (with `m > 1`) follow the balanced
#'   compound symmetry model in the "Clustered data" section
#'
#' @references
#' Jan, S.L. and Shieh, G. (2018). The Bland-Altman range of agreement: Exact
#' interval procedure and sample size determination. *Computers in Biology and Medicine*, **100**,
#' 247-252. \doi{10.1016/j.compbiomed.2018.06.020}
#'
#' Bland, J.M. and Altman, D.G. (2007). Agreement between methods of
#' measurement with multiple observations per individual. *Journal of
#' Biopharmaceutical Statistics*, **17**(4), 571-582.
#' \doi{10.1080/10543400701329422}
#'
#' Mee, R.W. and Owen, D.B. (1983). Improved factors for one-sided tolerance
#' limits for balanced one-way ANOVA random model. *Journal of the American
#' Statistical Association*, **78**(384), 901-905.
#' \doi{10.1080/01621459.1983.10477038}
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
#'
#' # Clustered data: 4 pairs per subject, ICC of the differences 0.3
#' agree_assurance(
#'   assurance = 0.90,
#'   omega = 2.5,
#'   pstar = 0.95,
#'   m = 4,
#'   icc = 0.3
#' )
#' @export
agree_assurance <- function(conf.level = 0.95,
                            assurance = 0.90,
                            omega = NULL,
                            pstar = 0.95,
                            sigma = 1,
                            n = NULL,
                            m = 1,
                            icc = 0) {

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

  if (!is.null(omega) && omega <= 0) {
    stop("'omega' must be positive")
  }

  if (!is.null(n)) .check_n(n)
  .check_cluster_args(m, icc)

  coverp <- conf.level

  # Calculate percentile
  pct <- (pstar + 1) / 2
  zp <- qnorm(pct)

  # Tolerance factor at n subjects
  g_at <- function(n) {
    .tol_g(.n_eff(n, m, icc), .s2_grid(n, m, icc), zp, coverp)
  }

  # Assurance Pr(H <= omega) = Pr(S^2 / sigma^2 <= (omega / (g * sigma))^2)
  assur_at <- function(n, g, omega) {
    .s2_cdf((omega / (g * sigma))^2, n, m, icc)
  }

  if (is.null(n)) {
    # Solve for n
    n <- .search_n(function(n) assur_at(n, g_at(n), omega) >= assurance,
                   lo = 2, hi = 20000)
    g_final <- g_at(n)
  } else {
    # Solve for omega: omega = g * sigma * sqrt(q), q the assurance quantile
    # of S^2 / sigma^2
    g_final <- g_at(n)
    if (m == 1) {
      q <- qchisq(assurance, n - 1) / (n - 1)
    } else {
      q <- .bisect(function(t) .s2_cdf(t, n, m, icc), assurance,
                   lo = 0, hi = 2, increasing = TRUE, side = "above")
    }
    # nudge up so the reported assurance is not below target by rounding
    omega <- g_final * sigma * sqrt(q) * (1 + 1e-10)
  }
  actual_assurance <- assur_at(n, g_final, omega)

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
      m = m,
      icc = icc,
      n.obs = n * m,
      method = "Assurance probability & sample size for Limits of Agreement",
      note = .agree_n_note(m, icc)
    ),
    class = "power.htest"
  )
}
