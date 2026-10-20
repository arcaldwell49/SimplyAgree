#' Power Calculation for Bland-Altman Limits of Agreement
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' Computes the sample size, power, maximal allowable difference, or standard
#' deviation for a method comparison study analyzed with the Bland-Altman
#' limits of agreement, using the method of Lu et al. (2016). Agreement is
#' concluded when the confidence intervals of both limits of agreement fall
#' within \[-delta, delta\].
#'
#' @param n Number of pairs (sample size), or the number of subjects when
#'   `m > 1`.
#' @param delta Maximal allowable difference: the methods agree if the
#'   confidence limits of the limits of agreement lie within \[-delta, delta\].
#' @param mu Mean of the paired differences (default 0).
#' @param sigma Standard deviation of the paired differences. With clustered
#'   data (`m > 1`), the *total* SD of the differences.
#' @param power Target power.
#' @param conf.level Confidence level of the confidence intervals of the
#'   limits of agreement (default 0.95).
#' @param agree.level Agreement level of the limits of agreement: the
#'   proportion of differences the limits should contain (default 0.95).
#' @inheritParams agree_expected_half
#'
#' @details
#' Specify three of `n`, `delta`, `sigma`, and `power`; the fourth is
#' calculated.
#'
#' The limits of agreement are mu +/- z \* sigma, with z the
#' `(1 + agree.level) / 2` quantile of the standard normal, and their
#' confidence intervals are those of Bland and Altman (1999), with standard
#' error sigma \* sqrt(1/n + z^2 / (2 (n - 1))). This is the interval from
#' [agreement_limit()] with `loa_calc = "blandaltman"`. The power is Lu et al.
#' (2016), eq. 5:
#'
#' power = 1 - T(t, n - 1, tau1) - T(t, n - 1, tau2),
#'
#' where T is the CDF of the non-central t distribution, t is the
#' `1 - (1 - conf.level) / 2` quantile of the central t distribution with
#' n - 1 df, tau1 = (delta - mu - z \* sigma) / SE, and
#' tau2 = (delta + mu - z \* sigma) / SE. Negative values are set to 0. These
#' are the values reported by PASS (Chapter 427), which notes that the sample
#' sizes in Table 1 of Lu et al. (2016), based on their approximate eq. 6, are
#' sometimes slightly too large.
#'
#' A sample size can only be found when the true limits of agreement lie
#' within the bounds, i.e. |mu| + z \* sigma < delta; otherwise power goes to 0
#' as n increases and an error is returned.
#'
#' With clustered data (`m > 1`), the standard error of the limits uses the
#' effective sample size n \* m / (1 + (m - 1) \* icc) for the mean difference
#' and a Satterthwaite df for the SD, under a balanced compound symmetry model
#' (see [agree_expected_half()] for the model). This is an approximation;
#' `m = 1` gives the original results.
#'
#' For a power curve over a range of sample sizes, see [blandPowerCurve()].
#'
#' @return An object of class `"power.htest"`, a list with components:
#' \item{n}{Sample size (pairs, or subjects when `m > 1`)}
#' \item{delta}{Maximal allowable difference}
#' \item{mu}{Mean of differences}
#' \item{sigma}{Standard deviation of differences}
#' \item{conf.level}{Confidence level of the CIs of the limits of agreement}
#' \item{agree.level}{Agreement level of the limits of agreement}
#' \item{power}{Power of the procedure}
#' \item{m, icc}{The clustering inputs}
#' \item{n.obs}{Total number of pairs, `n * m`}
#' \item{method}{Description of the method}
#' \item{note}{Additional notes}
#'
#' @references
#' Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between Two
#' Methods of Measurement by Bland-Altman Method. The International Journal of
#' Biostatistics, 12(2). \doi{10.1515/ijb-2015-0039}
#'
#' Bland, J. M., and Altman, D. G. (1999). Measuring agreement in method
#' comparison studies. Statistical Methods in Medical Research, 8(2), 135-160.
#' \doi{10.1177/096228029900800204}
#'
#' NCSS (PASS Sample Size Software). Chapter 427: Bland-Altman Method for
#' Assessing Agreement in Method Comparison Studies.
#'
#' @seealso [blandPowerCurve()], [power_agreement_exact()],
#'   [agree_expected_half()], [agreement_limit()]
#'
#' @examples
#' # Sample size for 80% power (PASS Example 1: n = 60)
#' power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.80)
#'
#' # Power at a given sample size (PASS Example 2)
#' power_agreement_limit(n = 201, delta = 2.5, mu = 0.2, sigma = 1)
#'
#' # Largest SD for which 90% power is reached with 100 pairs
#' power_agreement_limit(n = 100, delta = 7, mu = 0.5, power = 0.90)
#'
#' # Clustered data, 3 pairs per subject, ICC of the differences 0.15
#' power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.80,
#'                       m = 3, icc = 0.15)
#'
#' @export
power_agreement_limit <- function(n = NULL,
                                  delta = NULL,
                                  mu = 0,
                                  sigma = NULL,
                                  power = NULL,
                                  conf.level = 0.95,
                                  agree.level = 0.95,
                                  m = 1,
                                  icc = 0) {

  # Check that exactly one parameter is NULL
  n_nulls <- sum(sapply(list(n, delta, power, sigma), is.null))
  if (n_nulls != 1) {
    stop("Exactly one of 'n', 'delta', 'power', or 'sigma' must be NULL")
  }

  # Validate inputs
  if (!is.null(n)) .check_n(n)
  if (length(mu) != 1 || !is.numeric(mu) || is.na(mu)) {
    stop("'mu' must be a single number")
  }
  if (!is.null(delta) && (length(delta) != 1 || delta <= 0)) {
    stop("'delta' must be a single positive number")
  }
  if (!is.null(sigma) && (length(sigma) != 1 || sigma <= 0)) {
    stop("'sigma' must be a single positive number")
  }
  if (!is.null(power) && (length(power) != 1 || power <= 0 || power >= 1)) {
    stop("'power' must be between 0 and 1")
  }
  if (length(conf.level) != 1 || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1")
  }
  if (length(agree.level) != 1 || agree.level <= 0 || agree.level >= 1) {
    stop("'agree.level' must be between 0 and 1")
  }
  .check_cluster_args(m, icc)

  zgamma <- qnorm(1 - (1 - agree.level) / 2)
  power_at <- function(n, delta, sigma) {
    .lu_power(n = n, delta = delta, mu = mu, sigma = sigma,
              conf.level = conf.level, agree.level = agree.level,
              m = m, icc = icc)
  }

  # Solve for the missing parameter
  if (is.null(n)) {
    # Find required sample size
    if (abs(mu) + zgamma * sigma >= delta) {
      stop("The limits of agreement (|mu| + z * sigma) must lie within ",
           "[-delta, delta]; otherwise power cannot reach the target at any ",
           "sample size.")
    }
    n <- .search_n(function(n) power_at(n, delta, sigma) >= power,
                   lo = 2, hi = 10000)

  } else if (is.null(delta)) {
    # Find required delta (power increases with delta)
    delta <- .bisect(function(d) power_at(n, d, sigma), power,
                     lo = abs(mu) + zgamma * sigma,
                     hi = abs(mu) + (zgamma + 10) * sigma,
                     increasing = TRUE, side = "above", rel_tol = 1e-8)

  } else if (is.null(sigma)) {
    # Find required sigma (power decreases with sigma)
    if (abs(mu) >= delta) {
      stop("'mu' must lie within [-delta, delta] for power to exceed 0.")
    }
    # power -> 1 as sigma -> 0, so the root is always bracketed
    sig_lo <- (delta - abs(mu)) * 1e-6
    sigma <- .bisect(function(s) power_at(n, delta, s), power,
                     lo = sig_lo, hi = (delta - abs(mu)) / zgamma,
                     increasing = FALSE, side = "above", rel_tol = 1e-8)
  }

  power <- power_at(n, delta, sigma)

  note <- paste0(
    "H0: CIs of the ", round(agree.level * 100, 1),
    "% limits of agreement not within [-delta, delta]\n",
    "     H1: CIs of the limits of agreement within [-delta, delta]\n     ",
    .agree_n_note(m, icc)
  )

  structure(
    list(
      n = n,
      delta = delta,
      mu = mu,
      sigma = sigma,
      conf.level = conf.level,
      agree.level = agree.level,
      power = power,
      m = m,
      icc = icc,
      n.obs = n * m,
      method = "Power for Bland-Altman Limits of Agreement (Lu et al. 2016)",
      note = note
    ),
    class = "power.htest"
  )
}
