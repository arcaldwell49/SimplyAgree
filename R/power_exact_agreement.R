#' Power Calculation for Exact Agreement/Tolerance Test
#'
#' @description
#'
#' `r lifecycle::badge('maturing')`
#'
#' Computes sample size, power, or other parameters for the exact method of
#' assessing agreement between two measurement methods, as described in
#' Shieh (2019). This method tests whether the central portion of paired
#' differences falls within specified bounds.
#'
#' @param n Number of subject pairs (sample size), or the number of subjects
#'   when `m > 1`
#' @param delta Maximum allowable difference bound (half-width of tolerance interval)
#' @param mu Mean of paired differences
#' @param sigma Standard deviation of paired differences. With clustered data
#'   (`m > 1`), the *total* SD of the differences.
#' @param p0_star The coverage proportion (content) of the tolerance interval. Central proportion under null hypothesis (default = 0.95)
#' @param power Target power (probability of rejecting false null)
#' @param alpha Significance level (Type I error rate, default = 0.05, Confidence level = 1-alpha)
#' @param max_iter Maximum iterations for gamma computation (default = 1000)
#' @inheritParams agree_expected_half
#'
#' @details
#' This function implements the exact agreement test procedure of Shieh (2019)
#' for method comparison studies. The test evaluates whether the central
#' proportion of the distribution of paired differences lies within the
#' interval \[-delta, delta\].
#'
#' The null hypothesis is: H0: theta_(1-p) <= -delta or delta <= theta_p
#' The alternative is: H1: -delta < theta_(1-p) and theta_p < delta
#'
#' where p = (1 + p0_star)/2, and theta_p represents the 100p-th percentile
#' of the paired differences.
#'
#' Specify three of: n, delta, power, and sigma. The fourth will be calculated.
#' If mu is not specified, it defaults to 0. A sample size can only be found
#' when the proportion of differences within \[-delta, delta\] under the
#' alternative (`p1_star`) exceeds `p0_star`; otherwise power cannot exceed
#' alpha and an error is returned.
#'
#' Tolerance Interval Interpretation:
#'
#' The parameter `p0_star` represents the tolerance coverage proportion,
#' i.e., the proportion of the population that must fall within the specified
#' bounds \[-delta, delta\] under the null hypothesis. This is conceptually
#' related to tolerance intervals, but formulated as a
#' hypothesis test rather than an estimation problem.
#'
#' The power is that of Shieh's test (reject H0 when xbar -/+ gamma \* SE lies
#' within \[-delta, delta\]; for independent pairs this test is reported by
#' [agree_test()]). It is **not** the probability that tolerance limits from
#' [tolerance_limit()] fall within \[-delta, delta\]; that decision rule is less
#' powerful. In simulations at a planned power of 0.80, it was about 0.66 for
#' independent pairs and about 0.57 to 0.61 for clustered data (m = 3,
#' icc = 0.1 to 0.6). Allow a larger sample if the analysis will be based on
#' tolerance limits.
#'
#' Note: This differs from Bland-Altman's "95% limits of agreement," which
#' are confidence intervals for the 2.5th and 97.5th percentiles, *not*
#' tolerance intervals.
#'
#' @inheritSection agree_expected_half Clustered data
#'
#' @section Clustered test statistic:
#' With `m > 1`, the test is Shieh's test with the standard error of the mean
#' difference taken as S \* sqrt(DEFF / (n \* m)), i.e. with the design effect
#' treated as known. No function in this package currently carries out that
#' clustered test, so treat the clustered power as a planning approximation.
#'
#' @return An object of class "power.htest", a list with components:
#' \item{n}{Sample size (pairs, or subjects when `m > 1`)}
#' \item{delta}{Maximum allowable difference bound}
#' \item{mu}{Mean of differences}
#' \item{sigma}{Standard deviation of differences}
#' \item{p0_star}{Central proportion (null hypothesis)}
#' \item{p1_star}{Central proportion (alternative hypothesis)}
#' \item{alpha}{Significance level}
#' \item{power}{Power of the test}
#' \item{critical_value}{Critical value for test statistic}
#' \item{m, icc}{The clustering inputs}
#' \item{n.obs}{Total number of pairs, `n * m`}
#' \item{method}{Description of the method}
#' \item{note}{Additional notes}
#'
#' @references
#' Shieh, G. (2019). Assessing Agreement Between Two Methods of Quantitative
#' Measurements: Exact Test Procedure and Sample Size Calculation.
#' Statistics in Biopharmaceutical Research, 12(3), 352-359.
#' \doi{10.1080/19466315.2019.1677495}
#'
#' Bland, J.M. and Altman, D.G. (2007). Agreement between methods of
#' measurement with multiple observations per individual. *Journal of
#' Biopharmaceutical Statistics*, **17**(4), 571-582.
#' \doi{10.1080/10543400701329422}
#'
#' @examples
#' # Example 1: Find required sample size
#' power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
#'                       p0_star = 0.95, power = 0.80, alpha = 0.05)
#'
#' # Example 2: Calculate power for given sample size
#' power_agreement_exact(n = 15, delta = 0.1, mu = 0.011,
#'                       sigma = 0.044, p0_star = 0.80, alpha = 0.05)
#'
#' # Example 3: Find required delta for given power and sample size
#' power_agreement_exact(n = 50, mu = 0, sigma = 2.5,
#'                       p0_star = 0.95, power = 0.90, alpha = 0.05)
#'
#' # Example 4: Clustered data, 3 pairs per subject, ICC of the differences 0.15
#' power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
#'                       p0_star = 0.95, power = 0.80, m = 3, icc = 0.15)
#'
#' @export
power_agreement_exact <- function(n = NULL,
                                  delta = NULL,
                                  mu = 0,
                                  sigma = NULL,
                                  p0_star = 0.95,
                                  power = NULL,
                                  alpha = 0.05,
                                  max_iter = 1000,
                                  m = 1,
                                  icc = 0) {

  # Check that exactly one parameter is NULL
  n_nulls <- sum(sapply(list(n, delta, power, sigma), is.null))
  if (n_nulls != 1) {
    stop("Exactly one of 'n', 'delta', 'power', or 'sigma' must be NULL")
  }

  # Validate inputs
  if (!is.null(n) && (n < 2 || n != round(n))) {
    stop("'n' must be an integer >= 2")
  }
  if (!is.null(delta) && delta <= 0) {
    stop("'delta' must be positive")
  }
  if (!is.null(sigma) && sigma <= 0) {
    stop("'sigma' must be positive")
  }
  if (p0_star <= 0 || p0_star >= 1) {
    stop("'p0_star' must be between 0 and 1")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be between 0 and 1")
  }
  if (!is.null(power) && (power <= 0 || power >= 1)) {
    stop("'power' must be between 0 and 1")
  }
  .check_cluster_args(m, icc)

  # Calculate p and z_p for null hypothesis
  p <- (1 + p0_star) / 2
  z_p <- qnorm(p)

  # Calculate p1_star (actual central proportion)
  calc_p1_star <- function(delta, mu, sigma) {
    pnorm((delta - mu) / sigma) - pnorm((-delta - mu) / sigma)
  }

  # Critical value and grid at n subjects (do not depend on delta or sigma)
  setup <- function(n) {
    n_eff <- .n_eff(n, m, icc)
    grid <- .s2_grid(n, m, icc)
    gam <- .exact_gamma(n_eff, grid, z_p, alpha, max_iter = max_iter)
    list(n_eff = n_eff, grid = grid, gamma = gam)
  }
  power_at <- function(st, delta, sigma) {
    .exact_power(st$n_eff, st$grid, st$gamma, delta, mu, sigma)
  }

  # Solve for the missing parameter
  if (is.null(power)) {
    # Calculate power
    st <- setup(n)

  } else if (is.null(n)) {
    # Find required sample size
    if (calc_p1_star(delta, mu, sigma) <= p0_star) {
      stop("The central proportion within [-delta, delta] under the ",
           "alternative (p1_star) must exceed p0_star; otherwise power ",
           "cannot exceed alpha at any sample size.")
    }
    n <- .search_n(function(n) power_at(setup(n), delta, sigma) >= power,
                   lo = 2, hi = 10000)
    st <- setup(n)

  } else if (is.null(delta)) {
    # Find required delta (power increases with delta)
    st <- setup(n)
    delta <- .bisect(function(d) power_at(st, d, sigma), power,
                     lo = 0, hi = abs(mu) + 10 * sigma,
                     increasing = TRUE, side = "above", rel_tol = 1e-8)

  } else if (is.null(sigma)) {
    # Find required sigma (power decreases with sigma)
    if (abs(mu) >= delta) {
      stop("'mu' must lie within [-delta, delta] for power to exceed alpha.")
    }
    st <- setup(n)
    sig_lo <- (delta - abs(mu)) * 1e-6
    if (power_at(st, delta, sig_lo) < power) {
      stop("Target power cannot be reached at this n for any sigma.")
    }
    sigma <- .bisect(function(s) power_at(st, delta, s), power,
                     lo = sig_lo, hi = delta,
                     increasing = FALSE, side = "above", rel_tol = 1e-8)
  }

  power <- power_at(st, delta, sigma)
  gamma <- st$gamma
  p1_star <- calc_p1_star(delta, mu, sigma)

  # Create method description
  method <- "Power for Exact Method for Assessing Agreement Between Two Methods"

  # Create note
  note <- paste0(
    "H0: Central ", round(p0_star * 100, 1),
    "% of differences not within [-delta, delta]\n",
    "     H1: Central ", round(p1_star * 100, 1),
    "% of differences within [-delta, delta] \n     ",
    .agree_n_note(m, icc)
  )

  # Create power.htest object
  structure(
    list(
      n = n,
      delta = delta,
      mu = mu,
      sigma = sigma,
      p0_star = p0_star,
      p1_star = p1_star,
      alpha = alpha,
      power = power,
      critical_value = gamma,
      m = m,
      icc = icc,
      n.obs = n * m,
      method = method,
      note = note
    ),
    class = "power.htest"
  )
}
