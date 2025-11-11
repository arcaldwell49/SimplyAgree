#' Power Analysis for Exact Agreement/Tolerance Test
#'
#' @description
#' Computes sample size, power, or other parameters for the exact method of
#' assessing agreement between two measurement methods, as described in
#' Shieh (2019). This method tests whether the central portion of paired
#' differences falls within specified bounds.
#'
#' @param n Number of subject pairs (sample size)
#' @param delta Maximum allowable difference bound (half-width of tolerance interval)
#' @param mu Mean of paired differences
#' @param sigma Standard deviation of paired differences
#' @param p0_star The coverage proportion (content) of the tolerance interval. Central proportion under null hypothesis (default = 0.95)
#' @param power Target power (probability of rejecting false null)
#' @param alpha Significance level (Type I error rate, default = 0.05, Confidence level = 1-alpha)
#'
#' @details
#' This function implements the exact agreement test procedure of Shieh (2019)
#' for method comparison studies. The test evaluates whether the central
#' proportion of the distribution of paired differences lies within the
#' interval [-delta, delta].
#'
#' The null hypothesis is: H0: theta_(1-p) <= -delta or delta <= theta_p
#' The alternative is: H1: -delta < theta_(1-p) and theta_p < delta
#'
#' where p = (1 + p0_star)/2, and theta_p represents the 100p-th percentile
#' of the paired differences.
#'
#' Specify three of: n, delta, power, and sigma. The fourth will be calculated.
#' If mu is not specified, it defaults to 0.
#'
#' Tolerance Interval Interpretation:
#'
#' The parameter `p0_star` represents the tolerance coverage proportion,
#' i.e., the proportion of the population that must fall within the specified
#' bounds [-delta, delta] under the null hypothesis. This is conceptually
#' related to tolerance intervals, but formulated as a
#' hypothesis test rather than an estimation problem.
#'
#' Note: This differs from Bland-Altman's "95% limits of agreement," which
#' are confidence intervals for the 2.5th and 97.5th percentiles, *not*
#' tolerance intervals.
#'
#' @return An object of class "power.htest", a list with components:
#' \item{n}{Sample size}
#' \item{delta}{c}
#' \item{mu}{Mean of differences}
#' \item{sigma}{Standard deviation of differences}
#' \item{p0_star}{Central proportion (null hypothesis)}
#' \item{p1_star}{Central proportion (alternative hypothesis)}
#' \item{alpha}{Significance level}
#' \item{power}{Power of the test}
#' \item{critical_value}{Critical value for test statistic}
#' \item{method}{Description of the method}
#' \item{note}{Additional notes}
#'
#' @references
#' Shieh, G. (2019). Assessing Agreement Between Two Methods of Quantitative
#' Measurements: Exact Test Procedure and Sample Size Calculation.
#' Statistics in Biopharmaceutical Research, 12(3), 352-359.
#' https://doi.org/10.1080/19466315.2019.1677495
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
#' @export
power_agreement_exact <- function(n = NULL,
                                  delta = NULL,
                                  mu = 0,
                                  sigma = NULL,
                                  p0_star = 0.95,
                                  power = NULL,
                                  alpha = 0.05) {

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

  # Calculate p and z_p for null hypothesis
  p <- (1 + p0_star) / 2
  z_p <- qnorm(p)

  # Helper function to compute critical value gamma
  compute_gamma <- function(n, p, z_p, alpha, numint = 1000) {
    df <- n - 1

    # Set up numerical integration
    coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
    cl <- 1e-6
    cu <- qchisq(1 - cl, df)
    int <- cu - cl
    intl <- int / numint
    cvec <- cl + intl * (0:numint)
    wcpdf <- (intl / 3) * coevec * dchisq(cvec, df)

    # Binary search for gamma
    gam_l <- 0
    gam_u <- 100
    tol <- 1e-8
    max_iter <- 1000
    iter <- 0

    repeat {
      iter <- iter + 1
      gam <- (gam_l + gam_u) / 2

      h <- z_p * sqrt(n) - gam * sqrt(cvec / df)
      ht <- h * (cvec < n * df * (z_p / gam)^2)
      alpha_t <- sum(wcpdf * (2 * pnorm(ht) - 1))

      d_alpha <- alpha_t - alpha

      if (abs(d_alpha) < tol && d_alpha > 0) break
      if (iter > max_iter) {
        warning("Maximum iterations reached in gamma computation")
        break
      }

      if (alpha_t > alpha) {
        gam_l <- gam
      } else {
        gam_u <- gam
      }
    }

    return(gam)
  }

  # Helper function to compute power
  compute_power <- function(n, delta, mu, sigma, p, z_p, alpha, numint = 1000) {
    df <- n - 1
    std <- sqrt(sigma^2 / n)
    l <- -delta
    u <- delta

    # Set up numerical integration
    coevec <- c(1, rep(c(4, 2), numint / 2 - 1), 4, 1)
    cl <- 1e-6
    cu <- qchisq(1 - cl, df)
    int <- cu - cl
    intl <- int / numint
    cvec <- cl + intl * (0:numint)
    wcpdf <- (intl / 3) * coevec * dchisq(cvec, df)

    # Compute gamma
    gam <- compute_gamma(n, p, z_p, alpha, numint)

    # Compute power
    hel <- (l - mu) / std + gam * sqrt(cvec / df)
    heu <- (u - mu) / std - gam * sqrt(cvec / df)
    ke <- (n * df * (u - l)^2) / (4 * gam^2 * sigma^2)

    helt <- hel * (cvec < ke)
    heut <- heu * (cvec < ke)

    power <- sum(wcpdf * (pnorm(heut) - pnorm(helt)))

    return(list(power = power, gamma = gam))
  }

  # Calculate p1_star (actual central proportion)
  calc_p1_star <- function(delta, mu, sigma) {
    pnorm((delta - mu) / sigma) - pnorm((-delta - mu) / sigma)
  }

  # Solve for the missing parameter
  if (is.null(power)) {
    # Calculate power
    result <- compute_power(n, delta, mu, sigma, p, z_p, alpha)
    power <- result$power
    gamma <- result$gamma
    p1_star <- calc_p1_star(delta, mu, sigma)

  } else if (is.null(n)) {
    # Find required sample size
    n_low <- 5
    n_high <- 10000

    while (n_high - n_low > 1) {
      n_mid <- floor((n_low + n_high) / 2)
      result <- compute_power(n_mid, delta, mu, sigma, p, z_p, alpha)

      if (result$power < power) {
        n_low <- n_mid
      } else {
        n_high <- n_mid
      }
    }

    n <- n_high
    result <- compute_power(n, delta, mu, sigma, p, z_p, alpha)
    power <- result$power
    gamma <- result$gamma
    p1_star <- calc_p1_star(delta, mu, sigma)

  } else if (is.null(delta)) {
    # Find required delta
    delta_low <- 0.001
    delta_high <- 100 * sigma
    tol <- 1e-4

    while (delta_high - delta_low > tol) {
      delta_mid <- (delta_low + delta_high) / 2
      result <- compute_power(n, delta_mid, mu, sigma, p, z_p, alpha)

      if (result$power < power) {
        delta_low <- delta_mid
      } else {
        delta_high <- delta_mid
      }
    }

    delta <- delta_high
    result <- compute_power(n, delta, mu, sigma, p, z_p, alpha)
    power <- result$power
    gamma <- result$gamma
    p1_star <- calc_p1_star(delta, mu, sigma)

  } else if (is.null(sigma)) {
    # Find required sigma
    sigma_low <- 0.001
    sigma_high <- 100 * delta
    tol <- 1e-4

    while (sigma_high - sigma_low > tol) {
      sigma_mid <- (sigma_low + sigma_high) / 2
      result <- compute_power(n, delta, mu, sigma_mid, p, z_p, alpha)

      if (result$power < power) {
        sigma_high <- sigma_mid
      } else {
        sigma_low <- sigma_mid
      }
    }

    sigma <- sigma_low
    result <- compute_power(n, delta, mu, sigma, p, z_p, alpha)
    power <- result$power
    gamma <- result$gamma
    p1_star <- calc_p1_star(delta, mu, sigma)
  }

  # Create method description
  method <- "Power for Exact Method for Assessing Agreement Between Two Methods"

  # Create note
  note <- paste0(
    "H0: Central ", round(p0_star * 100, 1),
    "% of differences not within [-delta, delta]\n",
    "     H1: Central ", round(p1_star * 100, 1),
    "% of differences within [-delta, delta] \n",
    "n is number pairs. Two measurements per unit; one for each method."
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
      method = method,
      note = note
    ),
    class = "power.htest"
  )
}


