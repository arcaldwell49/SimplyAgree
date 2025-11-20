#' @title Passing-Bablok Regression for Method Comparison
#'
#' @description
#'
#' `r lifecycle::badge('maturing')`
#'
#' A robust, nonparametric method for fitting a straight line to two-dimensional data
#' where both variables (X and Y) are measured with error. Particularly useful for
#' method comparison studies.
#'
#' @param formula A formula of the form `y ~ x` specifying the model.
#' @param data Data frame with all data.
#' @param conf.level The confidence level required. Default is 95%.
#' @param keep_data Logical indicator (TRUE/FALSE). If TRUE, intermediate calculations
#'   are returned; default is FALSE.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#'
#' Passing-Bablok regression is a robust nonparametric method that estimates the
#' slope as the shifted median of all possible slopes between pairs of points.
#' The intercept is then calculated as the median of y - slope*x. This method
#' is particularly useful when:
#'
#' - Both X and Y are measured with error
#' - You want a robust method not sensitive to outliers
#' - The relationship is assumed to be linear
#' - X and Y are highly positively correlated
#'
#' The method automatically:
#' - Tests for high positive correlation using Kendall's tau
#' - Tests for linearity using a CUSUM test
#' - Computes confidence intervals based on the distribution of pairwise slopes
#'
#' @returns
#' The function returns a simple_eiv object with the following components:
#'
#'   - `coefficients`: Named vector of coefficients (intercept and slope).
#'   - `residuals`: Residuals from the fitted model.
#'   - `fitted.values`: Predicted Y values.
#'   - `model_table`: Data frame presenting the full results from the Passing-Bablok regression.
#'   - `vcov`: Variance-covariance matrix for slope and intercept. NULL for Passing-Bablok.
#'   - `df.residual`: Residual degrees of freedom.
#'   - `call`: The matched call.
#'   - `terms`: The terms object used.
#'   - `model`: The model frame.
#'   - `x_vals`: Original x values used in fitting.
#'   - `y_vals`: Original y values used in fitting.
#'   - `conf.level`: Confidence level used.
#'   - `method`: "passing-bablok" to identify the regression type.
#'   - `kendall_test`: Results of Kendall's tau correlation test.
#'   - `cusum_test`: Results of CUSUM linearity test.
#'   - `n_slopes`: Number of slopes used in estimation.
#'
#' @examples
#' \dontrun{
#' # Passing-Bablok regression
#' model <- pb_reg(method2 ~ method1, data = mydata)
#'
#' # View results
#' print(model)
#' summary(model)
#' plot(model)
#' }
#'
#' @references
#' Passing, H. and Bablok, W. (1983). A new biometrical procedure for testing the
#'   equality of measurements from two different analytical methods. Journal of
#'   Clinical Chemistry and Clinical Biochemistry, 21, 709-720.
#'
#' Passing, H. and Bablok, W. (1984). Comparison of several regression procedures
#'   for method comparison studies and determination of sample sizes. Journal of
#'   Clinical Chemistry and Clinical Biochemistry, 22, 431-445.
#'
#' Bablok, W., Passing, H., Bender, R. and Schneider, B. (1988). A general
#'   regression procedure for method transformation. Journal of Clinical Chemistry
#'   and Clinical Biochemistry, 26, 783-790.
#'
#' @importFrom stats pnorm pt qnorm qt model.frame model.matrix model.response terms complete.cases cor sd
#' @export

pb_reg <- function(formula,
                   data,
                   conf.level = 0.95,
                   keep_data = FALSE,
                   ...) {

  # Capture the call
  call2 <- match.call()

  # Error checking for conf.level
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("conf.level must be a single numeric value")
  }

  if (is.na(conf.level)) {
    stop("conf.level cannot be NA")
  }

  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1 (exclusive)")
  }

  # Extract model frame and terms
  mf <- model.frame(formula, data = data, na.action = na.omit)
  mt <- attr(mf, "terms")

  # Extract y and x from formula
  y_vals <- model.response(mf, "numeric")
  x_vals <- model.matrix(mt, mf)[, -1, drop = TRUE]  # Remove intercept column

  # Store variable names
  y_name <- names(mf)[1]
  x_name <- names(mf)[2]

  # Remove incomplete cases
  complete_idx <- complete.cases(x_vals, y_vals)
  x_vals <- x_vals[complete_idx]
  y_vals <- y_vals[complete_idx]
  n <- length(x_vals)

  if (n < 3) {
    stop("At least 3 complete observations are required")
  }

  # Test for high positive correlation using Kendall's tau
  kendall_result <- .test_kendall_tau(x_vals, y_vals, conf.level)

  if (kendall_result$tau <= 0) {
    warning("Kendall's tau is not positive. Passing-Bablok regression requires positive correlation.")
  }

  # Compute Passing-Bablok estimates
  pb_result <- .passing_bablok_fit(x_vals, y_vals, conf.level)

  # Extract coefficients
  b0 <- pb_result$intercept
  b1 <- pb_result$slope

  # Test linearity using CUSUM test
  cusum_result <- .test_cusum_linearity(x_vals, y_vals, b0, b1)

  # Compute fitted values and residuals
  y_fitted <- b0 + b1 * x_vals
  residuals <- y_vals - y_fitted

  # Compute variance-covariance matrix from confidence intervals
  # Using the width of CI to estimate SE
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha/2)

  se_slope <- (pb_result$slope_upper - pb_result$slope_lower) / (2 * z_crit)
  se_intercept <- (pb_result$intercept_upper - pb_result$intercept_lower) / (2 * z_crit)

  # For Passing-Bablok, no method?

  # vcov_matrix <- matrix(
  #   c(se_intercept^2, se_intercept * se_slope * rho_est,
  #     se_intercept * se_slope * rho_est, se_slope^2),
  #   nrow = 2, ncol = 2,
  #   dimnames = list(c("(Intercept)", x_name), c("(Intercept)", x_name))
  # )

  # Create model table
  model_table <- data.frame(
    term = c("Intercept", x_name),
    coef = c(b0, b1),
    se = c(se_intercept, se_slope),
    lower.ci = c(pb_result$intercept_lower, pb_result$slope_lower),
    upper.ci = c(pb_result$intercept_upper, pb_result$slope_upper),
    df = rep(n - 2, 2),
    stringsAsFactors = FALSE
  )

  # Add hypothesis tests (H0: intercept = 0, H0: slope = 1)
  model_table$null_value <- c(0, 1)
  model_table$reject_h0 <- c(
    pb_result$intercept_lower > 0 | pb_result$intercept_upper < 0,
    pb_result$slope_lower > 1 | pb_result$slope_upper < 1
  )

  # Create coefficients vector with names
  coefs <- setNames(c(b0, b1), c("(Intercept)", x_name))

  # Create the return object
  structure(
    list(
      coefficients = coefs,
      residuals = residuals,
      fitted.values = y_fitted,
      model_table = model_table,
      vcov = NULL,
      df.residual = n - 2,
      call = call2,
      terms = mt,
      model = mf,
      x_vals = x_vals,
      y_vals = y_vals,
      conf.level = conf.level,
      method = "passing-bablok",
      kendall_test = kendall_result,
      cusum_test = cusum_result,
      n_slopes = pb_result$n_slopes,
      slopes_data = if(keep_data) pb_result$slopes else NULL
    ),
    class = "simple_eiv"
  )
}


#' Compute Passing-Bablok regression coefficients
#' @keywords internal
#' @noRd
.passing_bablok_fit <- function(x, y, conf.level = 0.95) {

  n <- length(x)

  # Compute all pairwise slopes
  slopes <- numeric()
  K <- 0  # Counter for slopes less than -1

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {

      dx <- x[j] - x[i]
      dy <- y[j] - y[i]

      # Skip if both differences are zero (0/0)
      if (dx == 0 && dy == 0) {
        next
      }

      # Handle vertical differences
      if (dx == 0) {
        if (dy > 0) {
          slopes <- c(slopes, Inf)
        } else if (dy < 0) {
          slopes <- c(slopes, -Inf)
        }
        next
      }

      # Compute slope
      s <- dy / dx

      # Exclude slopes exactly equal to -1
      if (abs(s - (-1)) < .Machine$double.eps) {
        next
      }

      # Count slopes less than -1
      if (s < -1) {
        K <- K + 1
      }

      slopes <- c(slopes, s)
    }
  }

  # Remove infinite values for median computation
  finite_slopes <- slopes[is.finite(slopes)]
  N <- length(finite_slopes)

  if (N == 0) {
    stop("No valid slopes could be computed")
  }

  # Sort slopes
  sorted_slopes <- sort(finite_slopes)

  # Compute shifted median for slope
  # The median is shifted K positions to the right
  median_idx <- ceiling(N / 2) + K

  # Ensure index is valid
  if (median_idx < 1) median_idx <- 1
  if (median_idx > N) median_idx <- N

  b1 <- sorted_slopes[median_idx]

  # Compute intercept as median of (y - b1*x)
  intercepts <- y - b1 * x
  b0 <- median(intercepts)

  # Compute confidence intervals
  alpha <- 1 - conf.level
  z_alpha <- qnorm(1 - alpha/2)

  # CI for slope
  C_alpha <- z_alpha * sqrt(n * (n - 1) * (2*n + 5) / 18)
  M1 <- floor((N - C_alpha) / 2)
  M2 <- N - M1 + 1

  # Ensure indices are valid
  M1 <- max(1, M1)
  M2 <- min(N, M2)

  # Apply shift for confidence limits
  M1_shifted <- M1 + K
  M2_shifted <- M2 + K

  # Ensure shifted indices are valid
  M1_shifted <- max(1, min(N, M1_shifted))
  M2_shifted <- max(1, min(N, M2_shifted))

  slope_lower <- sorted_slopes[M1_shifted]
  slope_upper <- sorted_slopes[M2_shifted]

  # CI for intercept
  intercepts_lower <- y - slope_upper * x
  intercepts_upper <- y - slope_lower * x

  intercept_lower <- median(intercepts_lower)
  intercept_upper <- median(intercepts_upper)

  return(list(
    intercept = b0,
    slope = b1,
    intercept_lower = intercept_lower,
    intercept_upper = intercept_upper,
    slope_lower = slope_lower,
    slope_upper = slope_upper,
    n_slopes = N,
    slopes = sorted_slopes
  ))
}


#' Test for high positive correlation using Kendall's tau
#' @keywords internal
#' @noRd
.test_kendall_tau <- function(x, y, conf.level = 0.95) {

  n <- length(x)

  # Compute Kendall's tau
  concordant <- 0
  discordant <- 0

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sign_x <- sign(x[j] - x[i])
      sign_y <- sign(y[j] - y[i])

      if (sign_x * sign_y > 0) {
        concordant <- concordant + 1
      } else if (sign_x * sign_y < 0) {
        discordant <- discordant + 1
      }
    }
  }

  tau <- (concordant - discordant) / (concordant + discordant)

  # Test statistic
  var_tau <- (2 * (2*n + 5)) / (9 * n * (n - 1))
  z_stat <- tau / sqrt(var_tau)
  p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)

  # Confidence interval
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha/2)
  tau_lower <- tau - z_crit * sqrt(var_tau)
  tau_upper <- tau + z_crit * sqrt(var_tau)

  return(list(
    tau = tau,
    z_statistic = z_stat,
    p_value = p_value,
    lower = tau_lower,
    upper = tau_upper,
    significant = p_value < (1 - conf.level)
  ))
}


#' Test linearity using CUSUM test
#' @keywords internal
#' @noRd
.test_cusum_linearity <- function(x, y, b0, b1) {

  n <- length(x)

  # Compute residuals
  fitted <- b0 + b1 * x
  resid <- y - fitted

  # Count residuals above and below line
  n_pos <- sum(resid > 0)
  n_neg <- sum(resid < 0)

  if (n_pos == 0 || n_neg == 0) {
    # All points on one side - perfect alignment, pass linearity
    return(list(
      max_cusum = 0,
      test_statistic = 0,
      p_value = 1,
      linear = TRUE
    ))
  }

  # Assign scores
  r <- numeric(n)
  r[resid > 0] <- sqrt(n_neg / n_pos)
  r[resid < 0] <- -sqrt(n_pos / n_neg)
  r[resid == 0] <- 0

  # Compute distance scores
  D <- (y + x / b1 - b0) / sqrt(1 + 1/b1^2)

  # Sort by distance
  order_idx <- order(D)
  r_sorted <- r[order_idx]

  # Compute CUSUM
  cusum <- cumsum(r_sorted)
  max_cusum <- max(abs(cusum))

  # Test statistic
  H <- max_cusum / sqrt(n_pos + 1)

  # P-value from Kolmogorov-Smirnov distribution
  # Approximate p-value
  if (H < 1.36) {
    p_value <- 1 - 0.95  # Greater than 0.05
  } else if (H < 1.63) {
    p_value <- 0.025  # Between 0.01 and 0.05
  } else {
    p_value <- 0.005  # Less than 0.01
  }

  return(list(
    max_cusum = max_cusum,
    test_statistic = H,
    p_value = p_value,
    linear = H < 1.36  # 5% critical value
  ))
}
