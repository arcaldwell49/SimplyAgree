#' @title Passing-Bablok Regression for Method Comparison
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' A robust, nonparametric method for fitting a straight line to two-dimensional data
#' where both variables (X and Y) are measured with error. Particularly useful for
#' method comparison studies.
#'
#' @param formula A formula of the form `y ~ x` specifying the model.
#' @param data Data frame with all data.
#' @param id Column with subject identifier (optional). If provided, measurement error
#'   ratio is calculated from replicate measurements.
#' @param method Method for Passing-Bablok estimation. Options are:
#'   \itemize{
#'     \item "scissors": Scissors estimator (1988) - most robust, scale invariant (default)
#'     \item "symmetric": Original Passing-Bablok (1983) - symmetric around 45-degree line
#'     \item "invariant": Scale-invariant method (1984) - adaptive reference line
#'   }
#' @param conf.level The confidence level required. Default is 95%.
#' @param weights An optional vector of case weights to be used in the fitting process.
#'   Should be NULL or a numeric vector.
#' @param error.ratio Ratio of measurement error variances (var(x)/var(y)). Default is 1.
#'   This argument is ignored if subject identifiers are provided via `id`.
#' @param replicates Number of bootstrap iterations for confidence intervals. If 0 (default),
#'   analytical confidence intervals are used. Bootstrap is recommended for weighted
#'   data and 'invariant' or 'scissors' methods.
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
#' ## Methods
#'
#' Three Passing-Bablok methods are available:
#'
#' **"scissors"** (default): The scissors estimator (1988), most robust and
#' scale-invariant. Uses the median of absolute values of angles.
#'
#' **"symmetric"**: The original method (1983), symmetric about the y = x line.
#' Uses the line y = -x as the reference for partitioning points.
#'
#' **"invariant"**: Scale-invariant method (1984). First finds the median angle
#' of slopes below the horizontal, then uses this as the reference line.
#'
#' ## Measurement Error Handling
#'
#' If the data are measured in replicates, then the measurement error ratio can be
#' directly derived from the data. This can be accomplished by indicating the subject
#' identifier with the `id` argument. When replicates are not available in the data,
#' then the ratio of error variances (var(x)/var(y)) can be provided with the
#' `error.ratio` argument (default = 1, indicating equal measurement errors).
#'
#' The error ratio affects how pairwise slopes are weighted in the robust median
#' calculation. When error.ratio = 1, all pairs receive equal weight. When
#' error.ratio ≠ 1, pairs are weighted to account for heterogeneous measurement
#' precision.
#'
#' ## Weighting
#'
#' Case weights can be provided via the `weights` argument. These are distinct from
#' measurement error weighting (controlled by `error.ratio`). Case weights allow
#' you to down-weight or up-weight specific observations in the analysis.
#'
#' ## Bootstrap
#'
#' Wild bootstrap resampling is used when `replicates > 0`. This is particularly
#' useful for:
#' - Weighted regression (case weights or error.ratio ≠ 1)
#' - Methods 'invariant' and 'scissors' (where analytical CI validity is uncertain)
#' - Small sample sizes
#'
#' The method automatically:
#' - Tests for high positive correlation using Kendall's tau
#' - Tests for linearity using a CUSUM test
#' - Computes confidence intervals (analytical or bootstrap)
#'
#' @returns
#' The function returns a simple_eiv object with the following components:
#'
#'   - `coefficients`: Named vector of coefficients (intercept and slope).
#'   - `residuals`: Residuals from the fitted model.
#'   - `fitted.values`: Predicted Y values.
#'   - `model_table`: Data frame presenting the full results from the Passing-Bablok regression.
#'   - `vcov`: Variance-covariance matrix for slope and intercept (if bootstrap used).
#'   - `df.residual`: Residual degrees of freedom.
#'   - `call`: The matched call.
#'   - `terms`: The terms object used.
#'   - `model`: The model frame.
#'   - `x_vals`: Original x values used in fitting.
#'   - `y_vals`: Original y values used in fitting.
#'   - `weights`: Case weights (if provided).
#'   - `error.ratio`: Error ratio used in fitting.
#'   - `conf.level`: Confidence level used.
#'   - `method`: Character string describing the method.
#'   - `method_num`: Numeric method identifier (1, 2, or 3).
#'   - `kendall_test`: Results of Kendall's tau correlation test.
#'   - `cusum_test`: Results of CUSUM linearity test.
#'   - `n_slopes`: Number of slopes used in estimation.
#'   - `boot`: Bootstrap results (if replicates > 0).
#'
#' @examples
#' \dontrun{
#' # Basic Passing-Bablok regression (scissors method, default)
#' model <- pb_reg(method2 ~ method1, data = mydata)
#'
#' # With known error ratio
#' model_er <- pb_reg(method2 ~ method1, data = mydata, error.ratio = 2)
#'
#' # With replicate measurements
#' model_rep <- pb_reg(method2 ~ method1, data = mydata, id = "subject_id")
#'
#' # With bootstrap confidence intervals
#' model_boot <- pb_reg(method2 ~ method1, data = mydata,
#'                      error.ratio = 1.5, replicates = 1000)
#'
#' # Symmetric method
#' model_sym <- pb_reg(method2 ~ method1, data = mydata, method = "symmetric")
#'
#' # Scale-invariant method
#' model_inv <- pb_reg(method2 ~ method1, data = mydata, method = "invariant")
#'
#' # With case weights
#' model_wt <- pb_reg(method2 ~ method1, data = mydata,
#'                    weights = mydata$case_weights)
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
#' @importFrom stats rbinom psmirnov na.pass density approx IQR qsmirnov cor.test pnorm pt qnorm qt model.frame model.matrix model.response model.weights terms complete.cases cor sd var
#' @importFrom dplyr group_by mutate ungroup summarize %>%
#' @importFrom tidyr drop_na
#' @export

pb_reg <- function(formula,
                   data,
                   id = NULL,
                   method = c("scissors", "symmetric", "invariant"),
                   conf.level = 0.95,
                   weights = NULL,
                   error.ratio = 1,
                   replicates = 0,
                   keep_data = TRUE,
                   ...) {

  # Capture the call
  call2 <- match.call()

  # Match and validate method argument
  method <- match.arg(method)
  method_num <- switch(method,
                       "symmetric" = 1,
                       "invariant" = 2,
                       "scissors" = 3)

  # Error checking
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("conf.level must be a single numeric value")
  }
  if (is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1 (exclusive)")
  }
  if (!is.numeric(replicates) || replicates < 0) {
    stop("replicates must be a non-negative integer")
  }
  if (!is.numeric(error.ratio) || error.ratio <= 0) {
    stop("error.ratio must be a positive number")
  }

  # Extract model frame
  mf <- model.frame(formula, data = data, na.action = na.omit)
  mt <- attr(mf, "terms")

  # Extract y and x from formula
  y_vals <- model.response(mf, "numeric")
  x_vals <- model.matrix(mt, mf)[, -1, drop = TRUE]

  # Store variable names
  y_name <- names(mf)[1]
  x_name <- names(mf)[2]

  # Handle id if provided (replicate measurements)
  if (!is.null(id)) {
    # id can be either a column name (string) or the actual values
    if (is.character(id) && length(id) == 1) {
      id_vals <- data[[id]]
    } else {
      id_vals <- id
    }

    df <- data.frame(id = id_vals, x = x_vals, y = y_vals)
    df <- df[complete.cases(df), ]

    # Calculate error ratio from replicates (matching dem_reg approach)
    df2 <- df %>%
      group_by(id) %>%
      mutate(mean_y = mean(y, na.rm = TRUE),
             mean_x = mean(x, na.rm = TRUE),
             n_x = sum(!is.na(x)),
             n_y = sum(!is.na(y))) %>%
      ungroup() %>%
      mutate(diff_y = y - mean_y,
             diff_y2 = diff_y^2,
             diff_x = x - mean_x,
             diff_x2 = diff_x^2)

    df3 <- df2 %>%
      group_by(id) %>%
      summarize(n_x = mean(n_x),
                x = mean(x, na.rm = TRUE),
                sum_num_x = sum(diff_x2, na.rm = TRUE),
                n_y = mean(n_y),
                y = mean(y, na.rm = TRUE),
                sum_num_y = sum(diff_y2, na.rm = TRUE),
                .groups = 'drop') %>%
      drop_na()

    var_x <- sum(df3$sum_num_x) / sum(df3$n_x - 1)
    var_y <- sum(df3$sum_num_y) / sum(df3$n_y - 1)

    error.ratio <- var_x / var_y

    # Use averaged values
    x_vals <- df3$x
    y_vals <- df3$y
  } else {
    # No replicates - use data as-is
    df3 <- data.frame(x = x_vals, y = y_vals)
    df3 <- df3[complete.cases(df3), ]
    x_vals <- df3$x
    y_vals <- df3$y
  }

  n <- length(x_vals)
  if (n < 3) {
    stop("At least 3 complete observations are required")
  }

  # Handle case weights
  if (!is.null(weights)) {
    # Note: when id is provided, weights should correspond to original data rows
    # Here we just use provided weights or default to 1
    if (length(weights) != nrow(df3)) {
      stop("Length of 'weights' (", length(weights),
           ") must equal number of observations (", nrow(df3), ")")
    }
    if (any(weights < 0)) {
      stop("'weights' must be non-negative")
    }
    if (!is.null(id)) {
      # For replicate data, we already have df3 with averaged values
      # Weights would need to be pre-aggregated by user or set to 1
      wts <- rep(1, n)
      warning("Case weights with replicate data not fully supported. Using equal weights.")
    } else {
      # Match weights to complete cases
      complete_idx <- complete.cases(data.frame(x_vals, y_vals))
      if (length(weights) == nrow(data)) {
        wts <- weights[complete_idx]
      } else if (length(weights) == n) {
        wts <- weights
      } else {
        stop("Length of weights must match number of observations")
      }
    }
  } else {
    wts <- rep(1, n)
  }

  # Check if bootstrap is needed
  has_weights <- !all(wts == wts[1]) || error.ratio != 1
  if (has_weights && replicates == 0) {
    warning("Bootstrap confidence intervals are recommended when error.ratio != 1 or with case weights. Consider setting replicates > 0.")
  }
  if (method_num > 1 && replicates == 0) {
    warning("Bootstrap confidence intervals are recommended for 'invariant' and 'scissors' methods. Consider setting replicates > 0.")
  }

  # Compute weights from error ratio if needed
  pair_weights <- NULL
  if (error.ratio != 1) {
    # Compute pairwise weights based on error ratio
    pair_weights <- .compute_pair_weights_from_ratio(x_vals, y_vals, error.ratio, wts, n)
  }

  # Test for high positive correlation using Kendall's tau
  kendall_result <- cor.test(x_vals, y_vals,
                             method = "kendall",
                             alternative = "greater",
                             exact = FALSE,
                             continuity = TRUE,
                             conf.level = conf.level)

  if (kendall_result$p.value >= (1-conf.level)) {
    message("Kendall's tau is not significantly positive. Passing-Bablok regression requires positive correlation.")
  }

  # Compute Passing-Bablok estimates
  pb_result <- .passing_bablok_fit(x_vals, y_vals, method_num, conf.level,
                                   pair_weights, case_weights = wts)

  # Extract coefficients
  b0 <- pb_result$intercept
  b1 <- pb_result$slope

  # Test linearity using CUSUM test
  cusum_result <- .test_cusum_linearity(x_vals, y_vals, b0, b1,
                                        conf.level = 0.95)


  # Compute fitted values and residuals
  y_fitted <- b0 + b1 * x_vals
  residuals <- y_vals - y_fitted

  # Bootstrap confidence intervals if requested
  boot_result <- NULL
  vcov_mat <- NULL

  if (replicates > 0) {
    boot_result <- .bootstrap_pb(x_vals, y_vals, wts, error.ratio,
                                 method_num, conf.level, replicates, b0, b1)

    # Update confidence intervals from bootstrap
    pb_result$intercept_lower <- boot_result$ci[1, 1]
    pb_result$intercept_upper <- boot_result$ci[1, 2]
    pb_result$slope_lower <- boot_result$ci[2, 1]
    pb_result$slope_upper <- boot_result$ci[2, 2]

    # Variance-covariance matrix from bootstrap
    vcov_mat <- boot_result$vcov
  }

  # Compute standard errors from confidence intervals
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha/2)

  se_slope <- (pb_result$slope_upper - pb_result$slope_lower) / (2 * z_crit)
  se_intercept <- (pb_result$intercept_upper - pb_result$intercept_lower) / (2 * z_crit)

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

  # Create method description
  method_desc <- switch(method,
                        "symmetric" = "Passing-Bablok (symmetric)",
                        "invariant" = "Passing-Bablok (scale-invariant)",
                        "scissors" = "Passing-Bablok (scissors)")

  # Create the return object
  result <- structure(
    list(
      coefficients = coefs,
      #residuals = residuals,
      #fitted.values = y_fitted,
      model_table = model_table,
      vcov = vcov_mat,
      df.residual = n - 2,
      call = call2,
      terms = mt,
      #model = mf,
      #x_vals = x_vals,
      #y_vals = y_vals,
      weights = if (!all(wts == 1)) wts else NULL,
      error.ratio = error.ratio,
      conf.level = conf.level,
      method = method_desc,
      method_num = method_num,
      kendall_test = kendall_result,
      cusum_test = cusum_result,
      slopes = pb_result$theta,
      n_slopes = pb_result$n_slopes,
      ci_slopes = if(keep_data) pb_result$ci_slopes else NULL,
      slopes_data = if(keep_data) pb_result$slopes else NULL,
      boot =  if(keep_data && !is.null(boot_result)) boot_result$boot_obj else NULL,
      replicates = replicates
    ),
    class = "simple_eiv"
  )

  result
}


#' Compute pairwise weights from error ratio
#' @keywords internal
#' @noRd
.compute_pair_weights_from_ratio <- function(x, y, error.ratio, case_wts, n) {

  # For Passing-Bablok with error ratio, we weight each pair by
  # the inverse of the expected variance of the slope estimate
  # slope_ij = (y_j - y_i) / (x_j - x_i)
  # var(slope_ij) ~ var_y/dx^2 + dy^2 * var_x/dx^4
  # With error.ratio = var_x/var_y, this becomes:
  # var(slope_ij) ~ var_y * (1/dx^2 + error.ratio * dy^2/dx^4)

  weights <- numeric()

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dx <- x[j] - x[i]
      dy <- y[j] - y[i]

      # Skip if dx is too small
      if (abs(dx) < sqrt(.Machine$double.eps)) {
        weights <- c(weights, 1)
        next
      }

      # Approximate weight (inverse variance of slope)
      # Simplified: weight by geometric mean adjusted for error ratio
      wt <- sqrt(case_wts[i] * case_wts[j]) / (1 + error.ratio * (dy/dx)^2)

      weights <- c(weights, wt)
    }
  }

  return(weights)
}


#' Compute Passing-Bablok regression coefficients
#' @keywords internal
#' @noRd

.passing_bablok_fit <- function(x, y, method = 1, conf.level = 0.95,
                                pair_weights = NULL, case_weights = NULL) {

  n <- length(x)
  eps <- sqrt(.Machine$double.eps)

  # Helper function for pairwise differences (vectorized)
  pdiff <- function(v, fun = `-`) {
    n <- length(v)
    indx1 <- rep(1:(n - 1), (n - 1):1)
    indx2 <- unlist(lapply(2:n, function(i) i:n))
    as.vector(fun(v[indx2], v[indx1]))
  }

  # Compute all pairwise differences
  xx <- pdiff(x)
  yy <- pdiff(y)

  # Compute weights
  if (!is.null(pair_weights)) {
    ww <- pair_weights
  } else if (!is.null(case_weights)) {
    ww <- pdiff(case_weights, `*`)
  } else {
    ww <- rep(1, length(xx))
  }
  weighted <- !all(ww == ww[1])

  # Remove uninformative pairs (both differences near zero)
  uninformative <- (abs(xx) < eps & abs(yy) < eps)
  if (any(uninformative)) {
    xx <- xx[!uninformative]
    yy <- yy[!uninformative]
    ww <- ww[!uninformative]
  }

  N <- length(xx)
  if (N == 0) {
    stop("No valid slopes could be computed")
  }

  # Convert to polar coordinates (angles)
  # theta ranges from -pi/2 to pi/2
  theta <- atan(yy / xx)

  # Method-specific transformations
  if (method == 1) {
    # Original Passing-Bablok (1983): symmetric around 45-degree line
    # Shift angles below -pi/4 by pi (this handles the K correction)
    theta <- ifelse(theta < -pi/4, theta + pi, theta)
    # Exclude pairs on the -45 degree line (slope = -1)
    keep <- abs(xx + yy) > eps

  } else if (method == 2) {
    # Passing-Bablok method 2 (1984): scale-invariant
    # Find median angle of negative slopes
    below <- (theta < 0 & abs(xx) > eps)
    if (any(below)) {
      if (weighted) {
        m <- .weighted_median(theta[below], ww[below])
      } else {
        m <- median(theta[below])
      }
    } else {
      m <- -1  # Dummy value for rare case of monotone data
    }
    # Shift angles below m by pi
    theta <- ifelse(theta < m, theta + pi, theta)
    # Exclude pairs on the reference line
    keep <- abs(xx * cos(m) + yy * sin(m)) > eps

  } else {
    # Method 3: Scissors estimator (1988)
    theta <- abs(theta)
    keep <- rep(TRUE, length(theta))
  }

  # Apply exclusions
  theta <- theta[keep]
  ww <- ww[keep]
  N_theta <- length(theta)

  if (N_theta == 0) {
    stop("No valid slopes remain after filtering")
  }

  # Initialize ci_slopes as NULL (will be populated for unweighted case)
  ci_slopes <- NULL

  # Compute slope as (weighted) median of angles
  if (!weighted) {
    b1 <- tan(median(theta))

    # Analytical confidence intervals (Theil-Sen style)
    if (conf.level > 0) {
      alpha <- 1 - conf.level
      z_alpha <- qnorm(1 - alpha / 2)

      # Variance adjustment for ties
      v <- sqrt(n * (n - 1) * (2 * n + 5) / 18)
      tiecount <- as.vector(table(x))
      v <- v - sum(tiecount * (tiecount - 1) * (2 * tiecount + 5)) / 18

      dist <- ceiling(v * z_alpha / 2)

      # Sort theta for CI computation
      sorted_theta <- sort(theta)

      # Interpolate to get CI bounds
      ci_theta <- approx(
        x = seq_along(sorted_theta) - 0.5,
        y = sorted_theta,
        xout = N_theta / 2 + c(-dist, dist)
      )$y

      slope_lower <- tan(ci_theta[1])
      slope_upper <- tan(ci_theta[2])

      # Compute M1 and M2 indices for CI slopes (MethComp approach)
      M1 <- round((N_theta - z_alpha * sqrt((n * (n - 1) * (2 * n + 5)) / 18)) / 2, 0)
      M2 <- N_theta - M1 + 1

      # Ensure indices are within bounds
      M1 <- max(1, M1)
      M2 <- min(N_theta, M2)

      # Extract slopes within CI bounds for prediction intervals
      ci_slopes <- tan(sorted_theta[M1:M2])

    } else {
      slope_lower <- NA
      slope_upper <- NA
    }

  } else {
    # Weighted case
    b1 <- tan(.weighted_median(theta, ww))

    # For weighted case, analytical CI not well-defined; use bootstrap
    slope_lower <- NA
    slope_upper <- NA
  }

  # Compute intercept as (weighted) median of (y - b1*x)
  intercepts <- y - b1 * x

  if (is.null(case_weights) || all(case_weights == 1)) {
    b0 <- median(intercepts)
  } else {
    b0 <- .weighted_median(intercepts, case_weights)
  }


  # CI for intercept (derived from slope CI)
  if (!is.na(slope_lower) && !is.na(slope_upper)) {
    intercepts_lower <- y - slope_upper * x
    intercepts_upper <- y - slope_lower * x

    if (is.null(case_weights) || all(case_weights == 1)) {
      intercept_lower <- median(intercepts_lower)
      intercept_upper <- median(intercepts_upper)
    } else {
      intercept_lower <- .weighted_median(intercepts_lower, case_weights)
      intercept_upper <- .weighted_median(intercepts_upper, case_weights)
    }
  } else {
    intercept_lower <- NA
    intercept_upper <- NA
  }

  return(list(
    intercept = b0,
    slope = b1,
    intercept_lower = intercept_lower,
    intercept_upper = intercept_upper,
    slope_lower = slope_lower,
    slope_upper = slope_upper,
    n_slopes = N,
    n_used = N_theta,
    ci_slopes = ci_slopes,
    theta = theta
  ))
}


#' Weighted median calculation
#' @keywords internal
#' @noRd
.weighted_median <- function(x, w) {
  if (length(x) != length(w)) {
    stop("x and w must have same length")
  }

  # Sort by x
  ord <- order(x)
  x_sort <- x[ord]
  w_sort <- w[ord]

  # Cumulative weights
  cum_w <- cumsum(w_sort)
  total_w <- sum(w_sort)

  # Find median position
  median_pos <- total_w / 2

  # Linear interpolation
  approx(cum_w - w_sort/2, x_sort, median_pos)$y
}


#' Bootstrap confidence intervals for Passing-Bablok regression
#' @keywords internal
#' @noRd
.bootstrap_pb <- function(x, y, wts, error.ratio, method, conf.level, replicates,
                          b0, b1) {

  n <- length(x)

  # Wild bootstrap function (Rademacher-like weights)
  wild_weights <- function(n) {
    # Golden ratio based weights as in the deming package
    temp <- rbinom(n, 1, (1 + sqrt(5)) / sqrt(20))
    ifelse(temp == 1, 1 - sqrt(5), 1 + sqrt(5)) / 2
  }

  # Compute orthogonal residuals
  d <- sqrt(1 + b1^2)
  u <- (x + b1 * (y - b0)) / d
  px <- u / d
  py <- b0 + b1 * u / d
  resid_x <- x - px
  resid_y <- y - py

  # Bootstrap resampling
  boot_coefs <- matrix(0, nrow = replicates, ncol = 2)

  for (b in 1:replicates) {
    # Generate wild bootstrap weights
    rb <- wild_weights(n)

    # Create bootstrap sample
    x_boot <- x + resid_x * rb
    y_boot <- y + resid_y * rb

    # Compute weights for bootstrap sample
    pair_weights_boot <- NULL
    if (error.ratio != 1) {
      pair_weights_boot <- .compute_pair_weights_from_ratio(x_boot, y_boot, error.ratio, wts, n)
    }

    # Fit Passing-Bablok to bootstrap sample
    tryCatch({
      fit_boot <- .passing_bablok_fit(x_boot, y_boot, method, conf.level = 0,
                                      pair_weights_boot, case_weights = wts)
      boot_coefs[b, ] <- c(fit_boot$intercept, fit_boot$slope)
    }, error = function(e) {
      # If bootstrap sample fails, use original estimates
      boot_coefs[b, ] <<- c(b0, b1)
    })
  }

  # Compute percentile confidence intervals
  alpha <- 1 - conf.level
  ci_lower <- alpha / 2
  ci_upper <- 1 - alpha / 2

  ci_matrix <- matrix(0, nrow = 2, ncol = 2)
  ci_matrix[1, ] <- quantile(boot_coefs[, 1], c(ci_lower, ci_upper), na.rm = TRUE)
  ci_matrix[2, ] <- quantile(boot_coefs[, 2], c(ci_lower, ci_upper), na.rm = TRUE)

  # Variance-covariance matrix
  vcov_mat <- var(boot_coefs, na.rm = TRUE)
  dimnames(vcov_mat) <- list(c("Intercept", "Slope"), c("Intercept", "Slope"))

  # Create boot object (simplified version)
  boot_obj <- list(
    t = boot_coefs,
    R = replicates,
    data = list(x = x, y = y)
  )
  class(boot_obj) <- "boot"

  return(list(
    ci = ci_matrix,
    vcov = vcov_mat,
    boot_obj = boot_obj
  ))
}



#' @keywords internal
#' @noRd
.test_cusum_linearity <- function(x, y, b0, b1, conf.level = 0.95) {
  n <- length(x)
  data_name <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

  # Compute residuals
  fitted <- b0 + b1 * x
  resid <- y - fitted

  # Count residuals above and below line
  n_pos <- sum(resid > 0)
  n_neg <- sum(resid < 0)
  n_zero <- sum(resid == 0)

  # Handle degenerate cases
  if (n_pos == 0 || n_neg == 0) {
    result <- list(
      statistic = c(H = 0),
      p.value = 1,
      method = "Passing-Bablok CUSUM test for linearity",
      data.name = data_name,
      parameter = c(n_pos = n_pos, n_neg = n_neg, n_zero = n_zero),
      alternative = "two.sided",
      conf.level = conf.level,
      cumsum = numeric(0),
      cusum_lower = NA_real_,
      cusum_upper = NA_real_
    )
    class(result) <- "htest"
    return(result)
  }

  # Assign weighted scores (Passing & Bablok 1983, Section 5, Appendix 4)
  r <- numeric(n)
  r[resid > 0] <- sqrt(n_neg / n_pos)
  r[resid < 0] <- -sqrt(n_pos / n_neg)
  r[resid == 0] <- 0

  # Compute distance scores (projection perpendicular to regression line)
  D <- (y + x / b1 - b0) / sqrt(1 + 1 / b1^2)

  # Sort scores by distance along the line
  order_idx <- order(D)
  r_sorted <- r[order_idx]

  # Compute CUSUM
  cusum <- cumsum(r_sorted)
  max_cusum <- max(abs(cusum))

  # Test statistic H (normalized by sqrt(n_neg + 1))
  # From Passing & Bablok (1983), Appendix Section 4
  H <- max_cusum / sqrt(n_neg + 1)

  # P-value using Smirnov distribution (two-sample KS test)
  # The test compares the distribution of positive vs negative scores
  # The relationship: H = T * sqrt(l*L/(l+L)) where T is the Smirnov statistic
  # Convert H back to Smirnov scale
  T <- H * sqrt((n_pos + n_neg) / (n_pos * n_neg))

  p_value <- psmirnov(T,
                      sizes = c(n_pos, n_neg),
                      alternative = "two.sided",
                      lower.tail = FALSE)

  # Compute confidence limits for CUSUM
  alpha <- 1 - conf.level

  # Get critical value from Smirnov distribution
  T_critical <- qsmirnov(1 - alpha,
                         sizes = c(n_pos, n_neg),
                         alternative = "two.sided")

  # Convert back to H scale
  h_critical <- T_critical * sqrt((n_pos * n_neg) / (n_pos + n_neg))

  # Confidence limits (constant horizontal lines)
  # From Passing & Bablok (1983): P(max|cusum(i)| < h_γ * sqrt(L+1)) = 1 - γ
  conf_limit <- h_critical * sqrt(n_neg + 1)
  cusum_lower <- -conf_limit
  cusum_upper <- conf_limit

  # Construct htest object
  result <- list(
    statistic = c(H = H),
    p.value = p_value,
    method = "Passing-Bablok (1983) CUSUM test for linearity",
    data.name = data_name,
    parameter = c(n_pos = n_pos, n_neg = n_neg, n_zero = n_zero),
    alternative = "two.sided",
    conf.level = conf.level,
    cumsum = cusum,
    max_cusum = max_cusum,
    Di = D,
    cusum_limit = cusum_upper,
    h_critical = h_critical
  )
  class(result) <- "htest"

  return(result)
}

#' Calculate p-value for CUSUM test using Kolmogorov distribution
#'
#' Uses the standard series expansion for the Kolmogorov distribution
#' survival function: P(K > x) = 2 * sum_{k=1}^{inf} (-1)^{k-1} * exp(-2*k^2*x^2)
#'
#' @param H CUSUM test statistic
#' @return P-value
#'
#' @keywords internal
#' @noRd
.kolmogorov_pvalue <- function(H) {
  if (H <= 0) return(1)
  if (H >= 3) return(0)  # Essentially zero probability

  # Series expansion (typically converges quickly)
  max_terms <- 100
  j <- seq_len(max_terms)

  # Compute series
  terms <- (-1)^(j - 1) * exp(-2 * j^2 * H^2)
  p_value <- 2 * sum(terms)

  # Ensure valid probability
  p_value <- max(0, min(1, p_value))

  return(p_value)
}
