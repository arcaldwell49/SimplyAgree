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
#' @param replicates Number of resamples for confidence intervals and the
#'   variance-covariance matrix. For `se_method = "bootstrap"` this is the number of
#'   bootstrap resamples; for `se_method = "jackknife"` it is the number of random
#'   delete-d subsets. If 0 (default), analytical confidence intervals are used and no
#'   variance-covariance matrix is returned (unless `se_method = "dufey"`). Resampling
#'   is recommended for weighted data and 'invariant' or 'scissors' methods.
#' @param se_method Method used to estimate the variance-covariance matrix and
#'   confidence intervals of the coefficients. Options are:
#'   \itemize{
#'     \item "bootstrap": Nonparametric pairs (case) bootstrap with percentile
#'       confidence intervals (default). Used when `replicates > 0`.
#'     \item "jackknife": Delete-d jackknife with d = floor(n/2), using `replicates`
#'       random subsets (Shao & Wu, 1989). Used when `replicates > 0`.
#'     \item "dufey": `r lifecycle::badge('experimental')` Analytic sandwich
#'       estimator of Dufey (2020). Does not require resampling (`replicates` is
#'       ignored). Only available for the "scissors" method without case weights
#'       and with `error.ratio = 1`. See Details before using it for joint tests.
#'   }
#' @param model Logical. If TRUE (default), the model frame is stored in the returned object.
#'   This is needed for methods like `plot()`, `fitted()`, `residuals()`, and `predict()` to work
#'   without supplying `data`. If FALSE, the model frame is not stored (saves memory for large datasets),
#'   but these methods will require a `data` argument.
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
#' error.ratio != 1, pairs are weighted to account for heterogeneous measurement
#' precision.
#'
#' ## Weighting
#'
#' Case weights can be provided via the `weights` argument. These are distinct from
#' measurement error weighting (controlled by `error.ratio`). Case weights allow
#' you to down-weight or up-weight specific observations in the analysis.
#'
#' ## Standard Errors and Variance-Covariance Matrix
#'
#' The analytical (Passing & Bablok, 1983) confidence intervals do not provide a
#' covariance between the intercept and slope, so a variance-covariance matrix
#' (needed by [joint_test()] and [plot_joint()]) is only returned when one of the
#' following is used:
#'
#' - **Pairs bootstrap** (`se_method = "bootstrap"`, `replicates > 0`): whole
#'   (x, y) observations are resampled with replacement and the model is refit.
#'   The variance-covariance matrix is the covariance of the bootstrap estimates
#'   and the confidence intervals are percentile intervals.
#' - **Delete-d jackknife** (`se_method = "jackknife"`, `replicates > 0`): the
#'   model is refit on `replicates` random subsets that each leave out
#'   d = floor(n/2) observations. The delete-1 jackknife is inconsistent for
#'   median-type estimators such as Passing-Bablok; deleting d observations with
#'   \eqn{\sqrt{n}/d \to 0} restores consistency (Shao & Wu, 1989). Confidence
#'   intervals are Wald-type intervals using a t(n-2) quantile.
#' - **Dufey (2020)** (`se_method = "dufey"`): an analytic, distribution-free
#'   estimator for the equivariant ("scissors") Passing-Bablok estimator, based on
#'   the U-statistic variance of Kendall's tau and a sandwich estimator for the
#'   intercept. The slope interval is formed from order statistics of the pairwise
#'   slopes and the intercept interval is Wald-type. **This option is
#'   experimental.** In simulations its marginal standard errors were accurate
#'   for n >= 20, but joint tests of intercept and slope ([joint_test()]) rejected
#'   a true null hypothesis about 7-11% of the time at a nominal 5%, suggesting the
#'   intercept-slope covariance is not yet reliable. Prefer the pairs bootstrap
#'   or delete-d jackknife for joint tests.
#'
#' Resampling is particularly useful for:
#' - Weighted regression (case weights or error.ratio != 1)
#' - Methods 'invariant' and 'scissors' (where analytical CI validity is uncertain)
#' - Joint tests of intercept and slope
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
#'   - `vcov`: Variance-covariance matrix for slope and intercept (if resampling
#'     or `se_method = "dufey"` is used; otherwise NULL).
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
#'   - `boot`: Resampling results (if replicates > 0 and `se_method` is
#'     "bootstrap" or "jackknife").
#'   - `se_method`: Method actually used for standard errors ("analytic",
#'     "bootstrap", "jackknife", or "dufey").
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
#' # Delete-d jackknife or analytic (Dufey 2020) variance-covariance matrix
#' model_jack <- pb_reg(method2 ~ method1, data = mydata,
#'                      se_method = "jackknife", replicates = 1000)
#' model_dufey <- pb_reg(method2 ~ method1, data = mydata, se_method = "dufey")
#' joint_test(model_dufey)
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
#' Passing, H., & Bablok, W. (1983). A New Biometrical Procedure for Testing the Equality of Measurements from Two Different Analytical Methods. Application of linear regression procedures for method comparison studies in Clinical Chemistry,
#'   Part I. Cclm, 21(11), 709-720. doi: 10.1515/cclm.1983.21.11.709
#'
#' Passing, H., & Bablok, W. (1984). Comparison of Several Regression Procedures for Method Comparison Studies and Determination of Sample Sizes Application of linear regression procedures for method comparison studies in Clinical Chemistry, Part II.
#'   Clinical Chemistry and Laboratory Medicine,
#'   22(6). doi: 10.1515/cclm.1984.22.6.431
#'
#' Bablok, W., Passing, H., Bender, R., & Schneider, B. (1988). A General Regression Procedure for Method Transformation. Application of Linear Regression Procedures for Method Comparison Studies in Clinical Chemistry, Part III.
#'   Clinical Chemistry and Laboratory Medicine,
#'   26(11). doi: 10.1515/cclm.1988.26.11.783
#'
#' Dufey, F. (2020). Derivation of Passing-Bablok regression from Kendall's tau.
#'   The International Journal of Biostatistics, 16(2), 20190157.
#'   doi: 10.1515/ijb-2019-0157
#'
#' Sen, P. K. (1968). Estimates of the regression coefficient based on
#'   Kendall's tau. Journal of the American Statistical Association, 63(324),
#'   1379-1389. doi: 10.1080/01621459.1968.10480934
#'
#' Shao, J., & Wu, C. F. J. (1989). A general theory for jackknife variance
#'   estimation. The Annals of Statistics, 17(3), 1176-1197.
#'   doi: 10.1214/aos/1176347263
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
                   se_method = c("bootstrap", "jackknife", "dufey"),
                   model = TRUE,
                   keep_data = TRUE,
                   ...) {

  # Capture the call
  call2 <- match.call()

  # Match and validate method arguments
  method <- match.arg(method)
  se_method <- match.arg(se_method)
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
  if (se_method == "dufey") {
    if (method != "scissors") {
      stop("se_method = 'dufey' is only available for method = 'scissors'.")
    }
    if (has_weights) {
      stop("se_method = 'dufey' is not available with case weights or error.ratio != 1.")
    }
  }
  if (has_weights && replicates == 0) {
    warning("Bootstrap confidence intervals are recommended when error.ratio != 1 or with case weights. Consider setting replicates > 0.")
  }
  if (method_num > 1 && replicates == 0 && se_method != "dufey") {
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

  # Variance-covariance matrix and confidence intervals
  boot_result <- NULL
  vcov_mat <- NULL
  se_used <- "analytic"
  se_result <- NULL

  if (se_method == "dufey") {
    se_result <- .dufey_pb(x_vals, y_vals, b0, b1, conf.level)
    se_used <- "dufey"
  } else if (replicates > 0) {
    boot_result <- .resample_pb(x_vals, y_vals, wts, error.ratio,
                                method_num, conf.level, replicates, b0, b1,
                                type = se_method)
    se_result <- boot_result
    se_used <- se_method
  }

  if (!is.null(se_result)) {
    pb_result$intercept_lower <- se_result$ci[1, 1]
    pb_result$intercept_upper <- se_result$ci[1, 2]
    pb_result$slope_lower <- se_result$ci[2, 1]
    pb_result$slope_upper <- se_result$ci[2, 2]
    vcov_mat <- se_result$vcov
  }

  # Standard errors: from the vcov when available, otherwise from CI width
  if (!is.null(vcov_mat)) {
    se_intercept <- sqrt(vcov_mat[1, 1])
    se_slope <- sqrt(vcov_mat[2, 2])
  } else {
    alpha <- 1 - conf.level
    z_crit <- qnorm(1 - alpha/2)
    se_slope <- (pb_result$slope_upper - pb_result$slope_lower) / (2 * z_crit)
    se_intercept <- (pb_result$intercept_upper - pb_result$intercept_lower) / (2 * z_crit)
  }

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
  # Tolerance guards against floating-point error from tan(atan(.)), e.g.
  # tan(pi/4) = 0.9999999999999999, which matters when tied data collapse the
  # CI onto the null value
  tol <- sqrt(.Machine$double.eps)
  model_table$reject_h0 <- c(
    pb_result$intercept_lower > tol | pb_result$intercept_upper < -tol,
    pb_result$slope_lower > 1 + tol | pb_result$slope_upper < 1 - tol
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
      model = if (model) df3 else NULL,
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
      replicates = if (se_used %in% c("bootstrap", "jackknife")) replicates else 0,
      se_method = se_used
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

      # SD of Kendall's S with the tie correction for tied x values
      # (Sen, 1968): Var(S) = [n(n-1)(2n+5) - sum t(t-1)(2t+5)] / 18.
      # The correction belongs inside the square root.
      tiecount <- as.vector(table(x))
      var_s <- (n * (n - 1) * (2 * n + 5) -
                  sum(tiecount * (tiecount - 1) * (2 * tiecount + 5))) / 18
      v <- sqrt(max(var_s, 0))

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
      M1 <- round((N_theta - z_alpha * v) / 2, 0)
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


#' Resampling variance-covariance and confidence intervals for Passing-Bablok
#'
#' type = "bootstrap": nonparametric pairs (case) bootstrap, percentile CIs.
#' type = "jackknife": delete-d jackknife with d = floor(n/2) over `replicates`
#'   random subsets (Shao & Wu, 1989), Wald-type t(n-2) CIs.
#' @keywords internal
#' @noRd
.resample_pb <- function(x, y, wts, error.ratio, method, conf.level, replicates,
                         b0, b1, type = c("bootstrap", "jackknife")) {

  type <- match.arg(type)
  n <- length(x)
  d <- floor(n / 2)

  coefs <- matrix(NA_real_, nrow = replicates, ncol = 2)

  for (b in seq_len(replicates)) {
    idx <- if (type == "bootstrap") {
      sample.int(n, n, replace = TRUE)
    } else {
      sort(sample.int(n, n - d))
    }
    x_b <- x[idx]
    y_b <- y[idx]
    w_b <- wts[idx]

    pair_weights_b <- NULL
    if (error.ratio != 1) {
      pair_weights_b <- .compute_pair_weights_from_ratio(x_b, y_b, error.ratio,
                                                         w_b, length(x_b))
    }

    fit_b <- tryCatch(
      .passing_bablok_fit(x_b, y_b, method, conf.level = 0,
                          pair_weights_b, case_weights = w_b),
      error = function(e) NULL
    )
    if (!is.null(fit_b)) {
      coefs[b, ] <- c(fit_b$intercept, fit_b$slope)
    }
  }

  # Drop failed refits rather than imputing the original estimates,
  # which would shrink the variance
  ok <- stats::complete.cases(coefs) & is.finite(coefs[, 1]) & is.finite(coefs[, 2])
  n_failed <- sum(!ok)
  if (n_failed > 0) {
    warning(sprintf("%d of %d resamples failed and were dropped.",
                    n_failed, replicates))
  }
  coefs <- coefs[ok, , drop = FALSE]
  if (nrow(coefs) < 2) {
    stop("Too few successful resamples to estimate the variance-covariance matrix.")
  }

  alpha <- 1 - conf.level

  if (type == "bootstrap") {
    vcov_mat <- var(coefs)
    ci_matrix <- rbind(
      quantile(coefs[, 1], c(alpha / 2, 1 - alpha / 2), names = FALSE),
      quantile(coefs[, 2], c(alpha / 2, 1 - alpha / 2), names = FALSE)
    )
  } else {
    centered <- sweep(coefs, 2, colMeans(coefs))
    vcov_mat <- (n - d) / (d * nrow(coefs)) * crossprod(centered)
    t_crit <- qt(1 - alpha / 2, df = n - 2)
    se <- sqrt(diag(vcov_mat))
    ci_matrix <- rbind(b0 + c(-1, 1) * t_crit * se[1],
                       b1 + c(-1, 1) * t_crit * se[2])
  }
  dimnames(vcov_mat) <- list(c("Intercept", "Slope"), c("Intercept", "Slope"))

  boot_obj <- list(
    t = coefs,
    R = replicates,
    type = type,
    d = if (type == "jackknife") d else NULL,
    data = list(x = x, y = y)
  )
  class(boot_obj) <- "boot"

  list(
    ci = ci_matrix,
    vcov = vcov_mat,
    boot_obj = boot_obj
  )
}


#' Analytic variance-covariance for the equivariant (scissors) Passing-Bablok
#' estimator (Dufey, 2020)
#'
#' Port of the quadratic-time branch of mcr:::mc.PBequi (radian slope measure).
#' The slope SE uses the distribution-free U-statistic variance of Kendall's tau
#' converted with a McKean-Schrader interval; the intercept SE and the
#' intercept-slope covariance come from Dufey's sandwich estimator:
#' Var(b0) = sez^2 (1 - covtx^2) + se1^2 xw^2 and Cov(b0, b1) = -xw se1^2.
#' @keywords internal
#' @noRd
.dufey_pb <- function(x, y, b0, b1, conf.level = 0.95) {

  n <- length(x)
  if (n < 4) {
    stop("se_method = 'dufey' requires at least 4 observations.")
  }
  z0 <- 1.4        # z score for McKean-Schrader interval of the median
  minvt <- 1e-5    # lower bound for the variance of tau
  alpha <- 1 - conf.level
  z <- qt(1 - alpha / 2, n - 2)

  rel_diff <- function(a, b, eps = 1e-12) {
    d <- a - b
    d[abs(d) < eps * (abs(a) + abs(b)) / 2] <- 0
    d
  }
  ktau <- function(u, v) suppressWarnings(cor(u, v, method = "kendall"))

  ysign <- sign(ktau(x, y))
  if (is.na(ysign) || ysign == 0) ysign <- 1
  Y <- y * ysign

  # Pairwise angles of absolute slopes
  dx <- outer(x, x, rel_diff); diag(dx) <- 1
  dy <- outer(Y, Y, rel_diff); diag(dy) <- 0
  s <- atan(abs(dy / dx)); diag(s) <- NA
  sut <- s[upper.tri(s)]
  sut <- sut[!is.na(sut)]
  theta <- median(sut)
  s[is.na(s)] <- theta
  slope <- tan(theta)
  Z <- Y - slope * x
  intercept <- median(Z)

  # Slope: U-statistic variance of tau -> order-statistic interval
  taui <- rowSums(sign(s - theta))
  vartau <- max(minvt, (4 * sum(taui^2) - 2 * n * (n - 1)) /
                  (n * (n - 1) * (n - 2) * (n - 3)))
  probs <- c(max((1 - sqrt(vartau) * z) / 2, 0),
             min((1 + sqrt(vartau) * z) / 2, 1))
  ci_theta <- quantile(sut, probs = probs, names = FALSE)
  slope_ci <- tan(ci_theta)
  se1 <- (slope_ci[2] - slope_ci[1]) / (2 * z)
  if (!is.finite(se1) || se1 <= 0) {
    stop("Dufey standard error for the slope could not be computed.")
  }

  # Intercept: McKean-Schrader SE of the median residual
  SZ <- sort(Z)
  k <- round((n + 1) / 2 - z0 * sqrt(n / 4))
  k <- min(max(k, 1), n)
  sez <- (SZ[n + 1 - k] - SZ[k]) / (2 * z0)

  # Correlation between residual signs and position along the line
  Zc <- Z - intercept
  Q <- Y + slope * x
  ord <- order(Zc)
  Zc <- Zc[ord]
  Q <- Q[ord]
  for (i in seq_len(n - 1)) {
    if (rel_diff(Zc[i], Zc[i + 1]) == 0) {
      Zc[i] <- Zc[i + 1] <- (Zc[i] + Zc[i + 1]) / 2
    }
  }
  tau_part <- function(zz, qq) {
    m <- length(zz)
    if (m > 1) ktau(zz, qq) * m * (m - 1) else 0
  }
  covtx <- 2 * (tau_part(Zc[Zc > 0], Q[Zc > 0]) - tau_part(Zc[Zc < 0], Q[Zc < 0])) /
    (n * (n - 1)) / sqrt(n * vartau)
  if (!is.finite(covtx)) covtx <- 0

  # Abscissa of minimal intercept variance
  x0 <- (median(Y - (slope - se1 * z0) * x) - median(Y - (slope + se1 * z0) * x)) /
    (2 * se1 * z0)
  # TODO: joint tests using this vcov are anti-conservative (~7-11% at nominal
  # 5%) while marginal SEs are accurate; check -xw * se1^2 against the
  # empirical intercept-slope covariance before promoting from experimental.
  xw <- x0 - sez / se1 * covtx
  se0 <- sqrt(max(0, sez^2 * (1 - covtx^2) + se1^2 * xw^2))

  # Undo sign flip: Y -> -Y flips slope and intercept, so Cov keeps its sign
  slope_ci <- sort(slope_ci * ysign)
  cov01 <- -xw * se1^2

  vcov_mat <- matrix(c(se0^2, cov01, cov01, se1^2), 2,
                     dimnames = list(c("Intercept", "Slope"),
                                     c("Intercept", "Slope")))

  ci_matrix <- rbind(b0 + c(-1, 1) * z * se0,
                     slope_ci)

  list(
    ci = ci_matrix,
    vcov = vcov_mat,
    details = list(vartau = vartau, sez = sez, covtx = covtx * ysign,
                   x0 = x0, xw = xw)
  )
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

  # Handle edge case where n_pos or n_neg is too small for qsmirnov
  if (n_pos < 2 || n_neg < 2) {
    result <- list(
      statistic = c(H = H),
      p.value = NA_real_,
      method = "Passing-Bablok CUSUM test for linearity",
      data.name = data_name,
      parameter = c(n_pos = n_pos, n_neg = n_neg, n_zero = n_zero),
      alternative = "two.sided",
      conf.level = conf.level,
      cumsum = cusum,
      max_cusum = max_cusum,
      Di = D,
      cusum_limit = NA_real_,
      h_critical = NA_real_,
      cusum_lower = NA_real_,
      cusum_upper = NA_real_
    )
    class(result) <- "htest"
    return(result)
  }

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
  # Get critical value from Smirnov distribution (with fallback for edge cases)
  T_critical <- tryCatch(
    qsmirnov(1 - alpha, sizes = c(n_pos, n_neg), alternative = "two.sided"),
    warning = function(w) NA_real_,
    error = function(e) NA_real_
  )

  # Compute h_critical and confidence limits, handling NA from tryCatch
  if (is.na(T_critical) || !is.finite(T_critical)) {
    h_critical <- NA_real_
    conf_limit <- NA_real_
    cusum_lower <- NA_real_
    cusum_upper <- NA_real_
  } else {
    # Convert back to H scale
    h_critical <- T_critical * sqrt((n_pos * n_neg) / (n_pos + n_neg))
    # Confidence limits (constant horizontal lines)
    # From Passing & Bablok (1983): P(max|cusum(i)| < h_γ * sqrt(L+1)) = 1 - γ
    conf_limit <- h_critical * sqrt(n_neg + 1)
    cusum_lower <- -conf_limit
    cusum_upper <- conf_limit
  }

  # Construct htest object
  result <- list(
    statistic = c(H = H),
    p.value = p_value,
    method = "Passing-Bablok CUSUM test for linearity",
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
