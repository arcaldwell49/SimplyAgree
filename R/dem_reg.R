#' @title Deming Regression
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' A function for fitting a straight line to two-dimensional data (i.e., X and Y) that are measured with error.
#'
#' @param formula A formula of the form `y ~ x` specifying the model. If provided, takes precedence over `x` and `y` arguments.
#' @param data Data frame with all data.
#' @param id Column with subject identifier (optional).
#' @param x Name of column with first measurement (deprecated in favor of formula interface).
#' @param y Name of other column with the other measurement to compare to the first (deprecated in favor of formula interface).
#' @param conf.level The confidence level required. Default is 95%.
#' @param weighted Logical indicator (TRUE/FALSE) for whether to use weighted Deming regression. Default is FALSE.
#' @param weights an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' @param error.ratio Ratio of the two error variances. Default is 1. This argument is ignored if subject identifiers are provided.
#' @param model Logical. If TRUE (default), the model frame is stored in the returned object.
#'   This is needed for methods like `plot()`, `fitted()`, `residuals()`, and `predict()` to work
#'   without supplying `data`. If FALSE, the model frame is not stored (saves memory for large datasets),
#'   but these methods will require a `data` argument.
#' @param keep_data Logical indicator (TRUE/FALSE). If TRUE, the jacknife samples are returned; default is FALSE.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#'
#' This function provides a Deming regression analysis wherein the sum of distances in both x and y direction is minimized.
#' Deming regression, also known as error-in-variable regression, is useful in situations where both X & Y are measured with error.
#' The use of Deming regression is beneficial when comparing to methods for measuring the same continuous variable.
#'
#' Currently, the `dem_reg` function covers simple Deming regression and weighted Deming regression.
#' Weighted Deming regression can be used by setting the weighted argument to TRUE.
#' The weights can be provided by the user or can be calculated within function.
#'
#' If the data are measured in replicates, then the measurement error can be directly derived from the data.
#' This can be accomplished by indicating the subject identifier with the id argument.
#' When the replicates are not available in the data,
#' then the ratio of error variances (y/x) can be provided with the error.ratio argument.
#'
#'
#' @section Interface Change:
#' The `x` and `y` arguments are deprecated. Please use the `formula` interface instead:
#' \itemize{
#'   \item Old: `dem_reg(x = "x_var", y = "y_var", data = df)`
#'   \item New: `dem_reg(y_var ~ x_var, data = df)`
#' }
#'
#' @returns
#' The function returns a simple_eiv (eiv meaning "error in variables") object with the following components:
#'
#'   - `coefficients`: Named vector of coefficients (intercept and slope).
#'   - `residuals`: Optimized residuals from the fitted model.
#'   - `fitted.values`: Estimated true Y values (Y-hat).
#'   - `model_table`: Data frame presenting the full results from the Deming regression analysis.
#'   - `vcov`: Variance-covariance matrix for slope and intercept.
#'   - `df.residual`: Residual degrees of freedom.
#'   - `call`: The matched call.
#'   - `terms`: The terms object used.
#'   - `xlevels`: (Only for models with factors) levels of factors.
#'   - `model`: The model frame.
#'   - `x_vals`: Original x values used in fitting.
#'   - `y_vals`: Original y values used in fitting.
#'   - `x_hat`: Estimated true X values.
#'   - `y_hat`: Estimated true Y values.
#'   - `error.ratio`: Error ratio used in fitting.
#'   - `weighted`: Whether weighted regression was used.
#'   - `weights`: Weights used in fitting.
#'   - `conf.level`: Confidence level used.
#'   - `resamples`: List containing resamples from jacknife procedure (if keep_data = TRUE).
#'
#' @examples
#' \dontrun{
#' # New formula interface (recommended)
#' model <- dem_reg(y ~ x, data = mydata)
#'
#' # Old interface (still works with deprecation warning)
#' model <- dem_reg(x = "x", y = "y", data = mydata)
#' }
#'
#' @references
#' Linnet, K. (1990) Estimation of the linear relationship between the measurements of two methods with proportional errors. Statistics in Medicine, 9, 1463-1473.
#'
#' Linnet, K. (1993). Evaluation of regression procedures for methods comparison studies. Clinical chemistry, 39, 424-432.
#'
#' Sadler, W.A. (2010). Joint parameter confidence regions improve the power of parametric regression in method-comparison studies. Accreditation and Quality Assurance, 15, 547-554.
#'
#' @importFrom stats pnorm pt qnorm qt lm anova aov complete.cases cor dchisq qchisq sd var prcomp model.frame model.matrix model.response terms delete.response na.pass
#' @importFrom graphics text
#' @import ggplot2
#' @export

dem_reg <- function(formula = NULL,
                    data,
                    id = NULL,
                    x = NULL,
                    y = NULL,
                    conf.level = .95,
                    weighted = FALSE,
                    weights = NULL,
                    error.ratio = 1,
                    model = TRUE,
                    keep_data = FALSE,
                    ...) {

  # Capture the call
  call2 = match.call()
  call2$weighted = weighted
  call2$conf.level = conf.level
  call2$id = id

  # Error checking for conf.level
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("conf.level must be a single numeric value")
  }

  if (!is.numeric(error.ratio) || length(error.ratio) != 1 || error.ratio <= 0) {
    stop("error.ratio must be a single numeric value that is positive.")
  }

  if (is.na(conf.level)) {
    stop("conf.level cannot be NA")
  }

  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be between 0 and 1 (exclusive)")
  }

  conf2 <- 1 - (1 - conf.level) / 2

  # Determine which interface is being used
  using_formula <- !is.null(formula)
  using_xy <- !is.null(x) && !is.null(y)

  if (!using_formula && !using_xy) {
    stop("Either 'formula' or both 'x' and 'y' must be provided")
  }

  # Deprecation warning for old x/y interface
  if (!is.null(x) || !is.null(y)) {
    warning("The 'x' and 'y' arguments are deprecated. ",
            "Please use the formula interface instead: dem_reg(y ~ x, data = ...)",
            call. = FALSE)
  }

  if (using_formula && using_xy) {
    warning("Both 'formula' and 'x'/'y' provided. Using 'formula' interface.")
    using_xy <- FALSE
  }

  # Handle old interface with deprecation warning
  if (using_xy) {


    # Convert old interface to formula
    formula <- as.formula(paste(y, "~", x))


  }
  if (!is.null(id)) {
    formula <- update(formula, ~ . + id)
  }
  call2$formula <- formula
  # Extract model frame and terms
  mf <- model.frame(formula, data = data, na.action = na.pass)
  mt <- attr(mf, "terms")

  # Extract y and x from formula
  y_vals <- model.response(mf, "numeric")
  x_vals <- mf[[2]]  # Remove intercept column

  # Store variable names
  y_name <- names(mf)[1]
  x_name <- names(mf)[2]

  # Handle id if provided
  if (!is.null(id)) {
    # id can be either a column name (string) or the actual values

    id_vals <- mf[[3]]


    df <- data.frame(id = id_vals, x = x_vals, y = y_vals)
    df <- df[complete.cases(df), ]

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
  } else {
    df3 <- data.frame(x = x_vals, y = y_vals)
    df3 <- df3[complete.cases(df3), ]
  }
  # Validate weights if provided
  if (!is.null(weights)) {
    if (length(weights) != nrow(df3)) {
      stop("Length of 'weights' (", length(weights),
           ") must equal number of observations (", nrow(df3), ")")
    }
    if (any(weights < 0)) {
      stop("'weights' must be non-negative")
    }
  }
  # Compute weights
  if (weighted == FALSE) {
    w_i <- rep(1, nrow(df3))
  } else if (!is.null(weights)) {
    w_i <- weights
  } else {
    w_i <- 1 / ((df3$x + error.ratio * df3$y) / (1 + error.ratio))^2
  }

  # Fit the model
  res <- jack_dem(df3$x, df3$y,
                  w_i = w_i,
                  error.ratio = error.ratio)

  # Extract vcov matrix from jack_dem (it computes it correctly there)
  vcov_matrix <- res$vcov

  # Ensure proper dimnames match the formula terms
  if (!is.null(vcov_matrix)) {
    dimnames(vcov_matrix) <- list(c("(Intercept)", x_name),
                                  c("(Intercept)", x_name))
  }

  # Always keep jacks temporarily for vcov computation
  jacks_temp <- res$jacks

  if (keep_data == TRUE) {
    jacks <- res$jacks
  } else {
    jacks <- NULL
  }

  res <- res$df
  confq <- qt(conf2, nrow(df3) - 2)
  res$df <- nrow(df3) - 2
  res$lower.ci <- res$coef - confq * res$se
  res$upper.ci <- res$coef + confq * res$se
  res$t <- 0
  res$t[1] <- res$coef[1] / res$se[1]
  res$t[2] <- (res$coef[2] - 1) / res$se[2]
  res$p.value <- 2 * pt(abs(res$t), res$df, lower.tail = FALSE)

  # Compute fitted values and residuals
  b0 <- res$coef[1]
  b1 <- res$coef[2]

  # Compute d_i (raw y residuals)
  d_i <- df3$y - (b0 + b1 * df3$x)

  # Compute estimated true values (from NCSS documentation page 5)
  x_hat <- df3$x + (error.ratio * b1 * d_i) / (1 + error.ratio * b1^2)
  y_hat <- df3$y - d_i / (1 + error.ratio * b1^2)

  # Compute residuals (from NCSS documentation page 10)
  res_x <- df3$x - x_hat
  res_y <- df3$y - y_hat
  d_sign <- ifelse(d_i >= 0, 1, -1)
  opt_res <- d_sign * sqrt(w_i * res_x^2 + w_i * error.ratio * res_y^2)

  # vcov_matrix was already computed by jack_dem and extracted above




  # Create coefficients vector with names
  coefs <- setNames(c(b0, b1), c("(Intercept)", x_name))

  # Create the return object with lm-like structure
  structure(
    list(
      coefficients = coefs,
     # residuals = opt_res,
     # fitted.values = y_hat,
      model_table = res,
      vcov = vcov_matrix,
      df.residual = nrow(df3) - 2,
      call = call2,
      terms = mt,
      model = if (model) df3 else NULL,
      error.ratio = error.ratio,
      #weighted = weighted,
      weights = w_i,
      conf.level = conf.level,
      resamples = jacks
    ),
    class = "simple_eiv"
  )
}


.compute_joint_region <- function(intercept, slope, vcov, conf.level = 0.95, n_points = 100) {

  # Check for invalid vcov matrix
  if (any(!is.finite(vcov))) {
    warning("Cannot compute joint confidence region: variance-covariance matrix contains non-finite values")
    return(NULL)
  }

  # Chi-square critical value for 2 df
  chi2_crit <- qchisq(conf.level, df = 2)

  # Eigendecomposition of covariance matrix with error handling
  eig <- tryCatch(
    eigen(vcov),
    error = function(e) {
      warning("Cannot compute joint confidence region: ", e$message)
      return(NULL)
    }
  )

  if (is.null(eig)) {
    return(NULL)
  }

  lambda <- eig$values
  v <- eig$vectors

  # Check for negative eigenvalues (indicates non-positive definite matrix)
  if (any(lambda < 0)) {
    warning("Cannot compute joint confidence region: covariance matrix is not positive definite")
    return(NULL)
  }

  # Generate circle
  theta <- seq(0, 2 * pi, length.out = n_points)
  circle <- cbind(cos(theta), sin(theta))

  # Transform to ellipse
  ellipse <- t(v %*% diag(sqrt(lambda * chi2_crit)) %*% t(circle))

  # Center at (intercept, slope)
  ellipse[, 1] <- ellipse[, 1] + intercept
  ellipse[, 2] <- ellipse[, 2] + slope

  colnames(ellipse) <- c("intercept", "slope")

  return(as.data.frame(ellipse))
}


.test_joint_enclosure <- function(intercept, slope, vcov,
                                  ideal_intercept, ideal_slope,
                                  conf.level = 0.95) {

  # Check for invalid vcov matrix
  if (any(!is.finite(vcov))) {
    warning("Cannot perform joint test: variance-covariance matrix contains non-finite values")
    return(NULL)
  }

  # Test point
  point <- c(ideal_intercept, ideal_slope)
  center <- c(intercept, slope)

  # Mahalanobis distance with error handling
  diff <- point - center

  vcov_inv <- tryCatch(
    solve(vcov),
    error = function(e) {
      warning("Cannot perform joint test: ", e$message)
      return(NULL)
    }
  )

  if (is.null(vcov_inv)) {
    return(NULL)
  }

  mahal_dist <- as.numeric(t(diff) %*% vcov_inv %*% diff)

  # Chi-square critical value
  chi2_crit <- qchisq(conf.level, df = 2)

  is_enclosed <- mahal_dist <= chi2_crit

  list(
    mahalanobis_distance = mahal_dist,
    chi2_critical = chi2_crit,
    is_enclosed = is_enclosed,
    p_value = 1 - pchisq(mahal_dist, df = 2)
  )
}

.get_simple_eiv_data <- function(object) {
  if (!inherits(object, "simple_eiv")) {
    stop("Object must be of class 'simple_eiv'")
  }

  mf <- model.frame(object, na.action = na.pass)
  mt <- attr(mf, "terms")

  # Extract y and x from formula
  # model.response gets column 1 (y)
  y_vals <- model.response(mf, "numeric")

  # mf[[2]] gets column 2 (x) directly from model frame, not model matrix
  x_vals <- mf[[2]]

  if (ncol(mf) == 3) {
    # id can be either a column name (string) or the actual values

    id_vals <- mf[[3]]


    df <- data.frame(id = id_vals, x = x_vals, y = y_vals)
    df <- df[complete.cases(df), ]

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
      summarize(x = mean(x, na.rm = TRUE),
                y = mean(y, na.rm = TRUE),
                .groups = 'drop') %>%
      drop_na()


  } else {
    df3 <- data.frame(x = x_vals, y = y_vals)
    df3 <- df3[complete.cases(df3), ]
  }
  return(df3)
}
