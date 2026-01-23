#' Concordance Correlation Coefficient
#'
#' @description
#' Computes Lin's Concordance Correlation Coefficient (CCC) and performs a
#' hypothesis test, returning an object of class \code{"htest"}.
#'
#' @param x Either a numeric vector of measurements for method 1, or a character
#'   string naming the column in \code{data} containing method 1 measurements.
#' @param y Either a numeric vector of measurements for method 2, or a character
#'   string naming the column in \code{data} containing method 2 measurements.
#' @param id A character string naming the column in \code{data} containing subject
#'   identifiers. Required when \code{data_type} is \code{"reps"} or \code{"nest"}.
#' @param data An optional data frame containing the variables named in \code{x},
#'   \code{y}, and \code{id}.
#' @param data_type Character string specifying the data structure:
#'   \code{"simple"} for paired data with one observation per subject (default),
#'   \code{"reps"} for replicate data where subjects are measured multiple times,
#'   \code{"nest"} for nested data structures.
#' @param conf.level Confidence level for the interval, default is 0.95.
#' @param null.value The hypothesized value of CCC under the null hypothesis,
#'   default is 0.
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#'
#' @return A list with class \code{"htest"} containing the following components:
#'   \item{statistic}{The Z-statistic (Fisher-transformed).}
#'   \item{parameter}{The sample size (n for simple data, number of subjects for reps/nest).}
#'   \item{stderr}{The standard errors for CCC and Z-transformed CCC.}
#'   \item{p.value}{The p-value for the test.}
#'   \item{conf.int}{A confidence interval for the CCC.}
#'   \item{estimate}{The estimated CCC.}
#'   \item{null.value}{The specified hypothesized value of CCC.}
#'   \item{alternative}{A character string describing the alternative hypothesis.}
#'   \item{method}{A character string indicating the method used.}
#'   \item{data.name}{A character string giving the names of the data.}
#'
#' @details
#' The concordance correlation coefficient measures the agreement between two
#' measurements of the same quantity. It combines measures of precision (Pearson
#' correlation) and accuracy (bias correction factor) to determine how far the
#' observed data deviate from the line of perfect concordance (45-degree line
#' through the origin).
#'
#' For simple paired data, Lin's original method is used. For replicate or nested
#' data, U-statistics are employed following King, Chinchilli, and Carrasco.
#'
#' The hypothesis test is performed using Fisher's Z-transformation of the CCC:
#' \deqn{Z = 0.5 \times \log\left(\frac{1 + CCC}{1 - CCC}\right)}
#'
#' @references
#' Lin, L. I. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. Biometrics, 45(1), 255-268.
#'
#' King, T. S., & Chinchilli, V. M. (2001). A generalized concordance correlation
#' coefficient for continuous and categorical data. Statistics in Medicine,
#' 20(14), 2131-2147.
#'
#' King, T. S., Chinchilli, V. M., & Carrasco, J. L. (2007). A repeated measures
#' concordance correlation coefficient. Statistics in Medicine, 26(16), 3095-3113.
#'
#' @examples
#' # Simple paired data using vectors
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(1.1, 2.2, 2.9, 4.1, 5.2)
#' ccc_test(x, y)
#'
#' # Using data frame interface
#' data("reps")
#' # Simple data
#' ccc_test(x = "x", y = "y", data = reps)
#'
#' # Test against specific null value
#' ccc_test(x = "x", y = "y", data = reps,
#'          null.value = 0.8, alternative = "greater")
#'
#' # Replicate data
#' ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
#'
#' # Nested data
#' ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")
#'
#' @importFrom stats pnorm qnorm complete.cases cor sd var
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom tidyselect all_of
#' @import dplyr
#' @export
ccc_test <- function(x,
                     y = NULL,
                     id = NULL,

                     data = NULL,
                     data_type = c("simple", "reps", "nest"),
                     conf.level = 0.95,
                     null.value = 0,
                     alternative = c("two.sided", "greater", "less")) {

  # Match arguments
  data_type <- match.arg(data_type)
  alternative <- match.arg(alternative)

  # Validate conf.level

  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be a single number between 0 and 1")
  }

  # Validate null.value
  if (!is.numeric(null.value) || length(null.value) != 1 ||
      null.value < -1 || null.value > 1) {
    stop("'null.value' must be a single number between -1 and 1")
  }

  # Prepare data
  prepared <- .ccc_prepare_data(x = x, y = y, id = id, data = data,
                                 data_type = data_type)
  x_vec <- prepared$x
  y_vec <- prepared$y
  id_vec <- prepared$id
  data_name <- prepared$data_name
  effective_data_type <- prepared$data_type

  # Compute CCC based on data type
  if (effective_data_type == "simple") {
    # Use ccc.xy for simple paired data
    ccc_result <- ccc.xy(x = x_vec, y = y_vec,
                         conf.level = conf.level,
                         agree.level = 0.95,
                         TOST = FALSE,
                         prop_bias = FALSE)

    ccc_est <- ccc_result$rho.c$est.ccc
    ccc_lower <- ccc_result$rho.c$lower.ci
    ccc_upper <- ccc_result$rho.c$upper.ci

    # Compute SE for z-transformed CCC
    # From ccc.xy: sep is SE of CCC, set is SE of Z-transformed
    k <- sum(complete.cases(data.frame(x_vec, y_vec)))
    n_param <- k

    # Recompute the SE_Z using the formula from ccc.xy
    sx2 <- var(x_vec, na.rm = TRUE) * (k - 1) / k
    sy2 <- var(y_vec, na.rm = TRUE) * (k - 1) / k
    r <- cor(x_vec, y_vec, use = "complete.obs")
    v <- sd(y_vec, na.rm = TRUE) / sd(x_vec, na.rm = TRUE)
    xb <- mean(x_vec, na.rm = TRUE)
    yb <- mean(y_vec, na.rm = TRUE)
    u <- (yb - xb) / ((sx2 * sy2)^0.25)

    # SE of CCC on original scale
    sep <- sqrt(((1 - r^2) * ccc_est^2 * (1 - ccc_est^2) / r^2 +
                   (2 * ccc_est^3 * (1 - ccc_est) * u^2 / r) -
                   0.5 * ccc_est^4 * u^4 / r^2) / (k - 2))
    names(sep) <- "SE CCC"
    # SE of Z-transformed CCC
    se_z <- sep / (1 - ccc_est^2)
    names(se_z) <- "SE Z"

    method <- "Lin's Concordance Correlation Coefficient"

  } else {
    # Use cccUst for reps/nest data
    # Prepare long-format data for cccUst
    # Note: For 'nest' data, drop_na() is applied BEFORE pivot_longer
    #       For 'reps' data, drop_na() is applied AFTER pivot_longer
    df_wide <- data.frame(id = id_vec, x = x_vec, y = y_vec)

    if (effective_data_type == "nest") {
      # For nested data: drop rows with any NA first, then pivot
      df_wide <- tidyr::drop_na(df_wide)
      df_long <- tidyr::pivot_longer(df_wide, !id,
                                      names_to = "method",
                                      values_to = "measure")
    } else {
      # For replicate data: pivot first, then drop NA
      df_long <- tidyr::pivot_longer(df_wide, !id,
                                      names_to = "method",
                                      values_to = "measure") %>%
        tidyr::drop_na()
    }

    ccc_ust_result <- cccUst(dataset = df_long,
                              ry = "measure",
                              rmet = "method",
                              cl = conf.level)

    sep <- ccc_ust_result["SE CCC"]  # SE of CCC
    se_z <- ccc_ust_result["SE Z"]  # SE of Z-transformed CCC
    ccc_est <- ccc_ust_result[1]  # CCC
    ccc_lower <- ccc_ust_result[2]  # Lower CI
    ccc_upper <- ccc_ust_result[3]  # Upper CI
    se_z <- ccc_ust_result[6]  # SE Z

    # Number of subjects
    n_param <- length(unique(id_vec))

    method <- "Concordance Correlation Coefficient (U-statistics)"
  }

  # Perform hypothesis test
  test_result <- .ccc_hypothesis_test(ccc_est = ccc_est,
                                       se_z = se_z,
                                       null.value = null.value,
                                       alternative = alternative)

  # Build confidence interval with attribute
  ci <- c(ccc_lower, ccc_upper)
  attr(ci, "conf.level") <- conf.level

  # Build alternative hypothesis description
  alt_text <- switch(alternative,
                     "two.sided" = paste("true CCC is not equal to", null.value),
                     "less" = paste("true CCC is less than", null.value),
                     "greater" = paste("true CCC is greater than", null.value))

  # Name the statistic
  statistic <- test_result$z_stat
  names(statistic) <- "Z"

  # Name the parameter
  parameter <- n_param
  names(parameter) <- "n"

  # Name the estimate
  estimate <- ccc_est
  names(estimate) <- "CCC"

  # Name the null value
  null_val <- null.value
  names(null_val) <- "CCC"

  # Construct htest object
  structure(
    list(
      statistic = statistic,
      parameter = parameter,
      stderr = c(sep, se_z),
      p.value = test_result$p_value,
      conf.int = ci,
      estimate = estimate,
      null.value = null_val,
      alternative = alt_text,
      method = method,
      data.name = data_name
    ),
    class = "htest"
  )
}


# Internal helper: Prepare and validate data
.ccc_prepare_data <- function(x, y, id, data, data_type) {

  # Determine if using vector or data frame interface
  if (is.null(data)) {
    # Vector interface
    if (is.null(y)) {
      stop("'y' must be provided when 'data' is NULL")
    }

    if (!is.numeric(x) || !is.numeric(y)) {
      stop("'x' and 'y' must be numeric vectors when 'data' is NULL")
    }

    if (length(x) != length(y)) {
      stop("'x' and 'y' must have the same length")
    }

    x_vec <- x
    y_vec <- y

    # Generate data name
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    data_name <- paste(x_name, "and", y_name)

    # For vector input, force simple data type
    if (data_type != "simple") {
      warning("'data_type' is ignored when vectors are provided directly. Using 'simple'.")
    }
    effective_data_type <- "simple"
    id_vec <- NULL

  } else {
    # Data frame interface
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }

    if (!is.character(x) || length(x) != 1) {
      stop("'x' must be a single column name (character string) when 'data' is provided")
    }

    if (is.null(y) || !is.character(y) || length(y) != 1) {
      stop("'y' must be a single column name (character string) when 'data' is provided")
    }

    # Check columns exist
    if (!x %in% names(data)) {
      stop(paste0("Column '", x, "' not found in data"))
    }
    if (!y %in% names(data)) {
      stop(paste0("Column '", y, "' not found in data"))
    }

    # Extract data
    x_vec <- data[[x]]
    y_vec <- data[[y]]

    if (!is.numeric(x_vec) || !is.numeric(y_vec)) {
      stop("Columns '", x, "' and '", y, "' must be numeric")
    }

    data_name <- paste(x, "and", y)
    effective_data_type <- data_type

    # Handle id for reps/nest
    if (data_type %in% c("reps", "nest")) {
      if (is.null(id)) {
        stop("'id' must be provided when data_type is 'reps' or 'nest'")
      }
      if (!is.character(id) || length(id) != 1) {
        stop("'id' must be a single column name (character string)")
      }
      if (!id %in% names(data)) {
        stop(paste0("Column '", id, "' not found in data"))
      }
      id_vec <- data[[id]]
    } else {
      id_vec <- NULL
    }
  }

  list(
    x = x_vec,
    y = y_vec,
    id = id_vec,
    data_name = data_name,
    data_type = effective_data_type
  )
}


# Internal helper: Perform hypothesis test on Fisher-transformed CCC
.ccc_hypothesis_test <- function(ccc_est, se_z, null.value, alternative) {

  # Fisher Z-transformation
  # Handle edge cases where CCC is exactly 1 or -1
  if (abs(ccc_est) >= 1) {
    z_ccc <- sign(ccc_est) * Inf
  } else {
    z_ccc <- 0.5 * log((1 + ccc_est) / (1 - ccc_est))
  }

  if (abs(null.value) >= 1) {
    z_null <- sign(null.value) * Inf
  } else {
    z_null <- 0.5 * log((1 + null.value) / (1 - null.value))
  }

  # Z-statistic
  z_stat <- (z_ccc - z_null) / se_z

  # P-value based on alternative

  p_value <- switch(alternative,
                    "two.sided" = 2 * pnorm(-abs(z_stat)),
                    "less" = pnorm(z_stat),
                    "greater" = pnorm(z_stat, lower.tail = FALSE))

  list(
    z_stat = z_stat,
    p_value = p_value
  )
}
