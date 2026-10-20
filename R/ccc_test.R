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
#' @param null.value The CCC under the null hypothesis. The default, `NULL`,
#'   gives the estimate and interval without a test.
#' @param alternative Character string specifying the alternative hypothesis:
#'   \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}. Used
#'   only when `null.value` is given (without a test the interval is
#'   two-sided); a one-sided alternative gives a one-sided interval, as in
#'   [icc_test()].
#'
#' @return A list with class \code{c("ccc_htest", "htest")} containing the
#'   following components:
#'   \item{statistic}{The test statistic on the Fisher z scale (`Z` for
#'   simple data, `t` for reps/nest), when a test was requested.}
#'   \item{parameter}{When a test was requested: the number of pairs `n` for
#'   simple data, or the degrees of freedom `df` (number of subjects - 1) for
#'   reps/nest. The number of pairs or subjects is also in `settings$n`.}
#'   \item{stderr}{The standard errors for CCC and Z-transformed CCC (for
#'   reps/nest, the jackknife SE of z, and the SE of the CCC from it by the
#'   delta method).}
#'   \item{p.value}{The p-value for the test, when a test was requested.}
#'   \item{conf.int}{A confidence interval for the CCC.}
#'   \item{estimate}{The estimated CCC.}
#'   \item{null.value}{The specified hypothesized value of CCC, if any.}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{A character string indicating the method used.}
#'   \item{data.name}{A character string giving the names of the data.}
#'   \item{data}{The data used (`x`, `y`, and `id` for reps/nest), for the
#'   methods.}
#'   \item{components}{For simple data, Lin's decomposition: the Pearson
#'   correlation `r` (precision) and the bias correction factor `C_b`
#'   (accuracy), with CCC = r x C_b. `NULL` for reps/nest.}
#'   \item{settings}{The settings used, for the methods.}
#'
#' @details
#' The concordance correlation coefficient measures the agreement between two
#' measurements of the same quantity. It combines measures of precision (Pearson
#' correlation) and accuracy (bias correction factor) to determine how far the
#' observed data deviate from the line of perfect concordance (45-degree line
#' through the origin).
#'
#' **Simple data.** Lin's (1989) estimator and asymptotic standard error are
#' used, from the complete pairs; these match `DescTools::CCC()` and
#' `epiR::epi.ccc()` (with `ci = "z-transform"`).
#'
#' **Replicate data** (`"reps"`; the true value is constant within a
#' subject). The CCC is estimated from variance components (Carrasco &
#' Jover, 2003): a linear mixed model with a fixed method effect and random
#' subject and subject-by-method effects, fit by REML, gives
#' \deqn{CCC = \frac{\sigma^2_{s}}{\sigma^2_{s} + \sigma^2_{sm} +
#' \sigma^2_{m} + \sigma^2_{e}}}
#' where \eqn{\sigma^2_{m}} is the variance between the method means. All
#' measurements are used, so the replicates of the two methods need not be
#' paired or equal in number. The estimate equals that of
#' `cccrm::ccc_vc(int = TRUE)`.
#'
#' **Nested data** (`"nest"`; the true value may change within a subject,
#' and each row is a matched pair). Lin's estimator is computed over all
#' complete pairs, which keep their pairing.
#'
#' For both, the subject is the independent unit: the standard error of
#' Fisher's z is a delete-one-subject jackknife, so it allows for the
#' correlation of measurements within a subject. (Treating every pair as
#' independent understates the standard error.) The interval and test use a
#' t distribution with (number of subjects - 1) degrees of freedom. In
#' simulations (10% missing, 3-4 measurements per subject) 95% intervals
#' covered 94-95% of the time with 20 or more subjects, but only 91-93% with
#' 10 subjects, where the CCC is also biased downward; treat intervals from
#' few subjects as approximate. At least 3 subjects are needed.
#'
#' The confidence interval and the hypothesis test use Fisher's
#' Z-transformation of the CCC:
#' \deqn{Z = 0.5 \times \log\left(\frac{1 + CCC}{1 - CCC}\right)}
#' By default (`null.value = NULL`) no test is done.
#'
#' **Relation to the ICC.** For two methods, the CCC is close to the
#' absolute-agreement intraclass correlation, ICC(A,1)
#' (`icc_test(type = "agreement")`). Lin's estimator (simple and nested data)
#' uses n rather than n - 1 in the variances and covariance, as Lin (1989)
#' defined it and as other software computes it, so it is slightly below the
#' ICC in small samples; the difference vanishes as n grows (Nickerson, 1997;
#' Carrasco & Jover, 2003). The variance-components estimator (replicate
#' data) uses REML variance components and so corresponds to the ICC form. `ccc_test()` is meant for
#' method-comparison (agreement) studies; [icc_test()] for reliability
#' studies (see its "Choosing the form" and "Related coefficients"
#' sections).
#'
#' **Methods.** `summary()` describes the result in words (including, for
#' simple data, whether a low CCC comes from poor precision or poor
#' accuracy), `plot()` draws the identity or Bland-Altman plot, and
#' `check()` gives diagnostics; see [ccc_htest-methods].
#'
#' @references
#' Lin, L. I. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. Biometrics, 45(1), 255-268.
#'
#' Lin, L. I. (2000). A note on the concordance correlation coefficient.
#' Biometrics, 56(1), 324-325.
#'
#' Carrasco, J. L., & Jover, L. (2003). Estimating the generalized
#' concordance correlation coefficient through variance components.
#' Biometrics, 59(4), 849-858. \doi{10.1111/j.0006-341x.2003.00099.x}
#'
#' Nickerson, C. A. E. (1997). A note on "A concordance correlation
#' coefficient to evaluate reproducibility". Biometrics, 53(4), 1503-1507.
#' \doi{10.2307/2533516}
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
#' # Describe, plot and check
#' res <- ccc_test(x = "x", y = "y", data = reps)
#' summary(res)
#' plot(res)
#' check(res)
#'
#' @seealso [ccc_htest-methods], [icc_test()], [agreement_limit()]
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
                     null.value = NULL,
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
  if (!is.null(null.value) &&
      (!is.numeric(null.value) || length(null.value) != 1 ||
       is.na(null.value) || null.value <= -1 || null.value >= 1)) {
    stop("'null.value' must be NULL or a single number between -1 and 1")
  }
  # without a test the interval is two-sided
  if (is.null(null.value)) {
    alternative <- "two.sided"
  }

  # Prepare data
  prepared <- .ccc_prepare_data(x = x, y = y, id = id, data = data,
                                 data_type = data_type)
  x_vec <- prepared$x
  y_vec <- prepared$y
  id_vec <- prepared$id
  data_name <- prepared$data_name
  effective_data_type <- prepared$data_type
  labels <- strsplit(data_name, " and ", fixed = TRUE)[[1]]
  x_label <- labels[1]
  y_label <- if (length(labels) > 1) labels[2] else "y"

  # Compute CCC based on data type
  if (effective_data_type == "simple") {
    # Use ccc.xy for simple paired data
    ccc_result <- ccc.xy(x = x_vec, y = y_vec,
                         conf.level = conf.level,
                         agree.level = 0.95,
                         TOST = FALSE,
                         prop_bias = FALSE)

    ccc_est <- ccc_result$rho.c$est.ccc

    # Lin's (1989, 2000) asymptotic SE, from the complete pairs (as ccc.xy
    # computes the CCC); the same formula as DescTools::CCC and epiR::epi.ccc
    pairs <- complete.cases(x_vec, y_vec)
    xc <- x_vec[pairs]
    yc <- y_vec[pairs]
    k <- length(xc)
    sx2 <- var(xc) * (k - 1) / k
    sy2 <- var(yc) * (k - 1) / k
    r <- cor(xc, yc)
    u <- (mean(yc) - mean(xc)) / ((sx2 * sy2)^0.25)

    # SE of CCC on original scale
    sep <- sqrt(((1 - r^2) * ccc_est^2 * (1 - ccc_est^2) / r^2 +
                   (2 * ccc_est^3 * (1 - ccc_est) * u^2 / r) -
                   0.5 * ccc_est^4 * u^4 / r^2) / (k - 2))

    # SE of Z-transformed CCC
    se_z <- sep / (1 - ccc_est^2)
    n_param <- sum(complete.cases(data.frame(x_vec, y_vec)))
    method <- "Lin's Concordance Correlation Coefficient"
    # CCC = precision (Pearson r) x accuracy (bias correction factor C_b)
    c_b <- unname(ccc_result$bias)
    components <- c(r = unname(ccc_est) / c_b, C_b = c_b)

  } else {
    df_wide <- data.frame(id = id_vec, x = x_vec, y = y_vec)
    clustered <- .ccc_clustered(df_wide, effective_data_type)
    ccc_est <- clustered$estimate
    sep <- clustered$se
    se_z <- clustered$se_z
    n_param <- clustered$n
    # the nested CCC uses the complete pairs only
    if (effective_data_type == "nest") {
      df_wide <- tidyr::drop_na(df_wide)
    }

    method <- if (effective_data_type == "reps") {
      "Concordance Correlation Coefficient (variance components)"
    } else {
      "Concordance Correlation Coefficient (nested pairs)"
    }
    components <- NULL
  }

  ccc_est <- unname(ccc_est)
  se_z <- unname(se_z)

  # Confidence interval on the Fisher z scale (one-sided with a one-sided
  # alternative, as in icc_test())
  # (reps/nest: t with g - 1 df for the jackknife SE over g subjects)
  jack_df <- if (effective_data_type == "simple") NULL else n_param - 1
  ci <- .ccc_ci(ccc_est, se_z, conf.level, alternative, df = jack_df)

  # Hypothesis test only when a null value is given
  if (is.null(null.value)) {
    test_result <- list(z_stat = NULL, p_value = NULL)
  } else {
    test_result <- .ccc_hypothesis_test(ccc_est = ccc_est,
                                         se_z = se_z,
                                         null.value = null.value,
                                         alternative = alternative,
                                         df = jack_df)
  }

  # Data kept for summary(), plot() and check()
  stored <- if (effective_data_type == "simple") {
    keep <- complete.cases(x_vec, y_vec)
    data.frame(x = x_vec[keep], y = y_vec[keep])
  } else {
    df_wide
  }

  structure(
    list(
      statistic = if (is.null(null.value)) {
        NULL
      } else if (is.null(jack_df)) {
        c(Z = test_result$z_stat)
      } else {
        c(t = test_result$z_stat)
      },
      parameter = if (is.null(null.value)) {
        NULL
      } else if (is.null(jack_df)) {
        c(n = n_param)
      } else {
        c(df = jack_df)
      },
      stderr = c("SE CCC" = unname(sep), "SE Z" = se_z),
      p.value = test_result$p_value,
      conf.int = ci,
      estimate = c(CCC = ccc_est),
      null.value = if (is.null(null.value)) NULL else c(CCC = null.value),
      alternative = alternative,
      method = method,
      data.name = data_name,
      data = stored,
      components = components,
      settings = list(data_type = effective_data_type,
                      conf.level = conf.level,
                      n = n_param,
                      x_name = x_label,
                      y_name = y_label)
    ),
    class = c("ccc_htest", "htest")
  )
}

# Internal helper: confidence interval for the CCC on the Fisher z scale,
# with normal quantiles, or t quantiles with df degrees of freedom
.ccc_ci <- function(ccc_est, se_z, conf.level, alternative, df = NULL) {
  alpha <- 1 - conf.level
  q <- function(p) if (is.null(df)) qnorm(p) else stats::qt(p, df)
  z <- atanh(ccc_est)
  ci <- switch(alternative,
               two.sided = tanh(z + c(-1, 1) * q(1 - alpha / 2) * se_z),
               greater = c(tanh(z - q(1 - alpha) * se_z), 1),
               less = c(-1, tanh(z + q(1 - alpha) * se_z)))
  attr(ci, "conf.level") <- conf.level
  ci
}

# Internal helper: CCC for replicate or nested data, with a delete-one-subject
# jackknife SE of the Fisher z (the subject is the independent unit).
#   reps: variance-components CCC (Carrasco & Jover, 2003) from all
#         measurements, so replicates need not be paired; it equals the
#         point estimate of cccrm::ccc_vc(int = TRUE).
#   nest: Lin's CCC over the complete (x, y) pairs, which keep their pairing.
# df has columns id, x and y. Returns the estimate, its SE, the SE of z and
# the number of subjects.
.ccc_clustered <- function(df, data_type = c("reps", "nest")) {
  data_type <- match.arg(data_type)
  if (data_type == "nest") {
    df <- df[complete.cases(df), , drop = FALSE]
    est_fun <- function(d) .ccc_lin(d$x, d$y)
  } else {
    df <- tidyr::drop_na(tidyr::pivot_longer(df, !id, names_to = "method",
                                             values_to = "measure"))
    df <- as.data.frame(df)
    est_fun <- .ccc_vc
  }
  ids <- unique(df$id)
  g <- length(ids)
  if (g < 3) {
    stop("At least 3 subjects with data are needed for the CCC of ",
         if (data_type == "reps") "replicate" else "nested", " data")
  }
  est <- est_fun(df)
  z_loo <- vapply(ids, function(i) atanh(est_fun(df[df$id != i, ,
                                                     drop = FALSE])),
                  numeric(1))
  se_z <- sqrt((g - 1) / g * sum((z_loo - mean(z_loo))^2))
  list(estimate = est,
       se = se_z * (1 - est^2),
       se_z = se_z,
       n = g)
}

# Internal helper: Lin's CCC (variances and covariance with denominator n)
.ccc_lin <- function(x, y) {
  n <- length(x)
  sxy <- stats::cov(x, y) * (n - 1) / n
  sx2 <- var(x) * (n - 1) / n
  sy2 <- var(y) * (n - 1) / n
  2 * sxy / (sx2 + sy2 + (mean(x) - mean(y))^2)
}

# Internal helper: variance-components CCC for replicate data (Carrasco &
# Jover, 2003): y = mu + method + subject + subject:method + error, fit by
# REML; CCC = s2_subject / (s2_subject + s2_subject:method + s2_method +
# s2_error). The subject:method term is used only when some subject has
# replicates of a method. s2_method is the unbiased (b^2 - var(b)) / 2 for
# the method difference b, truncated at 0. d has columns id, method, measure.
.ccc_vc <- function(d) {
  inter <- any(table(d$id, d$method) > 1)
  form <- if (inter) {
    measure ~ method + (1 | id) + (1 | id:method)
  } else {
    measure ~ method + (1 | id)
  }
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(form, data = d, REML = TRUE)
  ))
  vc <- as.data.frame(lme4::VarCorr(fit))
  s2_id <- vc$vcov[vc$grp == "id"]
  s2_int <- if (inter) vc$vcov[vc$grp == "id:method"] else 0
  b <- lme4::fixef(fit)[[2]]
  s2_met <- max((b^2 - as.matrix(stats::vcov(fit))[2, 2]) / 2, 0)
  s2_id / (s2_id + s2_int + s2_met + stats::sigma(fit)^2)
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

    # Generate data name from the original ccc_test() call
    parent_call <- sys.call(sys.parent())
    if (!is.null(parent_call) && is.call(parent_call)) {
      if (!is.null(names(parent_call)) && "x" %in% names(parent_call)) {
        x_name <- deparse(parent_call[["x"]])
      } else if (length(parent_call) >= 2L) {
        x_name <- deparse(parent_call[[2L]])
      } else {
        x_name <- "x"
      }
      if (!is.null(names(parent_call)) && "y" %in% names(parent_call)) {
        y_name <- deparse(parent_call[["y"]])
      } else if (length(parent_call) >= 3L) {
        y_name <- deparse(parent_call[[3L]])
      } else {
        y_name <- "y"
      }
    } else {
      x_name <- "x"
      y_name <- "y"
    }
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
      stop(paste0("Columns '", x, "' and '", y, "' must be numeric"))
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


# Internal helper: Perform hypothesis test on Fisher-transformed CCC, with a
# normal reference distribution, or t with df degrees of freedom
.ccc_hypothesis_test <- function(ccc_est, se_z, null.value, alternative,
                                 df = NULL) {
  cdf <- function(q, ...) {
    if (is.null(df)) pnorm(q, ...) else stats::pt(q, df, ...)
  }

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
                    "two.sided" = 2 * cdf(-abs(z_stat)),
                    "less" = cdf(z_stat),
                    "greater" = cdf(z_stat, lower.tail = FALSE))

  list(
    z_stat = z_stat,
    p_value = p_value
  )
}
