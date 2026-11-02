#' Error-ratio Profile for Errors-in-Variables Regression
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Refits an errors-in-variables line over a range of error variance ratios,
#' `var(x) / var(y)`, from 0 (all error in y: ordinary least squares of y on
#' x) to `Inf` (all error in x: ordinary least squares of x on y). Use it when
#' there are no replicates and the error ratio is unknown, to see how much the
#' slope, the intercept and the test of identity depend on the assumed ratio
#' (the "full confidence intervals" of Francq & Govaerts, 2014).
#'
#' @param object A `simple_eiv` object from [dem_reg()] (unweighted) or
#'   [bls_reg()] (constant error variances). Its stored data (`model = TRUE`)
#'   are refitted; for [bls_reg()] these are the subject means with their
#'   numbers of replicates.
#' @param ratios Error ratios, `var(x) / var(y)` for a single measurement, to
#'   fit (`0` and `Inf` allowed). Default: 0, 61 ratios evenly spaced on the
#'   log scale from 0.001 to 1000, the ratio of `object`, and `Inf`.
#' @param conf.level Confidence level. Default: that of `object`.
#'
#' @details
#' At each ratio the line is fitted by bivariate least squares (BLS) with
#' constant error variances in that ratio; with one measurement per subject
#' this is Deming regression. The scale of the error variances is estimated
#' from the residuals, so only their ratio is assumed. The standard errors are
#' those of BLS, \eqn{s^2 R^{-1}} (see [bls_reg()]), on `n - 2` degrees of
#' freedom, for every object (for [dem_reg()] objects they replace the
#' jackknife standard errors). Equation error is not modelled.
#'
#' The exact confidence interval of the slope (Creasy, 1956) is given when the
#' ratio of replicate numbers is the same for all subjects; it is `NA` when
#' the exact confidence set is not a finite interval (the data do not exclude
#' a vertical or horizontal line). The joint test of identity (intercept 0,
#' slope 1) is the Wald F test on 2 and `n - 2` degrees of freedom, as
#' `joint_test(method = "wald")`.
#'
#' The full confidence interval is the union of the intervals over all fitted
#' ratios: the values of the slope (or intercept) not excluded by the data
#' whatever the error ratio. Identity is rejected for every ratio only if the
#' joint test rejects at every ratio.
#'
#' @return An object of class `eiv_ratio_profile`, a list with:
#'   \item{table}{One row per ratio: `ratio`, `slope`, `slope_lower`,
#'   `slope_upper`, `slope_exact_lower`, `slope_exact_upper`, `intercept`,
#'   `intercept_lower`, `intercept_upper`, `joint_F`, `joint_p`.}
#'   \item{full}{The full confidence intervals of the slope and intercept
#'   (and of the slope from the exact intervals, when available; infinite
#'   if the exact interval is unbounded at some ratio).}
#'   \item{identity}{The range of fitted ratios at which identity is not
#'   rejected (`NA` if none).}
#'   \item{fitted_ratio}{The error ratio of `object`.}
#'   \item{conf.level, n, x_name, y_name, call}{Settings.}
#'
#' @references
#' Creasy, M.A. (1956). Confidence limits for the gradient in the linear
#' functional relationship. Journal of the Royal Statistical Society B, 18,
#' 65-69.
#'
#' Francq, B.G. and Govaerts, B. (2014). Measurement methods comparison with
#' errors-in-variables regressions. From horizontal to vertical OLS
#' regression, review and new perspectives. Chemometrics and Intelligent
#' Laboratory Systems, 134, 123-139. \doi{10.1016/j.chemolab.2014.03.006}
#'
#' @seealso [dem_reg()], [bls_reg()], [joint_test()]
#'
#' @examples
#' data("ba1986")
#' fit <- dem_reg(mini1 ~ wright1, data = ba1986)
#' prof <- error_ratio_profile(fit)
#' prof
#' plot(prof)
#'
#' @export
error_ratio_profile <- function(object, ratios = NULL, conf.level = NULL) {
  if (!inherits(object, "simple_eiv")) {
    stop("object must be a simple_eiv object from dem_reg() or bls_reg().")
  }
  if (!is.null(object$method) && grepl("Passing-Bablok", object$method)) {
    stop("error_ratio_profile() does not apply to Passing-Bablok regression.")
  }
  if (inherits(object, "simple_bls")) {
    if (!(object$errors$model_x == "constant" &&
          object$errors$model_y == "constant")) {
      stop("error_ratio_profile() needs constant error variances.")
    }
    if (identical(object$errors$replicates, "linked")) {
      stop("error_ratio_profile() does not apply to linked replicates (the ",
           "errors of x and y are correlated).")
    }
  } else if (isTRUE(object$call$weighted)) {
    stop("error_ratio_profile() does not apply to weighted Deming regression.")
  }
  dat <- object$model
  if (is.null(dat)) {
    stop("The data are not stored in object; refit it with model = TRUE.")
  }
  if (is.null(conf.level)) conf.level <- object$conf.level
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be a single number between 0 and 1.")
  }
  fitted_ratio <- unname(object$error.ratio)
  if (is.null(ratios)) {
    ratios <- c(0, 10^seq(-3, 3, length.out = 61), fitted_ratio, Inf)
  }
  if (!is.numeric(ratios) || length(ratios) < 1 || anyNA(ratios) ||
      any(ratios < 0)) {
    stop("ratios must be non-negative numbers (0 and Inf allowed).")
  }
  ratios <- sort(unique(ratios))

  x <- dat$x
  y <- dat$y
  n_x <- if (!is.null(dat$n_x)) dat$n_x else rep(1, length(x))
  n_y <- if (!is.null(dat$n_y)) dat$n_y else rep(1, length(y))
  n <- length(x)
  df_res <- n - 2
  q_t <- stats::qt(1 - (1 - conf.level) / 2, df_res)
  balanced <- length(unique(round(n_x / n_y, 12))) == 1

  sxx <- sum((x - mean(x))^2)
  syy <- sum((y - mean(y))^2)
  sxy <- sum((x - mean(x)) * (y - mean(y)))

  rows <- lapply(ratios, function(r) {
    # error variances of the subject means in ratio r (scale free)
    if (is.infinite(r)) {
      vx <- 1 / n_x
      vy <- rep(0, n)
    } else {
      vx <- r / (1 + r) / n_x
      vy <- 1 / (1 + r) / n_y
    }
    # Deming start, lambda = var_y / var_x
    lam <- mean(vy) / mean(vx)
    b1 <- if (r == 0) sxy / sxx else if (is.infinite(r)) syy / sxy else
      (syy - lam * sxx + sqrt((syy - lam * sxx)^2 + 4 * lam * sxy^2)) /
      (2 * sxy)
    fit <- .bls_engine(u = x, v = y, var_u = vx, var_v = vy, cov_uv = 0,
                       start = c(mean(y) - b1 * mean(x), b1))
    b <- fit$coef
    se <- sqrt(diag(fit$vcov))
    d <- b - c(0, 1)
    joint_F <- as.numeric(t(d) %*% solve(fit$vcov, d)) / 2
    ex <- if (balanced) .creasy_ci(r * n_y[1] / n_x[1], sxx, syy, sxy, n,
                                   b[2], q_t) else c(NA_real_, NA_real_)
    data.frame(ratio = r, slope = b[2],
               slope_lower = b[2] - q_t * se[2],
               slope_upper = b[2] + q_t * se[2],
               slope_exact_lower = ex[1], slope_exact_upper = ex[2],
               intercept = b[1],
               intercept_lower = b[1] - q_t * se[1],
               intercept_upper = b[1] + q_t * se[1],
               joint_F = joint_F,
               joint_p = stats::pf(joint_F, 2, df_res, lower.tail = FALSE),
               converged = fit$converged)
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL
  if (!all(tab$converged)) {
    warning("The BLS iterations did not converge at some ratios.")
  }
  tab$converged <- NULL

  full <- data.frame(
    term = c("slope", "intercept"),
    lower = c(min(tab$slope_lower), min(tab$intercept_lower)),
    upper = c(max(tab$slope_upper), max(tab$intercept_upper))
  )
  if (balanced) {
    # unbounded at some ratio: the exact full interval is unbounded too
    ex_lo <- if (anyNA(tab$slope_exact_lower)) -Inf else
      min(tab$slope_exact_lower)
    ex_up <- if (anyNA(tab$slope_exact_upper)) Inf else
      max(tab$slope_exact_upper)
    full <- rbind(full, data.frame(term = "slope (exact)", lower = ex_lo,
                                   upper = ex_up))
  }
  ok <- tab$joint_p >= 1 - conf.level
  identity <- if (any(ok)) range(tab$ratio[ok]) else c(NA_real_, NA_real_)

  tn <- attr(object$terms, "term.labels")
  x_name <- if (length(tn)) tn[1] else "x"
  y_name <- if (!is.null(object$terms)) {
    as.character(attr(object$terms, "variables"))[2]
  } else "y"

  structure(
    list(table = tab, full = full, identity = identity,
         fitted_ratio = fitted_ratio, conf.level = conf.level, n = n,
         balanced = balanced, x_name = x_name, y_name = y_name,
         call = match.call()),
    class = "eiv_ratio_profile"
  )
}

# Exact confidence interval for the slope of the linear functional
# relationship with known error ratio (Creasy, 1956), for one measurement
# per subject, ratio = var(x) / var(y) of the plotted values. In the
# rescaled plot (x, y / sqrt(lambda)), lambda = var(y) / var(x), the
# orthogonal-regression angle theta has an exact interval theta +/- phi. NA
# when that interval reaches a vertical line (the confidence set of the
# slope is not a finite interval).
.creasy_ci <- function(ratio, sxx, syy, sxy, n, b1, q_t) {
  if (ratio == 0) {
    # OLS of y on x: the t interval is exact
    se <- sqrt((syy - sxy^2 / sxx) / (n - 2) / sxx)
    return(c(b1 - q_t * se, b1 + q_t * se))
  }
  if (is.infinite(ratio)) {
    # OLS of x on y, c = 1 / slope: invert its exact t interval
    cc <- sxy / syy
    se <- sqrt((sxx - sxy^2 / syy) / (n - 2) / syy)
    lim <- c(cc - q_t * se, cc + q_t * se)
    if (lim[1] <= 0 && lim[2] >= 0) return(c(NA_real_, NA_real_))
    return(sort(1 / lim))
  }
  lam <- 1 / ratio
  arg <- 2 * q_t / sqrt(n - 2) *
    sqrt(lam * (syy * sxx - sxy^2) / ((syy - lam * sxx)^2 + 4 * lam * sxy^2))
  if (!is.finite(arg) || arg > 1) return(c(NA_real_, NA_real_))
  phi <- asin(arg) / 2
  theta <- atan(b1 / sqrt(lam))
  if (theta - phi <= -pi / 2 || theta + phi >= pi / 2) {
    return(c(NA_real_, NA_real_))
  }
  sqrt(lam) * tan(c(theta - phi, theta + phi))
}
