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
#' @param joint_method The joint test of identity at each ratio: `"sadler"`
#'   (default; Sadler's F test, as `joint_test()` for Deming fits) or
#'   `"wald"` (the Wald F test on the BLS covariance). See Details.
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
#' a vertical or horizontal line).
#'
#' The joint test of identity (intercept 0, slope 1) is, by default, Sadler's
#' F test on 2 and `n - 2` degrees of freedom, from the criterion
#' \eqn{S(a, b) = \sum e_i^2 / (v_{y,i} + b^2 v_{x,i})} that the line
#' minimises (\eqn{v} the error variances of the subject means in the given
#' ratio): for one measurement per subject, the test of
#' `joint_test(method = "sadler")` at that ratio. It depends only on the
#' error ratio, and it held its size better than the Wald test in small
#' samples (see [joint_test()]). `joint_method = "wald"` gives the Wald F
#' test on the BLS covariance, as `joint_test(method = "wald")`.
#' `plot(type = "joint")` draws the joint
#' confidence regions at several ratios, showing how the conclusion about
#' identity depends on the assumed ratio.
#'
#' **Bland-Altman plot.** At each ratio the line of the differences
#' `x - y` on the averages is also fitted by correlated bivariate least
#' squares (CBLS, as `bls_reg()`'s `$cbls`), with its BLS confidence
#' intervals (`md_` columns; `print(type = "md")`, `plot(type = "md")`),
#' as BivRegBLS's `FullCIs.MD()` (which uses `y - x`: the signs of its
#' estimates and limits are reversed).
#' It is the same line as the BLS line of y on x, drawn in the Bland-Altman
#' plot; the Sadler test of the CBLS line against intercept 0 and slope 0 is
#' the same test as of identity.
#'
#' The full confidence interval is the union of the intervals over all fitted
#' ratios: the values of the slope (or intercept) not excluded by the data
#' whatever the error ratio. Identity is rejected for every ratio only if the
#' joint test rejects at every ratio.
#'
#' @return An object of class `eiv_ratio_profile`, a list with:
#'   \item{table}{One row per ratio: `ratio`, `slope`, `slope_lower`,
#'   `slope_upper`, `slope_exact_lower`, `slope_exact_upper`, `intercept`,
#'   `intercept_lower`, `intercept_upper`, `joint_F`, `joint_p`, and the
#'   CBLS line of the differences on the averages: `md_slope`,
#'   `md_slope_lower`, `md_slope_upper`, `md_intercept`,
#'   `md_intercept_lower`, `md_intercept_upper`.}
#'   \item{full}{The full confidence intervals of the slope and intercept
#'   (and of the slope from the exact intervals, when available; infinite
#'   if the exact interval is unbounded at some ratio).}
#'   \item{full_md}{The full confidence intervals of the CBLS slope and
#'   intercept.}
#'   \item{joint_method}{The joint test used.}
#'   \item{data}{The data refitted (`x`, `y`, numbers of replicates
#'   `n_x`, `n_y`), used by `plot(type = "joint")`.}
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
#' Sadler, W.A. (2010). Joint parameter confidence regions improve the power
#' of parametric regression in method-comparison studies. Accreditation and
#' Quality Assurance, 15, 547-554.
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
#' plot(prof, type = "joint")
#'
#' @export
error_ratio_profile <- function(object, ratios = NULL, conf.level = NULL,
                                joint_method = c("sadler", "wald")) {
  joint_method <- match.arg(joint_method)
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

  dat_p <- list(x = x, y = y, n_x = n_x, n_y = n_y)
  rows <- lapply(ratios, function(r) {
    f <- .erp_fit(dat_p, r)
    b <- f$coef
    se <- sqrt(diag(f$vcov))
    cb <- f$cbls_coef
    cse <- sqrt(diag(f$cbls_vcov))
    jt <- .erp_joint(dat_p, f, joint_method, c(0, 1))
    ex <- if (balanced) .creasy_ci(r * n_y[1] / n_x[1], sxx, syy, sxy, n,
                                   b[2], q_t) else c(NA_real_, NA_real_)
    data.frame(ratio = r, slope = b[2],
               slope_lower = b[2] - q_t * se[2],
               slope_upper = b[2] + q_t * se[2],
               slope_exact_lower = ex[1], slope_exact_upper = ex[2],
               intercept = b[1],
               intercept_lower = b[1] - q_t * se[1],
               intercept_upper = b[1] + q_t * se[1],
               joint_F = jt$F,
               joint_p = stats::pf(jt$F, 2, df_res, lower.tail = FALSE),
               md_slope = cb[2],
               md_slope_lower = cb[2] - q_t * cse[2],
               md_slope_upper = cb[2] + q_t * cse[2],
               md_intercept = cb[1],
               md_intercept_lower = cb[1] - q_t * cse[1],
               md_intercept_upper = cb[1] + q_t * cse[1],
               converged = f$converged)
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
  full_md <- data.frame(
    term = c("slope", "intercept"),
    lower = c(min(tab$md_slope_lower), min(tab$md_intercept_lower)),
    upper = c(max(tab$md_slope_upper), max(tab$md_intercept_upper))
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
    list(table = tab, full = full, full_md = full_md, identity = identity,
         fitted_ratio = fitted_ratio, conf.level = conf.level, n = n,
         balanced = balanced, joint_method = joint_method,
         x_name = x_name, y_name = y_name, data = dat_p,
         call = match.call()),
    class = "eiv_ratio_profile"
  )
}

# Error variances of the subject means in ratio r = var(x) / var(y) (scale
# free; the scale is estimated from the residuals)
.erp_vars <- function(dat, r) {
  if (is.infinite(r)) {
    list(vx = 1 / dat$n_x, vy = rep(0, length(dat$x)))
  } else {
    list(vx = r / (1 + r) / dat$n_x, vy = 1 / (1 + r) / dat$n_y)
  }
}

# BLS line of y on x and CBLS line of D = x - y on M = (x + y) / 2 (as in
# bls_reg()) at error ratio r, with their s^2 R^-1 covariances
.erp_fit <- function(dat, r) {
  x <- dat$x
  y <- dat$y
  v <- .erp_vars(dat, r)
  sxx <- sum((x - mean(x))^2)
  syy <- sum((y - mean(y))^2)
  sxy <- sum((x - mean(x)) * (y - mean(y)))
  # Deming start, lambda = var_y / var_x
  lam <- mean(v$vy) / mean(v$vx)
  b1 <- if (r == 0) sxy / sxx else if (is.infinite(r)) syy / sxy else
    (syy - lam * sxx + sqrt((syy - lam * sxx)^2 + 4 * lam * sxy^2)) /
    (2 * sxy)
  fit <- .bls_engine(u = x, v = y, var_u = v$vx, var_v = v$vy, cov_uv = 0,
                     start = c(mean(y) - b1 * mean(x), b1))
  a <- fit$coef[1]
  b <- fit$coef[2]
  cb <- .bls_engine(u = (x + y) / 2, v = x - y,
                    var_u = (v$vx + v$vy) / 4, var_v = v$vx + v$vy,
                    cov_uv = (v$vx - v$vy) / 2,
                    start = c(-2 * a / (1 + b), 2 * (1 - b) / (1 + b)))
  list(coef = fit$coef, vcov = fit$vcov, cbls_coef = cb$coef,
       cbls_vcov = cb$vcov, vx = v$vx, vy = v$vy,
       converged = fit$converged && cb$converged)
}

# Sadler's criterion with error ratio r: S(a, b) = sum(e_i^2 / (vy_i +
# b^2 vx_i)), e_i = y_i - a - b x_i, minimised by the BLS line. For one
# measurement per subject it is the weighted sum of squares of Deming
# regression (Sadler, 2010) used by joint_test(); with subject means it
# weights each subject by its numbers of replicates.
.erp_S <- function(dat, f, a, b) {
  sum((dat$y - a - b * dat$x)^2 / (f$vy + b^2 * f$vx))
}

# F statistic of the joint test of (intercept, slope) = null at one ratio:
# Sadler, ((S(null) - S_hat) / 2) / (S_hat / (n - 2)), or Wald,
# d' V^-1 d / 2 with V = s^2 R^-1; both on F(2, n - 2)
.erp_joint <- function(dat, f, method, null) {
  if (method == "wald") {
    d <- f$coef - null
    return(list(F = as.numeric(t(d) %*% solve(f$vcov, d)) / 2))
  }
  s_hat <- .erp_S(dat, f, f$coef[1], f$coef[2])
  s0 <- .erp_S(dat, f, null[1], null[2])
  list(F = (max(s0 - s_hat, 0) / 2) / (s_hat / (length(dat$x) - 2)))
}

# Joint confidence region of (intercept, slope) at one ratio, as a closed
# polygon (data frame with intercept, slope), or NULL if it is unbounded in
# the slope. Wald: the ellipse of joint_test(method = "wald"). Sadler: the
# set S(a, b) <= K, K = S_hat (1 + 2 F_crit / (n - 2)). For a given slope,
# S is quadratic in the intercept, with minimum S_min(b) at the weighted
# mean a_w(b), so the intercept limits are a_w(b) +/- sqrt((K - S_min(b)) /
# W(b)), W(b) = sum(1 / (vy_i + b^2 vx_i)). The slope limits, where
# S_min(b) = K, are bracketed on a grid of angles of the standardized line
# around the estimate and refined by uniroot(); the region is unbounded if
# S_min stays below K up to a vertical (or, with all the error in x, a
# horizontal) line.
.erp_region <- function(dat, f, method, conf.level, n_points = 100) {
  n <- length(dat$x)
  if (method == "wald") {
    return(.compute_joint_region(f$coef[1], f$coef[2], f$vcov,
                                 conf.level = conf.level, n_points = n_points,
                                 df_residual = n - 2))
  }
  x <- dat$x
  y <- dat$y
  K <- .erp_S(dat, f, f$coef[1], f$coef[2]) *
    (1 + 2 * stats::qf(conf.level, 2, n - 2) / (n - 2))
  w_at <- function(b) 1 / (f$vy + b^2 * f$vx)
  s_min <- function(b) {
    w <- w_at(b)
    if (any(!is.finite(w))) return(Inf)
    r <- y - b * x
    sum(w * (r - sum(w * r) / sum(w))^2)
  }
  g <- function(theta) K - s_min(sc * tan(theta))
  sc <- stats::sd(y) / stats::sd(x)
  th0 <- atan(f$coef[2] / sc)
  eps <- 1e-8
  grid_hi <- seq(th0, pi / 2 - eps, length.out = 200)
  grid_lo <- seq(th0, -pi / 2 + eps, length.out = 200)
  edge <- function(grid) {
    gv <- vapply(grid, g, numeric(1))
    k <- which(gv < 0)[1]
    if (is.na(k)) return(NA_real_)
    stats::uniroot(g, sort(grid[c(k - 1, k)]), tol = 1e-12)$root
  }
  th <- c(edge(grid_lo), edge(grid_hi))
  if (anyNA(th)) {
    return(NULL)
  }
  b_lim <- sc * tan(th)
  n_half <- max(3, ceiling(n_points / 2))
  b <- b_lim[1] + diff(b_lim) * (1 - cos(seq(0, pi, length.out = n_half))) / 2
  ctr <- half <- numeric(n_half)
  for (i in seq_len(n_half)) {
    w <- w_at(b[i])
    r <- y - b[i] * x
    ctr[i] <- sum(w * r) / sum(w)
    half[i] <- sqrt(max(K - s_min(b[i]), 0) / sum(w))
  }
  data.frame(intercept = c(ctr + half, rev(ctr - half), ctr[1] + half[1]),
             slope = c(b, rev(b), b[1]))
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
