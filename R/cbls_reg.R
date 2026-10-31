#' @title Bivariate Least Squares Regression
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Fits a straight line between two methods that are both measured with
#' error, using bivariate least squares (BLS) in the plot of one method
#' against the other and correlated bivariate least squares (CBLS) in the
#' Bland-Altman (mean-difference) plot (Francq & Govaerts, 2014, 2016). The
#' measurement error variances of the two methods are known, or estimated
#' from replicates, and may differ between the methods and between subjects.
#'
#' @param formula A formula of the form `y ~ x`.
#' @param data Data frame with all data: one row per pair of measurements, or
#'   per replicate when `id` is given (a measurement of one method may be
#'   missing).
#' @param id Column with the subject identifier, for replicate measurements.
#' @param var_x,var_y Measurement error variance of a single measurement by
#'   each method, if known (e.g., from a validation study). When missing,
#'   they are estimated from the replicates (pooled within-subject variance).
#' @param df_var_x,df_var_y Degrees of freedom of `var_x` and `var_y` when
#'   given (default `Inf`, treated as known). Used only for the prediction
#'   intervals.
#' @param error.ratio Ratio of the measurement error variances,
#'   var(x)/var(y), as in [dem_reg()]. Used only when neither variance is
#'   given or estimable (one measurement per subject); the estimates and
#'   confidence intervals are then available, but not the prediction
#'   intervals, which need the variances themselves.
#' @param conf.level Confidence level for the coefficients and the line.
#'   Default is 0.95.
#' @param pred_level Default level for prediction intervals. Default is 0.95.
#' @param model Logical. If TRUE (default), the subject-level data (means,
#'   numbers of replicates, and error variances) are stored in the returned
#'   object. They are needed by `plot()`, `check()`, `fitted()`,
#'   `residuals()`, and `predict()` without `newdata`.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#'
#' **Data.** Each subject `i` contributes the mean of its `n_xi` measurements
#' by `x` and its `n_yi` measurements by `y`. The variance of the measurement
#' error of these means is `var_x / n_xi` and `var_y / n_yi`, so subjects
#' with more replicates get more weight; with the same number of replicates
#' for every subject the variances are constant.
#'
#' **BLS.** The line `y = b0 + b1 x` minimises
#' `sum((y_i - b0 - b1 x_i)^2 / (var_yi + b1^2 var_xi))` and is found by the
#' iterative BLS algorithm (Lisy et al., 1990; Francq & Govaerts, 2014),
#' started from the Deming solution. With constant variances it is Deming
#' regression with `error.ratio = var_x / var_y`. The variance-covariance
#' matrix of `(b0, b1)` is `s^2 R^-1`, where `R` is the weighted cross-product
#' matrix of `(1, x_i)` with weights `1 / (var_yi + b1^2 var_xi)` and `s^2` the
#' weighted residual variance on `n - 2` df; confidence intervals use `t` on
#' `n - 2` df.
#'
#' **CBLS.** In the Bland-Altman plot, with `D = x - y` and `M = (x + y) / 2`
#' (the convention of [agreement_limit()]), the errors of `D` and `M` are
#' correlated, `cov = (var_x - var_y) / 2`. CBLS fits `D = g0 + g1 M` with the
#' residual variance `var_D + g1^2 var_M - 2 g1 cov`. It describes the same
#' line as BLS (Francq & Govaerts, 2016), with its own standard errors; the
#' bias is constant when `g1 = 0`.
#'
#' **Prediction intervals** (`predict(interval = "prediction")`) are for a
#' new measurement by `y` (or a new difference) given a new measurement by `x`
#' (or a new mean), each the mean of `reps_x`, `reps_y` replicates: the
#' variance is that of the line plus the measurement error variances, with
#' Satterthwaite degrees of freedom from `n - 2`, `df_var_x` and `df_var_y`.
#' For CBLS, the covariance between the errors of the new difference and the
#' new mean is included (BivRegBLS omits it; the two agree when
#' `var_x = var_y`).
#'
#' The joint test of intercept 0 and slope 1 ([joint_test()]) uses the Wald
#' statistic with the BLS variance-covariance matrix and the `F(2, n - 2)`
#' distribution. BivRegBLS multiplies the critical value by the Hotelling
#' factor `(n - 1) / (n - 2)`, which is slightly more conservative.
#'
#' @returns An object of class `c("simple_bls", "simple_eiv")`, with the
#'   elements of [dem_reg()] results (`coefficients`, `model_table`, `vcov`,
#'   `df.residual`, `call`, `terms`, `model`, `error.ratio`, `weights`,
#'   `conf.level`) plus:
#'
#'   - `cbls`: the CBLS fit in the Bland-Altman plot (`coefficients`,
#'     `model_table`, `vcov`);
#'   - `errors`: the measurement error variances of a single measurement, their
#'     df and source (`"known"`, `"replicates"`, or `"ratio"`);
#'   - `pred_level`, `method`, `iterations`, `converged`.
#'
#'   The stored data (`model`) have one row per subject: the means `x`, `y`,
#'   the numbers of replicates `n_x`, `n_y`, and the error variances of the
#'   means `var_x`, `var_y`.
#'
#' @examples
#' data("reps")
#' fit <- cbls_reg(y ~ x, data = reps, id = "id")
#' fit
#' summary(fit)
#' predict(fit, newdata = c(4, 6), interval = "prediction")
#' plot(fit, interval = "prediction")
#' plot(fit, type = "md", interval = "prediction")
#' joint_test(fit)
#'
#' # known error variances from a validation study, one measurement each
#' data("ba1986")
#' cbls_reg(mini1 ~ wright1, data = ba1986, var_x = 400, var_y = 600)
#'
#' @references
#' Francq, B. G., & Govaerts, B. B. (2014). Measurement methods comparison
#' with errors-in-variables regressions. From horizontal to vertical OLS
#' regression, review and new perspectives. Chemometrics and Intelligent
#' Laboratory Systems, 134, 123-139. \doi{10.1016/j.chemolab.2014.03.006}
#'
#' Francq, B. G., & Govaerts, B. (2016). How to regress and predict in a
#' Bland-Altman plot? Review and contribution based on tolerance intervals
#' and correlated-errors-in-variables models. Statistics in Medicine, 35(14),
#' 2328-2358. \doi{10.1002/sim.6872}
#'
#' Lisy, J. M., Cholvadova, A., & Kutej, J. (1990). Multiple straight-line
#' least-squares analysis with uncertainties in all variables. Computers &
#' Chemistry, 14(3), 189-192.
#'
#' @seealso [dem_reg()], [pb_reg()], [joint_test()], [agreement_limit()]
#' @export

cbls_reg <- function(formula,
                     data,
                     id = NULL,
                     var_x = NULL,
                     var_y = NULL,
                     df_var_x = Inf,
                     df_var_y = Inf,
                     error.ratio = NULL,
                     conf.level = 0.95,
                     pred_level = 0.95,
                     model = TRUE,
                     ...) {
  call2 <- match.call()
  for (lv in c("conf.level", "pred_level")) {
    v <- get(lv)
    if (!is.numeric(v) || length(v) != 1 || v <= 0 || v >= 1) {
      stop(lv, " must be a single number between 0 and 1.")
    }
  }
  for (nm in c("var_x", "var_y", "error.ratio")) {
    v <- get(nm)
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1 || !(v > 0))) {
      stop(nm, " must be a single positive number.")
    }
  }
  for (nm in c("df_var_x", "df_var_y")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1 || !(v > 0)) {
      stop(nm, " must be a single positive number (or Inf).")
    }
  }
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula of the form y ~ x.")
  }
  call2$conf.level <- conf.level
  call2$pred_level <- pred_level
  call2$id <- id

  fml <- if (!is.null(id)) update(formula, ~ . + id) else formula
  environment(fml) <- environment(formula)
  mf <- model.frame(fml, data = data, na.action = na.pass)
  mt <- attr(mf, "terms")
  x_name <- names(mf)[2]
  raw <- data.frame(x = mf[[2]], y = model.response(mf, "numeric"))
  raw$id <- if (!is.null(id)) mf[[3]] else seq_len(nrow(raw))
  raw <- raw[!is.na(raw$id) & !(is.na(raw$x) & is.na(raw$y)), ]

  # subject means, numbers of replicates, within-subject sums of squares
  agg <- function(v, f) {
    tapply(v, raw$id, function(z) f(z[!is.na(z)]))
  }
  sub <- data.frame(id = names(agg(raw$x, length)),
                    x = as.vector(agg(raw$x, mean)),
                    y = as.vector(agg(raw$y, mean)),
                    n_x = as.vector(agg(raw$x, length)),
                    n_y = as.vector(agg(raw$y, length)),
                    ss_x = as.vector(agg(raw$x, function(z) sum((z - mean(z))^2))),
                    ss_y = as.vector(agg(raw$y, function(z) sum((z - mean(z))^2))),
                    stringsAsFactors = FALSE)
  sub <- sub[sub$n_x > 0 & sub$n_y > 0, ]
  n <- nrow(sub)
  if (n < 3) {
    stop("At least 3 subjects with measurements by both methods are needed.")
  }

  # measurement error variances of a single measurement
  pooled <- function(ss, n_i) {
    d <- sum(n_i - 1)
    if (d > 0) c(var = sum(ss) / d, df = d) else c(var = NA_real_, df = NA_real_)
  }
  px <- pooled(sub$ss_x, sub$n_x)
  py <- pooled(sub$ss_y, sub$n_y)
  vx1 <- if (!is.null(var_x)) var_x else px[["var"]]
  vy1 <- if (!is.null(var_y)) var_y else py[["var"]]
  dfx <- if (!is.null(var_x)) df_var_x else px[["df"]]
  dfy <- if (!is.null(var_y)) df_var_y else py[["df"]]
  if (!is.null(var_x) && isTRUE(px[["df"]] > 0)) {
    message("var_x is given; the replicates of x are not used to estimate it.")
  }
  if (!is.null(var_y) && isTRUE(py[["df"]] > 0)) {
    message("var_y is given; the replicates of y are not used to estimate it.")
  }
  known <- is.finite(vx1) && is.finite(vy1)
  if (known) {
    source <- if (!is.null(var_x) && !is.null(var_y)) "known" else
      if (is.null(var_x) && is.null(var_y)) "replicates" else "known/replicates"
    if (!is.null(error.ratio)) {
      message("error.ratio is ignored: the error variances are known or ",
              "estimated from the replicates.")
    }
  } else if (!is.null(error.ratio)) {
    # only the ratio is known: fix the scale (the estimates and their
    # standard errors do not depend on it)
    if (is.finite(vx1)) {
      vy1 <- vx1 / error.ratio
    } else if (is.finite(vy1)) {
      vx1 <- vy1 * error.ratio
    } else {
      vx1 <- error.ratio
      vy1 <- 1
    }
    dfx <- dfy <- NA_real_
    source <- "ratio"
  } else {
    stop("The measurement error variances are not known and cannot be ",
         "estimated (one measurement per subject). Give var_x and var_y, ",
         "replicate measurements with id, or error.ratio = var(x) / var(y).")
  }
  if (!(vx1 > 0) || !(vy1 > 0)) {
    stop("The measurement error variances must be positive.")
  }
  sub$var_x <- vx1 / sub$n_x
  sub$var_y <- vy1 / sub$n_y

  # BLS in the (x, y) plot, started from Deming with lambda = var_y / var_x
  lam <- mean(sub$var_y) / mean(sub$var_x)
  sxx <- sum((sub$x - mean(sub$x))^2)
  syy <- sum((sub$y - mean(sub$y))^2)
  sxy <- sum((sub$x - mean(sub$x)) * (sub$y - mean(sub$y)))
  b1 <- (syy - lam * sxx + sqrt((syy - lam * sxx)^2 + 4 * lam * sxy^2)) /
    (2 * sxy)
  start <- c(mean(sub$y) - b1 * mean(sub$x), b1)
  bls <- .bls_engine(u = sub$x, v = sub$y, var_u = sub$var_x,
                     var_v = sub$var_y, cov_uv = 0, start = start)

  # CBLS in the Bland-Altman plot (D = x - y, M = (x + y) / 2), started from
  # the BLS line: y = a + b x is D = -2a / (1 + b) + 2 (1 - b) / (1 + b) M
  a <- bls$coef[1]
  b <- bls$coef[2]
  m_i <- (sub$x + sub$y) / 2
  d_i <- sub$x - sub$y
  cbls <- .bls_engine(u = m_i, v = d_i,
                      var_u = (sub$var_x + sub$var_y) / 4,
                      var_v = sub$var_x + sub$var_y,
                      cov_uv = (sub$var_x - sub$var_y) / 2,
                      start = c(-2 * a / (1 + b), 2 * (1 - b) / (1 + b)))

  df_res <- n - 2
  coefs <- stats::setNames(bls$coef, c("(Intercept)", x_name))
  vc <- bls$vcov
  dimnames(vc) <- list(names(coefs), names(coefs))
  cb_coefs <- stats::setNames(cbls$coef, c("(Intercept)", "avg"))
  cb_vc <- cbls$vcov
  dimnames(cb_vc) <- list(names(cb_coefs), names(cb_coefs))

  tab <- function(est, v, null) {
    se <- sqrt(diag(v))
    q <- stats::qt(1 - (1 - conf.level) / 2, df_res)
    t <- (est - null) / se
    data.frame(coef = unname(est), se = unname(se), df = df_res,
               lower.ci = unname(est - q * se), upper.ci = unname(est + q * se),
               null = null, t = unname(t),
               p.value = unname(2 * stats::pt(abs(t), df_res, lower.tail = FALSE)),
               row.names = names(est))
  }

  if (!bls$converged || !cbls$converged) {
    warning("The BLS iterations did not converge; the estimates may be ",
            "inaccurate.")
  }

  sub_out <- sub[, c("id", "x", "y", "n_x", "n_y", "var_x", "var_y")]
  structure(
    list(
      coefficients = coefs,
      model_table = tab(coefs, vc, c(0, 1)),
      vcov = vc,
      df.residual = df_res,
      call = call2,
      terms = mt,
      model = if (model) sub_out else NULL,
      error.ratio = vx1 / vy1,
      weights = bls$weights,
      conf.level = conf.level,
      pred_level = pred_level,
      method = "Bivariate least squares (BLS) regression",
      cbls = list(coefficients = cb_coefs,
                  model_table = tab(cb_coefs, cb_vc, c(0, 0)),
                  vcov = cb_vc),
      errors = list(var_x = vx1, var_y = vy1, df_var_x = dfx, df_var_y = dfy,
                    source = source, known = source != "ratio"),
      iterations = c(bls = bls$iterations, cbls = cbls$iterations),
      converged = bls$converged && cbls$converged
    ),
    class = c("simple_bls", "simple_eiv")
  )
}

# Iterative (correlated) bivariate least squares for v = b0 + b1 u, where
# the errors of u and v have variances var_u, var_v and covariance cov_uv for
# each point (vectors, or scalars recycled). With cov_uv = 0 this is BLS
# (Lisy et al., 1990); with correlated errors, CBLS (Francq & Govaerts,
# 2016). Each step solves R b = g, with weights w_i = 1 / s2_i,
# s2_i = var_v + b1^2 var_u - 2 b1 cov_uv,
# R = sum w_i (1, u_i)'(1, u_i), g = sum w_i v_i (1, u_i)' plus the gradient
# correction sum (e_i w_i)^2 (b1 var_u - cov_uv) for the slope. The
# variance-covariance matrix is s^2 R^-1 with s^2 = sum w_i e_i^2 / (n - 2).
.bls_engine <- function(u, v, var_u, var_v, cov_uv, start,
                        tol = 1e-12, max_iter = 500) {
  n <- length(u)
  b <- unname(start)
  converged <- FALSE
  step_sys <- function(b) {
    e <- v - b[1] - b[2] * u
    s2 <- var_v + b[2]^2 * var_u - 2 * b[2] * cov_uv
    w <- 1 / s2
    R <- matrix(c(sum(w), sum(w * u), sum(w * u), sum(w * u^2)), 2, 2)
    g <- c(sum(w * v), sum(w * u * v + (e * w)^2 * (b[2] * var_u - cov_uv)))
    list(R = R, g = g, w = w)
  }
  for (it in seq_len(max_iter)) {
    sy <- step_sys(b)
    b_new <- as.vector(solve(sy$R, sy$g))
    if (max(abs(b_new - b) / pmax(abs(b), 1)) < tol) {
      b <- b_new
      converged <- TRUE
      break
    }
    b <- b_new
  }
  sy <- step_sys(b)
  e <- v - b[1] - b[2] * u
  s2 <- sum(sy$w * e^2) / (n - 2)
  list(coef = b, vcov = solve(sy$R) * s2, weights = sy$w, s2 = s2,
       iterations = it, converged = converged)
}
