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
#'   each method, if known (e.g., from a validation study): a number, or a
#'   function of the level that returns the variance, e.g.
#'   `function(level) (0.05 * level)^2` for a coefficient of variation of 5%,
#'   or the name of a column of `data` with each subject's own variance
#'   (the same on all rows of a subject; e.g., from that subject's
#'   replicates in a separate study). When missing, they are estimated from
#'   the replicates (see `var_model`).
#' @param df_var_x,df_var_y Degrees of freedom of `var_x` and `var_y` when
#'   given (default `Inf`, treated as known): a number, or, with a column of
#'   variances, the name of a column with each subject's df. Used only for
#'   the prediction intervals and the dispersion test.
#' @param error.ratio Ratio of the measurement error variances,
#'   var(x)/var(y), as in [dem_reg()]. Used only when neither variance is
#'   given or estimable (one measurement per subject); the estimates and
#'   confidence intervals are then available, but not the prediction
#'   intervals, which need the variances themselves.
#' @param var_model How the error variances are estimated from the
#'   replicates (for a method whose variance is not given): `"constant"`
#'   (default; the pooled within-subject variance), `"proportional"` (the
#'   error SD is proportional to the level, a constant coefficient of
#'   variation), or `"power"` (variance `sigma^2 * level^(2 * theta)`, with
#'   `theta` estimated). The last two need positive measurements. See
#'   Details.
#' @param equation_error Logical. If TRUE (default), equation error is
#'   modeled: deviations of the subjects' true values from the line (e.g.,
#'   method-by-item or matrix effects), estimated from the excess scatter
#'   about the line, are included in the fit and the intervals. It is 0, and
#'   the results are those of BivRegBLS, when the measurement errors explain
#'   all the scatter. FALSE always gives BivRegBLS's results. See Details.
#' @param transform `"none"` (default) or `"log"`. With `"log"`, the
#'   line is fitted to the logarithms of the measurements (which must be
#'   positive): `log(y) = b0 + b1 log(x)`, i.e. `y = exp(b0) x^b1`, and in
#'   the Bland-Altman plot the log ratio `log(x / y)` against the mean of the
#'   logs. Use it when the error SD is proportional to the level. The error
#'   variances (given or estimated) are then on the log scale (a coefficient
#'   of variation `cv` is a variance of about `log(1 + cv^2)`), and must be
#'   constant. [predict()] and [plot()] back-transform to the original
#'   scale (fitted values and limits for `y`, or ratios `x / y`).
#' @param conf.level Confidence level for the coefficients and the line.
#'   Default is 0.95.
#' @param pred_level Default level for prediction intervals, and the
#'   proportion of new measurements that tolerance limits contain. Default is
#'   0.95.
#' @param tol_level Default confidence level of the tolerance limits
#'   (experimental; `predict(interval = "tolerance")`). Default is 0.95.
#' @param bound_type Default type of tolerance limits, as in
#'   [tolerance_limit()]: `"joint"` (default), limits that together contain a
#'   proportion `pred_level` of new measurements with confidence `tol_level`;
#'   or `"iu"`, a `tol_level` confidence bound on each of the `pred_level`
#'   prediction limits.
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
#' **Error variances that change with the level.** When `var_x` or `var_y` is
#' a function, or `var_model` is `"proportional"` or `"power"`, the error
#' variance of each subject's measurements is evaluated at its estimated
#' true values (the points on the line nearest the data, given the error
#' variances), not at the observed means, whose own errors would bias the
#' weights. The fit starts with the variances at the observed means and is
#' repeated, re-evaluating them at the new estimated true values, until the
#' line is stable. With replicates, `"proportional"` estimates the
#' coefficient of variation from the within-subject variances relative to
#' the squared subject means (`sum(n_i - 1)` df); `"power"` regresses the log
#' within-subject variances (corrected for the bias of the log of a variance
#' estimate) on the log subject means, weighted by their df
#' (`sum(n_i - 1) - 1` df). For prediction intervals, the variances of the new
#' measurements are evaluated at the new value of `x` and the fitted `y` (or,
#' in the Bland-Altman plot, at the mean plus and minus half the fitted
#' difference); the uncertainty in an estimated `theta` is not included.
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
#' **Tolerance limits (experimental).** `predict(interval = "tolerance")`
#' gives tolerance limits around the line: with confidence `tol_level`, they
#' contain at least a proportion `pred_level` of new measurements by `y` at
#' the given `x` (`bound_type = "joint"`), or bound each prediction limit
#' (`"iu"`). New measurements are normal about the true line with the spread
#' `s^2` (their measurement error variances and any equation error), and the
#' fitted line is off by an unknown amount with its standard error. Without
#' equation error, `s^2` gets an upper confidence bound from the df of the
#' error variances (MOVER, as in [tolerance_limit()]; exact when they are
#' known) and the line a `t` bound on `n - 2` df; the joint limits are then
#' the `pred_level` quantile of the absolute value of a normal variable with
#' these bounds as mean and SD. With equation error, the residuals estimate
#' the spread of the subject means about the line on `n - 2` df, and that
#' part of `s^2` and the variance of the line share a chi-square bound;
#' with one measurement per subject the joint limits are then the textbook
#' regression tolerance limits (Wallis's approximation). The `"iu"` limits
#' bound each prediction limit with the MOVER. Francq and Govaerts (2016)
#' give tolerance intervals only for a constant bias (see
#' [tolerance_limit()]); these limits around a line are SimplyAgree's own
#' extension and are marked experimental. In simulations (95% content and
#' confidence; 10 to 60 subjects), their confidence was 0.93 to 0.97 with
#' known error variances or with equation error and replicates, conservative
#' (up to 0.99) with replicates and no equation error, and 0.875 with 10
#' subjects, known variances, and equation error (study 4 in
#' `references/general/cbls_sims/` of the package source).
#'
#' **Dispersion and equation error.** BLS assumes that the measurement errors
#' explain all the scatter about the line, i.e., that the true values lie on
#' it. Then the weighted residuals `e_i^2 / (var_yi + b1^2 var_xi)` average 1;
#' their mean, the dispersion `s^2`, is reported by `summary()` with a test of
#' `s^2 = 1` (chi-square when the error variances are known, F against their
#' df when estimated). The confidence intervals of the coefficients are scaled
#' by `s^2` in any case. Item-specific deviations of the true values from the
#' line (equation error, e.g. method-by-item or matrix effects) make
#' `s^2 > 1`. They act like extra error variance that the replicates cannot
#' estimate, so ignoring them gives prediction intervals that are too narrow
#' and, through a wrong error ratio, a biased slope. With
#' `equation_error = TRUE`, each subject's true value is taken to deviate by
#' `c_xi` for `x` and `b1 c_yi` for `y`, with `c ~ N(0, tau^2)` for both
#' methods (as `method_vc(tau = "common")`), so the error variances become
#' `var_xi + tau^2` and `var_yi + b1^2 tau^2`. `tau^2` solves
#' `sum(e_i^2 / (var_yi + b1^2 var_xi + 2 b1^2 tau^2)) = n - 2` (0 if the
#' dispersion is at most 1), and the fit is repeated until stable; the
#' prediction intervals include `tau^2` (on `n - 2` df). Equal deviations for
#' both methods is an assumption: with two methods, how the equation error
#' divides between them cannot be estimated.
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
#'   - `errors`: the measurement error variances of a single measurement
#'     (`var_x`, `var_y`, at the mean level), the functions of the level
#'     (`fun_x`, `fun_y`), the models (`model_x`, `model_y`) and their
#'     parameters, their df, and their source (`"known"`, `"replicates"`, or
#'     `"ratio"`);
#'   - `pred_level`, `method`, `iterations`, `converged`.
#'
#'   The stored data (`model`) have one row per subject: the means `x`, `y`,
#'   the numbers of replicates `n_x`, `n_y`, and the error variances of the
#'   means `var_x`, `var_y`.
#'
#' @examples
#' data("reps")
#' fit <- bls_reg(y ~ x, data = reps, id = "id")
#' fit
#' summary(fit)
#' predict(fit, newdata = c(4, 6), interval = "prediction")
#' plot(fit, interval = "prediction")
#' plot(fit, type = "md", interval = "prediction")
#' joint_test(fit)
#'
#' # known error variances from a validation study, one measurement each
#' data("ba1986")
#' bls_reg(mini1 ~ wright1, data = ba1986, var_x = 400, var_y = 600)
#'
#' # error SD proportional to the level (constant CV), known or estimated
#' bls_reg(mini1 ~ wright1, data = ba1986,
#'          var_x = function(level) (0.05 * level)^2,
#'          var_y = function(level) (0.07 * level)^2)
#' bls_reg(y ~ x, data = reps, id = "id", var_model = "proportional")
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

bls_reg <- function(formula,
                     data,
                     id = NULL,
                     var_x = NULL,
                     var_y = NULL,
                     df_var_x = Inf,
                     df_var_y = Inf,
                     error.ratio = NULL,
                     var_model = c("constant", "proportional", "power"),
                     equation_error = TRUE,
                     transform = c("none", "log"),
                     conf.level = 0.95,
                     pred_level = 0.95,
                     tol_level = 0.95,
                     bound_type = c("joint", "iu"),
                     model = TRUE,
                     ...) {
  call2 <- match.call()
  var_model <- match.arg(var_model)
  bound_type <- match.arg(bound_type)
  transform <- match.arg(transform)
  if (transform == "log" &&
      (is.function(var_x) || is.function(var_y) || var_model != "constant")) {
    stop("With transform = 'log', the error variances must be constant on ",
         "the log scale (var_model = 'constant', no variance functions).")
  }
  if (!is.logical(equation_error) || length(equation_error) != 1 ||
      is.na(equation_error)) {
    stop("equation_error must be TRUE or FALSE.")
  }
  for (lv in c("conf.level", "pred_level", "tol_level")) {
    v <- get(lv)
    if (!is.numeric(v) || length(v) != 1 || v <= 0 || v >= 1) {
      stop(lv, " must be a single number between 0 and 1.")
    }
  }
  for (nm in c("var_x", "var_y")) {
    v <- get(nm)
    if (is.character(v)) {
      if (length(v) != 1 || !(v %in% names(data))) {
        stop(nm, " must be a single positive number, a function of the ",
             "level, or the name of a column of data.")
      }
    } else if (!is.null(v) && !is.function(v) &&
               (!is.numeric(v) || length(v) != 1 || !(v > 0))) {
      stop(nm, " must be a single positive number, a function of the ",
           "level, or the name of a column of data.")
    }
  }
  if (!is.null(error.ratio) &&
      (!is.numeric(error.ratio) || length(error.ratio) != 1 ||
       !(error.ratio > 0))) {
    stop("error.ratio must be a single positive number.")
  }
  for (nm in c("df_var_x", "df_var_y")) {
    v <- get(nm)
    if (is.character(v)) {
      if (length(v) != 1 || !(v %in% names(data)) ||
          !is.character(get(sub("df_", "", nm)))) {
        stop(nm, " can be a column name only when ", sub("df_", "", nm),
             " is a column of data.")
      }
    } else if (!is.numeric(v) || length(v) != 1 || !(v > 0)) {
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
  # per-row error variances (and df) given as columns of data
  for (nm in c("var_x", "var_y", "df_var_x", "df_var_y")) {
    v <- get(nm)
    raw[[nm]] <- if (is.character(v)) data[[v]] else NA_real_
  }
  raw <- raw[!is.na(raw$id) & !(is.na(raw$x) & is.na(raw$y)), ]
  if (transform == "log") {
    if (any(raw$x <= 0, na.rm = TRUE) || any(raw$y <= 0, na.rm = TRUE)) {
      stop("transform = 'log' needs positive measurements.")
    }
    raw$x <- log(raw$x)
    raw$y <- log(raw$y)
  }

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
  for (nm in c("var_x", "var_y", "df_var_x", "df_var_y")) {
    if (is.character(get(nm))) {
      sub[[paste0("given_", nm)]] <- as.vector(agg(raw[[nm]], function(z) {
        if (length(z) && diff(range(z)) > 1e-8 * max(abs(z))) {
          stop("The column ", get(nm), " must have one value per subject.")
        }
        if (length(z)) z[1] else NA_real_
      }))
    }
  }
  sub <- sub[sub$n_x > 0 & sub$n_y > 0, ]
  n <- nrow(sub)
  if (n < 3) {
    stop("At least 3 subjects with measurements by both methods are needed.")
  }

  # measurement error variance of a single measurement, as a function of
  # the level: given (number or function) or estimated from the replicates
  given_col <- function(nm) {
    if (!is.character(get(nm))) return(get(nm))
    v <- sub[[paste0("given_", nm)]]
    ok <- if (grepl("^df_", nm)) all(v > 0) else all(is.finite(v) & v > 0)
    if (anyNA(v) || !ok) {
      stop("The column ", get(nm), " must have a positive value for ",
           "every subject.")
    }
    v
  }
  ex <- .bls_error_model(given_col("var_x"), given_col("df_var_x"),
                         sub$ss_x, sub$n_x, sub$x, var_model, "x")
  ey <- .bls_error_model(given_col("var_y"), given_col("df_var_y"),
                         sub$ss_y, sub$n_y, sub$y, var_model, "y")
  if (!is.null(ex) && !is.null(ey)) {
    sources <- c(ex$source, ey$source)
    source <- if (all(sources == "known")) "known" else
      if (all(sources == "replicates")) "replicates" else "known/replicates"
    if (!is.null(error.ratio)) {
      message("error.ratio is ignored: the error variances are known or ",
              "estimated from the replicates.")
    }
  } else if (!is.null(error.ratio)) {
    # only the ratio is known: fix the scale (the estimates and their
    # standard errors do not depend on it)
    if (is.function(var_x) || is.function(var_y) || is.character(var_x) ||
        is.character(var_y) || var_model != "constant") {
      stop("With error.ratio, the error variances must be constant.")
    }
    const <- function(v) .bls_const_model(v, NA_real_, "ratio")
    if (!is.null(ex)) {
      ey <- const(ex$fun(1) / error.ratio)
    } else if (!is.null(ey)) {
      ex <- const(ey$fun(1) * error.ratio)
    } else {
      ex <- const(error.ratio)
      ey <- const(1)
    }
    ex$df <- ey$df <- NA_real_
    source <- "ratio"
  } else {
    stop("The measurement error variances are not known and cannot be ",
         "estimated (one measurement per subject). Give var_x and var_y, ",
         "replicate measurements with id, or error.ratio = var(x) / var(y).")
  }
  # subject variances do not depend on the level: no iteration needed
  constant <- ex$model %in% c("constant", "subject") &&
    ey$model %in% c("constant", "subject")

  # BLS in the (x, y) plot. The variances are evaluated at the estimated true
  # values of each subject, starting from the subject means, and the fit is
  # repeated until the line is stable (one pass with constant variances).
  lev_x <- sub$x
  lev_y <- sub$y
  coef_old <- c(Inf, Inf)
  for (pass in seq_len(100)) {
    sub$var_x <- (if (!is.null(ex$values)) ex$values else
      .bls_eval(ex$fun, lev_x, "var_x")) / sub$n_x
    sub$var_y <- (if (!is.null(ey$values)) ey$values else
      .bls_eval(ey$fun, lev_y, "var_y")) / sub$n_y
    # started from Deming with lambda = var_y / var_x
    lam <- mean(sub$var_y) / mean(sub$var_x)
    sxx <- sum((sub$x - mean(sub$x))^2)
    syy <- sum((sub$y - mean(sub$y))^2)
    sxy <- sum((sub$x - mean(sub$x)) * (sub$y - mean(sub$y)))
    b1 <- (syy - lam * sxx + sqrt((syy - lam * sxx)^2 + 4 * lam * sxy^2)) /
      (2 * sxy)
    start <- c(mean(sub$y) - b1 * mean(sub$x), b1)
    bls <- .bls_engine(u = sub$x, v = sub$y, var_u = sub$var_x,
                       var_v = sub$var_y, cov_uv = 0, start = start)
    if (constant ||
        max(abs(bls$coef - coef_old) / pmax(abs(bls$coef), 1)) < 1e-10) {
      break
    }
    coef_old <- bls$coef
    e <- sub$y - bls$coef[1] - bls$coef[2] * sub$x
    s2 <- sub$var_y + bls$coef[2]^2 * sub$var_x
    lev_x <- sub$x + bls$coef[2] * sub$var_x * e / s2
    lev_y <- bls$coef[1] + bls$coef[2] * lev_x
  }
  passes_ok <- constant || pass < 100

  # dispersion of the measurement-error-only fit (the test of no equation
  # error), then, with equation_error, the common-tau equation error: each
  # subject's true value deviates from the line by c_xi for x and b c_yi for
  # y, c ~ N(0, tau2), as for the method-by-item effects of method_vc(tau =
  # "common"). The effective error variances are var_x + tau2 and
  # var_y + b^2 tau2; tau2 solves sum(e_i^2 / (s_i + 2 b^2 tau2)) = n - 2
  # (0 if the dispersion is at most 1), and the fit is repeated until stable.
  e_i <- sub$y - bls$coef[1] - bls$coef[2] * sub$x
  disp <- .bls_dispersion(e_i, sub$var_y + bls$coef[2]^2 * sub$var_x,
                          bls$coef[2], sub, ex$df, ey$df,
                          known = source != "ratio")
  tau2 <- 0
  tau_ok <- TRUE
  if (equation_error && isTRUE(disp$s2 > 1) && source != "ratio") {
    tau_ok <- FALSE
    for (k in seq_len(200)) {
      b <- bls$coef[2]
      e_i <- sub$y - bls$coef[1] - b * sub$x
      s_i <- sub$var_y + b^2 * sub$var_x
      tau2_new <- .bls_tau2(e_i, s_i, b)
      bls <- .bls_engine(u = sub$x, v = sub$y,
                         var_u = sub$var_x + tau2_new,
                         var_v = sub$var_y + b^2 * tau2_new,
                         cov_uv = 0, start = bls$coef)
      if (abs(tau2_new - tau2) <= 1e-10 * max(tau2_new, 1e-300) &&
          abs(bls$coef[2] - b) <= 1e-10 * max(abs(b), 1)) {
        tau2 <- tau2_new
        tau_ok <- TRUE
        break
      }
      tau2 <- tau2_new
    }
  }
  disp$tau2 <- tau2
  # effective error variances of the subject means
  vx_eff <- sub$var_x + tau2
  vy_eff <- sub$var_y + bls$coef[2]^2 * tau2

  # CBLS in the Bland-Altman plot (D = x - y, M = (x + y) / 2), started from
  # the BLS line: y = a + b x is D = -2a / (1 + b) + 2 (1 - b) / (1 + b) M
  a <- bls$coef[1]
  b <- bls$coef[2]
  m_i <- (sub$x + sub$y) / 2
  d_i <- sub$x - sub$y
  cbls <- .bls_engine(u = m_i, v = d_i,
                      var_u = (vx_eff + vy_eff) / 4,
                      var_v = vx_eff + vy_eff,
                      cov_uv = (vx_eff - vy_eff) / 2,
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

  converged <- bls$converged && cbls$converged && passes_ok && tau_ok
  if (!converged) {
    warning("The BLS iterations did not converge; the estimates may be ",
            "inaccurate.")
  }

  sub_out <- sub[, c("id", "x", "y", "n_x", "n_y", "var_x", "var_y")]
  # the error variances at the mean level, for the printed ratio
  mid_x <- mean(lev_x)
  mid_y <- mean(lev_y)
  vx1 <- ex$fun(mid_x)
  vy1 <- ey$fun(mid_y)
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
      tol_level = tol_level,
      bound_type = bound_type,
      transform = transform,
      method = "Bivariate least squares (BLS) regression",
      cbls = list(coefficients = cb_coefs,
                  model_table = tab(cb_coefs, cb_vc, c(0, 0)),
                  vcov = cb_vc),
      errors = list(var_x = vx1, var_y = vy1,
                    fun_x = ex$fun, fun_y = ey$fun,
                    model_x = ex$model, model_y = ey$model,
                    params_x = ex$params, params_y = ey$params,
                    df_var_x = ex$df, df_var_y = ey$df,
                    level_x = mid_x, level_y = mid_y,
                    source = source, known = source != "ratio"),
      dispersion = disp,
      equation_error = equation_error,
      iterations = c(passes = pass, bls = bls$iterations,
                     cbls = cbls$iterations),
      converged = converged
    ),
    class = c("simple_bls", "simple_eiv")
  )
}

# Dispersion. Under the model (true values on the line), e_i^2 / s_i,
# s_i = var_yi + b1^2 var_xi, average 1 and their sum is chi-square on n - 2
# df (F against the df of estimated error variances, combined by
# Satterthwaite).
.bls_dispersion <- function(e, s, b1, sub, df_x, df_y, known) {
  n <- length(e)
  q <- sum(e^2 / s)
  s2 <- q / (n - 2)
  if (!known) {
    return(list(s2 = s2, statistic = NA_real_, df1 = n - 2, df2 = NA_real_,
                p.value = NA_real_))
  }
  # df of the measurement error part of s_i
  a_x <- mean(b1^2 * sub$var_x / s)
  a_y <- mean(sub$var_y / s)
  df2 <- (a_x + a_y)^2 / (a_x^2 / df_x + a_y^2 / df_y)
  p <- if (is.finite(df2)) {
    stats::pf(s2, n - 2, df2, lower.tail = FALSE)
  } else {
    stats::pchisq(q, n - 2, lower.tail = FALSE)
  }
  list(s2 = s2, statistic = s2, df1 = n - 2, df2 = df2, p.value = p)
}

# Common-tau equation error: the residual variance of subject i is
# s_i + 2 b1^2 tau2; tau2 solves sum(e_i^2 / (s_i + 2 b1^2 tau2)) = n - 2 (a
# Paule-Mandel type moment estimator), 0 if the dispersion is at most 1.
.bls_tau2 <- function(e, s, b1) {
  n <- length(e)
  k <- 2 * b1^2
  f <- function(v) sum(e^2 / (s + k * v)) - (n - 2)
  if (f(0) <= 0 || k == 0) {
    return(0)
  }
  hi <- (max(s) + max(e^2)) / k
  while (f(hi) > 0) hi <- 2 * hi
  stats::uniroot(f, c(0, hi), tol = 1e-12 * hi)$root
}

# A constant error model
.bls_const_model <- function(v, df, source) {
  list(fun = function(level) rep(v, length(level)), df = df,
       model = "constant", params = c(var = v), source = source)
}

# The error variance of a single measurement by one method, as a function
# of the level:
# - given as a number (constant) or a function of the level, with df `df_given`
# - else estimated from the replicates (subjects with n_i > 1):
#   "constant": pooled within-subject variance, sum(n_i - 1) df;
#   "proportional": var = cv^2 level^2, cv^2 = sum(ss_i / level_i^2) /
#     sum(n_i - 1), at the subject means, sum(n_i - 1) df;
#   "power": var = sigma^2 level^(2 theta), from the df-weighted regression
#     of log(s_i^2) - (digamma(d_i / 2) - log(d_i / 2)) (unbiased for
#     log(var)) on log(level_i), sum(n_i - 1) - 1 df.
# Returns NULL if the variance is neither given nor estimable.
.bls_error_model <- function(given, df_given, ss, n_i, level, var_model, nm) {
  if (!is.null(given) && any(n_i > 1)) {
    message("var_", nm, " is given; the replicates of ", nm,
            " are not used to estimate it.")
  }
  if (is.function(given)) {
    return(list(fun = given, df = df_given, model = "function",
                params = NULL, source = "known"))
  }
  if (length(given) > 1) {
    # one variance per subject. A new measurement (prediction) gets their
    # mean, with the Satterthwaite df of the mean of independent estimates.
    df_i <- rep_len(df_given, length(given))
    m <- mean(given)
    df_m <- if (all(is.infinite(df_i))) Inf else
      sum(given)^2 / sum(given^2 / df_i)
    return(list(fun = function(level) rep(m, length(level)), values = given,
                df = df_m, model = "subject", params = c(mean = m),
                source = "known"))
  }
  if (!is.null(given)) {
    return(.bls_const_model(given, df_given, "known"))
  }
  ok <- n_i > 1
  d <- n_i[ok] - 1
  if (sum(d) == 0) {
    return(NULL)
  }
  if (var_model == "constant") {
    return(.bls_const_model(sum(ss[ok]) / sum(d), sum(d), "replicates"))
  }
  lev <- level[ok]
  if (any(lev <= 0)) {
    stop("var_model = '", var_model, "' needs positive measurements by ", nm,
         " (the error SD is modeled as a function of the level).")
  }
  if (var_model == "proportional") {
    cv2 <- sum(ss[ok] / lev^2) / sum(d)
    return(list(fun = function(level) cv2 * level^2, df = sum(d),
                model = "proportional", params = c(cv = sqrt(cv2)),
                source = "replicates"))
  }
  s2 <- ss[ok] / d
  keep <- s2 > 0
  if (sum(keep) < 3) {
    stop("var_model = 'power' needs at least 3 subjects with replicates of ",
         nm, " that are not all equal.")
  }
  z <- log(s2[keep]) - (digamma(d[keep] / 2) - log(d[keep] / 2))
  cf <- stats::coef(stats::lm(z ~ log(lev[keep]), weights = d[keep]))
  sigma2 <- exp(unname(cf[1]))
  theta <- unname(cf[2]) / 2
  list(fun = function(level) sigma2 * level^(2 * theta), df = sum(d) - 1,
       model = "power", params = c(sigma = sqrt(sigma2), theta = theta),
       source = "replicates")
}

# Evaluate an error variance function at the given levels
.bls_eval <- function(fun, level, nm) {
  v <- fun(level)
  if (length(v) == 1 && length(level) > 1) v <- rep(v, length(level))
  if (length(v) != length(level) || any(!is.finite(v)) || any(v <= 0)) {
    stop("The error variance function for ", nm, " must return one ",
         "positive value per level.")
  }
  v
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
