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
#' @param replicates With `id`: `"exchangeable"` (default), replicates of
#'   each method are interchangeable, or `"linked"`, the rows of a subject
#'   are occasions on which both methods measured (x and y on the same row
#'   are the same occasion), so their errors share the occasion effects and
#'   are correlated. See "Linked replicates" in Details.
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
#' @param se_method How the standard errors of the coefficients are
#'   computed: `"delta"` (default), an errors-in-variables sandwich plus a
#'   delta-method term for estimated error variances, or `"bivregbls"`, the
#'   standard errors of BivRegBLS. See "Standard errors" in Details.
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
#' started from the Deming solution. It is Deming regression ([dem_reg()])
#' with `error.ratio = var_x / var_y` when the variances are constant, every
#' subject has the same number of replicates by each method (none
#' missing), and there is no equation error (`equation_error = FALSE`, or a
#' dispersion of at most 1); with only `error.ratio`, always. Otherwise the
#' lines differ: the equation error adds the same variance to both methods,
#' which changes the effective error ratio and the slope. Confidence intervals use
#' `t` on `n - 2` df with the standard errors below.
#'
#' **Standard errors.** BivRegBLS (`se_method = "bivregbls"`) uses
#' `s^2 R^-1`, where `R` is the weighted cross-product matrix of `(1, x_i)`
#' with weights `1 / (var_yi + b1^2 var_xi)` and `s^2` the weighted residual
#' variance on `n - 2` df. This treats the observed `x_i` as if they were the
#' true values and the error variances as known, and is too small when the
#' error of `x` is not small relative to the spread of the true values (in
#' simulations, coverage of 95% intervals fell to 0.87 with one measurement
#' each and to 0.71 with linked replicates). The default
#' (`se_method = "delta"`) has two parts. The first is the sandwich of the
#' estimating equations with `R` built from the estimated true values and
#' the error left in them removed, which with equal weights is the
#' errors-in-variables variance of Fuller (1987, sec. 1.3). The second, when
#' the error variances are estimated (from replicates, or given with finite
#' `df_var_x`, `df_var_y`), is a delta-method term for their uncertainty:
#' the derivatives of the coefficients with respect to the log error
#' variances (and, with linked replicates, the error correlation), taken
#' numerically over the whole fit, times their sampling covariance on their
#' df. If one of these refits does not converge, or moves between having an
#' equation-error solution and having none (a jump, not a derivative), the
#' term is left out and a warning says so. With only `error.ratio`, the
#' error variances are scaled by `s^2`. In
#' simulations the default covered 0.93 to 0.98 where BivRegBLS's covered
#' 0.71 to 0.96, and the two agree when the true values are widely spread.
#' The uncertainty in the estimated equation error (`tau^2`) and in the
#' shape of a `"power"` variance function is not included; for `tau^2`, a
#' simulation found it unnecessary (coverage 0.94 to 0.98, and up to 1.00
#' with a large equation error and little spread in the true values).
#' [error_ratio_profile()] uses the BivRegBLS standard errors.
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
#' dispersion is at most 1), solved jointly with the line by a root search
#' over the slope, which stays stable when `tau^2` is large against the
#' spread of the true values (a warning is given if more than one solution
#' is found). When the line is barely determined (e.g. linked replicates
#' whose shared occasion effects are larger than the spread of the true
#' values), the equations can have no solution once `tau^2` is added: the
#' lines are then fitted with measurement error only, `tau^2` estimated
#' about the BLS line is used in the prediction intervals, and a warning is
#' given. The prediction intervals include `tau^2` (on `n - 2` df). Equal deviations for
#' both methods is an assumption: with two methods, how the equation error
#' divides between them cannot be estimated.
#'
#' **Linked replicates.** When both methods measure each subject on the
#' same occasions and the true value varies between occasions (or the
#' conditions of an occasion affect both methods), the errors of `x` and `y`
#' about the subject's mean true value include a shared occasion effect, so
#' they are correlated. With `replicates = "linked"`, the within-subject
#' variances (which then include the occasion variation) are estimated from
#' the replicates as before, and the correlation `rho` of the two methods'
#' errors from the pooled within-subject cross-products of the paired
#' occasions (standardised by the error variances at the subject means, so
#' that it also applies with `var_model = "proportional"` or `"power"`), on
#' `sum(k_i - 1)` df, with `k_i` the subject's number of occasions with both
#' methods. The covariance of the errors of the subject means is
#' `rho sqrt(var_x var_y) k_i / (n_xi n_yi)`, and it enters the BLS and CBLS
#' fits (`var_y + b1^2 var_x - 2 b1 cov`), the dispersion, and the
#' prediction and tolerance intervals, which are for new measurements by
#' both methods on the same occasion(s). The measurement error part of these
#' intervals, a combination of the paired errors, is given the paired df.
#' The variances cannot be given (`var_x`, `var_y`): a measurement error
#' variance from a separate study does not include the occasion effects. In
#' simulations (15 or 40 subjects, 2 or 4 occasions), the prediction
#' intervals covered 0.95 to 0.97 (an exchangeable fit of the same data
#' covered up to 1.00, with intervals up to 2.9 times as wide), and the
#' confidence intervals of the coefficients 0.94 to 0.96 (0.91 to 0.95 with
#' `se_method = "bivregbls"` when the occasion effect was about three times
#' the larger measurement error SD).
#' This is the CBLS fit used by [method_convert()] with
#' `replicates = "linked"` and `estimator = "cbls"`.
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
#'     `"ratio"`), and `replicates`, the error correlation `rho` (0 unless
#'     linked) with its df `df_rho`, and the covariance `cov_xy` of a single
#'     pair of measurements at the mean level;
#'   - `pred_level`, `method`, `iterations`, `converged`.
#'
#'   The stored data (`model`) have one row per subject: the means `x`, `y`,
#'   the numbers of replicates `n_x`, `n_y`, and the error variances of the
#'   means `var_x`, `var_y` (with linked replicates also the number of
#'   paired occasions `n_xy` and the error covariance of the means
#'   `cov_xy`).
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
#' Fuller, W. A. (1987). Measurement Error Models. Wiley.
#' \doi{10.1002/9780470316665}
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
                     replicates = c("exchangeable", "linked"),
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
                     se_method = c("delta", "bivregbls"),
                     model = TRUE,
                     ...) {
  call2 <- match.call()
  se_method <- match.arg(se_method)
  var_model <- match.arg(var_model)
  bound_type <- match.arg(bound_type)
  transform <- match.arg(transform)
  replicates <- match.arg(replicates)
  linked <- replicates == "linked"
  if (linked && is.null(id)) {
    stop("replicates = 'linked' needs id: the rows of a subject are its ",
         "occasions.")
  }
  if (linked && (!is.null(var_x) || !is.null(var_y))) {
    stop("With replicates = 'linked', var_x and var_y are estimated from the ",
         "replicates: the errors of the subject means include the occasion ",
         "effects, which a measurement error variance from another study ",
         "does not.")
  }
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
  call2$replicates <- replicates
  call2$se_method <- se_method

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
  # linked replicates: x and y on the same row are the same occasion. For
  # each subject, the number of such paired occasions and the cross-product
  # of their deviations from the paired means.
  sub$k <- 0
  sub$sxy <- 0
  if (linked) {
    pr <- raw[!is.na(raw$x) & !is.na(raw$y), ]
    k_i <- tapply(pr$x, pr$id, length)
    sxy_i <- tapply(seq_len(nrow(pr)), pr$id, function(j) {
      sum((pr$x[j] - mean(pr$x[j])) * (pr$y[j] - mean(pr$y[j])))
    })
    m <- match(sub$id, names(k_i))
    sub$k <- ifelse(is.na(m), 0, k_i[m])
    sub$sxy <- ifelse(is.na(m), 0, sxy_i[m])
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

  # linked replicates: the errors of x and y on the same occasion are
  # correlated (shared occasion effects). Their correlation is the pooled
  # within-subject cross-product of the paired occasions, standardised by
  # the single-measurement variances at the subject means, on sum(k_i - 1)
  # df; the covariance at any level is rho sqrt(var_x var_y), and that of
  # the subject means rho sqrt(var_x var_y) k_i / (n_xi n_yi).
  rho <- 0
  df_rho <- NA_real_
  if (linked) {
    df_rho <- sum(pmax(sub$k - 1, 0))
    if (df_rho == 0) {
      stop("replicates = 'linked' needs subjects measured by both methods ",
           "on two or more occasions (rows with both x and y).")
    }
    fx0 <- .bls_eval(ex$fun, sub$x, "var_x")
    fy0 <- .bls_eval(ey$fun, sub$y, "var_y")
    ok <- sub$k > 1
    rho <- sum(sub$sxy[ok] / sqrt(fx0[ok] * fy0[ok])) / df_rho
    rho <- max(min(rho, 1), -1)
  }

  # The fit for given error parameters: the error variances scaled by kx and
  # ky and the error correlation rho. Called once at the estimates (kx = ky
  # = 1) and again, perturbed, for the delta-method standard errors below.
  fit_core <- function(kx, ky, rho) {
  cov_means <- function(vx1, vy1) {
    rho * sqrt(vx1 * vy1) * sub$k / (sub$n_x * sub$n_y)
  }
  # BLS in the (x, y) plot. The variances are evaluated at the estimated true
  # values of each subject, starting from the subject means, and the fit is
  # repeated until the line is stable (one pass with constant variances).
  lev_x <- sub$x
  lev_y <- sub$y
  coef_old <- c(Inf, Inf)
  for (pass in seq_len(100)) {
    vx1 <- kx * (if (!is.null(ex$values)) ex$values else
      .bls_eval(ex$fun, lev_x, "var_x"))
    vy1 <- ky * (if (!is.null(ey$values)) ey$values else
      .bls_eval(ey$fun, lev_y, "var_y"))
    sub$var_x <- vx1 / sub$n_x
    sub$var_y <- vy1 / sub$n_y
    sub$cov_xy <- cov_means(vx1, vy1)
    # started from Deming with lambda = var_y / var_x
    lam <- mean(sub$var_y) / mean(sub$var_x)
    sxx <- sum((sub$x - mean(sub$x))^2)
    syy <- sum((sub$y - mean(sub$y))^2)
    sxy <- sum((sub$x - mean(sub$x)) * (sub$y - mean(sub$y)))
    b1 <- (syy - lam * sxx + sqrt((syy - lam * sxx)^2 + 4 * lam * sxy^2)) /
      (2 * sxy)
    start <- c(mean(sub$y) - b1 * mean(sub$x), b1)
    bls <- .bls_engine(u = sub$x, v = sub$y, var_u = sub$var_x,
                       var_v = sub$var_y, cov_uv = sub$cov_xy, start = start,
                       scale_from_dispersion = source == "ratio")
    if (constant ||
        max(abs(bls$coef - coef_old) / pmax(abs(bls$coef), 1)) < 1e-10) {
      break
    }
    coef_old <- bls$coef
    e <- sub$y - bls$coef[1] - bls$coef[2] * sub$x
    s2 <- sub$var_y + bls$coef[2]^2 * sub$var_x -
      2 * bls$coef[2] * sub$cov_xy
    lev_x <- sub$x + (bls$coef[2] * sub$var_x - sub$cov_xy) * e / s2
    lev_y <- bls$coef[1] + bls$coef[2] * lev_x
  }
  passes_ok <- constant || pass < 100

  # dispersion of the measurement-error-only fit (the test of no equation
  # error), then, with equation_error, the common-tau equation error: each
  # subject's true value deviates from the line by c_xi for x and b c_yi for
  # y, c ~ N(0, tau2), as for the method-by-item effects of method_vc(tau =
  # "common"). The effective error variances are var_x + tau2 and
  # var_y + b^2 tau2; tau2 solves sum(e_i^2 / (s_i + 2 b^2 tau2)) = n - 2
  # (0 if the dispersion is at most 1). The slope and tau2 are solved
  # jointly (.bls_eq_solve()); the final BLS fit at that tau2 gives the
  # weights and standard errors.
  e_i <- sub$y - bls$coef[1] - bls$coef[2] * sub$x
  disp <- .bls_dispersion(e_i, sub$var_y + bls$coef[2]^2 * sub$var_x -
                            2 * bls$coef[2] * sub$cov_xy,
                          bls$coef[2], sub, ex$df, ey$df,
                          known = source != "ratio", df_c = df_rho)
  tau2 <- 0
  # tau2 in the error variances of the lines (0 if the equation-error model
  # has no solution)
  tau2_fit <- 0
  n_sol <- NA_integer_
  if (equation_error && isTRUE(disp$s2 > 1) && source != "ratio") {
    sol <- .bls_eq_solve(sub$x, sub$y, sub$var_x, sub$var_y, sub$cov_xy)
    n_sol <- sol$n_solutions
    if (n_sol > 0) {
      tau2 <- tau2_fit <- sol$tau2
      bls <- .bls_engine(u = sub$x, v = sub$y, var_u = sub$var_x + tau2,
                         var_v = sub$var_y + sol$coef[2]^2 * tau2,
                         cov_uv = sub$cov_xy, start = sol$coef)
    } else {
      # no solution: with tau2 in the variances the slope equation keeps one
      # sign up to a vertical line (the data cannot place the line once the
      # equation error is added). The lines stay those of measurement error
      # only, and tau2 estimated about the BLS line goes into the prediction
      # intervals only.
      tau2 <- .bls_tau2(e_i, sub$var_y + bls$coef[2]^2 * sub$var_x -
                          2 * bls$coef[2] * sub$cov_xy, bls$coef[2])
    }
  }
  disp$tau2 <- tau2
  disp$n_solutions <- n_sol
  # effective error variances of the subject means
  vx_eff <- sub$var_x + tau2_fit
  vy_eff <- sub$var_y + bls$coef[2]^2 * tau2_fit

  # CBLS in the Bland-Altman plot (D = x - y, M = (x + y) / 2), started from
  # the BLS line: y = a + b x is D = -2a / (1 + b) + 2 (1 - b) / (1 + b) M
  a <- bls$coef[1]
  b <- bls$coef[2]
  m_i <- (sub$x + sub$y) / 2
  d_i <- sub$x - sub$y
  c_eff <- sub$cov_xy
  cbls <- .bls_engine(u = m_i, v = d_i,
                      var_u = (vx_eff + vy_eff + 2 * c_eff) / 4,
                      var_v = vx_eff + vy_eff - 2 * c_eff,
                      cov_uv = (vx_eff - vy_eff) / 2,
                      start = c(-2 * a / (1 + b), 2 * (1 - b) / (1 + b)),
                      scale_from_dispersion = source == "ratio")
  list(bls = bls, cbls = cbls, disp = disp, tau2 = tau2, sub = sub,
       lev_x = lev_x, lev_y = lev_y, pass = pass,
       passes_ok = passes_ok)
  }
  fc <- fit_core(1, 1, rho)
  bls <- fc$bls
  cbls <- fc$cbls
  disp <- fc$disp
  tau2 <- fc$tau2
  sub <- fc$sub
  lev_x <- fc$lev_x
  lev_y <- fc$lev_y
  pass <- fc$pass
  passes_ok <- fc$passes_ok

  # Standard errors (se_method):
  # - "bivregbls": s^2 R^-1 of the engine (Lisy et al.; BivRegBLS), with
  #   the error parameters treated as known. Too small when the error of x
  #   is not small relative to the spread of the true values (coverage down
  #   to 0.81 in simulations), and when the estimated error parameters are
  #   uncertain (with linked replicates and large occasion effects the
  #   residual variance var_y + b1^2 var_x - 2 b1 cov is a small difference
  #   of large estimated terms; references/general/cbls_sims, study 5).
  # - "delta" (default): the errors-in-variables sandwich of the engine
  #   (vcov_eiv, Fuller's variance with equal weights), plus a delta-method
  #   term for the estimated error parameters p = (log kx, log ky, rho), the
  #   scales of the two error variance functions and the error correlation.
  #   Their sampling covariance Sig is that of within-subject (co)variances
  #   on their df (log variance 2 / df; with linked replicates, paired on
  #   df_rho: cov(log s2_x, log s2_y) = 2 rho^2 / df, cov(log s2, rho) =
  #   rho (1 - rho^2) / df, var(rho) = (1 - rho^2)^2 / df), independent of
  #   the subject means. The coefficients' derivatives J are taken
  #   numerically over the whole fit (including the equation error), and
  #   J Sig J' is added. The variance functions' shape parameters (theta of
  #   "power") are taken as known.
  if (se_method == "delta") {
    bls$vcov <- bls$vcov_eiv
    cbls$vcov <- cbls$vcov_eiv
    Sig <- matrix(0, 3, 3)
    if (source != "ratio") {
      v_log <- function(d) if (is.finite(d) && d > 0) 2 / d else 0
      Sig[1, 1] <- v_log(ex$df)
      Sig[2, 2] <- v_log(ey$df)
      if (linked) {
        Sig[3, 3] <- (1 - rho^2)^2 / df_rho
        Sig[1, 2] <- Sig[2, 1] <- 2 * rho^2 / df_rho
        Sig[1, 3] <- Sig[3, 1] <- Sig[2, 3] <- Sig[3, 2] <-
          rho * (1 - rho^2) / df_rho
      }
    }
    if (any(Sig != 0)) {
      # a perturbed refit is usable if it converged and stays on the same
      # side of the equation-error case (a solution for tau2, or none), so
      # that the difference is a derivative and not a jump between them
      no_sol <- function(d) isTRUE(d$n_solutions == 0)
      refit_ok <- TRUE
      coef_at <- function(p) {
        f <- fit_core(exp(p[1]), exp(p[2]), p[3])
        if (!(f$bls$converged && f$cbls$converged && f$passes_ok) ||
            no_sol(f$disp) != no_sol(disp)) {
          refit_ok <<- FALSE
        }
        c(f$bls$coef, f$cbls$coef)
      }
      p0 <- c(0, 0, rho)
      h <- 1e-4
      J <- vapply(1:3, function(j) {
        if (all(Sig[j, ] == 0)) return(numeric(4))
        lo <- hi <- p0
        lo[j] <- p0[j] - h
        hi[j] <- p0[j] + h
        if (j == 3) {
          lo[3] <- max(lo[3], -1)
          hi[3] <- min(hi[3], 1)
        }
        (coef_at(hi) - coef_at(lo)) / (hi[j] - lo[j])
      }, numeric(4))
      if (refit_ok && all(is.finite(J))) {
        add <- J %*% Sig %*% t(J)
        bls$vcov <- bls$vcov + add[1:2, 1:2]
        cbls$vcov <- cbls$vcov + add[3:4, 3:4]
      } else {
        # keep the sandwich alone rather than a corrupted derivative
        warning("The delta-method term of the standard errors could not be ",
                "computed (a refit with perturbed error variances did not ",
                "converge); the standard errors leave out the uncertainty in ",
                "the estimated error variances and may be too small.")
      }
    }
  }

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

  converged <- bls$converged && cbls$converged && passes_ok
  if (!converged) {
    warning("The BLS iterations did not converge; the estimates may be ",
            "inaccurate.")
  }
  if (isTRUE(disp$n_solutions == 0)) {
    warning("The equation-error model has no solution with these data (the ",
            "line is barely determined once tau^2 is added). The lines are ",
            "fitted with measurement error only; tau^2 estimated about the ",
            "BLS line is used in the prediction intervals.")
  }
  if (isTRUE(disp$n_solutions > 1)) {
    warning("The equation-error fit has ", disp$n_solutions, " solutions; ",
            "the one with the smallest equation error (tau^2) is used.")
  }

  sub_out <- sub[, c("id", "x", "y", "n_x", "n_y", "var_x", "var_y")]
  if (linked) {
    sub_out$n_xy <- sub$k
    sub_out$cov_xy <- sub$cov_xy
  }
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
                    source = source, known = source != "ratio",
                    replicates = replicates, rho = rho, df_rho = df_rho,
                    cov_xy = rho * sqrt(vx1 * vy1)),
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
.bls_dispersion <- function(e, s, b1, sub, df_x, df_y, known, df_c = NA_real_) {
  n <- length(e)
  q <- sum(e^2 / s)
  s2 <- q / (n - 2)
  if (!known) {
    return(list(s2 = s2, statistic = NA_real_, df1 = n - 2, df2 = NA_real_,
                p.value = NA_real_))
  }
  # df of the measurement error part of s_i: with linked replicates, the
  # variance of one combination of the paired errors, on the paired df
  # (df_c); otherwise Satterthwaite over the two variances
  a_x <- mean(b1^2 * sub$var_x / s)
  a_y <- mean(sub$var_y / s)
  df2 <- if (is.finite(df_c)) df_c else
    (a_x + a_y)^2 / (a_x^2 / df_x + a_y^2 / df_y)
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

# Direct solution of the BLS equations of .bls_engine(), for when its
# iteration fails. With the intercept profiled out, the slope equation
# sum w_i e_i xh_i = 0 is minus half the derivative of the criterion
# Q(b1) = sum e_i^2 / s2_i, so its roots are the stationary points of Q
# (with constant variances, the two roots of Deming regression). They are
# found by a bracketed search over the angle of the standardized line, both
# signs, and the root nearest the start (the line the iteration began from:
# Deming, or for CBLS the BLS line) is returned as c(b0, b1) (NULL if there
# is none). The smallest Q is not used: with strongly correlated errors the
# error variance s2_i = var_v + b1^2 var_u - 2 b1 cov_uv grows for slopes of
# the other sign, which can give the spurious root the smaller Q.
.bls_slope_search <- function(u, v, var_u, var_v, cov_uv, start,
                              n_grid = 240) {
  n <- length(u)
  var_u <- rep_len(var_u, n)
  var_v <- rep_len(var_v, n)
  cov_uv <- rep_len(cov_uv, n)
  profile <- function(b1) {
    w <- 1 / (var_v + b1^2 * var_u - 2 * b1 * cov_uv)
    b0 <- sum(w * (v - b1 * u)) / sum(w)
    e <- v - b0 - b1 * u
    c(eq = sum(w * e * (u + (b1 * var_u - cov_uv) * e * w)), b0 = b0)
  }
  sc <- stats::sd(v) / stats::sd(u)
  th <- seq(-pi / 2, pi / 2, length.out = n_grid + 2)[-c(1, n_grid + 2)]
  slope <- function(t) tan(t) * sc
  eq <- vapply(th, function(t) profile(slope(t))[["eq"]], numeric(1))
  ok <- is.finite(eq)
  j <- which(ok[-1] & ok[-n_grid] & sign(eq[-1]) != sign(eq[-n_grid]))
  if (length(j) == 0) {
    return(NULL)
  }
  sols <- vapply(j, function(k) {
    t <- stats::uniroot(function(t) profile(slope(t))[["eq"]],
                        c(th[k], th[k + 1]), tol = 1e-13)$root
    c(profile(slope(t))[["b0"]], slope(t), t)
  }, numeric(3))
  sols[1:2, which.min(abs(sols[3, ] - atan(start[2] / sc)))]
}

# Joint solution of the slope, intercept and common tau2 (equation error).
# With x-errors of variance var_x + tau2 and y-errors of variance
# var_y + b1^2 tau2, the BLS estimating equations are, with
# s_i = var_y + b1^2 var_x - 2 b1 cov + 2 b1^2 tau2, w_i = 1 / s_i,
# e_i = y_i - b0 - b1 x_i and xh_i = x_i + (b1 (var_x + tau2) - cov) e_i w_i:
#   sum w_i e_i = 0, sum w_i e_i xh_i = 0, sum w_i e_i^2 = n - 2.
# The y-error variance is evaluated at b1 but not differentiated (as the
# variances of BLS are taken as known), which keeps the equations unbiased.
# Iterating "tau2 given the line, then the line given tau2" can fall into a
# 2-cycle of near-reciprocal slopes when tau2 is large against the spread
# of the true values, so instead b0 and tau2 are profiled out for each
# slope and the slope equation is solved by a bracketed root search over
# the angle of the standardized line. More than one root is possible in
# principle (none was seen in simulations); the one with the smallest tau2
# is kept and their number returned.
.bls_eq_solve <- function(x, y, var_x, var_y, cov_xy, n_grid = 120) {
  n <- length(x)
  var_x <- rep_len(var_x, n)
  var_y <- rep_len(var_y, n)
  cov_xy <- rep_len(cov_xy, n)
  # b0 and tau2 for a given slope (b0 depends on tau2 through the weights
  # unless the variances are constant), and the slope equation
  profile <- function(b1) {
    s0 <- var_y + b1^2 * var_x - 2 * b1 * cov_xy
    tau2 <- 0
    w <- 1 / s0
    b0 <- sum(w * (y - b1 * x)) / sum(w)
    for (i in seq_len(100)) {
      tau2_new <- .bls_tau2(y - b0 - b1 * x, s0, b1)
      w <- 1 / (s0 + 2 * b1^2 * tau2_new)
      b0_new <- sum(w * (y - b1 * x)) / sum(w)
      done <- abs(b0_new - b0) <= 1e-12 * max(abs(b0), 1) &&
        abs(tau2_new - tau2) <= 1e-12 * max(tau2_new, 1e-300)
      b0 <- b0_new
      tau2 <- tau2_new
      if (done) break
    }
    e <- y - b0 - b1 * x
    xh <- x + (b1 * (var_x + tau2) - cov_xy) * e * w
    c(eq = sum(w * e * xh), b0 = b0, tau2 = tau2)
  }
  # slopes b1 = sign * tan(theta) * sd(y) / sd(x), theta in (0, pi / 2)
  sc <- stats::sd(y) / stats::sd(x)
  sg <- if (stats::cov(x, y) < 0) -1 else 1
  th <- seq(0, pi / 2, length.out = n_grid + 2)[-c(1, n_grid + 2)]
  slope <- function(t) sg * tan(t) * sc
  eq <- vapply(th, function(t) profile(slope(t))[["eq"]], numeric(1))
  ok <- is.finite(eq)
  j <- which(ok[-1] & ok[-n_grid] & sign(eq[-1]) != sign(eq[-n_grid]))
  if (length(j) == 0) {
    return(list(coef = c(NA_real_, NA_real_), tau2 = NA_real_,
                n_solutions = 0L))
  }
  sols <- vapply(j, function(k) {
    t <- stats::uniroot(function(t) profile(slope(t))[["eq"]],
                        c(th[k], th[k + 1]), tol = 1e-13)$root
    p <- profile(slope(t))
    c(p[["b0"]], slope(t), p[["tau2"]])
  }, numeric(3))
  best <- which.min(sols[3, ])
  list(coef = sols[1:2, best], tau2 = sols[3, best],
       n_solutions = length(j))
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
# correction sum (e_i w_i)^2 (b1 var_u - cov_uv) for the slope.
#
# Two variance-covariance matrices, both scaled by the dispersion
# s^2 = sum w_i e_i^2 / (n - 2):
# - vcov: s^2 R^-1 (Lisy et al.; BivRegBLS). R uses the observed u, whose
#   spread includes its measurement error, so it is too small when the
#   error of u is not small relative to the spread of the true values
#   (coverage down to 0.81 in simulations).
# - vcov_eiv: the sandwich of the estimating equations
#   psi_i = (e_i / s2_i) (1, xh_i), xh_i = u_i + (b1 var_u - cov_uv) e_i w_i
#   the adjusted (estimated true) values: s^2 A^-1 R* A^-1, with
#   R* = sum w_i (1, xh_i)'(1, xh_i) and A = R* less sum w_i var(r_i) in
#   the slope term, var(r_i) = var_u - (cov_uv - b1 var_u)^2 w_i the error
#   left in xh_i. With equal weights this is the variance of Fuller (1987,
#   sec. 1.3) for an errors-in-variables slope with known error covariance.
#   With only the error ratio known (scale_from_dispersion = TRUE), the
#   error variances are the assumed ones times the dispersion, so the
#   error left in xh_i is scaled by s^2 as well.
.bls_engine <- function(u, v, var_u, var_v, cov_uv, start,
                        tol = 1e-12, max_iter = 500,
                        scale_from_dispersion = FALSE) {
  n <- length(u)
  var_u <- rep_len(var_u, n)
  var_v <- rep_len(var_v, n)
  cov_uv <- rep_len(cov_uv, n)
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
  if (!converged) {
    # the iteration can move away from the solution when the line is
    # barely determined (e.g. linked replicates whose shared occasion
    # effect swamps the spread of the true values): solve directly
    b_rs <- .bls_slope_search(u, v, var_u, var_v, cov_uv, start)
    if (!is.null(b_rs)) {
      b <- b_rs
      converged <- TRUE
    }
  }
  sy <- step_sys(b)
  e <- v - b[1] - b[2] * u
  w <- rep_len(sy$w, n)
  s2 <- sum(w * e^2) / (n - 2)
  # errors-in-variables sandwich (see above)
  vu <- rep_len(var_u, n)
  cu <- rep_len(cov_uv, n)
  xh <- u + (b[2] * vu - cu) * e * w
  r_var <- pmax(vu - (cu - b[2] * vu)^2 * w, 0)
  R_star <- matrix(c(sum(w), sum(w * xh), sum(w * xh), sum(w * xh^2)), 2, 2)
  A <- R_star
  A[2, 2] <- A[2, 2] -
    sum(w * r_var) * (if (scale_from_dispersion) s2 else 1)
  A_inv <- tryCatch(solve(A), error = function(err) NULL)
  vcov_eiv <- if (is.null(A_inv) || det(A) <= 0) {
    # the adjusted values do not separate the true spread from the error
    # (very low reliability of u): fall back to R*
    solve(R_star) * s2
  } else {
    A_inv %*% R_star %*% A_inv * s2
  }
  list(coef = b, vcov = solve(sy$R) * s2, vcov_eiv = vcov_eiv,
       weights = sy$w, s2 = s2, iterations = it, converged = converged)
}
