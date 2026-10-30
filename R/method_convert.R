#' @title Conversion Equations Between Measurement Methods
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Estimates the linear relation between each pair of methods, as conversion
#' equations from one method to another with prediction limits. The bias
#' between methods may be constant (from a [method_vc()] fit), change
#' linearly with the level of measurement (regression of the differences on
#' the averages; Carstensen, 2010, section 4.7), or follow the scaled model,
#' in which each method is a linear function of a common true value
#' (Carstensen, 2010, ch. 7).
#'
#' @inheritParams method_vc
#' @param data A data frame in long format (one row per measurement). Not
#'   needed when `object` is supplied.
#' @param bias Model for the bias between methods. "linear" (default):
#'   regression of the differences on the averages for each pair of methods.
#'   "scaled": the scaled model fit by alternating regression (requires
#'   replicates). See details.
#' @param variance Model for the SD of the differences with
#'   `bias = "linear"`: "constant" (default) or "linear" in the average.
#' @param replicates Whether replicates are "exchangeable" (default) or
#'   "linked"; see [method_vc()]. Exchangeable replicates are paired at random
#'   within item (see `pair_seed`), linked replicates by replicate.
#' @param pred_level Coverage of the prediction limits. Default is 0.95.
#' @param conf_level Confidence level for the conversion parameters. Default
#'   is 0.95.
#' @param ci_method Method for the confidence intervals of the conversion
#'   parameters: "delta" (default; delta method) or, for `bias = "scaled"`,
#'   "boot" (parametric bootstrap). See details.
#' @param boot_reps Number of bootstrap replicates for `ci_method = "boot"`.
#'   Default is 999. Each replicate refits the model, so this can take
#'   several minutes; set a seed with [set.seed()] for reproducible
#'   intervals.
#' @param pair_seed Seed for the random pairing of exchangeable replicates;
#'   the global random number generator state is restored afterwards.
#' @param max_iter,tol Iteration limit and convergence tolerance (largest
#'   relative change in the conversion parameters and SDs) for
#'   `bias = "scaled"`. Bootstrap refits use a tolerance of at least 1e-4.
#' @param object A [method_vc()] fit (`simple_mvc`). If supplied, the data and
#'   column arguments are ignored and the conversions are derived from its
#'   estimates with a constant bias (slope 1).
#' @param keep_model Logical. Whether to keep the fitted models. Default
#'   is `TRUE`.
#'
#' @details
#' **Constant bias** (`object` supplied): the conversion from method `m` to
#' method `k` is \eqn{y_k = (\alpha_k - \alpha_m) + y_m}, with prediction SD
#' \eqn{\sqrt{V_{mk}}} from [method_vc()] (Carstensen, 2010, ch. 5), and
#' \eqn{\sqrt{2}\sigma_m} for two measurements by the same method (with
#' replicates).
#'
#' **Linear bias** (`bias = "linear"`): for each pair, with differences
#' \eqn{D = y_1 - y_2} and averages \eqn{A = (y_1 + y_2)/2} on the
#' (transformed) scale, \eqn{D = a + bA + e} is fit by least squares. With
#' `variance = "constant"`, \eqn{SD(e) = s}; with `variance = "linear"`,
#' \eqn{SD(e \mid A) = c' + d'A}, from regressing the absolute residuals on
#' \eqn{A} (multiplied by \eqn{\sqrt{\pi/2}}). The relation is converted to
#' \deqn{y_1 = \frac{a}{1 - b/2} + \frac{1 + b/2}{1 - b/2} y_2,}
#' and the prediction limits are the lines \eqn{D = (a \pm z c') + (b \pm z
#' d')A} (with \eqn{c' = s, d' = 0} for constant variance) transformed the
#' same way (Carstensen, 2010, eq. 4.11); the conversion from \eqn{y_1} to
#' \eqn{y_2} inverts these lines. This requires \eqn{|b| < 2}. With constant
#' variance the prediction SD is \eqn{s/(1 - b/2)} (to method 1) and
#' \eqn{s/(1 + b/2)} (to method 2); with linear variance it depends on the
#' level and is reported as `NA`.
#'
#' When items contribute several pairs (replicates), the covariance of
#' \eqn{(a, b)} is cluster-robust by item, with (items - 1) degrees of
#' freedom. Confidence intervals for the conversion intercepts and slopes use
#' the delta method.
#'
#' A slope of the differences on the averages does not by itself show that
#' the methods are related by a slope other than 1. Whenever the two methods
#' have unequal measurement error variances, cov(D, A) = (var(y1) - var(y2)) /
#' 2 is not zero, so \eqn{b} is biased away from zero (Bland & Altman, 1999).
#' The scaled model (`bias = "scaled"`) or errors-in-variables regression
#' ([dem_reg()], [pb_reg()]) account for the measurement errors.
#'
#' **Scaled model** (`bias = "scaled"`): measurement `r` of item `i` by
#' method `m` is
#' \deqn{y_{mir} = \alpha_m + \beta_m(\mu_i + a_{ir} + c_{mi}) + e_{mir},}
#' with the variance components of [method_vc()] on the scale of the true
#' values \eqn{\mu_i} (Carstensen, 2010, eq. 7.1). It is fit by alternating
#' regression as in the `AltReg()` function of the MethComp package: each
#' method is regressed on the current predicted true values
#' \eqn{\mu_i + a_{ir} + c_{mi}} (with a random item intercept), and the
#' variance component model is refit to \eqn{(y - \alpha_m)/\beta_m}, until
#' the conversions converge. The conversion from method `m` to `k` is
#' \eqn{\alpha_{k|m} = \alpha_k - \alpha_m\beta_k/\beta_m},
#' \eqn{\beta_{k|m} = \beta_k/\beta_m}, with prediction SD
#' \eqn{\sqrt{\tau_k^2 + \sigma_k^2 + \beta_{k|m}^2(\tau_m^2 + \sigma_m^2)}}
#' from the variance components on each method's scale (\eqn{\omega} cancels),
#' and \eqn{\sqrt{2}\sigma_m} for a method to itself. As in MethComp, the
#' method-by-item SD is multiplied by \eqn{I/(I-2)}. The per-method
#' \eqn{\alpha_m, \beta_m} are reported with the first method as reference
#' (\eqn{\alpha = 0, \beta = 1}); the conversions do not depend on this
#' choice.
#'
#' Confidence intervals for the scaled model: "delta" (default) uses the
#' standard errors of \eqn{(\alpha_m, \beta_m)} from the final regressions of
#' each method on the predicted true values. These treat the predicted true
#' values as known, so they are much too narrow: in simulations (15 to 60
#' items, 2 or 3 methods, 3 replicates, linked or exchangeable; 500 data sets
#' each), 95% intervals for the conversion intercepts and slopes covered the
#' true values only 53% to 73% of the time, with no improvement with more
#' items. They are useful only as a rough indication of precision. "boot"
#' simulates data from the fitted model with the observed design (the same
#' items, methods, and replicates, and the fitted item values), refits the
#' model, and reports percentile intervals for the conversion parameters, the
#' prediction SDs, and the variance components. The bootstrap may improve on
#' the delta method, but its coverage has not yet been checked by simulation,
#' and each replicate refits the model (999 replicates can take several
#' minutes). MethComp itself gives no intervals for the alternating
#' regression; Carstensen (2010, ch. 7) recommends a Bayesian (MCMC) fit of
#' the scaled model for its uncertainty, which is likely needed for
#' intervals with the stated coverage and is not yet available here. Treat
#' the intervals from either method as approximate. The conversions and
#' prediction limits themselves do not depend on `ci_method`.
#'
#' All estimates are on the transformed scale; [predict()] and the
#' conversion plot back-transform the conversions and limits to the original
#' scale (Carstensen, 2010, ch. 8).
#'
#' @return An object of class `simple_mconv`, a list with:
#'
#'   - `conversion`: tibble with one row per ordered pair (`to`, `from`),
#'     including each method to itself: the intercept `alpha`, slope `beta`,
#'     prediction SD `sd_pred`, their standard errors and df, and confidence
#'     limits (`alpha_lower`, `alpha_upper`, `beta_lower`, `beta_upper`; and,
#'     for the scaled model, `sd_lower`, `sd_upper` with the bootstrap).
#'   - `limits_par`: list, by ordered pair (named "to|from"), of the
#'     parameters used to evaluate the prediction limits at any level.
#'   - `da`: for `bias = "linear"`, tibble of the DA regressions for each
#'     pair (`a`, `b` and their SEs, `s`, `c_sd`, `d_sd`, df, numbers of pairs
#'     and items, and whether the SEs are cluster-robust).
#'   - `varcomp`: the variance components (for the scaled model, on each
#'     method's scale, with bootstrap intervals if requested; for a
#'     conversion from a [method_vc()] fit, its variance components);
#'     otherwise `NULL`.
#'   - `vc_fit`: for a conversion from a [method_vc()] fit, its bias and
#'     limits tables at `pred_level` and `conf_level`, and its design (used
#'     by the Bland-Altman plot); otherwise absent.
#'   - `scaled`: for the scaled model, the per-method intercepts and slopes,
#'     the item values, the variance components on the scale of the true
#'     values, and the iteration history (`trace`).
#'   - `boot`: for `ci_method = "boot"`, the matrix of bootstrap estimates.
#'   - `data`: the processed data.
#'   - `pairs`: list of the paired data used for each pair of methods.
#'   - `transform`: the transformation (functions included).
#'   - `model`: list of the fitted `lm` objects (linear bias), or the final
#'     variance component model (scaled), or `NULL`.
#'   - `fit_info`: list describing the fit and settings (including the
#'     number of iterations, convergence, and failed bootstrap refits).
#'   - `call`: the matched call, with evaluated settings.
#'
#' @examples
#' dat = sim_methods(n_items = 50, n_reps = 2, methods = c("A", "B"),
#'                   alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
#'                   mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)
#'
#' # linear bias
#' conv = method_convert(dat, y = "y", method = "method", id = "id",
#'                       replicate = "repl", pair_seed = 1)
#' conv
#' predict(conv, newdata = c(5, 10, 15), from = "A", to = "B")
#'
#' # constant bias, from a variance component fit
#' fit = method_vc(dat, y = "y", method = "method", id = "id",
#'                 replicate = "repl")
#' method_convert(object = fit)
#'
#' # scaled model (alternating regression)
#' sc = method_convert(dat, y = "y", method = "method", id = "id",
#'                     replicate = "repl", bias = "scaled", tol = 1e-4)
#' sc
#' \donttest{
#' # parametric bootstrap intervals (use more replicates in practice)
#' set.seed(1)
#' method_convert(dat, y = "y", method = "method", id = "id",
#'                replicate = "repl", bias = "scaled", tol = 1e-4,
#'                ci_method = "boot", boot_reps = 50)
#' }
#'
#' @references
#' Carstensen, B. (2010). Comparing Clinical Measurement Methods: A Practical
#' Guide. Wiley.
#'
#' Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
#' comparison studies. Statistical Methods in Medical Research, 8(2),
#' 135-160.
#'
#' @seealso [simple_mconv-methods], [method_vc()], [dem_reg()], [pb_reg()]
#' @export

method_convert = function(data = NULL,
                          y,
                          method,
                          id,
                          replicate = NULL,
                          bias = c("linear", "scaled"),
                          variance = c("constant", "linear"),
                          replicates = c("exchangeable", "linked"),
                          pred_level = 0.95,
                          conf_level = 0.95,
                          transform = "none",
                          ci_method = c("delta", "boot"),
                          boot_reps = 999,
                          pair_seed = NULL,
                          max_iter = 100,
                          tol = 1e-6,
                          object = NULL,
                          keep_model = TRUE) {
  bias = match.arg(bias)
  variance = match.arg(variance)
  replicates = match.arg(replicates)
  ci_method = match.arg(ci_method)
  for (nm in c("pred_level", "conf_level")) {
    val = get(nm)
    if (!is.numeric(val) || length(val) != 1 || is.na(val) ||
        val <= 0 || val >= 1) {
      stop("`", nm, "` must be a single number between 0 and 1.",
           call. = FALSE)
    }
  }

  call2 = match.call()
  call2$pred_level = pred_level
  call2$conf_level = conf_level
  call2$ci_method = ci_method
  call2$pair_seed = pair_seed

  # constant bias from a method_vc() fit ----
  if (!is.null(object)) {
    if (!inherits(object, "simple_mvc")) {
      stop("`object` must be a method_vc() fit (class 'simple_mvc').",
           call. = FALSE)
    }
    call2$bias = "constant"
    return(mc_convert_constant(object, pred_level = pred_level,
                               conf_level = conf_level, call2 = call2))
  }

  if (is.null(data)) {
    stop("Supply `data` (with `y`, `method`, and `id`) or a method_vc() ",
         "fit as `object`.", call. = FALSE)
  }
  if (bias == "linear" && ci_method == "boot") {
    stop("ci_method = 'boot' is only available for bias = 'scaled'.",
         call. = FALSE)
  }
  if (bias == "scaled" && variance == "linear") {
    stop("variance = 'linear' is only available with bias = 'linear'.",
         call. = FALSE)
  }

  call2$y = y
  call2$method = method
  call2$id = id
  call2$replicate = replicate
  call2$bias = bias
  call2$variance = variance
  call2$replicates = replicates
  call2$keep_model = keep_model

  df = mc_data_org(data = data,
                   y = y,
                   method = method,
                   id = id,
                   replicate = replicate,
                   transform = transform,
                   replicates = replicates)
  tf = attr(df, "transform")
  call2$transform = tf$name
  lv = levels(df$method)

  if (bias == "scaled") {
    call2$ci_method = ci_method
    call2$boot_reps = boot_reps
    call2$max_iter = max_iter
    call2$tol = tol
    return(mc_convert_scaled(df, replicates = replicates,
                             pred_level = pred_level,
                             conf_level = conf_level,
                             ci_method = ci_method, boot_reps = boot_reps,
                             pair_seed = pair_seed, max_iter = max_iter,
                             tol = tol, keep_model = keep_model,
                             call2 = call2))
  }

  # DA regressions for each pair ----
  prs = mc_pairs(lv)
  pairs = da_rows = conv_rows = models = list()
  limits_par = list()
  for (j in seq_len(nrow(prs))) {
    m1 = prs$method1[j]
    m2 = prs$method2[j]
    key = paste0(m1, "-", m2)
    pp = mc_pair(df, m1, m2, replicates = replicates, seed = pair_seed)
    da = mc_da_fit(pp, variance = variance)
    if (variance == "linear") {
      z = qnorm((1 + pred_level) / 2)
      if (any(abs(da$b + c(-1, 1) * z * da$d_sd) >= 2)) {
        stop("With variance = 'linear', the slopes of the limit lines for ",
             "the differences of ", m1, " and ", m2, " are outside (-2, 2) ",
             "at pred_level = ", pred_level, ". Use variance = 'constant' ",
             "or a transformation.", call. = FALSE)
      }
      sd_rng = da$c_sd + da$d_sd * range(pp$avg)
      if (any(sd_rng <= 0)) {
        warning("With variance = 'linear', the fitted SD of the differences ",
                "of ", m1, " and ", m2, " is not positive over part of the ",
                "observed range of the averages (from ", signif(sd_rng[1], 3),
                " to ", signif(sd_rng[2], 3), "); the prediction limits are ",
                "not valid there. Use variance = 'constant' or a ",
                "transformation.", call. = FALSE)
      }
    }
    pairs[[key]] = pp
    models[[key]] = list(da = da$fit, sd = da$sd_fit)
    conv_rows[[key]] = mc_da_conversion(da, m1, m2, conf_level = conf_level)
    se = sqrt(diag(da$vcov))
    da_rows[[key]] = dplyr::tibble(method1 = m1, method2 = m2,
                                   a = da$a, a_se = se[1],
                                   b = da$b, b_se = se[2],
                                   s = da$s, c_sd = da$c_sd, d_sd = da$d_sd,
                                   df = da$df, n_pairs = da$n_pairs,
                                   n_items = da$n_items,
                                   cluster_se = da$clustered)
    for (dir in list(c(m1, m2), c(m2, m1))) {
      limits_par[[paste0(dir[1], "|", dir[2])]] =
        mc_limits_par("da", to = dir[1], from = dir[2], m1 = m1, m2 = m2,
                      a = da$a, b = da$b, c_sd = da$c_sd, d_sd = da$d_sd)
    }
  }

  # each method to itself ----
  self_rows = dplyr::tibble(to = lv, from = lv, alpha = 0, beta = 1,
                            sd_pred = NA_real_, alpha_se = NA_real_,
                            beta_se = NA_real_, df = NA_real_,
                            alpha_lower = NA_real_, alpha_upper = NA_real_,
                            beta_lower = NA_real_, beta_upper = NA_real_)
  for (m in lv) {
    limits_par[[paste0(m, "|", m)]] =
      mc_limits_par("self", to = m, from = m)
  }
  conversion = dplyr::bind_rows(self_rows, dplyr::bind_rows(conv_rows)) %>%
    dplyr::arrange(match(to, lv), match(from, lv))

  structure(list(conversion = conversion,
                 limits_par = limits_par,
                 da = dplyr::bind_rows(da_rows),
                 varcomp = NULL,
                 data = df,
                 pairs = pairs,
                 transform = tf,
                 model = if (keep_model) models else NULL,
                 fit_info = list(bias = bias,
                                 variance = variance,
                                 replicates = replicates,
                                 has_reps = attr(df, "has_reps"),
                                 pred_level = pred_level,
                                 conf_level = conf_level,
                                 ci_method = ci_method,
                                 iterations = NA_integer_,
                                 converged = NA,
                                 criterion = NA_real_,
                                 n_boot_fail = NA_integer_),
                 call = call2),
            class = "simple_mconv")
}

# constant bias conversion from a simple_mvc object ----
# alpha_{k|m} = alpha_k - alpha_m, beta = 1, sd_{k|m} = sqrt(V_mk); for a
# method to itself sqrt(2) * sigma_m (replicates only).
mc_convert_constant = function(object,
                               pred_level,
                               conf_level,
                               call2) {
  lv = levels(object$data$method)
  b = object$bias
  l = object$limits
  rows = list()
  limits_par = list()
  for (j in seq_len(nrow(b))) {
    m1 = b$method1[j]
    m2 = b$method2[j]
    qt_c = qt((1 + conf_level) / 2, b$df[j])
    for (sgn in c(1, -1)) {
      to = if (sgn == 1) m1 else m2
      from = if (sgn == 1) m2 else m1
      est = sgn * b$bias[j]
      rows[[paste0(to, "|", from)]] = dplyr::tibble(
        to = to, from = from, alpha = est, beta = 1, sd_pred = l$sd[j],
        alpha_se = b$se[j], beta_se = NA_real_, df = b$df[j],
        alpha_lower = est - qt_c * b$se[j],
        alpha_upper = est + qt_c * b$se[j],
        beta_lower = NA_real_, beta_upper = NA_real_)
      limits_par[[paste0(to, "|", from)]] =
        mc_limits_par("constant", to = to, from = from, alpha = est,
                      sd = l$sd[j])
    }
  }
  vc = object$varcomp
  for (m in lv) {
    sig = vc$sd[vc$component == "sigma" & vc$method %in% m]
    sd_self = if (object$design$model_type == "reps" && length(sig) == 1) {
      sqrt(2) * sig
    } else NA_real_
    rows[[paste0(m, "|", m)]] = dplyr::tibble(
      to = m, from = m, alpha = 0, beta = 1, sd_pred = sd_self,
      alpha_se = NA_real_, beta_se = NA_real_, df = NA_real_,
      alpha_lower = NA_real_, alpha_upper = NA_real_,
      beta_lower = NA_real_, beta_upper = NA_real_)
    limits_par[[paste0(m, "|", m)]] = mc_limits_par("self", to = m, from = m)
  }
  conversion = dplyr::bind_rows(rows) %>%
    dplyr::arrange(match(to, lv), match(from, lv))

  des = object$design
  # bias and limits of the method_vc() fit at the levels of the conversion,
  # for the Bland-Altman plot
  qt_b = qt((1 + conf_level) / 2, b$df)
  b_vc = b
  b_vc$lower.CL = b$bias - qt_b * b$se
  b_vc$upper.CL = b$bias + qt_b * b$se
  l_vc = if (pred_level == des$pred_level && conf_level == des$conf_level) {
    l
  } else {
    mc_limits(bias_tab = b_vc,
              varcomp = vc,
              cov_v = object$vc_cov,
              design = des,
              interval = des$interval,
              pred_level = pred_level,
              conf_level = conf_level,
              bound_type = des$bound_type,
              include_omega = des$include_omega)
  }
  vc_fit = list(bias = b_vc,
                limits = l_vc,
                design = utils::modifyList(des,
                                           list(pred_level = pred_level,
                                                conf_level = conf_level)))

  pairs = list()
  for (j in seq_len(nrow(b))) {
    pairs[[paste0(b$method1[j], "-", b$method2[j])]] =
      mc_pair(object$data, b$method1[j], b$method2[j],
              replicates = des$replicates,
              seed = call2$pair_seed)
  }

  structure(list(conversion = conversion,
                 limits_par = limits_par,
                 da = NULL,
                 varcomp = vc,
                 vc_fit = vc_fit,
                 data = object$data,
                 pairs = pairs,
                 transform = object$transform,
                 model = NULL,
                 fit_info = list(bias = "constant",
                                 variance = "constant",
                                 replicates = des$replicates,
                                 has_reps = des$has_reps,
                                 pred_level = pred_level,
                                 conf_level = conf_level,
                                 ci_method = "model",
                                 iterations = NA_integer_,
                                 converged = NA,
                                 criterion = NA_real_,
                                 n_boot_fail = NA_integer_),
                 call = call2),
            class = "simple_mconv")
}

# scaled model (alternating regression) ----
mc_convert_scaled = function(df,
                             replicates,
                             pred_level,
                             conf_level,
                             ci_method,
                             boot_reps,
                             pair_seed,
                             max_iter,
                             tol,
                             keep_model,
                             call2) {
  if (!attr(df, "has_reps")) {
    stop("bias = 'scaled' requires replicate measurements by every method, ",
         "to separate the method-by-item variation from the measurement ",
         "error. Use bias = 'linear' or dem_reg().", call. = FALSE)
  }
  lv = levels(df$method)
  linked = replicates == "linked"
  alt = mc_altreg(df, linked = linked, max_iter = max_iter, tol = tol)
  if (!alt$converged) {
    warning("The alternating regression did not converge in ", max_iter,
            " iterations (criterion ", signif(alt$criterion, 3), " > ",
            tol, "). Consider increasing max_iter.", call. = FALSE)
  }
  conversion = mc_scaled_conversion(alt, conf_level = conf_level)
  varcomp = mc_scaled_varcomp(alt)
  varcomp$lower = NA_real_
  varcomp$upper = NA_real_
  conversion$sd_lower = NA_real_
  conversion$sd_upper = NA_real_

  n_boot_fail = NA_integer_
  boot = NULL
  if (ci_method == "boot") {
    boot = mc_scaled_boot(alt, df, boot_reps = boot_reps,
                          conf_level = conf_level, max_iter = max_iter,
                          tol = max(tol, 1e-4))
    n_boot_fail = boot$n_fail
    off = conversion$to != conversion$from
    conversion$alpha_lower[off] = boot$alpha[off, 1]
    conversion$alpha_upper[off] = boot$alpha[off, 2]
    conversion$beta_lower[off] = boot$beta[off, 1]
    conversion$beta_upper[off] = boot$beta[off, 2]
    conversion$sd_lower = boot$sd_pred[, 1]
    conversion$sd_upper = boot$sd_pred[, 2]
    varcomp$lower = boot$varcomp[, 1]
    varcomp$upper = boot$varcomp[, 2]
    if (n_boot_fail > 0) {
      message(n_boot_fail, " of ", boot_reps, " bootstrap refits failed or ",
              "did not converge and were dropped.")
    }
  }

  limits_par = list()
  for (j in seq_len(nrow(conversion))) {
    cv = conversion[j, ]
    limits_par[[paste0(cv$to, "|", cv$from)]] = if (cv$to == cv$from) {
      mc_limits_par("self", to = cv$to, from = cv$from)
    } else {
      mc_limits_par("scaled", to = cv$to, from = cv$from, alpha = cv$alpha,
                    beta = cv$beta, sd = cv$sd_pred)
    }
  }

  prs = mc_pairs(lv)
  pairs = list()
  for (j in seq_len(nrow(prs))) {
    pairs[[paste0(prs$method1[j], "-", prs$method2[j])]] =
      mc_pair(df, prs$method1[j], prs$method2[j], replicates = replicates,
              seed = pair_seed)
  }

  methods_tab = dplyr::tibble(method = lv,
                              alpha = unname(alt$a - alt$b * alt$a[1] /
                                               alt$b[1]),
                              beta = unname(alt$b / alt$b[1]))
  methods_tab$alpha[1] = 0
  methods_tab$beta[1] = 1

  structure(list(conversion = conversion,
                 limits_par = limits_par,
                 da = NULL,
                 varcomp = varcomp,
                 scaled = list(methods = methods_tab,
                               mu = alt$mu,
                               trace = alt$trace,
                               mu_scale = c(tau = alt$tau_z,
                                            omega = alt$omega_z,
                                            stats::setNames(alt$sigma_z,
                                                            paste0("sigma_",
                                                                   lv)))),
                 boot = if (!is.null(boot)) boot$reps else NULL,
                 data = df,
                 pairs = pairs,
                 transform = attr(df, "transform"),
                 model = if (keep_model) alt$model else NULL,
                 diagnostics = mc_scaled_diag(alt, df),
                 fit_info = list(bias = "scaled",
                                 variance = "constant",
                                 replicates = replicates,
                                 has_reps = TRUE,
                                 pred_level = pred_level,
                                 conf_level = conf_level,
                                 ci_method = ci_method,
                                 boot_reps = if (ci_method == "boot")
                                   boot_reps else NA_integer_,
                                 iterations = alt$iterations,
                                 converged = alt$converged,
                                 criterion = alt$criterion,
                                 n_boot_fail = n_boot_fail),
                 call = call2),
            class = "simple_mconv")
}

# residuals of the final variance component fit (mu scale), stored for
# check()
mc_scaled_diag = function(alt,
                          df) {
  vc = alt$model
  lvl = ncol(vc$fitted) - 1
  dplyr::tibble(method = df$method,
                id = df$id,
                fitted = unname(stats::fitted(vc, level = lvl)),
                resid = unname(stats::residuals(vc, type = "pearson",
                                                level = lvl)))
}
