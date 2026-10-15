#' @title Conversion Equations Between Measurement Methods
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Estimates the linear relation between each pair of methods, as conversion
#' equations from one method to another with prediction limits. The bias
#' between methods may be constant (from a [method_vc()] fit) or change
#' linearly with the level of measurement (regression of the differences on
#' the averages; Carstensen, 2010, section 4.7).
#'
#' @inheritParams method_vc
#' @param data A data frame in long format (one row per measurement). Not
#'   needed when `object` is supplied.
#' @param bias Model for the bias between methods. "linear" (default):
#'   regression of the differences on the averages for each pair of methods.
#'   "scaled": the scaled model fit by alternating regression (not yet
#'   available).
#' @param variance Model for the SD of the differences with
#'   `bias = "linear"`: "constant" (default) or "linear" in the average.
#' @param replicates Whether replicates are "exchangeable" (default) or
#'   "linked"; see [method_vc()]. Exchangeable replicates are paired at random
#'   within item (see `pair_seed`), linked replicates by replicate.
#' @param pred_level Coverage of the prediction limits. Default is 0.95.
#' @param conf_level Confidence level for the conversion parameters. Default
#'   is 0.95.
#' @param ci_method Method for the confidence intervals of the conversion
#'   parameters. Currently "delta" (delta method).
#' @param boot_reps Number of bootstrap replicates (for `ci_method = "boot"`,
#'   not yet available).
#' @param pair_seed Seed for the random pairing of exchangeable replicates;
#'   the global random number generator state is restored afterwards.
#' @param max_iter,tol Iteration limit and convergence tolerance for
#'   `bias = "scaled"` (not yet available).
#' @param object A [method_vc()] fit (`simple_mvc`). If supplied, the data and
#'   column arguments are ignored and the conversions are derived from its
#'   estimates with a constant bias (slope 1).
#' @param keep_model Logical. Whether to keep the fitted regressions. Default
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
#' All estimates are on the transformed scale; [predict()] and the
#' conversion plot back-transform the conversions and limits to the original
#' scale (Carstensen, 2010, ch. 8).
#'
#' @return An object of class `simple_mconv`, a list with:
#'
#'   - `conversion`: tibble with one row per ordered pair (`to`, `from`),
#'     including each method to itself: the intercept `alpha`, slope `beta`,
#'     prediction SD `sd_pred`, their standard errors and df, and confidence
#'     limits (`alpha_lower`, `alpha_upper`, `beta_lower`, `beta_upper`).
#'   - `limits_par`: list, by ordered pair (named "to|from"), of the
#'     parameters used to evaluate the prediction limits at any level.
#'   - `da`: for `bias = "linear"`, tibble of the DA regressions for each
#'     pair (`a`, `b` and their SEs, `s`, `c_sd`, `d_sd`, df, numbers of pairs
#'     and items, and whether the SEs are cluster-robust).
#'   - `varcomp`: for a conversion from a [method_vc()] fit, its variance
#'     components; otherwise `NULL`.
#'   - `data`: the processed data.
#'   - `pairs`: list of the paired data used for each pair of methods.
#'   - `transform`: the transformation (functions included).
#'   - `model`: list of the fitted `lm` objects (or `NULL`).
#'   - `fit_info`: list describing the fit and settings.
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
  if (bias == "scaled") {
    stop("bias = 'scaled' is not available yet; use bias = 'linear', or ",
         "dem_reg() for errors-in-variables regression.", call. = FALSE)
  }
  if (ci_method == "boot") {
    stop("ci_method = 'boot' is only available for bias = 'scaled'.",
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
