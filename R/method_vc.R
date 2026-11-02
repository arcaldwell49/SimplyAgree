#' @title Variance Component Limits for Comparing Several Methods
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Fits the variance component model of Carstensen (2010) to measurements of
#' a set of items by two or more methods, with or without replicate
#' measurements, and reports the bias between each pair of methods, the
#' variance components, and limits of agreement (or prediction and tolerance
#' limits) for the
#' difference between single measurements by two methods on a new item.
#'
#' @param data A data frame in long format (one row per measurement). Wide
#'   data can be converted with [mc_long()].
#' @param y Name of the column with the measurements.
#' @param method Name of the column identifying the method. The first level
#'   (if a factor) or the first value in sorted order is the reference
#'   method; differences are reported as earlier minus later level.
#' @param id Name of the column identifying the item (subject).
#' @param replicate Name of the column identifying the replicate. Required
#'   when an item is measured more than once by the same method.
#' @param replicates Whether replicates are "exchangeable" (default) or
#'   "linked". Replicates are linked when all methods measure an item on the
#'   same occasion (replicate `r` by every method shares the occasion), so
#'   measurements on the same occasion share an item-by-replicate effect.
#'   This is a property of the design, not of the data. With two methods,
#'   "exchangeable" corresponds to `data_type = "reps"` and "linked" to
#'   `data_type = "nest"` in [agreement_limit()]; see
#'   `vignette("method_comparison_vc")`.
#' @param tau Whether the method-by-item variance is "common" to all methods
#'   (default) or differs by "method". Method-specific values require at
#'   least three methods.
#' @param methods Whether the method effects are "fixed" (default; a bias
#'   for each pair of methods) or "random": the methods are a random sample
#'   of raters (observers, devices, laboratories), and the limits are for
#'   the difference between two randomly chosen raters. Needs at least three
#'   methods. See "Random raters" in details.
#' @param sigma Whether the residual SD is "common" to all methods or
#'   differs by "method". The default (`NULL`) is "method" with
#'   `methods = "fixed"` and "common" with `methods = "random"`. With one
#'   measurement per item by two methods only a common SD is identifiable.
#' @param interval Type of limits: "agreement" (default) for limits of
#'   agreement with confidence bounds, or "tolerance" for prediction limits
#'   and tolerance limits, as in [tolerance_limit()].
#' @param pred_level The proportion of differences the limits should contain
#'   (the agreement level). Default is 0.95.
#' @param conf_level Confidence level for the bias, the variance components,
#'   and the confidence bounds of the limits. Default is 0.95.
#' @param bound_type Which claim the confidence bounds of the limits support,
#'   "iu" or "joint", as in [agreement_limit()] and [tolerance_limit()]. The
#'   default (`NULL`) is "iu" for `interval = "agreement"` and "joint" for
#'   `interval = "tolerance"`. See details.
#' @param transform Transformation applied to the measurements before
#'   analysis: "none" (default), "log", "logit", "pctlogit" (logit of a
#'   percentage), "sqrt", "asinsqrt", or a list with functions `fun` and
#'   `inv` (its inverse) and, optionally, a `name`. All estimates are on the
#'   transformed scale.
#' @param include_omega Logical. With linked replicates, whether the
#'   item-by-replicate variance (omega) is included in the variance of a
#'   difference. Default is `FALSE`, for methods applied to an item on the
#'   same occasion, where it cancels. Use `TRUE` when the methods would be
#'   applied on different occasions in practice.
#' @param keep_model Logical. Whether to keep the fitted model in the
#'   returned object. Default is `TRUE`.
#' @param control Control values for [nlme::lme()]; see
#'   [nlme::lmeControl()].
#'
#' @details
#' Measurement `r` of item `i` by method `m` is modeled (Carstensen, 2010,
#' eq. 5.1 and 5.2) as
#' \deqn{y_{mir} = \alpha_m + \mu_i + a_{ir} + c_{mi} + e_{mir},}
#' with fixed item effects \eqn{\mu_i}, method-by-item effects
#' \eqn{c_{mi} \sim N(0, \tau_m^2)}, residuals
#' \eqn{e_{mir} \sim N(0, \sigma_m^2)} with a separate SD for each method (or
#' one common SD with `sigma = "common"`), and,
#' for linked replicates only, item-by-replicate effects
#' \eqn{a_{ir} \sim N(0, \omega^2)} common to all methods. The model is fit by
#' REML with [nlme::lme()].
#'
#' What can be estimated depends on the design:
#'
#'   - **Replicates**: all components. With two methods, only a common
#'     \eqn{\tau} can be estimated.
#'   - **One measurement per item and method, two methods**: only the variance
#'     of the difference is identifiable. The model is fit with
#'     [nlme::gls()] with a common residual SD, which is the paired-difference
#'     analysis of [agreement_limit()] with `data_type = "simple"`.
#'   - **One measurement per item and method, three or more methods**: the
#'     method-specific residual SDs (Grubbs-type estimates) absorb the
#'     method-by-item variation, and repeatability cannot be estimated. The
#'     items are random effects in this case, because with fixed item effects
#'     the likelihood is unbounded (one residual SD can shrink to zero).
#'
#' The bias for methods `m` and `k` is \eqn{\alpha_m - \alpha_k}, with
#' containment degrees of freedom (the number of method-by-item cells, minus
#' the number of items and methods, plus one; \eqn{(I-1)(M-1)} with complete
#' data). The variance of the difference between single measurements by the
#' two methods on a new item (on the same occasion) is
#' \deqn{V_{mk} = \tau_m^2 + \tau_k^2 + \sigma_m^2 + \sigma_k^2,}
#' plus \eqn{2\omega^2} with `include_omega = TRUE`. The limits of agreement
#' are \eqn{\alpha_m - \alpha_k \pm z \sqrt{V_{mk}}}, with
#' \eqn{z = \Phi^{-1}((1 + \code{pred\_level})/2)}. Note that MethComp uses a
#' multiplier of 2.
#'
#' The uncertainty of the variance components is obtained from the approximate
#' covariance matrix of the variance parameters of the model (the delta
#' method). Confidence intervals for the SDs are Wald intervals on the log
#' scale. \eqn{V_{mk}} is given Satterthwaite degrees of freedom
#' (`df_sd`), \eqn{2V^2/\mathrm{var}(\hat V)}, which account for the
#' covariance between the estimated components. With one measurement per
#' item by two methods these are exactly \eqn{n - 1}.
#'
#' A random component (rater, \eqn{\tau} or \eqn{\omega}) whose SD is
#' estimated at zero (below 0.001 times the smallest residual SD) has no
#' usable uncertainty from the model: the approximate covariance is often
#' unavailable, and otherwise gives meaningless Wald intervals. The model is
#' then refit without such components, which leaves the REML likelihood
#' unchanged, and the covariance of the other components comes from the
#' refit (the original fit is kept for the biases, diagnostics and
#' `keep_model`). A component left out is reported as 0, with interval 0 to
#' `NA` (the rater SD: 0 to the upper limit described below), and adds
#' nothing to the limits or their bounds. A warning names the components.
#'
#' With `interval = "tolerance"`, the prediction limits are
#' \eqn{\alpha_m - \alpha_k \pm t \cdot SEP}, with
#' \eqn{SEP = \sqrt{V_{mk} + SE^2}} (\eqn{SE} the standard error of the bias)
#' and \eqn{t} the \eqn{(1 + \code{pred\_level})/2} quantile of the t
#' distribution with `df_sd` degrees of freedom, as in [tolerance_limit()].
#' They are expected to contain a single new difference with probability
#' `pred_level` (a beta-expectation tolerance interval), but are not a
#' confidence statement, and they do not depend on `bound_type`.
#'
#' The confidence bounds of the limits follow [agreement_limit()]
#' (`interval = "agreement"`, MOVER bounds) or [tolerance_limit()]
#' (`interval = "tolerance"`, the tolerance limits), with the same meaning of
#' `bound_type`:
#'
#'   - "iu": a one-sided `conf_level` bound on each limit, for an
#'     intersection-union test against a maximal allowable difference.
#'   - "joint": joint confidence for both limits (agreement), or a
#'     `pred_level`-content, `conf_level`-confidence tolerance interval
#'     (tolerance).
#'
#' In simulations with 15 to 60 items, 2 or 3 replicates, 2 or 3 methods, and
#' linked or exchangeable replicates (1000 data sets each; 95% targets), the
#' bias CIs had coverage of 0.94 to 0.96. The one-sided bounds of the limits
#' of agreement had 0.92 to 0.96, the joint bounds 0.91 to 0.95, and the joint
#' tolerance limits 0.92 to 0.96; the lowest values (about 0.91 to 0.93) were
#' with 15 items, where the bounds are slightly liberal. The RC intervals had
#' 0.94 to 0.98, and the RpC intervals 0.93 to 0.95.
#'
#' **Random raters.** With `methods = "random"` the method effects
#' \eqn{\alpha_m} are a random sample from \eqn{N(0, \sigma_\alpha^2)}
#' (MethComp's `random.raters`), crossed with the items, and the item
#' effects are fixed. The bias between two randomly chosen raters is 0, and
#' the variance of the difference between single measurements by two such
#' raters on a new item is
#' \deqn{V = 2(\sigma_\alpha^2 + \bar\tau^2 + \bar\sigma^2),}
#' plus \eqn{2\omega^2} with `include_omega = TRUE`, where
#' \eqn{\bar\tau^2} and \eqn{\bar\sigma^2} are the common variances or the
#' means of the method-specific ones (as in MethComp's `BA.est()`). There is
#' one limits row, \eqn{\pm z \sqrt{V}}. Because \eqn{\sigma_\alpha^2} is
#' estimated from only \eqn{M} rater effects, it has \eqn{M - 1} degrees of
#' freedom. The upper confidence bound for \eqn{V} combines the components
#' by the MOVER, each with its own df (the rater variance with \eqn{M - 1}),
#' rather than one chi-square on the Satterthwaite df, which was liberal
#' when the rater variance dominates; [tdi()] does the same. The bounds of
#' the limits are therefore wide with few raters (a warning is given with
#' fewer than five). The interval for the rater SD writes
#' \eqn{\sigma_\alpha^2} as the variance of the method means (on
#' \eqn{M - 1} df) minus the part due to the items,
#' \eqn{(\bar\tau^2 + \bar\sigma^2 / r) / n}, and combines the two by the
#' MOVER, as for a between-group variance. In simulations (3 to 10 raters,
#' 15 or 30 items, 95% targets) the one-sided bounds of the limits covered
#' 0.94 to 0.96 with 5 or 10 raters when the rater variance dominated and
#' 0.98 to 1.00 otherwise, and the interval for the rater SD covered 0.94 to
#' 0.98 (a chi-square on \eqn{M - 1} df alone covered as little as 0.49 when
#' the rater SD was small). With 3 raters the rater SD is often estimated at
#' zero (up to 19% of data sets); the bounds then leave out the rater
#' variance, which is liberal when a large rater SD is hidden by three
#' similar rater effects (coverage 0.92 with rater SD 0.5 and 30 items).
#' With `interval = "tolerance"` the prediction limits are \eqn{\pm t
#' \sqrt{V}} (the bias is 0 with no estimation error) and the tolerance
#' limits are \eqn{\pm z U}, with \eqn{U} the one-sided `conf_level` upper
#' bound for \eqn{\sqrt{V}} from the MOVER: because the centre is known, a
#' limit is beyond its true value exactly when \eqn{U} is, so the tolerance
#' limits are the same for both `bound_type`s and equal the "iu" bounds of
#' the limits of agreement. `df_sd` (also used for \eqn{t}) is the
#' Satterthwaite df of the components, with the rater variance on
#' \eqn{M - 1} df.
#' Without replicates, the method-by-item variation is absorbed
#' into a common residual SD. Fits with random raters cannot be used for
#' conversion between methods
#' ([method_convert()], `predict()`). The model is fit with
#' [nlme::lme()], the rater effects being a `pdIdent` block for a constant
#' outer group, with the method-by-item and item-by-replicate effects nested
#' in it.
#'
#' With replicates, the repeatability coefficient of method `m`,
#' \eqn{RC_m = z \sqrt{2} \sigma_m} (Carstensen, 2010, ch. 9), is the limit
#' that the absolute difference between two measurements by the method on the
#' same item, under the same conditions, should not exceed with probability
#' `pred_level`. Its confidence limits are those of \eqn{\sigma_m}, scaled.
#' For linked replicates, the reproducibility coefficient
#' \eqn{RpC_m = z \sqrt{2} \sqrt{\sigma_m^2 + \omega^2}} is the same limit for
#' two measurements on different occasions.
#'
#' **Within-item reliability.** With linked replicates, where the true value
#' of an item may change between occasions (repeated measures of a changing
#' quantity, "trending"), \eqn{\omega^2 / (\omega^2 + \sigma_m^2)} is the
#' share of method `m`'s variation within an item that is true change: how
#' well the method follows changes. It is reported for each method and, with
#' method-specific SDs, for the mean of the residual variances ("average",
#' which equals the ICC of item-centred data). The confidence intervals come
#' from the delta method for \eqn{\log(\omega^2 / \sigma^2)}, back-transformed.
#' In simulations (2 or 3 methods, 20 or 50 items, 3 or 6 occasions; 95%
#' intervals) the "average" and common-SD intervals covered 0.93 to 0.96. The
#' per-method intervals covered 0.95 to 1.00 when \eqn{\omega} was
#' comparable to the residual SDs, and 0.93 to 0.99 with three methods, but
#' as little as 0.86 (for the method with the smaller SD) with two methods
#' and \eqn{\omega} four times the smaller SD, where the split of the
#' residual variance between the two methods is weakly identified; prefer
#' the "average" row then. A residual SD at zero gives a
#' reliability of 1 with interval `NA` to 1, and \eqn{\omega} at zero a
#' reliability of 0 with interval 0 to `NA`.
#'
#' A residual SD estimated at zero with linked replicates is noted by
#' `print()`. It happens when the SD is small relative to \eqn{\omega} (in
#' the simulations above, in up to 30% of data sets with two methods), but
#' also when a method does not follow within-item changes with slope 1: the
#' model has no term for that, so the systematic error moves into the other
#' methods' SDs, and the method that tracks poorly can look error-free.
#' Comparing the SDs with each method's known repeatability helps to tell
#' these apart.
#'
#' @return An object of class `simple_mvc`, a list with:
#'
#'   - `bias`: tibble with the bias (`method1` - `method2`) for each pair of
#'     methods, its standard error, df, and confidence limits (with
#'     `methods = "random"`, one row for two random raters, with bias 0).
#'   - `limits`: tibble with, for each pair, the bias, the SD of the
#'     difference (`sd`) and its df (`df_sd`), and the limits
#'     (`lower_loa`, `upper_loa`, `lower_loa_ci`, `upper_loa_ci` for agreement;
#'     the standard error of prediction `sep`, the prediction limits
#'     `lower.PL`, `upper.PL`, and the tolerance limits `lower.TL`,
#'     `upper.TL` for tolerance).
#'   - `varcomp`: tibble of the variance components (`rater` with
#'     `methods = "random"`, `tau`, `omega`, `sigma`), as SDs and
#'     variances, with confidence limits and df.
#'   - `repeatability`: tibble with, for each method, the residual SD
#'     (`sigma`), the repeatability coefficient (`rc`) with confidence limits,
#'     and for linked replicates the reproducibility coefficient (`rpc`) with
#'     confidence limits; `NULL` without replicates. See details.
#'   - `reliability`: tibble with the within-item reliability
#'     (`reliability`) for each method (`method`; `NA` with a common SD) and
#'     the "average", with `omega`, the residual SD (`sigma`) and confidence
#'     limits (`lower`, `upper`); `NULL` without linked replicates. See
#'     details.
#'   - `alpha`: list with the method means averaged over items (`alpha`) and
#'     their covariance matrix (`vcov`); `NULL` with `methods = "random"`.
#'   - `vc_cov`: approximate covariance matrix of the estimated variances in
#'     `varcomp`.
#'   - `diagnostics`: fitted values, Pearson residuals, and predicted random
#'     effects, used by `check()`.
#'   - `data`: the processed data (columns `y`, `y_t` (transformed), `method`,
#'     `id`, `repl`).
#'   - `rep_table`: the number of items by method and number of replicates.
#'   - `transform`: the transformation (functions included).
#'   - `model`: the fitted `lme` or `gls` model (`NULL` if `keep_model =
#'     FALSE`).
#'   - `design`: list describing the design and settings.
#'   - `call`: the matched call, with evaluated settings.
#'
#' @examples
#' # three methods, linked replicates
#' dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
#'                   alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
#'                   sigma = c(0.3, 0.5, 0.4), replicates = "linked",
#'                   seed = 1)
#' fit = method_vc(dat, y = "y", method = "method", id = "id",
#'                 replicate = "repl", replicates = "linked")
#' fit
#'
#' # two methods, one measurement per item (the paired-difference analysis)
#' dat2 = sim_methods(n_items = 40, methods = c("A", "B"), alpha = c(0, 0.3),
#'                    sigma = 0.5, seed = 2)
#' method_vc(dat2, y = "y", method = "method", id = "id")
#'
#' # five raters as a random sample: limits for two randomly chosen raters
#' dat3 = sim_methods(n_items = 30, n_reps = 2, methods = LETTERS[1:5],
#'                    alpha = c(0, 0.5, -0.3, 0.2, 0.1), tau = 0.3,
#'                    sigma = rep(0.4, 5), seed = 3)
#' method_vc(dat3, y = "y", method = "method", id = "id",
#'           replicate = "repl", methods = "random")
#'
#' @references
#' Carstensen, B. (2010). Comparing Clinical Measurement Methods: A Practical
#' Guide. Wiley.
#'
#' Carstensen, B., Simpson, J., & Gurrin, L. C. (2008). Statistical models for
#' assessing agreement in method comparison studies with replicate
#' measurements. The International Journal of Biostatistics, 4(1).
#'
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
#' limits of agreement with multiple observations per individual. Statistical
#' Methods in Medical Research, 22(6), 630-642.
#'
#' @seealso [simple_mvc-methods], [method_convert()], [sim_methods()], [mc_long()],
#'   [agreement_limit()], [tolerance_limit()]
#' @importFrom stats qnorm qt qchisq sigma vcov
#' @export

method_vc = function(data,
                     y,
                     method,
                     id,
                     replicate = NULL,
                     replicates = c("exchangeable", "linked"),
                     tau = c("common", "method"),
                     methods = c("fixed", "random"),
                     sigma = NULL,
                     interval = c("agreement", "tolerance"),
                     pred_level = 0.95,
                     conf_level = 0.95,
                     bound_type = NULL,
                     transform = "none",
                     include_omega = FALSE,
                     keep_model = TRUE,
                     control = nlme::lmeControl(returnObject = TRUE)) {
  replicates = match.arg(replicates)
  tau = match.arg(tau)
  methods = match.arg(methods)
  if (is.null(sigma)) {
    sigma = if (methods == "random") "common" else "method"
  }
  sigma = match.arg(sigma, c("common", "method"))
  interval = match.arg(interval)
  if (is.null(bound_type)) {
    bound_type = if (interval == "agreement") "iu" else "joint"
  }
  bound_type = match.arg(bound_type, c("iu", "joint"))
  for (nm in c("pred_level", "conf_level")) {
    val = get(nm)
    if (!is.numeric(val) || length(val) != 1 || is.na(val) ||
        val <= 0 || val >= 1) {
      stop("`", nm, "` must be a single number between 0 and 1.",
           call. = FALSE)
    }
  }
  if (!is.logical(include_omega) || length(include_omega) != 1 ||
      is.na(include_omega)) {
    stop("`include_omega` must be TRUE or FALSE.", call. = FALSE)
  }

  # data ----
  df = mc_data_org(data = data,
                   y = y,
                   method = method,
                   id = id,
                   replicate = replicate,
                   transform = transform,
                   replicates = replicates)
  tf = attr(df, "transform")
  lv = levels(df$method)

  call2 = match.call()
  call2$y = y
  call2$method = method
  call2$id = id
  call2$replicate = replicate
  call2$replicates = replicates
  call2$tau = tau
  call2$methods = methods
  call2$sigma = sigma
  call2$interval = interval
  call2$pred_level = pred_level
  call2$conf_level = conf_level
  call2$bound_type = bound_type
  call2$transform = tf$name
  call2$include_omega = include_omega
  call2$keep_model = keep_model

  # model ----
  design = mc_vc_design(df, replicates = replicates, tau = tau,
                        methods = methods, sigma = sigma)
  if (include_omega && !design$linked) {
    message("include_omega = TRUE has no effect without linked replicates.")
    include_omega = FALSE
  }
  fit = mc_vc_fit(df, design = design, control = control)

  # variance components ----
  varcomp = mc_vc_extract(fit, design = design, lv = lv)
  vc_unc = mc_vc_delta(fit, design = design, varcomp = varcomp, lv = lv,
                       conf_level = conf_level, warn = FALSE)
  # components at zero: refit without them for the covariance of the rest
  # (the fit itself is kept for the model, diagnostics and biases). Also when
  # apVar exists, since its Wald intervals for a zero component are
  # meaningless (log-SD standard errors of 100 or more).
  rf = mc_vc_refit_boundary(fit, df = df, design = design,
                            varcomp = varcomp, control = control)
  if (!is.null(rf)) {
    vc2 = mc_vc_extract(rf$fit, design = rf$design, lv = lv)
    unc2 = mc_vc_delta(rf$fit, design = rf$design, varcomp = vc2, lv = lv,
                       conf_level = conf_level, warn = FALSE)
    if (unc2$ok) {
      vc_unc = unc2
      design = rf$design
    }
  }
  if (!vc_unc$ok) {
    mc_vc_warn_no_cov()
  }
  varcomp = vc_unc$varcomp
  mc_vc_boundary(varcomp, drop = design$drop)
  design$sigma_zero = mc_vc_sigma_zero(varcomp, design = design)

  # bias and limits ----
  if (methods == "random") {
    alpha = NULL
    bias_tab = mc_vc_bias_random()
  } else {
    alpha = mc_vc_alpha(fit, lv = lv)
    bias_df = mc_vc_bias_df(df)
    bias_tab = mc_vc_bias(alpha, lv = lv, bias_df = bias_df,
                          conf_level = conf_level)
  }
  limits = mc_limits(bias_tab = bias_tab,
                     varcomp = varcomp,
                     cov_v = vc_unc$cov,
                     design = design,
                     interval = interval,
                     pred_level = pred_level,
                     conf_level = conf_level,
                     bound_type = bound_type,
                     include_omega = include_omega)

  repeatability = mc_vc_repeatability(varcomp = varcomp,
                                      cov_v = vc_unc$cov,
                                      design = design,
                                      pred_level = pred_level,
                                      conf_level = conf_level)
  reliability = mc_vc_reliability(varcomp = varcomp,
                                  cov_v = vc_unc$cov,
                                  design = design,
                                  conf_level = conf_level)

  design = c(design,
             list(replicates = replicates,
                  interval = interval,
                  bound_type = bound_type,
                  pred_level = pred_level,
                  conf_level = conf_level,
                  include_omega = include_omega))

  structure(list(bias = bias_tab,
                 limits = limits,
                 varcomp = varcomp,
                 repeatability = repeatability,
                 reliability = reliability,
                 alpha = alpha,
                 vc_cov = vc_unc$cov,
                 diagnostics = mc_vc_diag(fit, df, design),
                 data = df,
                 rep_table = attr(df, "rep_table"),
                 transform = tf,
                 model = if (keep_model) fit else NULL,
                 design = design,
                 call = call2),
            class = "simple_mvc")
}
