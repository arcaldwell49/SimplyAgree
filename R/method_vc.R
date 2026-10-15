#' @title Variance Component Limits for Comparing Several Methods
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Fits the variance component model of Carstensen (2010) to measurements of
#' a set of items by two or more methods, with or without replicate
#' measurements, and reports the bias between each pair of methods, the
#' variance components, and limits of agreement (or tolerance limits) for the
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
#'   This is a property of the design, not of the data.
#' @param tau Whether the method-by-item variance is "common" to all methods
#'   (default) or differs by "method". Method-specific values require at
#'   least three methods.
#' @param interval Type of limits: "agreement" (default) for limits of
#'   agreement with confidence bounds, or "tolerance" for tolerance limits.
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
#' \eqn{e_{mir} \sim N(0, \sigma_m^2)} with a separate SD for each method, and,
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
#' The confidence bounds of the limits follow [agreement_limit()]
#' (`interval = "agreement"`, MOVER bounds) or [tolerance_limit()]
#' (`interval = "tolerance"`), with the same meaning of `bound_type`:
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
#' With replicates, the repeatability coefficient of method `m`,
#' \eqn{RC_m = z \sqrt{2} \sigma_m} (Carstensen, 2010, ch. 9), is the limit
#' that the absolute difference between two measurements by the method on the
#' same item, under the same conditions, should not exceed with probability
#' `pred_level`. Its confidence limits are those of \eqn{\sigma_m}, scaled.
#' For linked replicates, the reproducibility coefficient
#' \eqn{RpC_m = z \sqrt{2} \sqrt{\sigma_m^2 + \omega^2}} is the same limit for
#' two measurements on different occasions.
#'
#' @return An object of class `simple_mvc`, a list with:
#'
#'   - `bias`: tibble with the bias (`method1` - `method2`) for each pair of
#'     methods, its standard error, df, and confidence limits.
#'   - `limits`: tibble with, for each pair, the bias, the SD of the
#'     difference (`sd`) and its df (`df_sd`), and the limits
#'     (`lower_loa`, `upper_loa`, `lower_loa_ci`, `upper_loa_ci` for agreement;
#'     `lower.TL`, `upper.TL` for tolerance).
#'   - `varcomp`: tibble of the variance components (`tau`, `omega`,
#'     `sigma`), as SDs and variances, with confidence limits and df.
#'   - `repeatability`: tibble with, for each method, the residual SD
#'     (`sigma`), the repeatability coefficient (`rc`) with confidence limits,
#'     and for linked replicates the reproducibility coefficient (`rpc`) with
#'     confidence limits; `NULL` without replicates. See details.
#'   - `alpha`: list with the method means averaged over items (`alpha`) and
#'     their covariance matrix (`vcov`).
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
  call2$interval = interval
  call2$pred_level = pred_level
  call2$conf_level = conf_level
  call2$bound_type = bound_type
  call2$transform = tf$name
  call2$include_omega = include_omega
  call2$keep_model = keep_model

  # model ----
  design = mc_vc_design(df, replicates = replicates, tau = tau)
  if (include_omega && !design$linked) {
    message("include_omega = TRUE has no effect without linked replicates.")
    include_omega = FALSE
  }
  fit = mc_vc_fit(df, design = design, control = control)

  # variance components ----
  varcomp = mc_vc_extract(fit, design = design, lv = lv)
  vc_unc = mc_vc_delta(fit, design = design, varcomp = varcomp, lv = lv,
                       conf_level = conf_level)
  varcomp = vc_unc$varcomp
  mc_vc_boundary(varcomp)

  # bias and limits ----
  alpha = mc_vc_alpha(fit, lv = lv)
  bias_df = mc_vc_bias_df(df)
  bias_tab = mc_vc_bias(alpha, lv = lv, bias_df = bias_df,
                        conf_level = conf_level)
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
