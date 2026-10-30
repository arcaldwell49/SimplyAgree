#' Methods for simple_mvc objects
#'
#' Methods defined for objects returned from [method_vc()].
#'
#' @param x,object Object of class `simple_mvc`, as returned from
#'   [method_vc()].
#' @param digits The number of digits to print.
#' @param scale For `print`, the scale of the bias, limits, and repeatability
#'   coefficients: "analysis" (default), the scale set by `transform`, or
#'   "original", back-transformed to the units of the measurements at the
#'   level(s) in `at`. Ignored without a transformation. See details.
#' @param at For `print` with `scale = "original"`, the level(s), in original
#'   units, at which to express the results. Default (`NULL`) is the median of
#'   the item averages (computed on the analysis scale and back-transformed).
#'   Supplying `at` implies `scale = "original"`.
#' @param type Type of plot. "ba" (default): a Bland-Altman plot of the
#'   differences for one pair of methods, with the bias and limits drawn as
#'   points to the left of the data, as in [agreement_limit()]: the whiskers
#'   show the confidence interval of the bias and the confidence bounds of the
#'   limits of agreement (with `interval = "tolerance"`, the points are the
#'   prediction limits and the whiskers extend to the tolerance limits).
#'   "matrix": all pairs, with the methods named
#'   on the diagonal, each pair of methods plotted against each other above
#'   it (identity line dashed; constant-bias conversion line with prediction
#'   limits on the original scale), and Bland-Altman plots below it. "vc":
#'   estimates and confidence intervals of the variance component SDs and the
#'   repeatability coefficients.
#' @param pair Character vector naming the two methods to plot, in the order
#'   of the difference (first minus second). Default is the first pair.
#' @param pair_seed Seed for the random pairing of exchangeable replicates
#'   in the plot or in `check()`. The global random number generator state is
#'   restored afterwards. With exchangeable replicates and no seed, the
#'   pairing changes from call to call, and so do the plotted differences and
#'   the `check()` p-values (when items have unequal numbers of replicates by
#'   the two methods); set `pair_seed` for reproducible results.
#' @param geom For `type = "ba"`, the geometry used to display the data
#'   points: "geom_point" (default), "geom_bin2d", "geom_density_2d",
#'   "geom_density_2d_filled", or "stat_density_2d", as in [loa-methods].
#' @param delta For `type = "ba"`, the maximal allowable difference, drawn as
#'   dashed lines: one value (drawn at `delta` and `-delta`, or, for ratios
#'   with `transform = "log"`, at `delta` and `1/delta`) or two values, on
#'   the plotted scale.
#' @param error.ratio For `check`, an optional known ratio of the measurement
#'   error variances of the two methods, var(error of the first) / var(error
#'   of the second), as in [dem_reg()], on the analysis scale. Used only with
#'   two methods and one measurement per item.
#' @param bias_line For `check` with replicates or `error.ratio`, the dashed
#'   line on the level dependence panel: "adjusted" (default; the
#'   error-adjusted slope) or "expected" (the slope expected from unequal
#'   measurement error alone). See "Choosing `bias_line`" in [loa-methods]
#'   for the pros and cons.
#' @param ... further arguments passed through, see description of return
#'   value for details.
#'
#' @details
#' With a transformation, a constant bias and constant limits on the analysis
#' scale correspond, in original units, to a bias and limits that depend on
#' the level. `print(x, scale = "original")` expresses them at a given level
#' \eqn{t} (on the analysis scale): a difference \eqn{d} between the methods,
#' centered on that level, is
#' \eqn{h(d) = g^{-1}(t + d/2) - g^{-1}(t - d/2)} in original units, where
#' \eqn{g^{-1}} is the inverse transformation. For a fixed level, \eqn{h} is
#' increasing in \eqn{d}, so the limits, their confidence bounds, and the
#' prediction and tolerance limits keep their coverage and confidence exactly, and the
#' back-transformed bias is the median difference at that level (not the
#' mean). The repeatability coefficients are converted in the same way. The
#' SDs and variance components stay on the analysis scale. Differences are
#' centered on the level of the two measurements, as on the x-axis of a
#' Bland-Altman plot, unlike `predict()`, which starts from a measurement by
#' one method.
#'
#' @return
#' \describe{
#'   \item{\code{print}}{Prints the biases, limits, and variance components.
#'   With `transform = "log"`, biases and limits are shown as ratios; with
#'   `scale = "original"`, in original units at the level(s) in `at`.}
#'   \item{\code{coef}}{Named vector: method means averaged over items
#'   (`alpha_<method>`) and the variance component SDs (`tau`, `omega`,
#'   `sigma_<method>`, on the transformed scale).}
#'   \item{\code{vcov}}{Covariance matrix of the method means.}
#'   \item{\code{summary}}{An object of class `summary.simple_mvc` whose print
#'   method shows the design (number of items by method and number of
#'   replicates), biases, variance components with confidence limits and df,
#'   limits, and repeatability coefficients.}
#'   \item{\code{confint}}{Matrix of confidence limits for the biases and the
#'   variance component SDs.}
#'   \item{\code{plot}}{A ggplot object (`type = "ba"` or `"vc"`) or a
#'   patchwork object (`type = "matrix"`).}
#'   \item{\code{check}}{A patchwork of diagnostic plots, as for
#'   [agreement_limit()], with what to look for in each subtitle and the
#'   tests in the captions: normal QQ plots of the standardized residuals by
#'   method and of the predicted random effects (Shapiro-Wilk tests, or
#'   Kolmogorov-Smirnov with 5000 or more values); and, for each pair, the
#'   homogeneity of variance of the differences (square root of the absolute
#'   standardized differences against the averages, Breusch-Pagan test) and
#'   proportional bias (centred differences against the averages, with a
#'   regression line). The tests for each pair use item means, so replicates
#'   are not counted more than once. With replicates, the test of the slope
#'   is corrected for unequal measurement error (and, with
#'   `tau = "method"`, unequal method-by-item variances), and the
#'   error-adjusted slope is drawn dashed; see "Proportional bias diagnostic"
#'   in [loa-methods]. A message suggests alternatives when the slope or the
#'   Breusch-Pagan test has p < 0.05.
#'   The proportional bias results are attached as the attribute
#'   \code{"prop_bias"}, one row per pair (named in \code{group}); \code{beta}
#'   is the scale of the second method of the pair relative to the first.}
#' }
#'
#' @name simple_mvc-methods

### methods for simple_mvc objects

#' @rdname simple_mvc-methods
#' @method print simple_mvc
#' @export

print.simple_mvc <- function(x,
                             digits = 4,
                             scale = c("analysis", "original"),
                             at = NULL,
                             ...){
  if (!is.null(at) && missing(scale)) {
    scale = "original"
  }
  scale = match.arg(scale)
  des = x$design
  tf = x$transform
  is_tol = des$interval == "tolerance"
  orig = scale == "original" && tf$name != "none"
  is_log = tf$name == "log" && !orig
  disp = if (is_log) exp else identity
  fmt = function(v) format(round(v, digits), nsmall = 0)
  ci = function(lo, hi) paste0("[", fmt(lo), ", ", fmt(hi), "]")

  if (orig) {
    l = mc_original_scale(x, at = at)
    b = l
  } else {
    b = x$bias
    l = x$limits
  }
  sep = if (is_log) " / " else " - "
  tab = data.frame(Methods = paste0(b$method1, sep, b$method2),
                   check.names = FALSE)
  if (orig) {
    tab$At = fmt(l$at)
  }
  tab$Bias = fmt(disp(b$bias))
  tab$`Bias CI` = ci(disp(b$lower.CL), disp(b$upper.CL))
  if (is_tol) {
    tab$`Prediction Interval` = ci(disp(l$lower.PL), disp(l$upper.PL))
    tab$`Tolerance Limits` = ci(disp(l$lower.TL), disp(l$upper.TL))
  } else {
    tab$`Lower LoA` = fmt(disp(l$lower_loa))
    tab$`Upper LoA` = fmt(disp(l$upper_loa))
    tab$`LoA CI` = ci(disp(l$lower_loa_ci), disp(l$upper_loa_ci))
  }
  if (!orig) {
    tab$SD = fmt(l$sd)
  }

  ## header ----
  title1 = paste0("Variance Component ",
                  if (is_tol) "Prediction and Tolerance Limits" else
                    "Limits of Agreement (LoA)",
                  if (is_log) " of the Ratio")
  subtitle1 = paste0(mc_pct(des$pred_level),
                     if (is_tol) "% Content @ " else "% LoA @ ",
                     des$conf_level * 100, "% Confidence")
  design_txt = switch(
    des$model_type,
    single_2 = "One measurement per item and method",
    single_m = paste0("One measurement per item and method; ",
                      "method-specific residual SDs"),
    reps = paste0(if (des$linked) "Linked" else "Exchangeable",
                  " replicates; ",
                  if (des$tau == "method") "method-specific" else "common",
                  " method-by-item variance (tau)")
  )
  design_txt = paste0(design_txt, " (", des$n_items, " items, ",
                      des$n_methods, " methods)")
  conf = des$conf_level
  bound_note = if (is_tol) {
    paste0("Prediction interval: expected to contain a single new ",
           "difference with probability ", mc_pct(des$pred_level), "%\n",
           switch(des$bound_type,
           joint = paste0("Tolerance limits: with ", conf * 100,
                          "% confidence, at least ", mc_pct(des$pred_level),
                          "% of differences lie within the limits"),
           iu = paste0("Tolerance limits: one-sided ", conf * 100,
                       "% bounds on each limit of agreement; not a joint ",
                       conf * 100, "% interval")))
  } else {
    switch(des$bound_type,
           iu = paste0("LoA CI: one-sided ", conf * 100,
                       "% bounds (outer ends of ", (1 - 2 * (1 - conf)) * 100,
                       "% CIs) for an intersection-union test;",
                       "\n  not a joint ", conf * 100,
                       "% interval for the LoA"),
           joint = paste0("LoA CI: outer ends of ", conf * 100,
                          "% CIs; at least ", conf * 100,
                          "% joint confidence for both LoA"))
  }

  cat(title1, "\n", sep = "")
  cat(subtitle1, "\n", sep = "")
  cat(design_txt, "\n", sep = "")
  if (tf$name != "none") {
    cat("Transformation: ", tf$name, "\n", sep = "")
  }
  cat("\n")
  print(tab, row.names = FALSE, right = FALSE)
  cat("\n")
  cat(bound_note, "\n", sep = "")
  if (des$include_omega) {
    cat("SD includes the item-by-replicate variation (include_omega = TRUE)\n")
  }
  if (orig) {
    cat("Original scale (back-transformed from ", tf$name, ") at ",
        if (attr(l, "at_median")) {
          paste0(fmt(l$at[1]), ", the median of the item averages")
        } else "the levels in 'At'",
        ";\n  differences centered on that level; Bias is the median ",
        "difference there\n", sep = "")
  } else if (is_log) {
    cat("Bias and limits are ratios (exp of the log-scale estimates);",
        "SD is on the log scale (approximately the CV)\n")
  } else if (tf$name != "none") {
    cat("Estimates are on the ", tf$name, " scale; ",
        "print(x, scale = \"original\") gives them in original units.\n",
        sep = "")
  }

  ## variance components ----
  vc = x$varcomp
  vc_tab = data.frame(
    Component = ifelse(is.na(vc$method), vc$component,
                       paste0(vc$component, " (", vc$method, ")")),
    SD = fmt(vc$sd),
    CI = ci(vc$lower, vc$upper),
    check.names = FALSE
  )
  names(vc_tab)[3] = paste0(conf * 100, "% CI")
  cat("\nVariance Components (SD", if (tf$name != "none")
    paste0(", ", tf$name, " scale"), ")\n", sep = "")
  print(vc_tab, row.names = FALSE, right = FALSE)
  if (des$model_type == "single_2") {
    cat("sigma: residual SD of the paired-difference model",
        "(SD of the differences / sqrt(2))\n")
  }
  mc_print_repeat(x, digits = digits,
                  rp = if (orig) attr(l, "repeatability") else NULL)
  invisible(x)
}

# repeatability table (shared by print and summary); `rp` replaces the
# stored table, e.g. with values on the original scale (with column `at`)
mc_print_repeat = function(x,
                           digits = 4,
                           rp = NULL) {
  orig = !is.null(rp)
  if (!orig) {
    rp = x$repeatability
  }
  if (is.null(rp)) {
    return(invisible(NULL))
  }
  fmt = function(v) format(round(v, digits), nsmall = 0)
  ci = function(lo, hi) paste0("[", fmt(lo), ", ", fmt(hi), "]")
  tab = data.frame(Method = rp$method, check.names = FALSE)
  if (orig) {
    tab$At = fmt(rp$at)
  }
  tab$RC = fmt(rp$rc)
  tab$`RC CI` = ci(rp$rc_lower, rp$rc_upper)
  if ("rpc" %in% names(rp)) {
    tab$RpC = fmt(rp$rpc)
    tab$`RpC CI` = ci(rp$rpc_lower, rp$rpc_upper)
  }
  scale_txt = if (orig) {
    ", original scale"
  } else if (x$transform$name != "none") {
    paste0(", ", x$transform$name, " scale")
  }
  cat("\nRepeatability Coefficients (", mc_pct(x$design$pred_level), "%",
      scale_txt, ")\n", sep = "")
  print(tab, row.names = FALSE, right = FALSE)
  cat("RC: limit for the difference between two measurements by a method",
      "under the same conditions\n")
  if ("rpc" %in% names(rp)) {
    cat("RpC: the same limit across replicate occasions",
        "(includes the item-by-replicate variation)\n")
  }
  invisible(NULL)
}

#' @rdname simple_mvc-methods
#' @method summary simple_mvc
#' @export

summary.simple_mvc <- function(object,
                               ...){
  structure(list(design = object$design,
                 rep_table = object$rep_table,
                 bias = object$bias,
                 varcomp = object$varcomp,
                 limits = object$limits,
                 repeatability = object$repeatability,
                 transform = object$transform,
                 call = object$call),
            class = "summary.simple_mvc")
}

#' @rdname simple_mvc-methods
#' @method print summary.simple_mvc
#' @export

print.summary.simple_mvc <- function(x,
                                     digits = 4,
                                     ...){
  des = x$design
  cat("Variance Component Model for Method Comparison\n")
  cat("Model: ",
      switch(des$model_type,
             single_2 = "paired differences (gls, common residual SD)",
             single_m = paste0("method effects, random items, ",
                               "method-specific residual SDs (lme)"),
             reps = paste0("method and item effects, random method-by-item",
                           if (des$linked) " and item-by-replicate",
                           " effects, method-specific residual SDs (lme)")),
      "\n", sep = "")
  if (x$transform$name != "none") {
    cat("Transformation: ", x$transform$name,
        " (all estimates on the transformed scale)\n", sep = "")
  }

  cat("\nDesign: number of items by method and number of replicates\n")
  rt = tidyr::pivot_wider(x$rep_table, names_from = n_reps,
                          values_from = n_items, values_fill = 0)
  print(as.data.frame(rt), row.names = FALSE)

  cat("\nBias (method1 - method2)\n")
  print(as.data.frame(x$bias), digits = digits, row.names = FALSE)

  cat("\nVariance components (SD)\n")
  print(as.data.frame(x$varcomp), digits = digits, row.names = FALSE)

  cat("\n", if (des$interval == "tolerance")
    "Prediction and tolerance limits" else
    "Limits of agreement", " (", mc_pct(des$pred_level), "%, ",
    des$bound_type, " bounds at ", des$conf_level * 100,
    "% confidence)\n", sep = "")
  print(as.data.frame(x$limits), digits = digits, row.names = FALSE)

  if (!is.null(x$repeatability)) {
    cat("\nRepeatability\n")
    print(as.data.frame(x$repeatability), digits = digits, row.names = FALSE)
  }
  invisible(x)
}

#' @rdname simple_mvc-methods
#' @method confint simple_mvc
#' @param parm Which parameters to include: "all" (default), "bias", or
#'   "sd" (variance component SDs).
#' @param level For `confint`, the confidence level, by default the `conf_level` used in
#'   [method_vc()]; other levels are recomputed from the stored standard
#'   errors and degrees of freedom. For `predict`, the coverage of the
#'   prediction limits (default: the `pred_level` of the fit).
#' @export

confint.simple_mvc <- function(object,
                               parm = c("all", "bias", "sd"),
                               level = NULL,
                               ...){
  parm = match.arg(parm)
  if (is.null(level)) {
    level = object$design$conf_level
  }
  b = object$bias
  vc = object$varcomp

  qt_b = qt((1 + level) / 2, b$df)
  bias_ci = cbind(b$bias - qt_b * b$se, b$bias + qt_b * b$se)
  rownames(bias_ci) = paste0("bias_", b$method1, "-", b$method2)

  if (object$design$model_type == "single_2") {
    # exact chi-square interval
    sd_ci = cbind(sqrt(vc$df * vc$var / qchisq((1 + level) / 2, vc$df)),
                  sqrt(vc$df * vc$var / qchisq((1 - level) / 2, vc$df)))
  } else {
    # Wald on the log-SD scale: var(log v) = 2 / df
    se_log_sd = sqrt(2 / vc$df) / 2
    q = qnorm((1 + level) / 2)
    sd_ci = cbind(vc$sd * exp(-q * se_log_sd), vc$sd * exp(q * se_log_sd))
  }
  rownames(sd_ci) = ifelse(is.na(vc$method), vc$component,
                           paste0(vc$component, "_", vc$method))

  out = switch(parm,
               all = rbind(bias_ci, sd_ci),
               bias = bias_ci,
               sd = sd_ci)
  colnames(out) = paste(format(100 * c((1 - level) / 2, (1 + level) / 2),
                               trim = TRUE, scientific = FALSE, digits = 3),
                        "%")
  out
}

#' @rdname simple_mvc-methods
#' @method coef simple_mvc
#' @export

coef.simple_mvc <- function(object,
                            ...){
  vc = object$varcomp
  vc_nm = ifelse(is.na(vc$method), vc$component,
                 paste0(vc$component, "_", vc$method))
  alpha = object$alpha$alpha
  c(stats::setNames(alpha, paste0("alpha_", names(alpha))),
    stats::setNames(vc$sd, vc_nm))
}

#' @rdname simple_mvc-methods
#' @method vcov simple_mvc
#' @export

vcov.simple_mvc <- function(object,
                            ...){
  V = object$alpha$vcov
  dimnames(V) = list(paste0("alpha_", rownames(V)),
                     paste0("alpha_", colnames(V)))
  V
}

#' @rdname simple_mvc-methods
#' @method plot simple_mvc
#' @export

plot.simple_mvc <- function(x,
                            type = c("ba", "matrix", "vc"),
                            pair = NULL,
                            pair_seed = NULL,
                            geom = c("geom_point",
                                     "geom_bin2d",
                                     "geom_density_2d",
                                     "geom_density_2d_filled",
                                     "stat_density_2d"),
                            delta = NULL,
                            ...){
  type = match.arg(type)
  geom = match.arg(geom)
  switch(type,
         ba = mc_ba_plot(x, pair = pair, pair_seed = pair_seed,
                         geom = geom, delta = delta),
         matrix = mc_matrix_plot(x, pair_seed = pair_seed),
         vc = mc_vc_plot(x))
}

#' @rdname simple_mvc-methods
#' @method check simple_mvc
#' @importFrom stats lm coef qqnorm
#' @export

check.simple_mvc <- function(x,
                             pair_seed = NULL,
                             error.ratio = NULL,
                             bias_line = c("adjusted", "expected"),
                             ...){
  bias_line = match.arg(bias_line)
  .pm_check_ratio(error.ratio)
  dg = x$diagnostics
  lv = levels(x$data$method)
  tf_note = if (x$transform$name != "none") {
    paste0(" (", x$transform$name, " scale)")
  } else ""

  ## 1. normality of the residuals, by method ----
  rs = dg$resid
  if (x$design$model_type == "single_2") {
    # with one measurement by each of two methods, the residuals of the two
    # methods are equal and opposite: one per item, for the difference
    rs = rs[rs$method == lv[1], ]
    rs$method = paste0(lv[1], " - ", lv[2])
  }
  p_norm = mc_check_norm(rs$resid, rs$method,
                         y_lab = "Standardized residuals")

  ## 2. normality of the predicted random effects ----
  if (is.null(dg$ranef) || nrow(dg$ranef) == 0) {
    p_re = ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "No random effects in this model") +
      theme_void() +
      labs(title = "Normality of Random Effects")
  } else {
    re = dg$ranef
    re$panel = ifelse(is.na(re$method), re$effect,
                      paste0(re$effect, " (", re$method, ")"))
    p_re = mc_check_norm(re$value, re$panel,
                         title = "Normality of Random Effects",
                         y_lab = "Predicted random effect",
                         scales = "free_y")
  }

  ## 3. and 4. differences against the averages, per pair ----
  # tests use item means of the paired differences and averages, so items
  # with replicates are not counted more than once. With replicates, the
  # slope test is corrected for unequal measurement error (and, with
  # tau = "method", unequal method-by-item variances); see
  # prop_bias_helpers.R.
  mt = x$design$model_type
  if (!is.null(error.ratio) && mt != "single_2") {
    message("error.ratio is only used with two methods and one measurement ",
            "per item and is ignored here",
            if (mt == "reps") {
              " (the error variances are estimated from the replicates)."
            } else ".")
    error.ratio = NULL
  }
  vc = x$varcomp
  tau_var = function(m) {
    tv = vc[vc$component == "tau", ]
    if (nrow(tv) == 0) 0 else if (all(is.na(tv$method))) tv$var[1] else
      tv$var[tv$method == m]
  }
  prs = mc_pairs(lv)
  pair_dat = list()
  adj_lines = list()
  res = list()
  for (j in seq_len(nrow(prs))) {
    m = prs$method1[j]
    k = prs$method2[j]
    pp = mc_pair(x$data, m, k, replicates = x$design$replicates,
                 seed = pair_seed)
    lab = paste0(m, " - ", k)
    r = if (!is.null(error.ratio)) {
      .pm_known_ratio(pp$y1, pp$y2, error.ratio = error.ratio)
    } else {
      .pm_slope_test(pp$y1, pp$y2, pp$id,
                     correct = mt == "reps",
                     extra_cov = tau_var(m) - tau_var(k),
                     extra_var = tau_var(m) + tau_var(k))
    }
    res[[lab]] = r
    im = r$data
    pair_dat[[j]] = data.frame(pair = lab, mean = im$mean,
                               resid = im$diff - mean(im$diff))
    line2 = .pm_line(r, bias_line)
    if (!is.null(line2)) {
      adj_lines[[j]] = data.frame(pair = lab, slope = line2,
                                  intercept = -line2 * mean(im$mean))
    }
  }
  pair_dat = do.call(rbind, pair_dat)
  pair_dat$pair = factor(pair_dat$pair, levels = names(res))
  x_lab = paste0("Average of methods",
                 if (mt == "reps") " (item means)", tf_note)

  het = mc_check_het(pair_dat$mean, pair_dat$resid, pair_dat$pair,
                     x_lab = x_lab)
  p_het = het$plot
  p_var = het$p.value

  adj_lines = do.call(rbind, adj_lines)
  if (!is.null(adj_lines)) {
    adj_lines$pair = factor(adj_lines$pair, levels = names(res))
  }
  p_bias = plot_bias(pair_dat,
                     adj_data = adj_lines,
                     adj_label = .pm_line_label(bias_line),
                     subtitle = .pm_subtitle(bias_line, !is.null(adj_lines)),
                     y_lab = paste0("Difference (centred)", tf_note),
                     x_lab = x_lab) +
    facet_wrap(~ pair, scales = "free") +
    labs(caption = paste0("Proportional Bias \n",
                          paste(vapply(names(res), function(nm) {
                            .pm_caption(res[[nm]], nm, bias_line = bias_line)
                          }, character(1)), collapse = " \n")))

  p_bias_p = vapply(res, function(r) r$p.value, numeric(1))
  p_unc = vapply(res, function(r) r$p.uncorrected, numeric(1))
  corrected = vapply(res, function(r) isTRUE(r$corrected), logical(1))
  sig = !is.na(p_bias_p) & p_bias_p < 0.05
  if (any(sig & !corrected)) {
    .pm_message(design = "single", which = names(res)[sig & !corrected],
                alt = paste0("consider a conversion with non-constant bias ",
                             "(method_convert())"))
  }
  if (any(sig & corrected)) {
    .pm_message(design = if (is.null(error.ratio)) "reps" else "ratio",
                which = names(res)[sig & corrected],
                alt = paste0("consider the scaled model ",
                             "(method_convert(bias = 'scaled'))"))
  }
  explained = corrected & !sig & !is.na(p_unc) & p_unc < 0.05 &
    is.null(error.ratio)
  if (any(explained)) {
    .pm_message_explained(which = names(res)[explained])
  }
  if (any(p_var < 0.05, na.rm = TRUE)) {
    message("The spread of the differences changes with the level of ",
            "measurement for at least one pair (Breusch-Pagan p < 0.05); ",
            "consider a transformation (e.g., transform = 'log').")
  }

  p = patchwork::wrap_plots(p_norm, p_re, p_het, p_bias, ncol = 2) &
    plot_annotation(theme = .check_theme())
  attr(p, "prop_bias") = .pm_table(res)
  p
}

#' @rdname model_equation
#' @export
model_equation.simple_mvc <- function(x,
                                      use_coefs = FALSE,
                                      coef_digits = 3,
                                      ...) {
  .eq_finish(.eq_simple_mvc(x), use_coefs, coef_digits)
}
