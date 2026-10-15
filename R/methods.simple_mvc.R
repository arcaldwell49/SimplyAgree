#' Methods for simple_mvc objects
#'
#' Methods defined for objects returned from [method_vc()].
#'
#' @param x,object Object of class `simple_mvc`, as returned from
#'   [method_vc()].
#' @param digits The number of digits to print.
#' @param type Type of plot. "ba" (default): a Bland-Altman plot of the
#'   differences for one pair of methods, with the bias and limits (shaded
#'   bands show the confidence intervals of the bias and the confidence bounds
#'   of the limits of agreement). "matrix": all pairs, with the methods named
#'   on the diagonal, each pair of methods plotted against each other above
#'   it (identity line dashed; constant-bias conversion line with prediction
#'   limits on the original scale), and Bland-Altman plots below it. "vc":
#'   estimates and confidence intervals of the variance component SDs and the
#'   repeatability coefficients.
#' @param pair Character vector naming the two methods to plot, in the order
#'   of the difference (first minus second). Default is the first pair.
#' @param pair_seed Seed for the random pairing of exchangeable replicates
#'   in the plot. The global random number generator state is restored
#'   afterwards.
#' @param ... further arguments passed through, see description of return
#'   value for details.
#'
#' @return
#' \describe{
#'   \item{\code{print}}{Prints the biases, limits, and variance components.
#'   With `transform = "log"`, biases and limits are shown as ratios.}
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
#'   \item{\code{check}}{A patchwork of diagnostic plots: standardized
#'   residuals against fitted values; normal QQ plots of the residuals by
#'   method and of the predicted random effects; and, for each pair, the
#'   differences against the averages with a regression line. The caption
#'   gives p-values for the slope of the differences on the averages
#'   (level-dependent bias) and of the absolute residuals of that regression
#'   on the averages (level-dependent variance), computed from item means so
#'   replicates are not counted more than once. A message suggests
#'   alternatives when either p < 0.05.}
#' }
#'
#' @name simple_mvc-methods

### methods for simple_mvc objects

#' @rdname simple_mvc-methods
#' @method print simple_mvc
#' @export

print.simple_mvc <- function(x,
                             digits = 4,
                             ...){
  des = x$design
  tf = x$transform
  is_tol = des$interval == "tolerance"
  is_log = tf$name == "log"
  disp = if (is_log) exp else identity
  fmt = function(v) format(round(v, digits), nsmall = 0)
  ci = function(lo, hi) paste0("[", fmt(lo), ", ", fmt(hi), "]")

  b = x$bias
  l = x$limits
  sep = if (is_log) " / " else " - "
  tab = data.frame(Methods = paste0(b$method1, sep, b$method2),
                   Bias = fmt(disp(b$bias)),
                   `Bias CI` = ci(disp(b$lower.CL), disp(b$upper.CL)),
                   check.names = FALSE)
  if (is_tol) {
    tab$`Lower TL` = fmt(disp(l$lower.TL))
    tab$`Upper TL` = fmt(disp(l$upper.TL))
  } else {
    tab$`Lower LoA` = fmt(disp(l$lower_loa))
    tab$`Upper LoA` = fmt(disp(l$upper_loa))
    tab$`LoA CI` = ci(disp(l$lower_loa_ci), disp(l$upper_loa_ci))
  }
  tab$SD = fmt(l$sd)

  ## header ----
  title1 = paste0("Variance Component ",
                  if (is_tol) "Tolerance Limits" else
                    "Limits of Agreement (LoA)",
                  if (is_log) " of the Ratio")
  subtitle1 = paste0(des$pred_level * 100,
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
    switch(des$bound_type,
           joint = paste0("Tolerance limits: with ", conf * 100,
                          "% confidence, at least ", des$pred_level * 100,
                          "% of differences lie within the limits"),
           iu = paste0("Tolerance limits: one-sided ", conf * 100,
                       "% bounds on each limit of agreement; not a joint ",
                       conf * 100, "% interval"))
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
  if (is_log) {
    cat("Bias and limits are ratios (exp of the log-scale estimates);",
        "SD is on the log scale (approximately the CV)\n")
  } else if (tf$name != "none") {
    cat("Estimates are on the ", tf$name, " scale.\n", sep = "")
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
  mc_print_repeat(x, digits = digits)
  invisible(x)
}

# repeatability table (shared by print and summary)
mc_print_repeat = function(x,
                           digits = 4) {
  rp = x$repeatability
  if (is.null(rp)) {
    return(invisible(NULL))
  }
  fmt = function(v) format(round(v, digits), nsmall = 0)
  ci = function(lo, hi) paste0("[", fmt(lo), ", ", fmt(hi), "]")
  tab = data.frame(Method = rp$method,
                   RC = fmt(rp$rc),
                   `RC CI` = ci(rp$rc_lower, rp$rc_upper),
                   check.names = FALSE)
  if ("rpc" %in% names(rp)) {
    tab$RpC = fmt(rp$rpc)
    tab$`RpC CI` = ci(rp$rpc_lower, rp$rpc_upper)
  }
  cat("\nRepeatability Coefficients (", x$design$pred_level * 100, "%",
      if (x$transform$name != "none") paste0(", ", x$transform$name,
                                              " scale"), ")\n", sep = "")
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

  cat("\n", if (des$interval == "tolerance") "Tolerance limits" else
    "Limits of agreement", " (", des$pred_level * 100, "%, ",
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
                            ...){
  type = match.arg(type)
  switch(type,
         ba = mc_ba_plot(x, pair = pair, pair_seed = pair_seed),
         matrix = mc_matrix_plot(x, pair_seed = pair_seed),
         vc = mc_vc_plot(x))
}

#' @rdname simple_mvc-methods
#' @method check simple_mvc
#' @importFrom stats lm coef qqnorm
#' @export

check.simple_mvc <- function(x,
                             pair_seed = NULL,
                             ...){
  dg = x$diagnostics
  lv = levels(x$data$method)
  tf_note = if (x$transform$name != "none") {
    paste0(" (", x$transform$name, " scale)")
  } else ""

  ## 1. residuals vs fitted ----
  p_res = ggplot(dg$resid, aes(x = fitted, y = resid, color = method)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(na.rm = TRUE, alpha = 0.7) +
    scale_color_viridis_d(option = "C", end = .8) +
    labs(x = paste0("Fitted values", tf_note),
         y = "Standardized residuals",
         title = "Residuals vs fitted", color = "") +
    theme_bw()

  ## 2. QQ plot of residuals by method ----
  p_qq = ggplot(dg$resid, aes(sample = resid)) +
    stat_qq(na.rm = TRUE) +
    stat_qq_line(na.rm = TRUE) +
    facet_wrap(~ method) +
    labs(x = "Theoretical quantiles", y = "Standardized residuals",
         title = "Normality of residuals") +
    theme_bw()

  ## 3. QQ plot of predicted random effects ----
  if (is.null(dg$ranef) || nrow(dg$ranef) == 0) {
    p_re = ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "No random effects in this model") +
      theme_void() +
      labs(title = "Normality of random effects")
  } else {
    re = dg$ranef
    re$panel = ifelse(is.na(re$method), re$effect,
                      paste0(re$effect, " (", re$method, ")"))
    p_re = ggplot(re, aes(sample = value)) +
      stat_qq(na.rm = TRUE) +
      stat_qq_line(na.rm = TRUE) +
      facet_wrap(~ panel, scales = "free_y") +
      labs(x = "Theoretical quantiles", y = "Predicted random effect",
           title = "Normality of random effects") +
      theme_bw()
  }

  ## 4. level dependence of the differences, per pair ----
  # tests use item means of the paired differences and averages, so items
  # with replicates are not counted more than once
  prs = mc_pairs(lv)
  pair_dat = list()
  tests = data.frame(pair = character(0), p_bias = numeric(0),
                     p_var = numeric(0))
  for (j in seq_len(nrow(prs))) {
    m = prs$method1[j]
    k = prs$method2[j]
    pp = mc_pair(x$data, m, k, replicates = x$design$replicates,
                 seed = pair_seed)
    lab = paste0(m, " - ", k)
    pair_dat[[j]] = data.frame(pair = lab, avg = pp$avg, d = pp$d)
    im = pp %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(d = mean(d), avg = mean(avg), .groups = "drop")
    p_bias = p_var = NA_real_
    if (nrow(im) >= 4) {
      fit_d = lm(d ~ avg, data = im)
      p_bias = summary(fit_d)$coefficients["avg", 4]
      fit_v = lm(abs(stats::resid(fit_d)) ~ im$avg)
      p_var = summary(fit_v)$coefficients[2, 4]
    }
    tests = rbind(tests, data.frame(pair = lab, p_bias = p_bias,
                                    p_var = p_var))
  }
  pair_dat = do.call(rbind, pair_dat)
  pair_dat$pair = factor(pair_dat$pair, levels = tests$pair)
  cap = paste0(tests$pair, ": slope of difference p = ",
               signif(tests$p_bias, 3),
               "; slope of |residual| p = ", signif(tests$p_var, 3),
               collapse = "\n")
  p_lvl = ggplot(pair_dat, aes(x = avg, y = d)) +
    geom_point(na.rm = TRUE, alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, na.rm = TRUE,
                color = "#0D0887") +
    facet_wrap(~ pair, scales = "free") +
    labs(x = paste0("Average of methods", tf_note),
         y = paste0("Difference", tf_note),
         title = "Level dependence",
         caption = cap) +
    theme_bw()

  if (any(tests$p_bias < 0.05, na.rm = TRUE)) {
    message("The differences change with the level of measurement for at ",
            "least one pair (p < 0.05). A constant bias may not hold; ",
            "consider a conversion with non-constant bias (method_convert()) ",
            "or a transformation. Note that a slope of the differences on ",
            "the averages also arises when the methods have unequal ",
            "variances, since cov(D, A) = (var(y1) - var(y2)) / 2 ",
            "(Bland & Altman, 1999).")
  }
  if (any(tests$p_var < 0.05, na.rm = TRUE)) {
    message("The spread of the differences changes with the level of ",
            "measurement for at least one pair (p < 0.05); consider a ",
            "transformation (e.g., transform = 'log').")
  }

  patchwork::wrap_plots(p_res, p_qq, p_re, p_lvl, ncol = 2)
}
