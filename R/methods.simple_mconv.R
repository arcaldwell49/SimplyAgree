#' Methods for simple_mconv objects
#'
#' Methods defined for objects returned from [method_convert()].
#'
#' @param x,object Object of class `simple_mconv`, as returned from
#'   [method_convert()].
#' @param digits The number of digits to print.
#' @param parm Which parameters to include: "all" (default), "alpha"
#'   (intercepts), or "beta" (slopes).
#' @param level For `confint`, the confidence level (default: the
#'   `conf_level` of the fit). For `predict`, the coverage of the prediction
#'   limits (default: the `pred_level` of the fit).
#' @param newdata Values of the `from` method on the original scale: a
#'   numeric vector, or a data frame with a column named after the `from`
#'   method.
#' @param from,to Names of the methods to convert from and to. Default: from
#'   the first method to the second.
#' @param type Type of plot. "conversion" (default): the `to` method against
#'   the `from` method on the original scale, with the observed pairs, the
#'   line of identity (dashed), and the conversion with its prediction limits.
#'   "ba": a Bland-Altman plot of the differences (first minus second method
#'   of `pair`) with the bias and prediction limits implied by the model, as
#'   lines that change with the level (linear bias or scaled model). For a
#'   conversion from a [method_vc()] fit, the bias is constant and the plot is
#'   that of the fit (see [simple_mvc-methods]): the bias and limits are
#'   points with their confidence intervals, to the left of the data.
#' @param geom,delta For `type = "ba"`, the geometry used to display the
#'   data points and the maximal allowable difference; see
#'   [simple_mvc-methods].
#' @param pair Character vector of two method names. For
#'   `type = "conversion"`, `c(from, to)`; for `type = "ba"`, the difference
#'   is first minus second. Default is the first two methods.
#' @param ... further arguments passed through, see description of return
#'   value for details.
#'
#' @return
#' \describe{
#'   \item{\code{print}}{Prints the conversion equations (intercept, slope,
#'   and prediction SD) with confidence intervals.}
#'   \item{\code{coef}}{Named vector of the conversion parameters,
#'   `alpha[to|from]` and `beta[to|from]`.}
#'   \item{\code{confint}}{Matrix of confidence limits for the conversion
#'   parameters: percentile intervals from the bootstrap estimates for
#'   `ci_method = "boot"`, otherwise from the standard errors and the
#'   t distribution.}
#'   \item{\code{predict}}{A tibble with the values converted from `from` to
#'   `to` on the original scale: `from_value`, `fit`, and the prediction limits
#'   `lower` and `upper`. Values outside the domain of the transformation give
#'   `NA`.}
#'   \item{\code{plot}}{A ggplot object.}
#'   \item{\code{summary}}{An object of class `summary.simple_mconv` whose
#'   print method shows the fit details (iterations and convergence for the
#'   scaled model, bootstrap failures), the design, the number of pairs used,
#'   the regressions of the differences on the averages (linear bias) or the
#'   per-method intercepts and slopes (scaled), the conversions, and the
#'   variance components.}
#'   \item{\code{check}}{A patchwork of diagnostic plots, as for
#'   [agreement_limit()], with what to look for in each subtitle and the
#'   tests in the captions. Linear bias, for each pair: a normal QQ plot of
#'   the standardized residuals of the regression of the differences on the
#'   averages (Shapiro-Wilk test); the square root of the absolute
#'   standardized residuals against the averages (Breusch-Pagan test; with
#'   `variance = "linear"`, the residuals are divided by the fitted SD); and
#'   the residuals against the averages (RESET test of linearity). The last
#'   two use item means, so replicates are not counted more than once.
#'   Scaled model: normal QQ plots of the standardized residuals of the final
#'   variance component model by method (Shapiro-Wilk tests), the square
#'   root of their absolute values against the fitted true values
#'   (Breusch-Pagan tests), and the intercepts and slopes at each iteration
#'   of the alternating regression. Not available for conversions from
#'   [method_vc()]; use `check()` on that fit.}
#' }
#'
#' @name simple_mconv-methods

### methods for simple_mconv objects

#' @rdname simple_mconv-methods
#' @method print simple_mconv
#' @export

print.simple_mconv <- function(x,
                               digits = 4,
                               ...){
  fi = x$fit_info
  tf = x$transform
  fmt = function(v) ifelse(is.na(v), "", format(round(v, digits), nsmall = 0))
  ci = function(lo, hi) ifelse(is.na(lo), "",
                               paste0("[", fmt(lo), ", ", fmt(hi), "]"))
  cv = x$conversion
  tab = data.frame(To = cv$to,
                   From = cv$from,
                   alpha = fmt(cv$alpha),
                   `alpha CI` = ci(cv$alpha_lower, cv$alpha_upper),
                   beta = fmt(cv$beta),
                   `beta CI` = ci(cv$beta_lower, cv$beta_upper),
                   sd = fmt(cv$sd_pred),
                   check.names = FALSE)
  names(tab)[c(4, 6)] = paste0(fi$conf_level * 100, "% CI")

  title1 = switch(fi$bias,
                  constant = "Conversion Between Methods: Constant Bias",
                  linear = "Conversion Between Methods: Linear Bias",
                  scaled = "Conversion Between Methods: Scaled Model")
  cat(title1, "\n", sep = "")
  cat("y(To) = alpha + beta * y(From); sd = prediction SD",
      if (tf$name != "none") paste0(" (", tf$name, " scale)"), "\n", sep = "")
  if (fi$bias == "linear") {
    cat("Regression of differences on averages, ",
        fi$variance, " SD of the differences\n", sep = "")
  }
  cat("\n")
  print(tab, row.names = FALSE, right = FALSE)
  cat("\n")

  note = switch(
    fi$bias,
    constant = paste0("From method_vc(): slope fixed at 1; intercept CIs ",
                      "are those of the biases"),
    linear = paste0("CIs: approximate (delta method",
                    if (any(x$da$cluster_se))
                      ", cluster-robust by item" else "",
                    ")"),
    scaled = if (fi$ci_method == "boot") {
      paste0("CIs: parametric bootstrap percentile intervals (",
             fi$boot_reps - fi$n_boot_fail, " of ", fi$boot_reps,
             " refits used); approximate, coverage not yet evaluated")
    } else {
      paste0("CIs: delta method, conditional on the predicted true values;",
             "\n  much too narrow (about 55-75% coverage for 95% in ",
             "simulations); see ?method_convert")
    }
  )
  cat(note, "\n", sep = "")
  if (fi$bias == "scaled") {
    cat("Alternating regression: ", fi$iterations, " iterations, ",
        if (isTRUE(fi$converged)) "converged" else "NOT converged",
        " (criterion ", signif(fi$criterion, 3), ")\n", sep = "")
    vc = x$varcomp
    vtab = tidyr::pivot_wider(vc[, c("component", "method", "sd")],
                              names_from = component, values_from = sd)
    vtab = as.data.frame(vtab)
    vtab[-1] = lapply(vtab[-1], fmt)
    names(vtab)[1] = "Method"
    cat("\nVariance components (SD, on each method's scale",
        if (tf$name != "none") paste0(", ", tf$name), ")\n", sep = "")
    print(vtab, row.names = FALSE, right = FALSE)
  }
  if (fi$bias == "linear" && fi$variance == "linear") {
    cat("Prediction SD depends on the level; SD of the differences:\n")
    for (j in seq_len(nrow(x$da))) {
      d = x$da[j, ]
      cat("  ", d$method1, " - ", d$method2, ": ", fmt(d$c_sd),
          if (d$d_sd < 0) " - " else " + ", fmt(abs(d$d_sd)),
          " * average\n", sep = "")
    }
  }
  if (fi$bias == "linear") {
    cat("A slope of the differences on the averages can arise from unequal",
        "measurement errors alone;\n  see ?method_convert\n")
  }
  if (tf$name != "none") {
    cat("Use predict() for conversions on the original scale.\n")
  }
  invisible(x)
}

#' @rdname simple_mconv-methods
#' @method summary simple_mconv
#' @export

summary.simple_mconv <- function(object,
                                 ...){
  pair_n = vapply(object$pairs, nrow, integer(1))
  structure(list(fit_info = object$fit_info,
                 conversion = object$conversion,
                 da = object$da,
                 varcomp = object$varcomp,
                 scaled = object$scaled,
                 pairs_used = data.frame(pair = names(pair_n),
                                         n_pairs = unname(pair_n)),
                 rep_table = attr(object$data, "rep_table"),
                 transform = object$transform),
            class = "summary.simple_mconv")
}

#' @rdname simple_mconv-methods
#' @method print summary.simple_mconv
#' @export

print.summary.simple_mconv <- function(x,
                                       digits = 4,
                                       ...){
  fi = x$fit_info
  cat("Conversion Between Methods\n")
  cat("Bias model: ", fi$bias,
      if (fi$bias == "linear") paste0("; SD of differences: ", fi$variance),
      "\nReplicates: ", fi$replicates,
      "\nPrediction level: ", mc_pct(fi$pred_level), "%; confidence level: ",
      fi$conf_level * 100, "%\n", sep = "")
  if (x$transform$name != "none") {
    cat("Transformation: ", x$transform$name,
        " (estimates on the transformed scale)\n", sep = "")
  }
  if (fi$bias == "scaled") {
    cat("Alternating regression: ", fi$iterations, " iterations; converged: ",
        fi$converged, "; criterion: ", signif(fi$criterion, 3), "\n",
        sep = "")
    cat("Confidence intervals: ", fi$ci_method,
        if (fi$ci_method == "boot")
          paste0(" (", fi$boot_reps, " replicates, ", fi$n_boot_fail,
                 " failed)"), "\n", sep = "")
  }

  cat("\nDesign: number of items by method and number of replicates\n")
  rt = tidyr::pivot_wider(x$rep_table, names_from = n_reps,
                          values_from = n_items, values_fill = 0)
  print(as.data.frame(rt), row.names = FALSE)
  cat("\nPairs of measurements used for each pair of methods\n")
  print(x$pairs_used, row.names = FALSE)

  if (!is.null(x$da)) {
    cat("\nRegressions of differences on averages (D = a + b * A)\n")
    print(as.data.frame(x$da), digits = digits, row.names = FALSE)
  }
  if (!is.null(x$scaled)) {
    cat("\nMethods on the scale of the first method (y = alpha + beta * mu)\n")
    print(as.data.frame(x$scaled$methods), digits = digits,
          row.names = FALSE)
  }
  cat("\nConversions\n")
  print(as.data.frame(x$conversion), digits = digits, row.names = FALSE)
  if (!is.null(x$varcomp)) {
    cat("\nVariance components (SD)\n")
    print(as.data.frame(x$varcomp), digits = digits, row.names = FALSE)
  }
  invisible(x)
}

#' @rdname simple_mconv-methods
#' @method check simple_mconv
#' @export

check.simple_mconv <- function(x,
                               ...){
  fi = x$fit_info
  if (fi$bias == "constant") {
    stop("This conversion was derived from a method_vc() fit; use check() ",
         "on that fit.", call. = FALSE)
  }
  tf_note = if (x$transform$name != "none") {
    paste0(" (", x$transform$name, " scale)")
  } else ""

  if (fi$bias == "linear") {
    ## residuals of the DA regressions, by pair ----
    # normality on all pairs; spread and linearity on item means, so items
    # with replicates are not counted more than once
    da = x$da
    norm_dat = het_dat = lin_dat = list()
    p_reset = numeric(0)
    for (j in seq_len(nrow(da))) {
      lab = paste0(da$method1[j], " - ", da$method2[j])
      pp = x$pairs[[paste0(da$method1[j], "-", da$method2[j])]]
      fit = stats::lm(d ~ avg, data = pp)
      r = stats::residuals(fit)
      if (fi$variance == "linear") {
        # residuals relative to the fitted SD of the differences
        r = r / (da$c_sd[j] + da$d_sd[j] * pp$avg)
        std = r
      } else {
        std = stats::rstandard(fit)
      }
      norm_dat[[j]] = data.frame(pair = lab, std = std)
      im = stats::aggregate(cbind(r, avg, d) ~ id,
                            data = data.frame(id = pp$id, r = r,
                                              avg = pp$avg, d = pp$d),
                            FUN = mean)
      het_dat[[j]] = data.frame(pair = lab, avg = im$avg, r = im$r)
      ft = .fitted_square_test(d ~ avg, data = im)
      lin_dat[[j]] = data.frame(pair = lab, x = im$avg,
                                y = stats::residuals(ft$fit))
      p_reset[lab] = ft$p.value
    }
    labs_pair = paste0(da$method1, " - ", da$method2)
    norm_dat = do.call(rbind, norm_dat)
    het_dat = do.call(rbind, het_dat)
    lin_dat = do.call(rbind, lin_dat)
    norm_dat$pair = factor(norm_dat$pair, levels = labs_pair)
    het_dat$pair = factor(het_dat$pair, levels = labs_pair)
    lin_dat$pair = factor(lin_dat$pair, levels = labs_pair)
    if (length(p_reset) == 1) {
      p_reset = unname(p_reset)
    }
    x_lab = paste0("Average of methods",
                   if (any(da$cluster_se)) " (item means)", tf_note)

    p1 = mc_check_norm(norm_dat$std, norm_dat$pair,
                       y_lab = if (fi$variance == "linear") {
                         "Residuals / fitted SD"
                       } else "Standardized residuals")
    het = mc_check_het(het_dat$avg, het_dat$r, het_dat$pair, x_lab = x_lab)
    p2 = het$plot
    p3 = plot_het(lin_dat, x_lab = x_lab, y_lab = "Residuals") +
      geom_hline(yintercept = 0, colour = "grey50") +
      facet_wrap(~ pair, scales = "free") +
      labs(title = "Linearity",
           subtitle = "Reference line should be flat and horizontal",
           caption = .check_caption("Linearity", "RESET Test", p_reset)) +
      .check_theme()
    if (fi$variance == "constant" && any(het$p.value < 0.05, na.rm = TRUE)) {
      message("The spread of the differences changes with the level for at ",
              "least one pair (Breusch-Pagan p < 0.05); consider ",
              "variance = 'linear' or a transformation.")
    }
    return(patchwork::wrap_plots(p1, p2, p3, ncol = 2) &
             plot_annotation(theme = .check_theme()))
  }

  ## scaled model ----
  dg = x$diagnostics
  x_lab = "Fitted true values (scale of the first method)"
  p1 = mc_check_norm(dg$resid, dg$method)
  p2 = mc_check_het(dg$fitted, dg$resid, dg$method, x_lab = x_lab)$plot
  tr = x$scaled$trace
  tr_long = rbind(
    data.frame(iteration = tr$iteration, method = tr$method,
               panel = "beta (slope on the first method)",
               value = tr$beta),
    data.frame(iteration = tr$iteration, method = tr$method,
               panel = "alpha (intercept)", value = tr$alpha)
  )
  p3 = ggplot(tr_long, aes(x = iteration, y = value, color = method)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~ panel, scales = "free_y") +
    scale_color_viridis_d(option = "C", end = .8) +
    labs(x = "Iteration", y = NULL, color = "",
         title = "Convergence of the Alternating Regression",
         subtitle = "Lines should level off by the last iterations",
         caption = paste0("Convergence \n", fi$iterations,
                          " iterations; final criterion ",
                          signif(fi$criterion, 3), "; ",
                          if (isTRUE(fi$converged)) "converged" else
                            "NOT converged")) +
    theme_bw() +
    .check_theme()
  patchwork::wrap_plots(p1, p2, p3, ncol = 2) &
    plot_annotation(theme = .check_theme())
}

#' @rdname simple_mconv-methods
#' @method coef simple_mconv
#' @export

coef.simple_mconv <- function(object,
                              ...){
  cv = object$conversion
  cv = cv[cv$to != cv$from, ]
  key = paste0("[", cv$to, "|", cv$from, "]")
  c(stats::setNames(cv$alpha, paste0("alpha", key)),
    stats::setNames(cv$beta, paste0("beta", key)))
}

#' @rdname simple_mconv-methods
#' @method confint simple_mconv
#' @export

confint.simple_mconv <- function(object,
                                 parm = c("all", "alpha", "beta"),
                                 level = NULL,
                                 ...){
  parm = match.arg(parm)
  if (is.null(level)) {
    level = object$fit_info$conf_level
  }
  cv = object$conversion
  off = which(cv$to != cv$from)
  cv = cv[off, ]
  key = paste0("[", cv$to, "|", cv$from, "]")
  if (identical(object$fit_info$ci_method, "boot") && !is.null(object$boot)) {
    # percentile intervals from the bootstrap estimates; columns are alpha,
    # beta, and sd_pred for every row of the conversion table, then varcomp
    n_cv = nrow(object$conversion)
    probs = c((1 - level) / 2, (1 + level) / 2)
    pct = function(cols) {
      t(apply(object$boot[, cols, drop = FALSE], 2, stats::quantile,
              probs = probs, na.rm = TRUE, names = FALSE))
    }
    a_ci = pct(off)
    b_ci = pct(n_cv + off)
  } else {
    q = qt((1 + level) / 2, cv$df)
    a_ci = cbind(cv$alpha - q * cv$alpha_se, cv$alpha + q * cv$alpha_se)
    b_ci = cbind(cv$beta - q * cv$beta_se, cv$beta + q * cv$beta_se)
  }
  rownames(a_ci) = paste0("alpha", key)
  rownames(b_ci) = paste0("beta", key)
  out = switch(parm,
               all = rbind(a_ci, b_ci),
               alpha = a_ci,
               beta = b_ci)
  colnames(out) = paste(format(100 * c((1 - level) / 2, (1 + level) / 2),
                               trim = TRUE, scientific = FALSE, digits = 3),
                        "%")
  out
}

#' @rdname simple_mconv-methods
#' @method predict simple_mconv
#' @export

predict.simple_mconv <- function(object,
                                 newdata,
                                 from = NULL,
                                 to = NULL,
                                 level = NULL,
                                 ...){
  lv = levels(object$data$method)
  if (is.null(from)) from = lv[1]
  if (is.null(to)) to = lv[2]
  from = as.character(from)
  to = as.character(to)
  if (!all(c(from, to) %in% lv)) {
    stop("`from` and `to` must be methods in the fit: ",
         paste(lv, collapse = ", "), ".", call. = FALSE)
  }
  if (missing(newdata) || is.null(newdata)) {
    newdata = object$data$y[object$data$method == from]
  }
  if (is.data.frame(newdata)) {
    if (!(from %in% names(newdata))) {
      stop("newdata must have a column named '", from, "'.", call. = FALSE)
    }
    newdata = newdata[[from]]
  }
  if (!is.numeric(newdata)) {
    stop("newdata must be numeric or a data frame.", call. = FALSE)
  }
  mc_conv_grid(object, from = from, to = to, values = newdata, level = level)
}

#' @rdname simple_mconv-methods
#' @method plot simple_mconv
#' @export

plot.simple_mconv <- function(x,
                              type = c("conversion", "ba"),
                              pair = NULL,
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
         conversion = mc_conversion_plot(x, pair = pair),
         ba = mc_conv_ba_plot(x, pair = pair, geom = geom, delta = delta))
}

# predict for simple_mvc ----

#' @rdname simple_mvc-methods
#' @method predict simple_mvc
#' @param newdata,from,to Passed to the `predict` method of
#'   [method_convert()] results: values of the `from` method (original
#'   scale) to convert to the `to` method, with prediction limits.
#' @export

predict.simple_mvc <- function(object,
                               newdata,
                               from = NULL,
                               to = NULL,
                               level = NULL,
                               ...){
  conv = method_convert(object = object,
                        pred_level = object$design$pred_level,
                        conf_level = object$design$conf_level)
  stats::predict(conv, newdata = newdata, from = from, to = to,
                 level = level)
}

#' @rdname model_equation
#' @export
model_equation.simple_mconv <- function(x,
                                        use_coefs = FALSE,
                                        coef_digits = 3,
                                        pair = NULL,
                                        ...) {
  .eq_finish(.eq_simple_mconv(x, pair = pair), use_coefs, coef_digits)
}
