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
#'   of `pair`) with the bias and prediction limits implied by the model.
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
#'   parameters.}
#'   \item{\code{predict}}{A tibble with the values converted from `from` to
#'   `to` on the original scale: `from_value`, `fit`, and the prediction limits
#'   `lower` and `upper`. Values outside the domain of the transformation give
#'   `NA`.}
#'   \item{\code{plot}}{A ggplot object.}
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
    scaled = paste0("CIs: ", fi$ci_method)
  )
  cat(note, "\n", sep = "")
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
  cv = cv[cv$to != cv$from, ]
  key = paste0("[", cv$to, "|", cv$from, "]")
  q = qt((1 + level) / 2, cv$df)
  a_ci = cbind(cv$alpha - q * cv$alpha_se, cv$alpha + q * cv$alpha_se)
  b_ci = cbind(cv$beta - q * cv$beta_se, cv$beta + q * cv$beta_se)
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
                              ...){
  type = match.arg(type)
  switch(type,
         conversion = mc_conversion_plot(x, pair = pair),
         ba = mc_conv_ba_plot(x, pair = pair))
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
