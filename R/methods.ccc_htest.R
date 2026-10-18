#' Methods for ccc_htest objects
#'
#' Methods for objects returned by [ccc_test()]. They print as standard
#' \code{"htest"} objects; `summary()` describes the result in words,
#' `plot()` shows the agreement between the two methods, and `check()` gives
#' diagnostics.
#'
#' @param x,object An object of class \code{ccc_htest}.
#' @param style How `summary()` words the result: `"auto"` (default),
#'   `"hypothesis"`, or `"estimation"`, as for [icc_test()] results (see
#'   [reli_htest-methods]).
#' @param type For `plot()`: `"identity"` (default), the measurements of `y`
#'   against `x` with the line of identity (perfect concordance) and a Deming
#'   regression line, coloured by subject for replicate or nested data; or
#'   `"ba"`, the Bland-Altman plot of the differences against the means
#'   (from [agreement_limit()] with the same data type).
#' @param ... Not used.
#'
#' @details
#' `summary()` describes the estimate and its interval, what was estimated,
#' the design, and how the inference was done, in the hypothesis or
#' estimation style described in [reli_htest-methods]. For simple paired
#' data it also reports Lin's decomposition of the CCC into precision (the
#' Pearson correlation, r) and accuracy (the bias correction factor, C_b):
#' CCC = r x C_b. A low CCC can come from either.
#'
#' `check()` shows the normality and heteroskedasticity of the differences
#' (as for [agreement_limit()] with the same data), and the linearity of the
#' relationship between the methods: the residuals of `y` regressed on `x`
#' against the fitted values, with a test of a quadratic term (Ramsey's
#' RESET test). The CCC, through its precision component, summarises a
#' linear relationship. Proportional bias is not shown: a slope different
#' from 1 between the methods lowers the bias correction factor C_b, so it
#' is part of what the CCC measures rather than a violation of its
#' assumptions. For simple paired data `check()` also shows a Q-Q plot of
#' the squared Mahalanobis distances of the pairs against a chi-square
#' distribution with 2 degrees of freedom: Lin's standard error assumes the
#' two measurements are bivariate normal, and points far above the line
#' suggest they are not. For replicate or nested data the linearity test
#' uses all pairs, which are correlated within subjects, so its p-value is
#' approximate; the U-statistic standard error makes few distributional
#' assumptions, so these panels mainly help interpret the CCC.
#'
#' @return
#' \describe{
#'   \item{\code{summary}}{An object of class \code{summary.ccc_htest} (which
#'   prints like \code{summary.reli_htest}): the description in `text`, the
#'   `style` used, the S-value (`s.value`, when there is a p-value), and the
#'   parts used to build it.}
#'   \item{\code{plot}}{A ggplot2 plot (see `type`).}
#'   \item{\code{check}}{Diagnostic plots combined with patchwork.}
#' }
#'
#' @references
#' Lin, L. I. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. Biometrics, 45(1), 255-268. \doi{10.2307/2532051}
#'
#' Ramsey, J. B. (1969). Tests for specification errors in classical linear
#' least-squares regression analysis. Journal of the Royal Statistical
#' Society: Series B, 31(2), 350-371.
#' \doi{10.1111/j.2517-6161.1969.tb00796.x}
#'
#' @examples
#' data("reps")
#' res <- ccc_test(x = "x", y = "y", data = reps)
#' summary(res)
#' plot(res)
#' plot(res, type = "ba")
#' check(res)
#'
#' @name ccc_htest-methods
NULL

#' @rdname ccc_htest-methods
#' @method summary ccc_htest
#' @export
summary.ccc_htest <- function(object,
                              style = c("auto", "hypothesis", "estimation"),
                              ...) {
  style <- .resolve_summary_style(match.arg(style), !is.null(object$p.value))
  s <- object$settings
  s_value <- if (!is.null(object$p.value)) -log2(object$p.value) else NULL
  result_text <- .htest_result_text(object, style, s_value,
                                    alpha = 1 - s$conf.level)
  text <- paste(result_text,
                .describe_ccc_estimand(object),
                .describe_ccc_design(object),
                .describe_ccc_inference(object))
  structure(list(text = text,
                 style = style,
                 estimate = object$estimate,
                 conf.int = object$conf.int,
                 p.value = object$p.value,
                 s.value = s_value,
                 components = object$components,
                 settings = s),
            class = c("summary.ccc_htest", "summary.reli_htest"))
}

#' @rdname ccc_htest-methods
#' @method plot ccc_htest
#' @import ggplot2
#' @export
plot.ccc_htest <- function(x, type = c("identity", "ba"), ...) {
  type <- match.arg(type)
  if (type == "ba") {
    return(plot(.ccc_loa_fit(x)))
  }
  .plot_ccc_identity(x)
}

#' @rdname ccc_htest-methods
#' @method check ccc_htest
#' @export
check.ccc_htest <- function(x, ...) {
  loa_panels <- .loa_check_panels(.ccc_loa_fit(x))
  plots <- list(loa_panels$p_norm, loa_panels$p_het, .plot_ccc_linearity(x))
  if (x$settings$data_type == "simple") {
    plots <- c(plots, list(.plot_bivariate_qq(x$data$x, x$data$y)))
  }
  wrap_plots(plots, ncol = 2)
}

# Residuals of y on x against the fitted values, with a quadratic (RESET)
# test: the CCC and its precision component (Pearson r) summarise a linear
# relationship between the methods.
.plot_ccc_linearity <- function(x) {
  d <- x$data[stats::complete.cases(x$data[c("x", "y")]), , drop = FALSE]
  tst <- .fitted_square_test(y ~ x, d)
  caption <- paste0(
    "Linearity \n",
    "Quadratic (RESET) test: ", .fmt_test_p(tst$p.value),
    if (x$settings$data_type != "simple") {
      " \n(all pairs; approximate, pairs within subjects are correlated)"
    } else "")
  plot_resid_fitted(stats::fitted(tst$fit), stats::residuals(tst$fit),
                    title = "Linearity",
                    subtitle = "Reference line should be flat and horizontal",
                    caption = caption)
}

# Helpers -----------------------------------------------------------------------

# The agreement_limit() fit for the same data, used by plot(type = "ba") and
# check().
.ccc_loa_fit <- function(x) {
  d <- x$data
  if (x$settings$data_type == "simple") {
    agreement_limit(x = "x", y = "y", data = d, data_type = "simple")
  } else {
    agreement_limit(x = "x", y = "y", id = "id", data = d,
                    data_type = x$settings$data_type)
  }
}

.describe_ccc_estimand <- function(object) {
  s <- object$settings
  pair <- paste0(s$x_name, " and ", s$y_name)
  if (s$data_type == "simple") {
    comp <- object$components
    paste0("The estimate is Lin's concordance correlation coefficient between ",
           pair, ": how closely the pairs fall on the line of identity. It ",
           "combines precision (Pearson r = ", .fmt_num(comp[["r"]]),
           ") and accuracy (bias correction factor C_b = ",
           .fmt_num(comp[["C_b"]]), "), CCC = r x C_b.")
  } else {
    paste0("The estimate is the concordance correlation coefficient between ",
           pair, " for ", if (s$data_type == "reps") "replicate" else "nested",
           " measurements, estimated by U-statistics (King, Chinchilli & ",
           "Carrasco, 2007): how closely the measurements fall on the line ",
           "of identity.")
  }
}

.describe_ccc_design <- function(object) {
  s <- object$settings
  d <- object$data
  if (s$data_type == "simple") {
    paste0("The data had ", s$n, " complete pairs.")
  } else {
    paste0("The data had ", s$n, " subjects (", sum(!is.na(d$x)),
           " measurements of ", s$x_name, " and ", sum(!is.na(d$y)), " of ",
           s$y_name, ").")
  }
}

.describe_ccc_inference <- function(object) {
  s <- object$settings
  tested <- !is.null(object$p.value)
  what <- if (tested) "The interval and z test use" else "The interval uses"
  how <- if (s$data_type == "simple") {
    paste0("Fisher's z transformation with Lin's (1989) delta-method ",
           "standard error, which assumes the two measurements are ",
           "bivariate normal")
  } else {
    "Fisher's z transformation with the U-statistic standard error"
  }
  paste0(what, " ", how, ".")
}

# Identity plot: y against x with the line of identity (perfect concordance)
# and the Deming regression line (equal error variances).
.plot_ccc_identity <- function(x) {
  d <- x$data
  d <- d[stats::complete.cases(d[c("x", "y")]), , drop = FALSE]
  s <- x$settings
  dem <- calc_dem(d$x, d$y, w_i = rep(1, nrow(d)), error.ratio = 1)
  lines <- data.frame(the_int = dem$b0, the_slope = dem$b1)
  lim <- range(c(d$x, d$y))
  by_subject <- s$data_type != "simple"
  p <- ggplot(d, aes(x = x, y = y)) +
    geom_abline(intercept = 0, slope = 1, colour = "grey30") +
    geom_abline(data = lines,
                aes(intercept = the_int, slope = the_slope),
                linetype = "dashed", colour = "#b2182b")
  p <- if (by_subject) {
    d$id <- factor(d$id)
    p + geom_point(data = d, aes(colour = id), size = 1.8, alpha = 0.85) +
      scale_colour_viridis_d(end = 0.9) +
      labs(colour = "Subject")
  } else {
    p + geom_point(colour = "#2166ac", size = 1.8, alpha = 0.85)
  }
  p <- p +
    coord_equal(xlim = lim, ylim = lim) +
    labs(x = paste0("Method: ", s$x_name), y = paste0("Method: ", s$y_name),
         caption = "Solid: line of identity (perfect concordance). Dashed: Deming regression.") +
    .reli_plot_theme()
  if (by_subject && nlevels(d$id) > 15) {
    p <- p + theme(legend.position = "none")
  }
  p
}

# Q-Q plot of squared Mahalanobis distances of the pairs against chi-square(2).
.plot_bivariate_qq <- function(x, y) {
  m <- cbind(x, y)
  d2 <- sort(stats::mahalanobis(m, colMeans(m), stats::cov(m)))
  n <- length(d2)
  q <- stats::qchisq(stats::ppoints(n), df = 2)
  dat <- data.frame(q = q, d2 = d2)
  ggplot(dat, aes(x = q, y = d2)) +
    geom_abline(intercept = 0, slope = 1, colour = "#3aaf85", linewidth = 0.8) +
    geom_point(colour = "#0077B5", shape = 16, size = 2, alpha = 0.8) +
    labs(title = "Bivariate Normality",
         subtitle = "Dots should fall along the line",
         x = "Chi-square (2 df) quantiles",
         y = "Squared Mahalanobis distance",
         caption = "Bivariate normality \nassumed by Lin's standard error") +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
