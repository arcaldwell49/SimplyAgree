#' Methods for eiv_ratio_profile objects
#'
#' Methods for the results of [error_ratio_profile()].
#'
#' @param x An `eiv_ratio_profile` object from [error_ratio_profile()].
#' @param digits Number of significant digits to print.
#' @param type What to plot: `"both"` (default; slope and intercept),
#'   `"slope"` or `"intercept"`.
#' @param ... Not used.
#'
#' @return `print()` returns `x` invisibly; `plot()` a ggplot2 plot of the
#'   estimate (line) and its confidence interval (ribbon) against the error
#'   ratio on the log scale. The dotted lines are the ordinary least squares
#'   fits at the ends of the range (ratio 0, y on x; ratio `Inf`, x on y), the
#'   dashed line is the value under identity, and the vertical line marks the
#'   error ratio of the fitted object.
#'
#' @examples
#' data("ba1986")
#' prof <- error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986))
#' prof
#' plot(prof, type = "slope")
#'
#' @name eiv_ratio_profile-methods
NULL

#' @rdname eiv_ratio_profile-methods
#' @method print eiv_ratio_profile
#' @export
print.eiv_ratio_profile <- function(x, digits = 4, ...) {
  tab <- x$table
  cl <- paste0(format(100 * x$conf.level), "%")
  cat("\nError-ratio profile of the errors-in-variables line\n")
  cat("Ratio: var(", x$x_name, ") / var(", x$y_name, "); n = ", x$n,
      "; fitted ratio: ", format(x$fitted_ratio, digits = digits), "\n\n",
      sep = "")
  # a readable subset of a long grid
  if (nrow(tab) > 20) {
    show <- c(0, 0.01, 0.1, 1, 10, 100, Inf, x$fitted_ratio)
    tab <- tab[sapply(tab$ratio, function(r) any(abs(r - show) <=
                                                   1e-9 * pmax(1, r) |
                                                   (is.infinite(r) &
                                                      is.infinite(show)))), ]
  }
  out <- data.frame(
    ratio = vapply(tab$ratio, format, "", digits = digits),
    slope = format(tab$slope, digits = digits),
    slope.ci = paste0("[", format(tab$slope_lower, digits = digits), ", ",
                      format(tab$slope_upper, digits = digits), "]"),
    intercept = format(tab$intercept, digits = digits),
    intercept.ci = paste0("[", format(tab$intercept_lower, digits = digits),
                          ", ", format(tab$intercept_upper, digits = digits),
                          "]"),
    joint.p = format.pval(tab$joint_p, digits = digits),
    stringsAsFactors = FALSE
  )
  if (x$balanced) {
    out$slope.exact <- ifelse(
      is.na(tab$slope_exact_lower), "unbounded",
      paste0("[", format(tab$slope_exact_lower, digits = digits), ", ",
             format(tab$slope_exact_upper, digits = digits), "]"))
  }
  names(out) <- sub("\\.ci$", paste0(" ", cl, " CI"), names(out))
  print(out, row.names = FALSE)
  cat("\nFull ", cl, " confidence intervals (all ratios):\n", sep = "")
  for (i in seq_len(nrow(x$full))) {
    cat("  ", format(x$full$term[i], width = 15), " [",
        format(x$full$lower[i], digits = digits), ", ",
        format(x$full$upper[i], digits = digits), "]\n", sep = "")
  }
  if (anyNA(x$identity)) {
    cat("Identity (intercept 0, slope 1) is rejected at every ratio.\n")
  } else {
    cat("Identity (intercept 0, slope 1) is not rejected for ratios from ",
        format(x$identity[1], digits = digits), " to ",
        format(x$identity[2], digits = digits), ".\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname eiv_ratio_profile-methods
#' @method plot eiv_ratio_profile
#' @export
plot.eiv_ratio_profile <- function(x, type = c("both", "slope", "intercept"),
                                   ...) {
  type <- match.arg(type)
  tab <- x$table
  one <- function(term, null) {
    d <- data.frame(ratio = tab$ratio, est = tab[[term]],
                    lower = tab[[paste0(term, "_lower")]],
                    upper = tab[[paste0(term, "_upper")]])
    ends <- d[d$ratio == 0 | is.infinite(d$ratio), ]
    d <- d[d$ratio > 0 & is.finite(d$ratio), ]
    p <- ggplot(d, aes(x = ratio)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
      geom_line(aes(y = est)) +
      geom_hline(yintercept = null, linetype = "dashed") +
      scale_x_log10(labels = function(b) vapply(b, format, "")) +
      labs(x = paste0("Error ratio, var(", x$x_name, ") / var(", x$y_name,
                      ")"),
           y = if (term == "slope") "Slope" else "Intercept") +
      theme_bw()
    if (nrow(ends)) {
      p <- p + geom_hline(yintercept = ends$est, linetype = "dotted")
    }
    if (is.finite(x$fitted_ratio) && x$fitted_ratio > 0) {
      p <- p + geom_vline(xintercept = x$fitted_ratio, colour = "grey40")
    }
    p
  }
  if (type == "slope") return(one("slope", 1))
  if (type == "intercept") return(one("intercept", 0))
  patchwork::wrap_plots(one("slope", 1), one("intercept", 0), ncol = 2)
}

utils::globalVariables(c("ratio", "est"))
