#' Methods for eiv_ratio_profile objects
#'
#' Methods for the results of [error_ratio_profile()].
#'
#' @param x An `eiv_ratio_profile` object from [error_ratio_profile()].
#' @param digits Number of significant digits to print.
#' @param type For `print()`: `"xy"` (default; the line of y on x, with the
#'   joint test of identity) or `"md"` (the line of the differences on the
#'   averages in the Bland-Altman plot, CBLS). For `plot()`: `"both"`
#'   (default; slope and intercept of the line of y on x), `"slope"`,
#'   `"intercept"`, `"joint"` (joint confidence regions of the intercept and
#'   slope at several ratios) or `"md"` (slope and intercept of the CBLS line
#'   of the differences on the averages).
#' @param ratios For `plot(type = "joint")`, the error ratios whose regions
#'   are drawn. Default: 0, 0.01, 0.1, 1, 10, 100, `Inf` and the ratio of
#'   the fitted object.
#' @param n_points Number of points on each region's boundary.
#' @param ... Not used.
#'
#' @return `print()` returns `x` invisibly. `plot()` returns a ggplot2 plot.
#'   For `"both"`, `"slope"`, `"intercept"` and `"md"`: the estimate (line)
#'   and its confidence interval (ribbon) against the error ratio on the log
#'   scale; the dotted lines are the fits at the ends of the range (ratio 0,
#'   OLS of y on x; ratio `Inf`, OLS of x on y), the dashed line is the value
#'   under identity (slope 1 and intercept 0; 0 and 0 for the differences),
#'   and the vertical line marks the error ratio of the fitted object. For
#'   `"joint"`: the joint confidence region of the intercept and slope at
#'   each ratio (Sadler or Wald, as `joint_method` of the profile), coloured
#'   by ratio, the path of the estimates over all the profile's ratios
#'   (grey), and identity (cross). The regions show how the conclusion
#'   about identity depends on the assumed error ratio; a region unbounded
#'   in the slope is not drawn (noted in the caption).
#'
#' @examples
#' data("ba1986")
#' prof <- error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986))
#' prof
#' print(prof, type = "md")
#' plot(prof, type = "slope")
#' plot(prof, type = "joint")
#'
#' @name eiv_ratio_profile-methods
NULL

#' @rdname eiv_ratio_profile-methods
#' @method print eiv_ratio_profile
#' @export
print.eiv_ratio_profile <- function(x, digits = 4, type = c("xy", "md"),
                                    ...) {
  type <- match.arg(type)
  tab <- x$table
  cl <- paste0(format(100 * x$conf.level), "%")
  cat("\nError-ratio profile of the errors-in-variables line",
      if (type == "md") " (Bland-Altman plot, CBLS)", "\n", sep = "")
  cat("Ratio: var(", x$x_name, ") / var(", x$y_name, "); n = ", x$n,
      "; fitted ratio: ", format(x$fitted_ratio, digits = digits), "\n",
      sep = "")
  if (type == "md") {
    cat("Line: ", x$x_name, " - ", x$y_name, " = intercept + slope * mean\n",
        sep = "")
  }
  cat("\n")
  # a readable subset of a long grid
  if (nrow(tab) > 20) {
    show <- c(0, 0.01, 0.1, 1, 10, 100, Inf, x$fitted_ratio)
    tab <- tab[sapply(tab$ratio, function(r) any(abs(r - show) <=
                                                   1e-9 * pmax(1, r) |
                                                   (is.infinite(r) &
                                                      is.infinite(show)))), ]
  }
  ci <- function(lo, up) {
    paste0("[", format(lo, digits = digits), ", ",
           format(up, digits = digits), "]")
  }
  pre <- if (type == "md") "md_" else ""
  col <- function(nm) tab[[paste0(pre, nm)]]
  out <- data.frame(
    ratio = vapply(tab$ratio, format, "", digits = digits),
    slope = format(col("slope"), digits = digits),
    slope.ci = ci(col("slope_lower"), col("slope_upper")),
    intercept = format(col("intercept"), digits = digits),
    intercept.ci = ci(col("intercept_lower"), col("intercept_upper")),
    stringsAsFactors = FALSE
  )
  if (type == "xy") {
    out$joint.p <- format.pval(tab$joint_p, digits = digits)
    if (x$balanced) {
      out$slope.exact <- ifelse(
        is.na(tab$slope_exact_lower), "unbounded",
        ci(tab$slope_exact_lower, tab$slope_exact_upper))
    }
  }
  names(out) <- sub("\\.ci$", paste0(" ", cl, " CI"), names(out))
  print(out, row.names = FALSE)
  full <- if (type == "md") x$full_md else x$full
  cat("\nFull ", cl, " confidence intervals (all ratios):\n", sep = "")
  for (i in seq_len(nrow(full))) {
    cat("  ", format(full$term[i], width = 15), " [",
        format(full$lower[i], digits = digits), ", ",
        format(full$upper[i], digits = digits), "]\n", sep = "")
  }
  jm <- if (identical(x$joint_method, "wald")) "Wald" else "Sadler"
  if (anyNA(x$identity)) {
    cat("Identity (intercept 0, slope 1) is rejected at every ratio (",
        jm, " joint test).\n", sep = "")
  } else {
    cat("Identity (intercept 0, slope 1) is not rejected for ratios from ",
        format(x$identity[1], digits = digits), " to ",
        format(x$identity[2], digits = digits), " (", jm,
        " joint test).\n", sep = "")
  }
  if (type == "md" && jm == "Sadler") {
    # the criterion is the same for the two lines (the CBLS residual is a
    # multiple of the BLS residual, with the same multiple in its variance)
    cat("(The Sadler test of the CBLS line against (0, 0) is the same ",
        "test.)\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname eiv_ratio_profile-methods
#' @method plot eiv_ratio_profile
#' @export
plot.eiv_ratio_profile <- function(x,
                                   type = c("both", "slope", "intercept",
                                            "joint", "md"),
                                   ratios = NULL,
                                   n_points = 100,
                                   ...) {
  type <- match.arg(type)
  if (type == "joint") {
    return(.erp_plot_joint(x, ratios = ratios, n_points = n_points))
  }
  tab <- x$table
  pre <- if (type == "md") "md_" else ""
  one <- function(term, null) {
    nm <- paste0(pre, term)
    d <- data.frame(ratio = tab$ratio, est = tab[[nm]],
                    lower = tab[[paste0(nm, "_lower")]],
                    upper = tab[[paste0(nm, "_upper")]])
    ends <- d[d$ratio == 0 | is.infinite(d$ratio), ]
    d <- d[d$ratio > 0 & is.finite(d$ratio), ]
    lab <- if (term == "slope") "Slope" else "Intercept"
    if (type == "md") lab <- paste0(lab, " (differences on means)")
    p <- ggplot(d, aes(x = ratio)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
      geom_line(aes(y = est)) +
      geom_hline(yintercept = null, linetype = "dashed") +
      scale_x_log10(labels = function(b) vapply(b, format, "")) +
      labs(x = paste0("Error ratio, var(", x$x_name, ") / var(", x$y_name,
                      ")"),
           y = lab) +
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
  if (type == "md") {
    return(patchwork::wrap_plots(one("slope", 0), one("intercept", 0),
                                 ncol = 2))
  }
  patchwork::wrap_plots(one("slope", 1), one("intercept", 0), ncol = 2)
}

# joint confidence regions at several error ratios
.erp_plot_joint <- function(x, ratios = NULL, n_points = 100) {
  if (is.null(x$data)) {
    stop("The profile does not store its data; rerun error_ratio_profile().")
  }
  if (is.null(ratios)) {
    ratios <- c(0, 0.01, 0.1, 1, 10, 100, Inf, x$fitted_ratio)
  }
  if (!is.numeric(ratios) || length(ratios) < 1 || anyNA(ratios) ||
      any(ratios < 0)) {
    stop("ratios must be non-negative numbers (0 and Inf allowed).")
  }
  ratios <- sort(unique(ratios))
  jm <- if (is.null(x$joint_method)) "wald" else x$joint_method
  lab_r <- vapply(ratios, format, "", digits = 3)
  regs <- list()
  skipped <- character(0)
  for (i in seq_along(ratios)) {
    f <- .erp_fit(x$data, ratios[i])
    rg <- suppressWarnings(.erp_region(x$data, f, jm, x$conf.level,
                                       n_points = n_points))
    if (is.null(rg)) {
      skipped <- c(skipped, lab_r[i])
    } else {
      rg$ratio <- lab_r[i]
      regs[[length(regs) + 1]] <- rg
    }
  }
  if (!length(regs)) {
    stop("The joint confidence regions are unbounded at every ratio.")
  }
  regs <- do.call(rbind, regs)
  regs$ratio <- factor(regs$ratio, levels = lab_r)
  path <- x$table[, c("ratio", "intercept", "slope")]
  cap <- paste0(format(100 * x$conf.level), "% joint regions (",
                if (jm == "wald") "Wald" else "Sadler", "); grey: estimates ",
                "over all ratios; cross: identity")
  if (length(skipped)) {
    cap <- paste0(cap, "\nUnbounded in the slope, not drawn: ratio ",
                  paste(skipped, collapse = ", "))
  }
  ggplot() +
    geom_path(data = path, aes(x = intercept, y = slope), colour = "grey60") +
    geom_path(data = regs, aes(x = intercept, y = slope, colour = ratio),
              linewidth = 0.7) +
    geom_point(aes(x = 0, y = 1), shape = 4, size = 3, stroke = 1.2) +
    scale_colour_viridis_d(option = "D", end = 0.9,
                           name = paste0("Error ratio\nvar(", x$x_name,
                                         ") / var(", x$y_name, ")")) +
    labs(x = "Intercept", y = "Slope", caption = cap) +
    theme_bw()
}

utils::globalVariables(c("ratio", "est", "intercept", "slope"))
