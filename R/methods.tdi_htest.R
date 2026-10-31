#' Methods for tdi_htest objects
#'
#' Methods for the results of [tdi()].
#'
#' @param x,object A `tdi_htest` object from [tdi()].
#' @param style For `summary()`: `"auto"` (default) describes a test when
#'   `delta` was given and the estimate otherwise; `"hypothesis"` or
#'   `"estimation"` choose the style.
#' @param ... Passed to the default print method.
#'
#' @return `print()` returns `x` invisibly; `summary()` an object with a text
#'   description; `plot()` a ggplot2 Bland-Altman plot with the TDI (dashed),
#'   its upper bound (solid), and `delta` (dotted).
#'
#' @examples
#' data("reps")
#' res <- tdi(agreement_limit(x = "x", y = "y", data = reps), delta = 2,
#'            seed = 1)
#' res
#' summary(res)
#' plot(res)
#'
#' @name tdi_htest-methods
NULL

#' @rdname tdi_htest-methods
#' @method print tdi_htest
#' @export
print.tdi_htest <- function(x, ...) {
  NextMethod()
  s <- x$settings
  tab <- x$table
  if (!is.null(x$cp)) {
    cat("Coverage probability of delta: ", format(signif(x$cp[["CP"]], 4)),
        " (", format(100 * s$conf.level), "% lower bound: ",
        format(signif(x$cp[["lower"]], 4)), ")\n", sep = "")
  }
  cat("Lin's approximation: ", format(signif(unname(x$lin_approx), 4)), "\n",
      sep = "")
  if (nrow(tab) > 1) {
    cat("\nBy row (the estimate and bound above are from row ", s$worst_row,
        ", which has the largest bound):\n", sep = "")
    keep <- intersect(c("panel", "avg", "bias", "sd", "tdi", "tdi_upper",
                        "tdi_ratio", "tdi_upper_ratio", "cp", "cp_lower",
                        "p.value"), names(tab))
    if (all(tab$panel == "all")) keep <- setdiff(keep, "panel")
    if (all(is.na(tab$avg))) keep <- setdiff(keep, "avg")
    print(format(tab[, keep], digits = 4), row.names = FALSE)
  }
  if (!is.null(s$scale_note)) cat(s$scale_note, "\n")
  invisible(x)
}

#' @rdname tdi_htest-methods
#' @method summary tdi_htest
#' @export
summary.tdi_htest <- function(object,
                              style = c("auto", "hypothesis", "estimation"),
                              ...) {
  style <- .resolve_summary_style(match.arg(style), !is.null(object$p.value))
  s <- object$settings
  s_value <- if (!is.null(object$p.value)) -log2(object$p.value) else NULL
  result_text <- .htest_result_text(object, style, s_value,
                                    alpha = 1 - s$conf.level)
  what <- paste0("The TDI is the value that ", format(100 * s$p),
                 "% of the absolute differences between the methods do not ",
                 "exceed",
                 if (s$log_scale) " (as a ratio: differences within a factor of the TDI)" else "",
                 ". It was computed from the bias and SD of a single difference ",
                 "estimated by ", s$source, ", assuming normally distributed ",
                 "differences; the upper bound is a ",
                 switch(s$method,
                        gpq = "generalized pivotal quantity bound.",
                        delta = "delta-method bound on the log scale (Lin, 2000).",
                        tolerance = "tolerance interval bound (Escaramis et al., 2010)."))
  cp_text <- if (!is.null(object$cp)) {
    paste0(" An estimated ", .fmt_num(100 * object$cp[["CP"]]),
           "% of the differences are within delta = ", format(s$delta),
           " (", format(100 * s$conf.level), "% lower bound ",
           .fmt_num(100 * object$cp[["lower"]]), "%).")
  } else ""
  rows_text <- if (nrow(object$table) > 1) {
    paste0(" The TDI was computed for ", nrow(object$table),
           " rows; the result above is for the row with the largest bound.")
  } else ""
  text <- trimws(paste0(result_text, " ", what, cp_text, rows_text))
  structure(list(text = text,
                 style = style,
                 estimate = object$estimate,
                 conf.int = object$conf.int,
                 p.value = object$p.value,
                 s.value = s_value,
                 settings = s),
            class = c("summary.tdi_htest", "summary.reli_htest"))
}

#' @rdname tdi_htest-methods
#' @method plot tdi_htest
#' @import ggplot2
#' @export
plot.tdi_htest <- function(x, ...) {
  s <- x$settings
  pd <- x$plot_data
  tab <- x$table
  rng <- tapply(pd$avg, pd$panel, range, na.rm = TRUE)
  # one line per row: horizontal over the panel's range, or through the
  # values of avg when the bias depends on the level
  lines <- do.call(rbind, lapply(split(tab, tab$panel), function(t) {
    xs <- if (all(is.na(t$avg))) rng[[t$panel[1]]] else t$avg
    rep_n <- if (all(is.na(t$avg))) 2 else 1
    data.frame(panel = t$panel[1],
               avg = xs,
               tdi = rep(t$tdi, each = rep_n),
               upper = rep(t$tdi_upper, each = rep_n))
  }))
  long <- rbind(
    data.frame(lines[, c("panel", "avg")], y = lines$tdi, sign = "+",
               what = "TDI"),
    data.frame(lines[, c("panel", "avg")], y = -lines$tdi, sign = "-",
               what = "TDI"),
    data.frame(lines[, c("panel", "avg")], y = lines$upper, sign = "+",
               what = "Upper bound"),
    data.frame(lines[, c("panel", "avg")], y = -lines$upper, sign = "-",
               what = "Upper bound"))
  p <- ggplot(pd, aes(x = avg, y = delta)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_point(alpha = 0.5) +
    geom_line(data = long,
              aes(x = avg, y = y,
                  group = interaction(sign, what),
                  linetype = what),
              colour = "#3aaf85", linewidth = 0.8) +
    scale_linetype_manual(values = c(TDI = "dashed", `Upper bound` = "solid"),
                          name = NULL) +
    labs(x = "Average of methods",
         y = if (s$log_scale) "Difference (log scale)" else "Difference",
         title = paste0("Total deviation index (p = ", format(s$p), ")"),
         caption = paste0("Upper bound: ", format(100 * s$conf.level),
                          "%, ", s$method)) +
    theme_bw() +
    theme(legend.position = "bottom")
  if (!is.null(s$delta)) {
    d <- if (s$log_scale) log(s$delta) else s$delta
    p <- p + geom_hline(yintercept = c(-d, d), linetype = "dotted",
                        colour = "red")
  }
  if (length(unique(pd$panel)) > 1) {
    p <- p + facet_wrap(~ panel)
  }
  p
}

utils::globalVariables(c("panel", "sign", "what"))
