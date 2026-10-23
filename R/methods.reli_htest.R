#' Methods for reli_htest objects
#'
#' Methods for objects returned by [icc_test()], [reli_err_test()],
#' [reli_facet_test()] and
#' [mdc_test()]. They
#' print as standard
#' \code{"htest"} objects; `summary()` describes the result in words,
#' `plot()` shows the data, and `check()` gives model diagnostics.
#'
#' @param x,object An object of class \code{reli_htest}.
#' @param style How `summary()` words the result: `"auto"` (default),
#'   `"hypothesis"`, or `"estimation"`. `"auto"` uses `"hypothesis"` when the
#'   result has a p-value (a `null.value` was given) and `"estimation"`
#'   otherwise. See Details.
#' @param type For `plot()`, the plot to draw: `"profile"` (default), each
#'   subject's measurements across the items in grey with the item means and
#'   their 95% confidence intervals (adjusted for subjects when measurements
#'   are missing); `"subjects"`, the subjects ordered by their mean, with
#'   each subject's measurements, coloured by item, and their range, which
#'   compares the spread between subjects with the scatter within subjects
#'   (what the ICC summarises); `"items"`, the deviation of each measurement
#'   from its subject's mean, by item, which shows systematic differences
#'   between items (error for an agreement ICC, but not for a consistency
#'   ICC); `"pairs"`, each pair of items plotted against each other with
#'   the line of identity; or, for [reli_facet_test()] results, `"components"`,
#'   the share of each variance component. For [reli_facet_test()] results the
#'   profile, items and pairs plots use the subject-by-item cell means.
#' @param ... Not used.
#'
#' @details
#' `summary()` describes the estimate and its interval, what was estimated,
#' the design, and how the inference was done, in one of two styles:
#'
#' - **`"hypothesis"`** (Neyman-Pearson; as in the TOSTER package): the test
#'   decision at \eqn{\alpha = 1 -} `conf.level`, i.e. whether the null
#'   hypothesis can be rejected. Requires a p-value; without one, `summary()`
#'   falls back to `"estimation"` with a message.
#' - **`"estimation"`** (following Greenland and colleagues): the interval is
#'   described as a compatibility interval, the range of values most
#'   compatible with the data under the model, with no decision and no
#'   \eqn{\alpha}. If there is a p-value, it is converted to an S-value
#'   (surprisal), \eqn{s = -\log_2 p}: the bits of information the data
#'   provide against the tested hypothesis, comparable to the surprise of
#'   getting s heads in a row from s tosses of a fair coin.
#'
#' Qualitative labels such as "good" or "excellent" are never attached to an
#' ICC; see [icc_test()] for why.
#'
#' **Diagnostics.** The two-way model assumes that subject and item effects
#' add. A subject-by-item interaction, for example items (raters, devices,
#' occasions) that differ in scale, is absorbed into the error: it lowers
#' the consistency ICC and inflates the SEM. `check()` tests for it with
#' Mandel's (1961) column-regression test: each item gets its own slope on
#' the estimated subject effects, and the k - 1 extra slopes are F-tested.
#' The panel plots the residuals of the additive model against the
#' estimated subject effect with one line per item, so items whose lines
#' slope are those that differ in scale. Mandel's test includes the
#' alternative of Tukey's (1949) one-degree-of-freedom test (scale growing
#' with the item mean, the multiplicative pattern) and also detects items
#' that differ in scale but not in location, which Tukey's test misses; for
#' two items the two tests are identical. The covariates are built from the
#' fitted additive model, as in Milliken and Graybill (1970), so the F test
#' remains exact with missing measurements. In simulations (30 subjects, 3
#' items, 3 missing cells, additive normal data), its type I error was
#' 0.0525 at the 0.05 level. It uses the fixed-effects fit (subjects and
#' items as factors) even when the ICC came from REML, and it has little
#' power with few subjects. A rising within-subject SD against the subject
#' mean means the error grows with the size of the measurement, so a
#' relative error ([reli_err_test()] with `scale = "log"`) describes it
#' better.
#'
#' For [reli_facet_test()] results, `check()` shows the residuals of the full
#' model (deviations from the cell means), the additivity panel on the
#' subject-by-item cell means, the within-cell SD by item with Bartlett's
#' test of equal repeat error across items (the intra-rater coefficient
#' pools it over items), the within-cell SD against the cell mean, and, with
#' occasions, each item's mean by occasion (shifts and rater-specific drift).
#' `summary()` adds the table of variance components.
#'
#' @references
#' Mandel, J. (1961). Non-additivity in two-way analysis of variance.
#' Journal of the American Statistical Association, 56(296), 878-888.
#' \doi{10.1080/01621459.1961.10482132}
#'
#' Milliken, G. A., & Graybill, F. A. (1970). Extensions of the general
#' linear hypothesis model. Journal of the American Statistical Association,
#' 65(330), 797-807. \doi{10.1080/01621459.1970.10481125}
#'
#' Tukey, J. W. (1949). One degree of freedom for non-additivity.
#' Biometrics, 5(3), 232-242. \doi{10.2307/3001938}
#'
#' Greenland, S. (2019). Valid p-values behave exactly as they should: Some
#' misleading criticisms of p-values and their resolution with S-values. The
#' American Statistician, 73(sup1), 106-114.
#' \doi{10.1080/00031305.2018.1529625}
#'
#' Rafi, Z., & Greenland, S. (2020). Semantic and cognitive tools to aid
#' statistical science: replace confidence and significance by compatibility
#' and surprise. BMC Medical Research Methodology, 20, 244.
#' \doi{10.1186/s12874-020-01105-9}
#'
#' @return
#' \describe{
#'   \item{\code{summary}}{An object of class \code{summary.reli_htest}: a
#'   list with the description in `text`, the `style` used, the S-value
#'   (`s.value`, when there is a p-value) and the parts used to build it. It
#'   prints as a paragraph.}
#'   \item{\code{plot}}{A ggplot2 plot (see `type`).}
#'   \item{\code{check}}{Diagnostic plots combined with patchwork: normality
#'   and heteroskedasticity of the model residuals; for two-way models,
#'   additivity (residuals of the additive subjects + items model against
#'   the estimated subject effect, one line per item, with Mandel's test for
#'   non-additivity); and the within-subject SD against the subject mean (on
#'   the scale the model was fitted). See Details.}
#' }
#'
#' @examples
#' sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6), J2 = c(2, 1, 4, 1, 5, 2),
#'                  J3 = c(5, 3, 6, 2, 6, 4), J4 = c(8, 2, 8, 6, 9, 7))
#' res <- icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf)
#' summary(res)
#' plot(res)
#' plot(res, type = "subjects")
#' check(res)
#'
#' # A test against a threshold, in both styles
#' res2 <- icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf,
#'                  null.value = 0.5, alternative = "greater")
#' summary(res2)
#' summary(res2, style = "estimation")
#'
#' @name reli_htest-methods
NULL

#' @rdname reli_htest-methods
#' @method summary reli_htest
#' @export
summary.reli_htest <- function(object,
                               style = c("auto", "hypothesis", "estimation"),
                               ...) {
  style <- .resolve_summary_style(match.arg(style), !is.null(object$p.value))
  s <- object$settings
  s_value <- if (!is.null(object$p.value)) -log2(object$p.value) else NULL
  result_text <- .htest_result_text(object, style, s_value,
                                    alpha = 1 - s$conf.level)

  text <- paste(result_text,
                .describe_estimand(object),
                .describe_design(object$fit),
                .describe_inference(object))

  structure(list(text = text,
                 style = style,
                 estimate = object$estimate,
                 conf.int = object$conf.int,
                 p.value = object$p.value,
                 s.value = s_value,
                 components = object$components,
                 settings = s),
            class = "summary.reli_htest")
}

#' @rdname reli_htest-methods
#' @method print summary.reli_htest
#' @export
print.summary.reli_htest <- function(x, ...) {
  cat(strwrap(x$text), sep = "\n")
  if (is.data.frame(x$components)) {
    comp <- x$components
    level <- format(100 * attr(comp, "conf.level"))
    tab <- data.frame(Component = comp$component,
                      Variance = .fmt_num(comp$variance),
                      CI = paste0("[", .fmt_num(comp$lower), ", ",
                                  .fmt_num(comp$upper), "]"),
                      SD = .fmt_num(comp$sd),
                      Percent = formatC(comp$percent, digits = 1,
                                        format = "f"))
    names(tab)[3] <- paste0(level, "% CI")
    cat("\nVariance components:\n")
    print(tab, row.names = FALSE, right = FALSE)
  }
  invisible(x)
}

#' @rdname reli_htest-methods
#' @method plot reli_htest
#' @import ggplot2
#' @export
plot.reli_htest <- function(x,
                            type = c("profile", "subjects", "items", "pairs",
                                     "components"),
                            ...) {
  type <- match.arg(type)
  df <- x$data
  repeats <- identical(x$settings$statistic, "facet")
  if (type == "components") {
    if (!repeats) {
      stop("type = \"components\" is available for reli_facet_test() results.",
           call. = FALSE)
    }
    return(.plot_components(x$components))
  }
  # with repeat measurements, the profile, item and pair plots use the cell
  # means and the subjects plot every measurement
  if (repeats && type != "subjects") {
    df <- .cell_mean_data(df)
  }
  switch(type,
         profile = .plot_profile(df),
         subjects = .plot_subjects(df),
         items = .plot_items(df),
         pairs = .plot_pairs(df))
}

#' @rdname reli_htest-methods
#' @method check reli_htest
#' @export
check.reli_htest <- function(x, ...) {
  if (identical(x$settings$statistic, "facet")) {
    return(.check_facet(x))
  }
  checks <- check_reli(list(lmer = x$fit$fit))
  # on the scale the model was fitted (log values for scale = "log")
  values <- x$data$values
  if (identical(x$settings$scale, "log")) {
    values <- log(values)
  }
  p_err <- .plot_error_vs_mean(x$data$id, values,
                               log_scale = identical(x$settings$scale, "log"))
  # additivity only makes sense with crossed items (two-way model)
  p_add <- if (identical(x$fit$model, "twoway")) {
    .plot_additivity(data.frame(values = values, id = x$data$id,
                                items = x$data$items))
  } else NULL
  extra <- Filter(Negate(is.null), list(p_add, p_err))
  if (length(extra) == 0) {
    return(checks)
  }
  out <- checks
  for (p in extra) {
    out <- out + p
  }
  out + patchwork::plot_layout(ncol = 2)
}

# Residuals of the additive two-way model against the estimated subject
# effect, with one line per item, and Mandel's test for non-additivity
# (items with different slopes, i.e. different scales). A subject-by-item
# interaction is absorbed into the error by the ICC model, lowering the
# consistency ICC and inflating the SEM.
.plot_additivity <- function(df) {
  df <- df[stats::complete.cases(df), , drop = FALSE]
  df$id <- droplevels(factor(df$id))
  df$items <- droplevels(factor(df$items))
  tst <- .mandel_test(df)
  d <- tst$data
  d$residual <- stats::residuals(tst$fit)
  ggplot(d, aes(x = subject_effect, y = residual, colour = items)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(shape = 16, size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
    scale_colour_viridis_d(end = 0.85) +
    labs(title = "Additivity",
         subtitle = "Lines should be flat (items on the same scale)",
         x = "Estimated subject effect", y = "Residuals", colour = "Item",
         caption = paste0("Subject-by-item interaction \n",
                          "Mandel's test (items differ in scale): ",
                          .fmt_test_p(tst$p.value))) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Diagnostics for repeat measurements: residuals of the full model (deviations
# from the cell means for ANOVA), additivity of the cell means, the
# within-cell SD by item (the intra-rater coefficient pools sigma2_e over
# items), and the within-cell SD against the cell mean.
.check_facet <- function(x) {
  checks <- check_reli(list(lmer = x$fit$fit))
  values <- x$data$values
  log_scale <- identical(x$settings$scale, "log")
  if (log_scale) {
    values <- log(values)
  }
  d <- data.frame(values = values, id = x$data$id, items = x$data$items)
  cell <- interaction(d$id, d$items, drop = TRUE)
  p_add <- .plot_additivity(.cell_mean_data(d))
  p_items <- .plot_within_by_item(d, cell)
  p_err <- .plot_error_vs_mean(cell, values, log_scale = log_scale,
                               unit = "cell")
  p_occ <- if (!is.null(x$data$occ)) {
    .plot_occasions(data.frame(values = values, items = x$data$items,
                               occ = x$data$occ))
  } else NULL
  out <- checks
  for (p in Filter(Negate(is.null), list(p_add, p_items, p_err, p_occ))) {
    out <- out + p
  }
  out + patchwork::plot_layout(ncol = 2)
}

# Mean by occasion for each item: a common shift is the occasion effect
# (sigma2_o), lines that are not parallel the item-by-occasion interaction
# (sigma2_io, rater-specific drift).
.plot_occasions <- function(d) {
  m <- stats::aggregate(values ~ items + occ, data = d, FUN = mean)
  ggplot(m, aes(x = occ, y = values, colour = items, group = items)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    scale_colour_viridis_d(end = 0.85) +
    labs(title = "Occasions",
         subtitle = "Parallel lines: no rater-specific drift",
         x = "Occasion", y = "Mean", colour = "Item",
         caption = "Mean of each item on each occasion") +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Long data with one row per observed subject-by-item cell (the cell mean).
.cell_mean_data <- function(df) {
  m <- stats::aggregate(values ~ items + id, data = df, FUN = mean)
  m$items <- factor(m$items, levels = levels(df$items))
  m$id <- factor(m$id, levels = levels(df$id))
  m[order(m$items, m$id), c("values", "items", "id")]
}

# Within-cell SDs by item, with Bartlett's test of equal within-cell
# variances across items (each item's variance pooled over its cells).
.plot_within_by_item <- function(d, cell) {
  cells <- split(d$values, cell)
  n_c <- lengths(cells)
  keep <- n_c >= 2
  sds <- vapply(cells[keep], stats::sd, numeric(1))
  item_of <- d$items[match(names(cells)[keep], as.character(cell))]
  sd_df <- data.frame(items = item_of, sd = sds)
  ss <- tapply((n_c[keep] - 1) * sds^2, item_of, sum)
  nu <- tapply(n_c[keep] - 1, item_of, sum)
  ss <- ss[nu > 0]
  nu <- nu[nu > 0]
  caption <- "Within-cell SD by item"
  if (length(nu) >= 2) {
    s2 <- ss / nu
    s2p <- sum(ss) / sum(nu)
    K <- (sum(nu) * log(s2p) - sum(nu * log(s2))) /
      (1 + (sum(1 / nu) - 1 / sum(nu)) / (3 * (length(nu) - 1)))
    p <- stats::pchisq(K, length(nu) - 1, lower.tail = FALSE)
    caption <- paste0(caption, " \nBartlett's test (equal repeat error ",
                      "across items): ", .fmt_test_p(p))
  }
  ggplot(sd_df, aes(x = items, y = sd)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey92") +
    geom_jitter(width = 0.08, height = 0, colour = "#0077B5", size = 1.3) +
    labs(title = "Repeat error by item",
         subtitle = "Boxes should be similar",
         x = "Item", y = "Within-cell SD", caption = caption) +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Variance components as horizontal bars (share of the variance of a single
# measurement), with the estimate and interval in the label.
.plot_components <- function(comp) {
  comp$component <- factor(comp$component, levels = rev(comp$component))
  comp$label <- paste0(formatC(comp$percent, digits = 1, format = "f"), "%")
  ggplot(comp, aes(x = percent, y = component)) +
    geom_col(fill = "#2166ac", width = 0.6) +
    geom_text(aes(label = label), hjust = -0.15, size = 3.5) +
    scale_x_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
    labs(x = "Share of the variance of a single measurement (%)", y = NULL,
         caption = "Negative estimates are shown as 0.") +
    .reli_plot_theme()
}

# Plot helpers ------------------------------------------------------------------

.reli_plot_theme <- function() {
  theme_bw() + theme(panel.grid.minor = element_blank())
}

# Item means adjusted for subjects (the average over all subjects of the fitted
# two-way model), with 95% CIs; equal to the raw item means with complete data.
.adjusted_item_means <- function(df) {
  mod <- stats::lm(values ~ id + items, data = df)
  grid <- expand.grid(id = levels(df$id), items = levels(df$items))
  grid$id <- factor(grid$id, levels = levels(df$id))
  grid$items <- factor(grid$items, levels = levels(df$items))
  X <- stats::model.matrix(~ id + items, data = grid)
  keep <- !is.na(stats::coef(mod))
  V <- stats::vcov(mod)
  t_crit <- stats::qt(0.975, stats::df.residual(mod))
  do.call(rbind, lapply(levels(df$items), function(j) {
    cvec <- colMeans(X[grid$items == j, keep, drop = FALSE])
    est <- sum(cvec * stats::coef(mod)[keep])
    se <- sqrt(drop(t(cvec) %*% V %*% cvec))
    data.frame(items = factor(j, levels = levels(df$items)), values = est,
               lower = est - t_crit * se, upper = est + t_crit * se)
  }))
}

# Default: each subject's measurements across items (grey), with the item means
# and their 95% CIs. Shows systematic differences between items (trial effects)
# against the spread of subjects.
.plot_profile <- function(df) {
  means <- .adjusted_item_means(df)
  ggplot(df, aes(x = items, y = values, group = id)) +
    geom_line(colour = "grey70") +
    geom_point(colour = "grey55", size = 1.3) +
    geom_line(data = means, aes(x = items, y = values, group = 1),
              inherit.aes = FALSE, colour = "#2166ac", linewidth = 0.9) +
    geom_pointrange(data = means,
                    aes(x = items, y = values, ymin = lower, ymax = upper),
                    inherit.aes = FALSE, colour = "#2166ac", linewidth = 0.9,
                    size = 0.5) +
    labs(x = "Item", y = "Measurement",
         caption = "Grey: subjects. Blue: item means with 95% CIs.") +
    .reli_plot_theme()
}

# Subjects ordered by their mean: each subject's measurements (coloured by
# item), the range, and the mean. Compares the spread between subjects with
# the scatter within subjects, which is what the ICC summarises.
.plot_subjects <- function(df) {
  m <- tapply(df$values, df$id, mean)
  ord <- names(sort(m))
  rng <- data.frame(id = factor(names(m), levels = ord),
                    lo = as.vector(tapply(df$values, df$id, min)),
                    hi = as.vector(tapply(df$values, df$id, max)),
                    m = as.vector(m))
  df$id <- factor(df$id, levels = ord)
  many <- length(ord) > 15
  ggplot(df, aes(x = id, y = values)) +
    geom_linerange(data = rng, aes(x = id, ymin = lo, ymax = hi),
                   inherit.aes = FALSE, colour = "grey60", linewidth = 1.1) +
    geom_point(data = rng, aes(x = id, y = m), inherit.aes = FALSE,
               shape = 3, size = 2) +
    geom_point(aes(colour = items), size = 1.8, alpha = 0.85) +
    scale_colour_viridis_d(end = 0.85) +
    labs(x = "Subject (ordered by mean)", y = "Measurement", colour = "Item",
         caption = "+: subject mean. Bar: range of the subject's measurements.") +
    .reli_plot_theme() +
    if (many) {
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
      theme()
    }
}

# Deviation of each measurement from its subject's mean, by item: systematic
# differences between items, which count as error for an agreement ICC but
# not for a consistency ICC.
.plot_items <- function(df) {
  df$deviation <- df$values - stats::ave(df$values, df$id)
  ggplot(df, aes(x = items, y = deviation)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey92") +
    geom_jitter(width = 0.08, height = 0, colour = "#2166ac", size = 1.3) +
    labs(x = "Item", y = "Deviation from subject mean",
         caption = "Boxes away from zero: systematic differences between items.") +
    .reli_plot_theme()
}

# Each pair of items against each other with the line of identity.
.plot_pairs <- function(df) {
  items <- levels(df$items)
  wide <- stats::reshape(df[c("id", "items", "values")], idvar = "id",
                         timevar = "items", direction = "wide")
  names(wide) <- sub("^values\\.", "", names(wide))
  pairs <- utils::combn(items, 2, simplify = FALSE)
  pd <- do.call(rbind, lapply(pairs, function(p) {
    data.frame(pair = paste(p[2], "vs", p[1]),
               x = wide[[p[1]]], y = wide[[p[2]]])
  }))
  pd <- pd[stats::complete.cases(pd), , drop = FALSE]
  pd$pair <- factor(pd$pair, levels = unique(pd$pair))
  lim <- range(df$values)
  ggplot(pd, aes(x = x, y = y)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "grey40") +
    geom_point(colour = "#2166ac") +
    coord_equal(xlim = lim, ylim = lim) +
    facet_wrap(~pair) +
    labs(x = "Earlier item", y = "Later item",
         caption = "Dashed: line of identity (perfect agreement).") +
    .reli_plot_theme()
}

# Within-subject SD against the subject mean (subjects with 2 or more
# measurements). A rising trend means the error grows with the size of the
# measurement, so a relative error (scale = "log") describes it better.
.plot_error_vs_mean <- function(id, values, log_scale = FALSE, unit = "subject") {
  n_i <- tapply(values, id, length)
  keep <- names(n_i)[n_i >= 2]
  if (length(keep) < 3) {
    return(NULL)
  }
  d <- data.frame(mean = as.vector(tapply(values, id, mean)[keep]),
                  sd = as.vector(tapply(values, id, stats::sd)[keep]))
  tau <- suppressWarnings(stats::cor.test(d$mean, d$sd, method = "kendall"))
  ggplot(d, aes(x = mean, y = sd)) +
    geom_point(colour = "#0077B5", shape = 16, size = 2, alpha = 0.8) +
    stat_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.2,
                linewidth = 0.8, colour = "#3aaf85") +
    labs(title = "Error vs Mean",
         subtitle = "Reference line should be flat",
         x = paste0(toupper(substr(unit, 1, 1)), substring(unit, 2), " mean",
                    if (log_scale) " (log scale)" else ""),
         y = paste0("Within-", unit, " SD"),
         caption = paste0("Error vs mean", " \n",
                          "Kendall's tau = ", signif(tau$estimate, 3),
                          ", p = ", signif(tau$p.value, 4),
                          if (!log_scale) "; if rising, consider scale = \"log\"" else "")) +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Description helpers ------------------------------------------------------------

# "auto" is "hypothesis" when there is a p-value and "estimation" otherwise;
# "hypothesis" without a p-value falls back to "estimation" with a message.
.resolve_summary_style <- function(style, tested) {
  if (style == "auto") {
    style <- if (tested) "hypothesis" else "estimation"
  }
  if (style == "hypothesis" && !tested) {
    message("No test was requested (null.value = NULL); ",
            "describing the result in the estimation style.")
    style <- "estimation"
  }
  style
}

# First sentence(s) of a summary: the test decision (hypothesis style) or the
# compatibility interval and S-value (estimation style). Shared by the
# reli_htest and ccc_htest summaries.
.htest_result_text <- function(object, style, s_value, alpha) {
  if (style == "estimation") {
    return(.describe_estimation(object, s_value))
  }
  estimate_text <- paste0(names(object$estimate), " = ",
                          .fmt_num(unname(object$estimate)), ", ",
                          .fmt_interval(object$conf.int, object$alternative))
  significant <- object$p.value < alpha
  stat_text <- if (!is.null(object$statistic)) {
    paste0(names(object$statistic),
           if (!is.null(object$parameter) &&
               !identical(names(object$parameter), "n")) {
             paste0("(", paste(.fmt_df(object$parameter), collapse = ", "), ")")
           } else "",
           " = ", .fmt_num(unname(object$statistic)), ", ")
  } else ""
  alt_text <- switch(object$alternative,
                     two.sided = "not equal to",
                     greater = "greater than",
                     less = "less than")
  paste0(
    "The test is ", if (significant) "" else "not ",
    "statistically significant (", stat_text, .fmt_p(object$p.value), "; ",
    estimate_text, ") at the ", format(alpha), " alpha level. ",
    "The null hypothesis ", if (significant) "can" else "cannot",
    " be rejected: at the desired error rate, it ",
    if (significant) "can" else "cannot", " be stated that the true ",
    names(object$null.value), " is ", alt_text, " ",
    format(unname(object$null.value)), ".")
}

# Estimation style: compatibility interval and, with a p-value, its S-value.
.describe_estimation <- function(object, s_value) {
  name <- names(object$estimate)
  ci <- object$conf.int
  level <- paste0(format(100 * attr(ci, "conf.level")), "%")
  interval <- switch(object$alternative,
                     two.sided = paste0(level, " compatibility interval [",
                                        .fmt_num(ci[1]), ", ",
                                        .fmt_num(ci[2]), "]"),
                     greater = paste0(level, " lower compatibility bound ",
                                      .fmt_num(ci[1])),
                     less = paste0(level, " upper compatibility bound ",
                                   .fmt_num(ci[2])))
  values <- switch(object$alternative,
                   two.sided = "Values in this interval",
                   greater = "Values above this bound",
                   less = "Values below this bound")
  text <- paste0("The estimated ", name, " was ",
                 .fmt_num(unname(object$estimate)), " (", interval, "). ",
                 values, " are the most compatible with the data, given the ",
                 "model and its assumptions.")
  if (is.null(s_value)) {
    return(text)
  }
  null <- format(unname(object$null.value))
  hypothesis <- switch(object$alternative,
                       two.sided = paste0("equal to ", null),
                       greater = paste0("at most ", null),
                       less = paste0("at least ", null))
  heads <- round(s_value)
  coin <- if (heads < 1) {
    "less surprising than a single head from one toss of a fair coin"
  } else {
    paste0("about as surprising as getting ", heads,
           if (heads == 1) " head" else " heads in a row", " from ", heads,
           " toss", if (heads == 1) "" else "es", " of a fair coin")
  }
  approx <- if (isTRUE(object$settings$boot)) " (approximate, from the bootstrap)" else ""
  paste0(text, " Against the hypothesis that the true ", name, " is ",
         hypothesis, ", ", .fmt_p(object$p.value), approx,
         ", an S-value of ", .fmt_num(s_value), " bits of information ",
         "(S = -log2 p): ", coin, ".")
}

.describe_estimand <- function(object) {
  s <- object$settings
  if (identical(s$statistic, "error")) {
    return(.describe_error_estimand(object))
  }
  if (identical(s$statistic, "facet")) {
    return(.describe_facet_estimand(object))
  }
  form <- s$form
  k <- object$fit$k
  what <- if (.icc_is_average(form)) {
    paste0("the reliability of the mean of all k = ", k, " items")
  } else {
    "the reliability of a single measurement"
  }
  treatment <- switch(s$type,
                      consistency = "excluding systematic differences between items",
                      agreement = if (s$model == "oneway") {
                        "with item effects not separable from error"
                      } else {
                        "including systematic differences between items"
                      })
  description <- .icc_description(form)
  description <- paste0(tolower(substr(description, 1, 1)),
                        substring(description, 2))
  paste0("The estimate is the ", description,
         " (McGraw & Wong ", s$label, "; Shrout & Fleiss ",
         .icc_shrout_fleiss(form), "): ", what, ", ", treatment, ".")
}

.describe_error_estimand <- function(object) {
  s <- object$settings
  treatment <- if (s$model == "oneway") {
    "with item effects not separable from error"
  } else if (s$type == "consistency") {
    "excluding systematic differences between items"
  } else {
    "including systematic differences between items"
  }
  what <- switch(
    s$stat,
    SEM = "the typical error of a single measurement",
    SEE = paste0("the standard error of a subject's estimated true score, ",
                 "SD * sqrt(ICC * (1 - ICC)) with ", .icc_label(s$form),
                 " and the SD matched to its denominator"),
    SEP = paste0("the standard error for judging change from a subject's ",
                 "estimated true score, SD * sqrt(1 - ICC^2) with ",
                 .icc_label(s$form), " and the SD matched to its denominator"),
    MDC = .describe_mdc(object))
  description <- .err_description(s$stat, s$model, s$type)
  description <- paste0(tolower(substr(description, 1, 1)),
                        substring(description, 2))
  units <- switch(if (is.null(s$scale)) "raw" else s$scale,
                  raw = "in the units of the measurement",
                  log = paste0("in sympercents (100 times the statistic of ",
                               "the log-transformed measurements, a ",
                               "coefficient of variation)"),
                  mean = paste0("as a percentage of the grand mean (a ",
                                "coefficient of variation)"))
  if (s$stat == "MDC") {
    return(paste0("The estimate is the ", description, ", ", units,
                  ", from the SEM ", treatment, ": ", what, "."))
  }
  paste0("The estimate is the ", description, ", ", units, ": ", what, ", ",
         treatment, ".")
}

# What the MDC is, with its multiplier, the percentage change on the log scale,
# and the reminder that it is an individual threshold.
.describe_mdc <- function(object) {
  m <- object$settings$mdc
  level <- paste0(format(100 * m$level), "%")
  direction <- if (m$sided == "two.sided") "in either direction" else
    "in one stated direction"
  averaged <- if (m$k_avg > 1) {
    paste0(" / sqrt(", m$k_avg, "), for scores that are means of ", m$k_avg,
           " trials")
  } else ""
  pct <- if (!is.null(object$percent)) {
    p <- object$percent
    paste0(" (as a percentage change, an increase of ",
           .fmt_num(p$estimate[1]), "% or a decrease of ",
           .fmt_num(p$estimate[2]), "%)")
  } else ""
  paste0("the smallest change ", direction, " between two measurements of ",
         "one subject that exceeds measurement error with ", level,
         " confidence, ", .fmt_num(m$z), " * sqrt(2) * SEM", averaged, pct,
         ". It applies to an individual's change, not to an average (group) ",
         "effect, which should be judged by its own confidence interval")
}

.describe_facet_estimand <- function(object) {
  s <- object$settings
  spec <- s[c("stat", "facet", "type", "k_avg", "m_avg", "r_avg", "scale")]
  if (s$stat == "MDC") {
    spec$stat <- "SEM"
  }
  fit <- object$fit
  weights <- .facet_weights(fit, spec)
  err <- if (spec$stat == "SEM") weights$var else weights$den - weights$num
  error_effects <- fit$effects[err != 0]
  error <- paste(vapply(error_effects, .facet_effect_label, "",
                        design = fit$design, replicated = fit$replicated),
                 collapse = ", ")
  description <- .facet_description(spec, fit$design)
  description <- paste0(tolower(substr(description, 1, 1)),
                        substring(description, 2))
  units <- switch(s$scale,
                  raw = "",
                  log = " in sympercents (a coefficient of variation)",
                  mean = " as a percentage of the grand mean")
  nested <- if (!is.null(object$nested)) {
    ne <- unname(object$nested$estimate)
    same <- isTRUE(all.equal(ne, unname(object$estimate)))
    paste0(" Treating the occasions as exchangeable repeats would give ",
           names(object$nested$estimate), " = ", .fmt_num(ne),
           if (same) {
             paste0(", the same estimate, but its interval would treat the ",
                    "occasion effects shared by all subjects as independent ",
                    "noise.")
           } else {
             ", with day-to-day shifts counted as repeat error."
           })
  } else ""
  if (s$stat == "MDC") {
    return(paste0("The estimate is the minimal detectable change from the ",
                  description, units, ", with error from: ", error, "; ",
                  .describe_mdc(object), "."))
  }
  paste0("The estimate is the ", description, units, ", with error from: ",
         error, ".", nested)
}

.describe_facet_design <- function(fit) {
  cell <- if (fit$design == "crossed") {
    "subject-by-item-by-occasion cell"
  } else "subject-by-item cell"
  repeats <- if (fit$balanced) {
    paste0(fit$r, " measurement", if (fit$r > 1) "s" else "", " per ", cell)
  } else {
    sprintf("%d of %d cells observed, with a harmonic mean of %s measurements per %s",
            fit$n_cells, as.integer(prod(fit$levels)), .fmt_num(fit$r), cell)
  }
  occ <- if (fit$design == "crossed") {
    paste0(", ", fit$levels[["o"]], " occasions")
  } else ""
  estimator <- if (fit$method == "anova") {
    "analysis of variance (unconstrained)"
  } else {
    "REML (linear mixed model)"
  }
  paste0("The data had ", fit$n, " subjects, ", fit$k, " items", occ, " (",
         fit$N, " measurements, ", repeats, "); variance components were ",
         "estimated by ", estimator, ".")
}

.describe_design <- function(fit) {
  if (inherits(fit, "facet_fit")) {
    return(.describe_facet_design(fit))
  }
  cells <- fit$n * fit$k
  missing_text <- if (fit$n_missing == 0) {
    "none missing"
  } else {
    sprintf("%d of %d subject-by-item cells missing, %.1f%%",
            fit$n_missing, cells, 100 * fit$n_missing / cells)
  }
  estimator <- if (fit$method == "anova") {
    "analysis of variance"
  } else {
    "REML (linear mixed model)"
  }
  paste0("The data had ", fit$n, " subjects and ", fit$k, " items (",
         fit$N, " measurements, ", missing_text,
         "); variance components were estimated by ", estimator, ".")
}

.describe_inference <- function(object) {
  s <- object$settings
  fit <- object$fit
  if (isTRUE(s$boot)) {
    b <- object$boot
    type <- switch(b$boot_ci, bca = "BCa", stud = "studentized",
                   perc = "percentile", basic = "basic")
    return(paste0(
      "The interval is a nonparametric bootstrap ", type, " interval from ",
      b$n_used, " of ", b$replicates, " replicates (subjects resampled)",
      if (is.null(object$p.value)) "." else
        "; the p-value inverts this interval and is approximate."))
  }
  tested <- !is.null(object$p.value)
  if (identical(s$statistic, "error")) {
    return(.describe_error_inference(object, tested))
  }
  if (identical(s$statistic, "facet")) {
    return(.describe_facet_inference(object, tested))
  }
  what <- if (tested) "The interval and F test" else "The F-based interval"
  if (!fit$balanced) {
    return(paste0(what, " (McGraw & Wong, 1996) use",
                  if (tested) "" else "s",
                  " degrees of freedom based on the observed data; with ",
                  "missing data ", if (tested) "they are" else "it is",
                  " approximate and may be liberal."))
  }
  if (s$form %in% c("1", "k", "C1", "Ck")) {
    paste0(what, if (tested) " are" else " is", " exact (McGraw & Wong, 1996).")
  } else {
    paste0(what, if (tested) " use" else " uses",
           " Satterthwaite approximations (McGraw & Wong, 1996).")
  }
}

.describe_error_inference <- function(object, tested) {
  s <- object$settings
  fit <- object$fit
  relative <- identical(s$scale, "mean")
  sem <- s$stat %in% c("SEM", "MDC") && !relative
  what <- if (tested) {
    if (sem) "The interval and chi-square test" else "The interval and z test"
  } else "The interval"
  how <- if (!sem) {
    paste0(" use the delta method on the log scale with normal-theory ",
           "variances of the mean squares",
           if (relative) " and the grand mean" else "")
  } else if (s$model == "twoway" && s$type == "agreement") {
    " use a chi-square distribution with Satterthwaite degrees of freedom"
  } else {
    " are exact under normality (chi-square distribution of the error variance)"
  }
  if (!tested) {
    # singular subject: "The interval uses ..." / "is exact ..."
    how <- sub("^ use ", " uses ", sub("^ are ", " is ", how))
  }
  if (!fit$balanced) {
    how <- paste0(how, ", with degrees of freedom based on the observed ",
                  "data; with missing data ", if (tested) "they are" else
                    "it is", " approximate and may be liberal")
  }
  paste0(what, how, ".")
}

.describe_facet_inference <- function(object, tested) {
  s <- object$settings
  coefficient <- s$stat == "ICC"
  how <- switch(
    s$ci_method,
    exact = if (coefficient) {
      "is exact under normality (an F ratio of two mean squares)"
    } else {
      "is exact under normality (chi-square distribution of one mean square)"
    },
    mls = paste0("is a modified large-sample (MLS) interval for ",
                 if (coefficient) "a ratio of " else "",
                 "linear combinations of mean squares (Graybill & Wang, ",
                 "1980; Ting et al., 1990)",
                 if (tested) "; the p-value inverts it" else ""),
    satterthwaite = paste0("uses ",
                           if (coefficient) "an F" else "a chi-square",
                           " distribution with Satterthwaite degrees of ",
                           "freedom"),
    delta = paste0("uses the delta method on the log scale with ",
                   "normal-theory variances of the mean squares and the ",
                   "grand mean"))
  text <- paste0("The interval ", how, ".")
  if (!object$fit$balanced) {
    text <- paste0(text, " With unbalanced data it uses observed-data ",
                   "degrees of freedom and is approximate.")
  }
  text
}

.fmt_num <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(format(v))
    formatC(signif(v, 3), digits = 3, format = "fg", flag = "#")
  }, character(1))
}

.fmt_df <- function(df) {
  vapply(df, function(d) {
    if (abs(d - round(d)) < 1e-8) format(round(d)) else format(round(d, 1), nsmall = 1)
  }, character(1))
}

.fmt_p <- function(p) {
  if (p < 0.001) "p < 0.001" else paste0("p = ", formatC(p, digits = 3, format = "f"))
}

.fmt_interval <- function(ci, alternative) {
  level <- paste0(format(100 * attr(ci, "conf.level")), "%")
  switch(alternative,
         two.sided = paste0(level, " CI [", .fmt_num(ci[1]), ", ",
                            .fmt_num(ci[2]), "]"),
         greater = paste0(level, " lower bound ", .fmt_num(ci[1])),
         less = paste0(level, " upper bound ", .fmt_num(ci[2])))
}
