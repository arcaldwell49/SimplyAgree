#' Methods for reli_htest objects
#'
#' Methods for objects returned by [icc_test()] and [reli_err_test()]. They
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
#'   ICC); or `"pairs"`, each pair of items plotted against each other with
#'   the line of identity.
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
#' @references
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
#'   and heteroskedasticity of the model residuals, and the within-subject SD
#'   against the subject mean (on the scale the model was fitted). A rising
#'   within-subject SD means the error grows with the size of the
#'   measurement, so a relative error ([reli_err_test()] with
#'   `scale = "log"`) describes it better.}
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
  style <- match.arg(style)
  s <- object$settings
  tested <- !is.null(object$p.value)
  if (style == "auto") {
    style <- if (tested) "hypothesis" else "estimation"
  }
  if (style == "hypothesis" && !tested) {
    message("No test was requested (null.value = NULL); ",
            "describing the result in the estimation style.")
    style <- "estimation"
  }
  alpha <- 1 - s$conf.level
  s_value <- if (tested) -log2(object$p.value) else NULL

  if (style == "estimation") {
    result_text <- .describe_estimation(object, s_value)
  } else {
    estimate_text <- paste0(names(object$estimate), " = ",
                            .fmt_num(unname(object$estimate)), ", ",
                            .fmt_interval(object$conf.int, object$alternative))
    significant <- object$p.value < alpha
    stat_text <- if (!is.null(object$statistic)) {
      paste0(names(object$statistic), "(",
             paste(.fmt_df(object$parameter), collapse = ", "), ") = ",
             .fmt_num(unname(object$statistic)), ", ")
    } else ""
    alt_text <- switch(object$alternative,
                       two.sided = "not equal to",
                       greater = "greater than",
                       less = "less than")
    result_text <- paste0(
      "The test is ", if (significant) "" else "not ",
      "statistically significant (", stat_text, .fmt_p(object$p.value), "; ",
      estimate_text, ") at the ", format(alpha), " alpha level. ",
      "The null hypothesis ", if (significant) "can" else "cannot",
      " be rejected: at the desired error rate, it ",
      if (significant) "can" else "cannot", " be stated that the true ",
      names(object$null.value), " is ", alt_text, " ",
      format(unname(object$null.value)), ".")
  }

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
                 settings = s),
            class = "summary.reli_htest")
}

#' @rdname reli_htest-methods
#' @method print summary.reli_htest
#' @export
print.summary.reli_htest <- function(x, ...) {
  cat(strwrap(x$text), sep = "\n")
  invisible(x)
}

#' @rdname reli_htest-methods
#' @method plot reli_htest
#' @import ggplot2
#' @export
plot.reli_htest <- function(x,
                            type = c("profile", "subjects", "items", "pairs"),
                            ...) {
  type <- match.arg(type)
  df <- x$data
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
  checks <- check_reli(list(lmer = x$fit$fit))
  # on the scale the model was fitted (log values for scale = "log")
  values <- x$data$values
  if (identical(x$settings$scale, "log")) {
    values <- log(values)
  }
  p_err <- .plot_error_vs_mean(x$data$id, values,
                               log_scale = identical(x$settings$scale, "log"))
  if (is.null(p_err)) {
    return(checks)
  }
  checks + p_err + patchwork::plot_layout(ncol = 3)
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
.plot_error_vs_mean <- function(id, values, log_scale = FALSE) {
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
         x = if (log_scale) "Subject mean (log scale)" else "Subject mean",
         y = "Within-subject SD",
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
                 .icc_label(s$form), " and the SD matched to its denominator"))
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
  paste0("The estimate is the ", description, ", ", units, ": ", what, ", ",
         treatment, ".")
}

.describe_design <- function(fit) {
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
  sem <- s$stat == "SEM" && !relative
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
