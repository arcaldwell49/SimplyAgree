#' Methods for cac_htest objects
#'
#' Methods for objects returned by [cac_test()] and [ord_kappa_test()]. They
#' print as standard \code{"htest"} objects; `summary()` describes the result
#' in words and `plot()` shows the ratings.
#'
#' @param x,object An object of class \code{cac_htest}.
#' @param style How `summary()` words the result: `"auto"` (default),
#'   `"hypothesis"`, or `"estimation"`; see [reli_htest-methods] for the two
#'   styles.
#' @param type For `plot()`, the plot to draw: `"raters"` (default), the
#'   distribution of each rater's ratings over the categories, which shows
#'   raters who use the scale differently; `"subjects"`, the ratings of each
#'   subject, with subjects ordered by their mean category, where a single
#'   large point marks complete agreement; or, for [ord_kappa_test()] only,
#'   `"effects"`, each rater's estimated effect (conditional mode) on the
#'   latent scale with a 95% interval, where positive values mark raters who
#'   give higher ratings.
#' @param ... Not used.
#'
#' @return
#' \describe{
#'   \item{\code{summary}}{An object of class \code{summary.cac_htest}: a list
#'   with the description in `text`, the `style` used, the S-value
#'   (`s.value`, when there is a p-value) and the parts used to build it. It
#'   prints as a paragraph.}
#'   \item{\code{plot}}{A ggplot2 plot (see `type`).}
#' }
#'
#' @examples
#' ratings <- data.frame(
#'   R1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
#'   R2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
#'   R3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
#'   R4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
#' res <- cac_test(col.names = c("R1", "R2", "R3", "R4"), data = ratings,
#'                 null.value = 0.4, alternative = "greater")
#' summary(res)
#' summary(res, style = "estimation")
#' plot(res)
#' plot(res, type = "subjects")
#'
#' @name cac_htest-methods
NULL

#' @rdname cac_htest-methods
#' @method summary cac_htest
#' @export
summary.cac_htest <- function(object,
                              style = c("auto", "hypothesis", "estimation"),
                              ...) {
  style <- .resolve_summary_style(match.arg(style), !is.null(object$p.value))
  s <- object$settings
  s_value <- if (!is.null(object$p.value)) -log2(object$p.value) else NULL
  result_text <- if (identical(s$statistic, "kappa_m_group")) {
    .describe_kappa_group(object, style, s_value, alpha = 1 - s$conf.level)
  } else {
    .htest_result_text(object, style, s_value, alpha = 1 - s$conf.level)
  }
  details <- if (identical(s$statistic, "kappa_m_group")) {
    .describe_kappa_group_model(object)
  } else if (identical(s$statistic, "kappa_m")) {
    c(.describe_kappa_m(object), .describe_kappa_m_inference(object))
  } else {
    c(.describe_cac(object), .describe_cac_inference(object))
  }
  structure(list(text = paste(c(result_text, details), collapse = " "),
                 style = style,
                 estimate = object$estimate,
                 conf.int = object$conf.int,
                 p.value = object$p.value,
                 s.value = s_value,
                 settings = s),
            class = "summary.cac_htest")
}

#' @rdname cac_htest-methods
#' @method print summary.cac_htest
#' @export
print.summary.cac_htest <- function(x, ...) {
  cat(strwrap(x$text), sep = "\n")
  invisible(x)
}

#' @rdname cac_htest-methods
#' @method plot cac_htest
#' @import ggplot2
#' @export
plot.cac_htest <- function(x, type = c("raters", "subjects", "effects"), ...) {
  type <- match.arg(type)
  df <- x$data
  df$values <- factor(as.character(df$values),
                      levels = as.character(x$fit$categories))
  if (type == "effects") {
    if (!grepl("^kappa_m", x$settings$statistic)) {
      stop("type = \"effects\" is only available for ord_kappa_test().",
           call. = FALSE)
    }
    return(.plot_rater_effects(x$fit$rater_effects))
  }
  switch(type,
         raters = .plot_category_use(df),
         subjects = .plot_subject_ratings(df))
}

# Plots ------------------------------------------------------------------------

# Share of each rater's ratings in each category.
.plot_category_use <- function(df) {
  ggplot(df, aes(x = items, fill = values)) +
    geom_bar(position = position_fill(reverse = TRUE), width = 0.7) +
    scale_fill_viridis_d(end = 0.9, drop = FALSE) +
    scale_y_continuous(labels = function(p) paste0(round(100 * p), "%")) +
    coord_flip() +
    labs(x = "Rater", y = "Share of ratings", fill = "Category",
         caption = "Raters who use the scale differently lower agreement.") +
    .reli_plot_theme()
}

# Ratings of each subject; one large point means all raters agreed.
.plot_subject_ratings <- function(df) {
  counts <- as.data.frame(table(id = df$id, values = df$values))
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  pos <- tapply(as.integer(df$values), df$id, mean)
  ord <- names(sort(pos))
  counts$id <- factor(counts$id, levels = ord)
  many <- length(ord) > 30
  ggplot(counts, aes(x = id, y = values, size = Freq)) +
    geom_point(colour = "#2166ac", alpha = 0.8) +
    scale_y_discrete(drop = FALSE) +
    scale_size_area(max_size = 6) +
    labs(x = "Subject (ordered by mean category)", y = "Category",
         size = "Ratings",
         caption = "A single point per subject means all raters agreed.") +
    .reli_plot_theme() +
    if (many) {
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    } else {
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
}

# Rater conditional modes with 95% intervals from the conditional variances.
.plot_rater_effects <- function(re) {
  re$rater <- factor(re$rater, levels = re$rater[order(re$mode)])
  half <- stats::qnorm(0.975) * sqrt(re$cond_var)
  re$lower <- re$mode - half
  re$upper <- re$mode + half
  grouped <- !is.null(re$group)
  p <- ggplot(re, aes(x = mode, y = rater, xmin = lower, xmax = upper)) +
    geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed")
  p <- if (grouped) {
    p + geom_pointrange(aes(colour = group)) +
      scale_colour_viridis_d(end = 0.75, name = "Rater group")
  } else {
    p + geom_pointrange(colour = "#2166ac")
  }
  p + labs(x = "Rater effect (latent scale)", y = "Rater",
           caption = paste0("Conditional modes with 95% intervals.\n",
                            "Positive: rates higher than the typical rater",
                            if (grouped) " of the same group." else ".")) +
    .reli_plot_theme()
}

# Description helpers -----------------------------------------------------------

.describe_cac <- function(object) {
  s <- object$settings
  fit <- object$fit
  what <- switch(s$coef,
                 pa = paste0("the proportion of pairs of ratings of the same ",
                             "subject that agree, not corrected for chance"),
                 gwet = paste0("agreement corrected for the chance that a ",
                               "rater classifies at random (Gwet, 2008)"),
                 fleiss = paste0("agreement corrected for chance agreement ",
                                 "from the overall category proportions"),
                 krippendorff = paste0("agreement corrected for chance ",
                                       "agreement from the overall category ",
                                       "proportions, with a small-sample ",
                                       "correction for the number of pairs"))
  weights <- if (s$weights == "unweighted") {
    "Only identical ratings counted as agreement."
  } else {
    paste0("Partial agreement between categories was credited with ",
           s$weights, " weights.")
  }
  chance <- if (s$coef == "pa") "" else
    paste0(" (observed agreement ", .fmt_num(fit$pa), ", chance agreement ",
           .fmt_num(fit$pe), ")")
  single <- if (fit$n_single > 0) {
    paste0(", of whom ", fit$n_single, " had a single rating")
  } else ""
  paste0("The estimate is ", s$label, ": ", what, chance, ". ", weights,
         " The data had ", fit$n, " subjects", single, ", rated by ", fit$r,
         " raters (", fit$N, " ratings) in ", length(fit$categories),
         " categories.")
}

.describe_cac_inference <- function(object) {
  tested <- !is.null(object$p.value)
  paste0("The interval", if (tested) " and t test use" else " uses",
         " the standard error of Gwet (2008), which treats the subjects as a ",
         "random sample and the raters as fixed, with ",
         .fmt_df(object$fit$df), " degrees of freedom.")
}

.describe_kappa_m <- function(object) {
  fit <- object$fit
  missing_text <- if (fit$n_missing == 0) "none missing" else
    sprintf("%d of %d missing", fit$n_missing, fit$n * fit$k)
  paste0("The estimate is the model-based kappa (Nelson & Edwards, 2015): ",
         "exact agreement between two raters from the population of raters, ",
         "on a subject from the population of subjects, corrected for chance ",
         "agreement (1/C with C = ", fit$C, " categories). It was ",
         "estimated from a cumulative probit mixed model with variances ",
         .fmt_num(fit$sigma2_subject), " for subjects and ",
         .fmt_num(fit$sigma2_rater), " for raters (rho = ",
         .fmt_num(fit$rho), "). The data had ", fit$n, " subjects and ",
         fit$k, " raters (", fit$N, " ratings, ", missing_text, ").")
}

.describe_kappa_m_inference <- function(object) {
  tested <- !is.null(object$p.value)
  paste0("The interval", if (tested) " and z test are" else " is",
         " formed on the logit of rho, with a delta-method standard error ",
         "from the Laplace approximation to the model likelihood; ",
         "it is approximate and may be narrow with few raters.")
}

# Result sentence(s) for ord_kappa_test() with a group: the likelihood ratio
# test and the two group kappas with the interval for their difference.
.describe_kappa_group <- function(object, style, s_value, alpha) {
  fit <- object$fit
  sub <- fit$subgroups
  unit <- fit$group_type
  levs <- fit$levels
  ci <- object$conf.int
  level <- paste0(format(100 * attr(ci, "conf.level")), "%")
  ci_name <- if (style == "estimation") "compatibility interval" else "CI"
  interval <- function(lo, hi) {
    if (is.na(lo)) "interval not available" else
      paste0(level, " ", ci_name, " [", .fmt_num(lo), ", ", .fmt_num(hi), "]")
  }
  kappas <- paste0("kappa_m was ", .fmt_num(sub$kappa_m[1]), " (",
                   interval(sub$lower[1], sub$upper[1]), ") for ", unit,
                   "s in group ", levs[1], " and ", .fmt_num(sub$kappa_m[2]),
                   " (", interval(sub$lower[2], sub$upper[2]), ") in group ",
                   levs[2], "; the difference (", levs[2], " minus ", levs[1],
                   ") was ", .fmt_num(sub$kappa_m[2] - sub$kappa_m[1]), " (",
                   interval(ci[1], ci[2]), ").")
  between <- if (unit == "rater") {
    paste0(" Between a rater from each group, kappa_m was ",
           .fmt_num(sub$kappa_m[3]), ".")
  } else ""
  test <- paste0("LRT = ", .fmt_num(unname(object$statistic)), ", ",
                 .fmt_p(object$p.value))
  null <- paste0("the ", unit, " variance is the same in both groups, so ",
                 "the group kappas are equal")
  if (style == "hypothesis") {
    significant <- object$p.value < alpha
    paste0("The likelihood ratio test is ", if (significant) "" else "not ",
           "statistically significant (", test, ") at the ", format(alpha),
           " alpha level: it ", if (significant) "can" else "cannot",
           " be stated that agreement differs between the ", unit, " groups (",
           fit$group, "; null hypothesis: ", null, "). ",
           "In this model, ", kappas, between)
  } else {
    heads <- round(s_value)
    paste0("In this model, ", kappas, between, " Against the hypothesis that ",
           null, ", ", test, ", an S-value of ", .fmt_num(s_value),
           " bits of information (S = -log2 p), about as surprising as ",
           if (heads < 1) "nothing at all" else paste0(heads, " heads in a row"),
           " from a fair coin.")
  }
}

.describe_kappa_group_model <- function(object) {
  fit <- object$fit
  unit <- fit$group_type
  vars <- if (unit == "rater") {
    paste0("a subject variance of ", .fmt_num(fit$sigma2_subject),
           " and rater variances of ", .fmt_num(fit$sigma2_rater[1]), " and ",
           .fmt_num(fit$sigma2_rater[2]))
  } else {
    paste0("subject variances of ", .fmt_num(fit$sigma2_subject[1]), " and ",
           .fmt_num(fit$sigma2_subject[2]), " and a rater variance of ",
           .fmt_num(fit$sigma2_rater))
  }
  paste0("The model-based kappa (Nelson, Mitani & Edwards, 2017) was ",
         "estimated from a cumulative probit mixed model with the group as a ",
         "fixed effect and a separate ", unit, " variance in each group: ",
         vars, " in groups ", fit$levels[1], " and ", fit$levels[2],
         ". The data had ", fit$n, " subjects and ", fit$k, " raters (",
         fit$N, " ratings). The likelihood ratio test compares this model ",
         "with one with a common ", unit, " variance (chi-square, 1 df); ",
         "intervals use delta-method standard errors and are approximate.")
}
