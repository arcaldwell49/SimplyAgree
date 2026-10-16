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
#'   \item{\code{plot}}{A ggplot2 plot of each subject's measurements across
#'   the items.}
#'   \item{\code{check}}{Normality and heteroskedasticity diagnostic plots of
#'   the model residuals, combined with patchwork.}
#' }
#'
#' @examples
#' sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6), J2 = c(2, 1, 4, 1, 5, 2),
#'                  J3 = c(5, 3, 6, 2, 6, 4), J4 = c(8, 2, 8, 6, 9, 7))
#' res <- icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf)
#' summary(res)
#' plot(res)
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
plot.reli_htest <- function(x, ...) {
  ggplot(x$data,
         aes(x = items, y = values, color = id, group = id)) +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_line(color = "black",
              alpha = .2,
              position = position_dodge(width = 0.2)) +
    labs(y = "Measurement",
         x = "Item",
         color = "Subject") +
    scale_color_viridis_d() +
    theme_bw()
}

#' @rdname reli_htest-methods
#' @method check reli_htest
#' @export
check.reli_htest <- function(x, ...) {
  check_reli(list(lmer = x$fit$fit))
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
  paste0("The estimate is the ", description, ", in the units of the ",
         "measurement: ", what, ", ", treatment, ".")
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
  sem <- s$stat == "SEM"
  what <- if (tested) {
    if (sem) "The interval and chi-square test" else "The interval and z test"
  } else "The interval"
  how <- if (!sem) {
    paste0(" use the delta method on the log scale with normal-theory ",
           "variances of the mean squares")
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
