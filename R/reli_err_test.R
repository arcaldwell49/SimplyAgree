#' Measurement Error Test
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' Estimates one measurement error statistic, the standard error of
#' measurement (SEM), the standard error of estimate (SEE), or the standard
#' error of prediction (SEP), with a confidence interval and an optional test
#' against an acceptable error, and returns an object of class
#' \code{"htest"}. The defaults give the typical error of Hopkins (2000):
#' the SEM from a two-way model with systematic differences between items
#' excluded, \eqn{\sqrt{MS_E}}.
#'
#' @inheritParams icc_test
#' @param stat `"SEM"` (default), `"SEE"`, or `"SEP"`. See Details.
#' @param model `"twoway"` (default) when every subject is measured on the
#'   same set of items (e.g. test-retest trials), or `"oneway"` when each
#'   subject is measured by a different set of items.
#' @param type `"consistency"` (default) excludes systematic differences
#'   between items from the error; `"agreement"` includes them. Only
#'   `"agreement"` is defined for `model = "oneway"`.
#' @param null.value An acceptable error, in the units of the measurement,
#'   to test against. The default, `NULL`, gives the estimate and interval
#'   without a test.
#' @param alternative `"less"` (default), `"greater"`, or `"two.sided"`. The
#'   default tests whether the error is smaller than `null.value`. Used only
#'   when `null.value` is given; without a test the interval is two-sided.
#' @param boot_ci Bootstrap interval type: `"auto"` (default), `"perc"`
#'   (percentile), `"bca"`, `"stud"` (studentized), or `"basic"`. With
#'   `method = "anova"`, `"auto"` uses `"bca"` for the two-way agreement
#'   statistics and `"stud"` otherwise (`"perc"` when `se_method = "normal"`);
#'   with `method = "reml"` it uses `"perc"`. See Details. Used only when
#'   `boot = TRUE`.
#' @param se_method How the standard error of the log of the statistic is
#'   estimated: `"jackknife"` (default), `"influence"`, or `"normal"`. It is
#'   reported in `stderr` and used by the studentized bootstrap; it does not
#'   change the analytic interval or test. See [icc_test()].
#'
#' @details
#' **Statistics.** With \eqn{MS} the mean squares of the subjects-by-items
#' analysis of variance (n subjects, k items):
#'
#' | `stat` | `model` | `type` | Estimate | Analytic inference |
#' |---|---|---|---|---|
#' | SEM | twoway | consistency | \eqn{\sqrt{MS_E}} | exact \eqn{\chi^2}, df \eqn{(n-1)(k-1)} |
#' | SEM | twoway | agreement | \eqn{\sqrt{(MS_J + (n-1) MS_E)/n}} | Satterthwaite \eqn{\chi^2} |
#' | SEM | oneway | agreement | \eqn{\sqrt{MS_W}} | exact \eqn{\chi^2}, df \eqn{n(k-1)} |
#' | SEE | any | any | \eqn{SD \sqrt{ICC(1 - ICC)}} | delta method, log scale |
#' | SEP | any | any | \eqn{SD \sqrt{1 - ICC^2}} | delta method, log scale |
#'
#' The consistency SEM is the typical error of Hopkins (2000) and the
#' \eqn{\sqrt{MS_E}} preferred by Weir (2005); the one-way SEM is the
#' within-subject SD of Bland and Altman. The agreement SEM adds the
#' variance between items to the error; with complete data it equals the
#' one-way SEM, but its degrees of freedom differ.
#'
#' **SEE and SEP** (Weir, 2005, eqs. 11 and 15) use the single-measure ICC of
#' the same `model` and `type` (ICC(C,1), ICC(A,1) or ICC(1)). The SEE is the
#' standard error of a subject's estimated true score; the SEP is used to
#' judge whether a change from the estimated true score is real. Both depend
#' on how much subjects differ from each other, so the SEM is generally the
#' better description of measurement error.
#'
#' **Total SD.** SD is the total SD matched to the denominator of the ICC:
#' \eqn{SD^2 = \sigma^2_{subjects} + \sigma^2_{error}} for consistency, plus
#' \eqn{\sigma^2_{items}} for agreement. With this SD,
#' \eqn{SD\sqrt{1 - ICC}} equals the SEM exactly, which is why there is no
#' separate ICC-based SEM. In mean squares (complete data):
#' \eqn{SD^2 = (MS_B + (k-1)MS_E)/k} (consistency),
#' \eqn{(MS_B + (k-1)MS_E + k(MS_J - MS_E)/n)/k} (agreement), and
#' \eqn{(MS_B + (k-1)MS_W)/k} (one-way). It is not the SD of all the
#' measurements, \eqn{\sqrt{SS_{total}/(N-1)}}, used by Weir (2005) and by
#' [reli_stats()], which estimates none of these variances; so SEE and SEP
#' differ slightly from Weir's (for his data set A, SD 32.83 vs 31.74, SEE
#' 7.35 vs 7.11, SEP 10.54 vs 10.20). A negative ICC estimate is treated as
#' zero (SEE = 0, SEP = SD).
#'
#' **Intervals and tests.** The SEM interval is the square root of the
#' \eqn{\chi^2} interval for the error variance, and the test of
#' \eqn{H_0: SEM = } `null.value` is the \eqn{\chi^2} test of the variance,
#' \eqn{\nu \cdot SEM^2 / null^2}. For the SEE and SEP the interval is a Wald
#' interval on the log scale, with a delta-method SE from the normal-theory
#' variances of the mean squares, and the test is the matching z test. With
#' a one-sided alternative the interval is one-sided. `method = "reml"` with
#' missing data uses degrees of freedom based on the observed data, as in
#' [icc_test()], and gives a warning.
#'
#' **Bootstrap.** `boot = TRUE` resamples subjects, as in [icc_test()],
#' including the hybrid step for the two-way agreement statistics (the item
#' mean square of each replicate is multiplied by an independent
#' \eqn{\chi^2_{k-1}/(k-1)} draw). The studentized interval works on the log
#' scale. The p-value inverts the interval and is approximate.
#'
#' In simulations (3 trials, 10 or 30 subjects, ICC 0.5 or 0.9, normal or
#' skewed and heavy-tailed data, complete data), the analytic intervals
#' covered 0.93 to 0.97 with normal data for the consistency and one-way
#' statistics, but only 0.79 to 0.90 with non-normal data (the
#' \eqn{\chi^2} interval for a variance is sensitive to heavy tails). The
#' studentized bootstrap was the best method for the consistency SEM, the SEP
#' and the one-way SEM (0.89 to 0.96 throughout), and BCa for the agreement
#' SEM and SEP (mean 0.91), hence `boot_ci = "auto"`. The percentile
#' interval was too narrow with 10 subjects (as low as 0.54). For the SEE the
#' analytic interval was best with normal data (0.93 to 0.97); no bootstrap
#' interval was reliable near an ICC of 0.5, where the SEE hardly changes
#' with the ICC.
#'
#' **Agreement statistics with few items.** The agreement SEM and SEP
#' include the variance between items, which is estimated from only k - 1
#' degrees of freedom. When it is a large part of the error and k is small,
#' every interval under-covers: with 3 trials and an item variance about
#' as large as the error variance (ICC 0.9), the Satterthwaite interval
#' covered 0.81 to 0.91 with normal data and BCa 0.91 to 0.95 (0.80 with 30
#' subjects and skewed data). Resampling subjects without the hybrid step
#' covered only 0.39 to 0.63 in those settings. More items help more than
#' more subjects.
#'
#' **Minimal detectable change.** The minimal detectable change (MDC),
#' smallest real difference, or repeatability coefficient is a scaled SEM:
#' \eqn{1.96 \sqrt{2} SEM} for 95% (Weir, 2005, eq. 13). Scale the estimate
#' and interval, e.g. `qnorm(0.975) * sqrt(2) * c(res$estimate, res$conf.int)`.
#' With two trials, [agreement_limit()] gives the same quantity as the half
#' width of the limits of agreement, with the bias reported separately.
#'
#' @return A list of class `c("reli_htest", "htest")` containing:
#'   \item{statistic}{The \eqn{\chi^2} (SEM) or z (SEE, SEP) statistic, when
#'   a test was requested without the bootstrap.}
#'   \item{parameter}{The degrees of freedom of the \eqn{\chi^2} test.}
#'   \item{p.value}{The p-value, when a test was requested.}
#'   \item{conf.int}{The confidence interval.}
#'   \item{estimate}{The estimate, named by `stat`.}
#'   \item{stderr}{Standard errors of the estimate (named by `stat`) and of
#'   its log (`log`), from `se_method`. With `boot = TRUE`, also `boot`: the
#'   standard deviation of the bootstrap estimates.}
#'   \item{null.value}{The acceptable error tested against, if any.}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{A description of the statistic and the inference.}
#'   \item{data.name}{A description of the data.}
#'   \item{fit}{The fitted variance components, mean squares and model.}
#'   \item{data}{The data in long format.}
#'   \item{settings}{The settings used, for the methods.}
#'   \item{boot}{Bootstrap details (`NULL` unless `boot = TRUE`).}
#'
#' @examples
#' # Weir (2005) data set A: 8 subjects, 2 trials
#' weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
#'                    t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
#' reli_err_test(col.names = c("t1", "t2"), data = weir)
#'
#' # Is the typical error below 10 units?
#' reli_err_test(col.names = c("t1", "t2"), data = weir, null.value = 10)
#'
#' # Standard error of prediction
#' res <- reli_err_test(col.names = c("t1", "t2"), data = weir, stat = "SEP")
#' summary(res)
#'
#' @references
#' Hopkins, W. G. (2000). Measures of reliability in sports medicine and
#' science. Sports Medicine, 30(1), 1-15. \doi{10.2165/00007256-200030010-00001}
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the
#' intraclass correlation coefficient and the SEM. Journal of Strength and
#' Conditioning Research, 19(1), 231-240. \doi{10.1519/15184.1}
#'
#' @seealso [icc_test()]; [reli_htest-methods] for `summary()`, `plot()` and
#'   `check()`.
#' @export
reli_err_test <- function(x = NULL,
                          data = NULL,
                          measure = NULL,
                          item = NULL,
                          id = NULL,
                          col.names = NULL,
                          stat = c("SEM", "SEE", "SEP"),
                          model = c("twoway", "oneway"),
                          type = c("consistency", "agreement"),
                          method = c("anova", "reml"),
                          conf.level = 0.95,
                          null.value = NULL,
                          alternative = c("less", "greater", "two.sided"),
                          boot = FALSE,
                          boot_ci = c("auto", "perc", "bca", "stud", "basic"),
                          replicates = 1999,
                          se_method = c("jackknife", "influence", "normal")) {
  stat <- match.arg(stat)
  model <- match.arg(model)
  type_given <- !missing(type)
  type <- match.arg(type)
  if (model == "oneway") {
    if (type_given && type == "consistency") {
      stop("type = \"consistency\" is not defined for model = \"oneway\".",
           call. = FALSE)
    }
    type <- "agreement"
  }
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  boot_ci <- match.arg(boot_ci)
  se_method <- match.arg(se_method)
  .check_conf_level(conf.level)
  .check_boot_args(boot, replicates)
  if (!is.null(null.value) &&
      (!is.numeric(null.value) || length(null.value) != 1 ||
       is.na(null.value) || null.value <= 0 || !is.finite(null.value))) {
    stop("'null.value' must be NULL or a single positive number.",
         call. = FALSE)
  }
  # without a test the interval is two-sided
  if (is.null(null.value)) {
    alternative <- "two.sided"
  }
  boot_ci <- .resolve_boot_ci(boot, boot_ci, method, se_method,
                              agreement_bca = .err_random_items(model, type))

  prep <- .reli_prepare(x = x, data = data, measure = measure, item = item,
                        id = id, col.names = col.names)
  fit <- .reli_fit(prep, method = method, model = model)
  if (method == "reml" && !prep$balanced && !boot) {
    .reli_warn_unbalanced(prep)
  }

  estimate <- .err_estimate(fit, stat, model, type)
  se_log <- .err_se_log(prep, fit, stat, model, type, se_method)
  stderr <- stats::setNames(c(estimate * se_log, se_log), c(stat, "log"))

  if (!boot) {
    conf.int <- .err_ci(fit, stat, model, type, conf.level, alternative)
    test <- if (is.null(null.value)) {
      list(statistic = NULL, parameter = NULL, p.value = NULL)
    } else {
      .err_test(fit, stat, model, type, null.value, alternative)
    }
    boot_info <- NULL
  } else {
    random_items <- .err_random_items(model, type)
    stat_fun <- function(p, resample) {
      f <- .reli_fit(p, method = method, model = model, keep_fit = FALSE)
      if (resample && random_items) {
        f <- .hybrid_item_ms(f)
      }
      est <- .err_estimate(f, stat, model, type)
      c(est = est,
        z = log(est),
        se_z = if (boot_ci == "stud") .err_se_log(p, f, stat, model, type,
                                                  se_method) else NA_real_)
    }
    bo <- .reli_boot(prep, stat_fun, R = replicates,
                     jackknife = boot_ci == "bca")
    .check_boot_failures(bo)
    conf.int <- .boot_interval(bo, boot_ci, conf.level, alternative,
                               back = exp, bounds = c(0, Inf))
    test <- list(statistic = NULL, parameter = NULL,
                 p.value = if (is.null(null.value)) NULL else
                   .boot_pvalue(bo, boot_ci, null.value, alternative,
                                fwd = log))
    boot_info <- list(boot_ci = boot_ci,
                      replicates = replicates,
                      n_used = nrow(bo$t),
                      n_failed = bo$n_failed,
                      boot_se = stats::sd(bo$t[, "est"]),
                      estimates = bo$t[, "est"])
    stderr <- c(stderr, boot = boot_info$boot_se)
  }

  settings <- list(statistic = "error",
                   stat = stat,
                   model = model,
                   type = type,
                   form = .err_icc_form(model, type),
                   method = method,
                   conf.level = conf.level,
                   boot = boot,
                   boot_ci = if (boot) boot_ci else NULL,
                   se_method = se_method,
                   label = stat)

  structure(list(statistic = test$statistic,
                 parameter = test$parameter,
                 p.value = test$p.value,
                 conf.int = conf.int,
                 estimate = stats::setNames(estimate, stat),
                 stderr = stderr,
                 null.value = if (is.null(null.value)) NULL else
                   stats::setNames(null.value, stat),
                 alternative = alternative,
                 method = .err_method_text(stat, model, type, method, boot,
                                           boot_ci),
                 data.name = prep$data.name,
                 fit = fit,
                 data = prep$df,
                 settings = settings,
                 boot = boot_info),
            class = c("reli_htest", "htest"))
}

# Helpers --------------------------------------------------------------------

.err_description <- function(stat, model, type) {
  what <- switch(stat,
                 SEM = "Standard error of measurement",
                 SEE = "Standard error of estimate",
                 SEP = "Standard error of prediction")
  design <- if (model == "oneway") "one-way" else
    paste0("two-way, ", if (type == "consistency") "consistency" else
      "absolute agreement")
  paste0(what, " (", design, ")")
}

.err_method_text <- function(stat, model, type, method, boot, boot_ci) {
  base <- .err_description(stat, model, type)
  if (stat != "SEM") {
    base <- paste0(base, " from ", .icc_label(.err_icc_form(model, type)))
  }
  fit <- if (method == "reml") " (REML)" else ""
  inference <- if (boot) {
    paste0(" with nonparametric (subject) bootstrap ",
           .boot_ci_name(boot_ci), " interval")
  } else ""
  paste0(base, fit, inference)
}
