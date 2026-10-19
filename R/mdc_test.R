#' Minimal Detectable Change
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Estimates the minimal detectable change (MDC), also called the smallest
#' detectable or real difference, the "minimal difference" (Weir, 2005), or
#' the repeatability coefficient: the smallest change in one subject's score
#' between two occasions that exceeds measurement error with a given
#' confidence. It is a scaled standard error of measurement (SEM), so it
#' comes with a confidence interval and an optional test against a change
#' that matters, and is returned as an object of class \code{"htest"}.
#'
#' @inheritParams reli_err_test
#' @param x A formula of the form `values ~ item | id`, or the result of
#'   [reli_err_test()] with `stat = "SEM"`, whose SEM is then used without
#'   refitting. Leave `NULL` to use `measure`, `item` and `id` (long data) or
#'   `col.names` (wide data).
#' @param mdc.level The confidence that a change of at least the MDC is not
#'   measurement error (default 0.95, the "MDC95"). This is not the
#'   confidence level of the interval, which is `conf.level`.
#' @param sided `"two.sided"` (default) for a change in either direction, or
#'   `"one.sided"` when only a change in one stated direction (e.g. an
#'   improvement) is of interest. A one-sided MDC95 uses the same multiplier
#'   as the two-sided "MDC90".
#' @param k_avg The number of measurements averaged at each occasion
#'   (default 1). Use it when the score used in practice is the mean of
#'   several trials: the MDC then shrinks by \eqn{\sqrt{k_{avg}}}.
#' @param null.value A change that matters, on the scale given by `scale`,
#'   to test against: by default (`alternative = "less"`), whether the MDC is
#'   below it, i.e. whether a change of that size can be distinguished from
#'   measurement error. It should come from an external anchor; see Details.
#'   The default, `NULL`, gives the estimate and interval without a test.
#' @param ... Further arguments passed to [reli_err_test()] when `x` is not
#'   an existing result: `scale`, `model`, `type`, `method`, `boot`,
#'   `boot_ci`, `replicates` and `se_method`.
#'
#' @details
#' **Estimate.** A change score is the difference between two measurements,
#' each with error SEM, so its error SD is \eqn{\sqrt{2} \cdot SEM}{sqrt(2) * SEM}, and
#' \deqn{MDC = \frac{z \cdot \sqrt{2} \cdot SEM}{\sqrt{k_{avg}}},}{MDC = z * sqrt(2) * SEM / sqrt(k_avg),}
#' with \eqn{z} the standard normal quantile for `mdc.level` (1.96 for a
#' two-sided MDC95; Weir, 2005, eq. 13) and \eqn{k_{avg}}{k_avg} the number
#' of measurements averaged to give the score at each occasion (`k_avg`;
#' 1 for single measurements). The mean of \eqn{k_{avg}}{k_avg} measurements
#' has error \eqn{SEM / \sqrt{k_{avg}}}{SEM / sqrt(k_avg)}, so averaging
#' shrinks the MDC by the same factor. The SEM is from [reli_err_test()],
#' with the same `model`, `type` and `scale` choices; the defaults give the
#' typical error of Hopkins (2000), \eqn{\sqrt{MS_E}}{sqrt(MS_E)}. With two trials the
#' MDC equals the half-width of the limits of agreement of
#' [agreement_limit()], which also reports the bias between trials.
#'
#' **Interval and test.** Because the MDC is a fixed multiple of the SEM, its
#' interval is the SEM interval times that multiple, and the test of
#' \eqn{H_0: MDC = } `null.value` is the SEM test of `null.value` divided by
#' it (a \eqn{\chi^2} test for the SEM, or the bootstrap with `boot = TRUE`).
#' With `scale = "log"` the MDC is in sympercents; `percent` gives the
#' equivalent percentage increase and decrease, which differ:
#' \eqn{100(e^{MDC/100} - 1)} and \eqn{100(1 - e^{-MDC/100})}.
#'
#' **An individual threshold, not a group one.** The MDC applies to a single
#' individual's change between two measurements. It is not a threshold for
#' an average (group) effect. The uncertainty in a mean change shrinks with
#' the number of subjects (its standard error is about
#' \eqn{\sqrt{2} \, SEM / \sqrt{n}}), so a mean effect well below the MDC can
#' be estimated precisely and be clearly different from zero. Judge a mean
#' effect by its own confidence interval. An effect smaller than the MDC is
#' small relative to one person's measurement noise; that does not make it
#' noise itself, and it does not show that the effect is absent!
#'
#' Nor should the MDC be used to count "responders". An individual's observed
#' change mixes their true response with within-person variation from
#' occasion to occasion, so the proportion whose change exceeds the MDC
#' reflects measurement noise as much as real differences between people:
#' even if every subject had exactly the same true response, some would
#' exceed the MDC and some would not (Senn, 2004). Real differences in
#' response are estimated from the variance of the change scores in a
#' treated group compared with a control group, or from replicated
#' crossover designs (Atkinson & Batterham, 2015; Senn, 2016).
#'
#' **The MDC and the smallest worthwhile change.** The MDC describes
#' measurement noise. It answers the question: how large an observed change must be before it can be
#' distinguished from error? Whether a change *matters* is a separate
#' question that the reliability data cannot answer. There needs to be an external
#' anchor, such as a clinically important difference or a change that
#' alters competitive outcomes. Give that value as `null.value` (with the
#' default `alternative = "less"`) to test whether the MDC is below it.
#'
#' A "smallest worthwhile change" (SWC) computed from the same reliability
#' data cannot serve as that anchor. With SWC = 0.2 times the
#' between-subject SD (Hopkins, 2004),
#' \eqn{SWC / SEM = 0.2 / \sqrt{1 - ICC}}{SWC / SEM = 0.2 / sqrt(1 - ICC)},
#' a restatement of the ICC. Hopkins (2004) offers it as a default (e.g. for
#' fitness tests in team sports) when no relationship to a real outcome is
#' available, and it should be treated as such a fallback.
#'
#' Hopkins's (2004) SWC for solo athletes, 0.3 times the CV, is an external
#' anchor, but only with the right CV: the within-athlete variability of top
#' athletes' performance from one *competition* to the next, not the test's
#' own typical error. Simulations (Hopkins, Hawley & Burke, 1999) showed that
#' an enhancement of 0.3 of that CV gives a top athlete about one extra win
#' in 10 events. That CV can be estimated with [reli_err_test()]
#' (`scale = "log"`) from repeated competition results, with competitions as
#' items, and 0.3 times it given here as `null.value`. Applied to the test's
#' own CV instead, the rule is circular:
#' \eqn{SWC / MDC = 0.3 / (1.96 \sqrt{2}) \approx 0.11}{SWC / MDC = 0.3 / (1.96 * sqrt(2)) = 0.11}
#' for every test. The anchor also rests on the one-in-10 convention and on
#' elite fields of evenly matched athletes; for athletes further back in the
#' field, Hopkins works out the worthwhile enhancement case by case.
#'
#' @return A list of class `c("reli_htest", "htest")`, as for
#'   [reli_err_test()], containing:
#'   \item{statistic, parameter, p.value}{The test of the SEM (equivalently
#'   of the MDC), when a `null.value` was given.}
#'   \item{conf.int}{The confidence interval for the MDC.}
#'   \item{estimate}{The MDC, on the scale given by `scale`.}
#'   \item{stderr}{Standard errors of the MDC and of its log.}
#'   \item{null.value}{The change tested against, if any.}
#'   \item{alternative, method, data.name}{As for an `htest`.}
#'   \item{percent}{With `scale = "log"`, the MDC and its interval as a
#'   percentage increase and decrease.}
#'   \item{sem}{The SEM result the MDC was computed from.}
#'   \item{fit, data, settings, boot}{As for [reli_err_test()]; `settings`
#'   also holds the MDC settings and multiplier (`mdc`).}
#'
#' @examples
#' # Weir (2005) data set A: 8 subjects, 2 trials
#' weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
#'                    t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
#' mdc_test(col.names = c("t1", "t2"), data = weir)
#'
#' # Can a change of 30 units be distinguished from measurement error?
#' res <- mdc_test(col.names = c("t1", "t2"), data = weir, null.value = 30)
#' summary(res)
#'
#' # From an existing SEM, one-sided, as a percentage (log scale)
#' # in the result we can access the MDC on the percentage scale (converted from sympercent)
#' sem <- reli_err_test(col.names = c("t1", "t2"), data = weir, scale = "log")
#' res_log <- mdc_test(sem, sided = "one.sided")
#' res_log$percent
#'
#' @references
#' Atkinson, G., & Batterham, A. M. (2015). True and false interindividual
#' differences in the physiological response to an intervention.
#' Experimental Physiology, 100(6), 577-588. \doi{10.1113/EP085070}
#'
#' Hopkins, W. G. (2000). Measures of reliability in sports medicine and
#' science. Sports Medicine, 30(1), 1-15. \doi{10.2165/00007256-200030010-00001}
#'
#' Hopkins, W. G. (2004). How to interpret changes in an athletic
#' performance test. Sportscience, 8, 1-7.
#'
#' Hopkins, W. G., Hawley, J. A., & Burke, L. M. (1999). Design and analysis
#' of research on sport performance enhancement. Medicine & Science in
#' Sports & Exercise, 31(3), 472-485. \doi{10.1097/00005768-199903000-00018}
#'
#' Senn, S. (2004). Individual response to treatment: is it a valid
#' assumption? BMJ, 329(7472), 966-968. \doi{10.1136/bmj.329.7472.966}
#'
#' Senn, S. (2016). Mastering variation: variance components and
#' personalised medicine. Statistics in Medicine, 35(7), 966-977.
#' \doi{10.1002/sim.6739}
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the
#' intraclass correlation coefficient and the SEM. Journal of Strength and
#' Conditioning Research, 19(1), 231-240. \doi{10.1519/15184.1}
#'
#' @seealso [reli_err_test()] for the SEM; [agreement_limit()] for the
#'   limits of agreement; [reli_htest-methods] for `summary()`, `plot()` and
#'   `check()`.
#' @export
mdc_test <- function(x = NULL,
                     data = NULL,
                     measure = NULL,
                     item = NULL,
                     id = NULL,
                     col.names = NULL,
                     mdc.level = 0.95,
                     sided = c("two.sided", "one.sided"),
                     k_avg = 1,
                     conf.level = 0.95,
                     null.value = NULL,
                     alternative = c("less", "greater", "two.sided"),
                     ...) {
  sided <- match.arg(sided)
  alternative <- match.arg(alternative)
  if (!is.numeric(mdc.level) || length(mdc.level) != 1 || is.na(mdc.level) ||
      mdc.level <= 0 || mdc.level >= 1) {
    stop("'mdc.level' must be a single number between 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(k_avg) || length(k_avg) != 1 || is.na(k_avg) ||
      k_avg < 1 || k_avg != round(k_avg)) {
    stop("'k_avg' must be a whole number of at least 1.", call. = FALSE)
  }
  .check_conf_level(conf.level)
  if (!is.null(null.value) &&
      (!is.numeric(null.value) || length(null.value) != 1 ||
       is.na(null.value) || null.value <= 0 || !is.finite(null.value))) {
    stop("'null.value' must be NULL or a single positive number.",
         call. = FALSE)
  }
  if (is.null(null.value)) {
    alternative <- "two.sided"
  }

  z <- if (sided == "two.sided") stats::qnorm(1 - (1 - mdc.level) / 2) else
    stats::qnorm(mdc.level)
  multiplier <- z * sqrt(2 / k_avg)
  null_sem <- if (is.null(null.value)) NULL else null.value / multiplier

  if (inherits(x, "reli_htest")) {
    sem <- .mdc_from_sem(x, conf.level, null_sem, alternative, ...)
  } else {
    sem <- reli_err_test(x = x, data = data, measure = measure, item = item,
                         id = id, col.names = col.names, stat = "SEM",
                         conf.level = conf.level, null.value = null_sem,
                         alternative = alternative, ...)
  }

  s <- sem$settings
  label <- .err_label("MDC", s$scale)
  estimate <- multiplier * unname(sem$estimate)
  conf.int <- multiplier * as.numeric(sem$conf.int)
  attr(conf.int, "conf.level") <- conf.level
  stderr <- sem$stderr
  stderr[names(stderr) != "log"] <- multiplier * stderr[names(stderr) != "log"]
  names(stderr)[1] <- label
  boot_info <- sem$boot
  if (!is.null(boot_info)) {
    boot_info$boot_se <- multiplier * boot_info$boot_se
    boot_info$estimates <- multiplier * boot_info$estimates
  }

  percent <- NULL
  if (s$scale == "log") {
    vals <- unname(c(estimate, as.numeric(conf.int)))
    percent <- data.frame(change = c("increase", "decrease"),
                          estimate = 100 * c(exp(vals[1] / 100) - 1,
                                             1 - exp(-vals[1] / 100)),
                          lower = 100 * c(exp(vals[2] / 100) - 1,
                                          1 - exp(-vals[2] / 100)),
                          upper = 100 * c(exp(vals[3] / 100) - 1,
                                          1 - exp(-vals[3] / 100)))
  }

  mdc <- list(level = mdc.level,
              sided = sided,
              k_avg = k_avg,
              z = z,
              multiplier = multiplier)
  settings <- utils::modifyList(s, list(stat = "MDC",
                                        conf.level = conf.level,
                                        label = label,
                                        mdc = mdc))
  sem_text <- sem$method
  sem_text <- paste0(tolower(substr(sem_text, 1, 1)), substring(sem_text, 2))
  method <- paste0("Minimal detectable change (", format(100 * mdc.level),
                   "%, ", sub("\\.", "-", sided),
                   if (k_avg > 1) paste0(", mean of ", k_avg, " trials")
                   else "", ") from the ", sem_text)

  structure(list(statistic = sem$statistic,
                 parameter = sem$parameter,
                 p.value = sem$p.value,
                 conf.int = conf.int,
                 estimate = stats::setNames(estimate, label),
                 stderr = stderr,
                 null.value = if (is.null(null.value)) NULL else
                   stats::setNames(null.value, label),
                 alternative = alternative,
                 method = method,
                 data.name = sem$data.name,
                 percent = percent,
                 sem = sem,
                 fit = sem$fit,
                 data = sem$data,
                 settings = settings,
                 boot = boot_info),
            class = c("reli_htest", "htest"))
}

#' Recompute the interval and test of an existing SEM result
#'
#' The analytic interval and test are recomputed from the stored fit, so
#' `conf.level`, `null.value` and `alternative` may differ from those used
#' for the SEM; a bootstrap interval can only be reused as it is.
#' @noRd
.mdc_from_sem <- function(x, conf.level, null_sem, alternative, ...) {
  s <- x$settings
  if (!identical(s$statistic, "error") || !identical(s$stat, "SEM")) {
    stop("'x' must be a formula or the result of reli_err_test() with ",
         "stat = \"SEM\".", call. = FALSE)
  }
  if (length(list(...)) > 0) {
    stop("Arguments in '...' are used only when fitting from data; they ",
         "cannot change an existing SEM result.", call. = FALSE)
  }
  if (isTRUE(s$boot)) {
    if (!isTRUE(all.equal(conf.level, s$conf.level)) || !is.null(null_sem)) {
      stop("A bootstrap SEM can only be rescaled at its own confidence ",
           "level and without a new test. Call mdc_test() on the data with ",
           "boot = TRUE instead.", call. = FALSE)
    }
    return(x)
  }
  ci <- .err_ci(x$fit, "SEM", s$model, s$type, conf.level, alternative,
                s$scale)
  test <- if (is.null(null_sem)) {
    list(statistic = NULL, parameter = NULL, p.value = NULL)
  } else {
    .err_test(x$fit, "SEM", s$model, s$type, null_sem, alternative, s$scale)
  }
  x$conf.int <- ci
  x$statistic <- test$statistic
  x$parameter <- test$parameter
  x$p.value <- test$p.value
  x$alternative <- alternative
  x$null.value <- if (is.null(null_sem)) NULL else
    stats::setNames(null_sem, names(x$estimate))
  x$settings$conf.level <- conf.level
  x
}
