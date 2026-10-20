#' Intraclass Correlation Coefficient Test
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Estimates one intraclass correlation coefficient (ICC), with a confidence
#' interval and a test against a null value, and returns an object of class
#' \code{"htest"}. The ICC form is chosen with `model`, `type` and `unit`,
#' following McGraw and Wong (1996).
#'
#' @param x A formula of the form `values ~ item | id`, where `values` is the
#'   measurement, `item` identifies the trial, rater or occasion, and `id`
#'   identifies the subject. Use either `x`, the `measure`/`item`/`id`
#'   arguments, or `col.names`.
#' @param data A data frame containing the variables.
#' @param measure,item,id Column names (character) of the measurement, the
#'   item (trial, rater, or occasion) and the subject identifier, for data in
#'   long format.
#' @param col.names Column names (character) of the items, for data in wide
#'   format (one row per subject).
#' @param model `"twoway"` (default) when every subject is measured on the
#'   same set of items (e.g. test-retest trials), or `"oneway"` when each
#'   subject is measured by a different set of items (e.g. different raters).
#' @param type `"consistency"` (default) excludes systematic differences
#'   between items from the ICC; `"agreement"` includes them (absolute
#'   agreement). Only `"agreement"` is defined for `model = "oneway"`.
#' @param unit `"single"` (default) for the reliability of a single
#'   measurement, or `"average"` for the reliability of the mean of all k
#'   items.
#' @param method `"anova"` (default) estimates the variance components from
#'   the analysis of variance and requires complete, balanced data. `"reml"`
#'   fits a linear mixed model and allows missing measurements.
#' @param conf.level Confidence level of the interval (default 0.95).
#' @param null.value The ICC under the null hypothesis. The default, `NULL`,
#'   gives the estimate and interval without a test.
#' @param alternative `"two.sided"` (default), `"greater"`, or `"less"`. Used
#'   only when `null.value` is given (without a test the interval is
#'   two-sided). A one-sided alternative gives a one-sided interval (see
#'   Details).
#' @param boot If `TRUE`, use a nonparametric bootstrap that resamples
#'   subjects for the interval and p-value. Default `FALSE`.
#' @param boot_ci Bootstrap interval type: `"auto"` (default), `"perc"`
#'   (percentile), `"bca"`, `"stud"` (studentized), or `"basic"`. `"auto"`
#'   uses `"stud"` for `method = "anova"` and `"perc"` for `method = "reml"`
#'   (and `"perc"` when `se_method = "normal"`); see Details. Used only when
#'   `boot = TRUE`.
#' @param replicates Number of bootstrap replicates (default 1999).
#' @param se_method How the standard error is estimated: `"jackknife"`
#'   (default; delete one subject at a time), `"influence"` (per-subject
#'   influence, or sandwich, estimate), or `"normal"` (normal theory). The SE
#'   is reported in `stderr` and is used by the studentized bootstrap; it does
#'   not change the F-based interval or test. See Details.
#'
#' @details
#'
#' **ICC forms.**. The different ICCs can be confusing. Therefore, I have summarized it here. McGraw and Wong (1996) and Shrout and Fleiss (1979) name the
#' forms differently:
#'
#' | `model` | `type` | `unit` | McGraw & Wong | Shrout & Fleiss |
#' |---|---|---|---|---|
#' | oneway | agreement | single | ICC(1) | ICC(1,1) |
#' | oneway | agreement | average | ICC(k) | ICC(1,k) |
#' | twoway | agreement | single | ICC(A,1) | ICC(2,1) |
#' | twoway | agreement | average | ICC(A,k) | ICC(2,k) |
#' | twoway | consistency | single | ICC(C,1) | ICC(3,1) |
#' | twoway | consistency | average | ICC(C,k) | ICC(3,k) |
#'
#' Whether items are treated as random or fixed changes the interpretation
#' but not the calculation (McGraw & Wong, 1996). Average-measure forms are
#' not defined for McGraw and Wong's two-way mixed model with an interaction.
#'
#' **Choosing the form.** Answer three questions in order:
#' (1) is every subject measured by the same items? Yes: `"twoway"`; no,
#' each subject by different items: `"oneway"`. (2) Should systematic
#' differences between items count as error? Yes, when measurements from
#' different items will be used interchangeably or on their absolute values
#' (for example compared with a cut-off): `"agreement"`; no, when only the
#' subjects' relative standing matters or systematic differences are
#' reported separately: `"consistency"`. (3) Will one measurement or the mean
#' of the k items be used in practice? `"single"` or `"average"`. The
#' "Reliability Analysis" vignette has a table of common designs
#' (`vignette("reliability_analysis", package = "SimplyAgree")`).
#'
#' **Related coefficients.** With complete data, Cronbach's (1951) alpha
#' equals ICC(C,k), the average-measure consistency ICC (McGraw & Wong,
#' 1996): it describes the mean of the k items, not a single item. For two
#' methods, Lin's concordance correlation coefficient ([ccc_test()]) is
#' essentially ICC(A,1); the estimators differ only in using n rather than
#' n - 1 in the variances (Nickerson, 1997; Carrasco & Jover, 2003).
#'
#' **Default form.** The default, ICC(C,1), follows Hopkins (2000) and Weir
#' (2005): check for systematic change between trials separately (for
#' example with the trials F test, or [t.test()] for two trials), then report
#' the consistency ICC, which is tied to the standard error of measurement
#' (\eqn{\sqrt{MS_E}}). Others, such as Koo and Li (2016), recommend absolute
#' agreement (`type = "agreement"`) for test-retest studies. Which form is
#' appropriate depends on whether systematic differences between trials
#' should count as error.
#'
#' **Average-measure forms** are computed as the Spearman-Brown projection of
#' the single-measure form to all k items. With complete data this is
#' identical to the McGraw and Wong formulas; with missing data it keeps the
#' estimand the reliability of the mean of all k items.
#'
#' **Confidence intervals** are the F-based intervals of Shrout and Fleiss
#' (1979) and McGraw and Wong (1996, Table 7), with \eqn{\alpha/2} in each
#' tail. They are exact for the one-way and consistency forms and
#' approximate (Satterthwaite degrees of freedom) for the agreement forms.
#' With `alternative = "greater"` the interval is one-sided, `[L, 1]`, where L
#' is the one-sided `conf.level` lower bound; with `"less"` it is
#' `[-Inf, U]`.
#'
#' **Tests.** By default (`null.value = NULL`) no test is done. The result is
#' the estimate and a two-sided interval when printed to the console. Testing against an ICC of 0, the
#' default in many programs, is rarely informative for reliability. When
#' `null.value` is given, the tests are the F tests of McGraw and Wong (1996,
#' Table 8) of \eqn{H_0: \rho = \rho_0}. They are exact for the one-way and consistency
#' forms, and invert exactly to the interval. For the agreement forms the
#' test and the interval are separate Satterthwaite approximations (degrees
#' of freedom evaluated at \eqn{\rho_0} for the test and at the estimate for
#' the interval), so with few subjects they can disagree near the interval
#' limits; McGraw and Wong note that their power cannot be determined
#' exactly.
#'
#' **Missing data.** `method = "reml"` estimates the variance components from
#' a linear mixed model and uses degrees of freedom based on the observed
#' data (subject df \eqn{n - 1}, error df \eqn{N - n - k + 1}, with N the
#' number of observations). The intervals and p-values are then approximate
#' and a warning is given. In simulations with 20 subjects and 3 trials,
#' 95% intervals covered 0.93 to 0.97 with 15% of measurements missing and
#' 0.91 to 0.95 with 30% missing; coverage depends on the design and the
#' pattern of missingness. `boot = TRUE` is an alternative.
#'
#' **Bootstrap.** With `boot = TRUE`, subjects are resampled with replacement
#' (keeping all of each subject's measurements) and the model is refitted.
#' The BCa interval uses a leave-one-subject-out jackknife for the
#' acceleration. The studentized interval works on the scale
#' \eqn{z = 0.5 \log F} and divides each replicate by its own standard error
#' from `se_method`; a normal-theory SE cannot be used because it depends
#' only on the degrees of freedom, which would reduce the studentized
#' interval to the basic interval on the z scale. The p-value is obtained by inverting the chosen
#' interval (as in the TOSTER package), so it is approximate and consistent
#' with the interval. With `method = "reml"` each replicate refits the mixed
#' model, which can be slow.
#'
#' For the two-way agreement forms, resampling subjects alone would treat the
#' items as fixed and ignore the uncertainty from sampling the items, which
#' under-covered badly in simulations (0.70 to 0.80 with an ICC of 0.9). Each
#' replicate therefore also multiplies the item mean square by an independent
#' \eqn{\chi^2_{k-1}/(k-1)} draw, its normal-theory sampling variation (a
#' hybrid bootstrap developed for this package). Like the F interval, this
#' treats the items as a random sample; when the items are fixed it is
#' conservative.
#'
#' The bootstrap is mainly useful when the data are clearly non-normal. In
#' simulations (3 trials, 10 or 30 subjects, ICC 0.5 or 0.9), the F
#' intervals covered about 0.92 to 0.97 with normal, complete data but only
#' 0.80 to 0.93 with skewed or heavy-tailed data. With complete data, the
#' studentized bootstrap with the jackknife SE covered 0.90 to 0.965 for the
#' consistency ICC, including 0.94 to 0.955 with skewed data and 30
#' subjects, and was the best method overall; for the agreement ICC it was
#' roughly tied with the percentile interval (0.89 to 0.955), whose lower
#' bound was too high less often. With 20% of measurements missing and
#' `method = "reml"`, the studentized interval (which then needs the
#' influence SE) was unreliable at moderate ICCs (as low as 0.80), while the
#' percentile interval covered 0.87 to 0.96. Hence `boot_ci = "auto"`, and a
#' warning when `"stud"` is requested with `method = "reml"`. BCa never
#' improved on the percentile interval. Every method under-covered with 10
#' subjects, skewed data and an ICC of 0.9 (0.84 to 0.91).
#'
#' **Standard errors.** `stderr` gives the standard error of
#' \eqn{z = 0.5 \log F} (the scale of the studentized bootstrap) and, by the
#' delta method, of the ICC. The jackknife (default) deletes one subject at a
#' time; with complete data it uses closed-form leave-one-out mean squares,
#' and with `method = "reml"` it refits the model n times. The influence
#' estimate is a sandwich estimate from each subject's influence on the mean
#' squares (with missing data, an approximation from each subject's observed
#' measurements); it is cheaper but too small with few subjects. The
#' normal-theory SE depends only on the degrees of freedom. In simulations (3
#' trials, 20 to 40 subjects, up to 30% missing), the jackknife SE averaged
#' 0.89 to 1.08 times the true sampling SD, the influence SE 0.69 to 1.00
#' (lowest with few subjects, skewed data, or data missing at random), and
#' the normal-theory SE 0.87 to 1.04 with normal data but 0.60 to 0.80 with
#' skewed or heavy-tailed data. For the agreement forms, the jackknife and
#' influence SEs add the normal-theory variance of the item mean square,
#' treating the items as random. The studentized bootstrap needs an SE
#' estimated from each resample, so it cannot use `"normal"`; with
#' `method = "reml"` it requires `"influence"`, because the jackknife would
#' refit the model n times in every replicate.
#'
#' **Interpreting the ICC.** No qualitative labels (such as "good" or
#' "excellent") are given: published cutoffs disagree, and the ICC depends on
#' how much subjects differ from each other (Weir, 2005). To claim that
#' reliability exceeds a threshold, test against it, e.g.
#' `null.value = 0.75, alternative = "greater"`.
#'
#' @return A list of class `c("reli_htest", "htest")` containing:
#'   \item{statistic}{The F statistic (omitted for bootstrap results).}
#'   \item{parameter}{The numerator and denominator degrees of freedom
#'   (omitted for bootstrap results).}
#'   \item{p.value}{The p-value of the test.}
#'   \item{conf.int}{The confidence interval for the ICC.}
#'   \item{estimate}{The estimated ICC, named by its McGraw and Wong form.}
#'   \item{stderr}{Standard errors of the ICC (`ICC`) and of
#'   \eqn{z = 0.5 \log F} (`z`), from `se_method`. With `boot = TRUE`, also
#'   `boot`: the standard deviation of the bootstrap ICC estimates.}
#'   \item{null.value}{The hypothesized ICC.}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{A description of the ICC and the inference.}
#'   \item{data.name}{A description of the data.}
#'   \item{fit}{The fitted variance components, mean squares and model.}
#'   \item{data}{The data in long format.}
#'   \item{settings}{The settings used, for the methods.}
#'   \item{boot}{Bootstrap details (`NULL` unless `boot = TRUE`).}
#'
#' @examples
#' # Shrout & Fleiss (1979) example: 6 subjects rated by 4 judges
#' sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6), J2 = c(2, 1, 4, 1, 5, 2),
#'                  J3 = c(5, 3, 6, 2, 6, 4), J4 = c(8, 2, 8, 6, 9, 7))
#' icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf)
#'
#' # Absolute agreement, tested against 0.5
#' icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf,
#'          type = "agreement", null.value = 0.5, alternative = "greater")
#'
#' # Long data with a formula
#' sf_long <- data.frame(id = rep(1:6, times = 4),
#'                       judge = rep(c("J1", "J2", "J3", "J4"), each = 6),
#'                       score = c(sf$J1, sf$J2, sf$J3, sf$J4))
#' res <- icc_test(score ~ judge | id, data = sf_long)
#' summary(res)
#'
#' @references
#' Carrasco, J. L., & Jover, L. (2003). Estimating the generalized
#' concordance correlation coefficient through variance components.
#' Biometrics, 59(4), 849-858. \doi{10.1111/j.0006-341x.2003.00099.x}
#'
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
#' tests. Psychometrika, 16(3), 297-334. \doi{10.1007/BF02310555}
#'
#' Hopkins, W. G. (2000). Measures of reliability in sports medicine and
#' science. Sports Medicine, 30(1), 1-15. \doi{10.2165/00007256-200030010-00001}
#'
#' Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
#' intraclass correlation coefficients for reliability research. Journal of
#' Chiropractic Medicine, 15(2), 155-163. \doi{10.1016/j.jcm.2016.02.012}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' Nickerson, C. A. E. (1997). A note on "A concordance correlation
#' coefficient to evaluate reproducibility". Biometrics, 53(4), 1503-1507.
#' \doi{10.2307/2533516}
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
#' assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
#' \doi{10.1037/0033-2909.86.2.420}
#'
#' Ukoumunne, O. C., Davison, A. C., Gulliford, M. C., & Chinn, S. (2003).
#' Non-parametric bootstrap confidence intervals for the intraclass
#' correlation coefficient. Statistics in Medicine, 22(24), 3805-3821.
#' \doi{10.1002/sim.1643}
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the
#' intraclass correlation coefficient and the SEM. Journal of Strength and
#' Conditioning Research, 19(1), 231-240. \doi{10.1519/15184.1}
#'
#' @seealso [reli_htest-methods] for `summary()`, `plot()` and `check()`.
#' @export
icc_test <- function(x = NULL,
                     data = NULL,
                     measure = NULL,
                     item = NULL,
                     id = NULL,
                     col.names = NULL,
                     model = c("twoway", "oneway"),
                     type = c("consistency", "agreement"),
                     unit = c("single", "average"),
                     method = c("anova", "reml"),
                     conf.level = 0.95,
                     null.value = NULL,
                     alternative = c("two.sided", "greater", "less"),
                     boot = FALSE,
                     boot_ci = c("auto", "perc", "bca", "stud", "basic"),
                     replicates = 1999,
                     se_method = c("jackknife", "influence", "normal")) {
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
  unit <- match.arg(unit)
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  boot_ci <- match.arg(boot_ci)
  se_method <- match.arg(se_method)
  .check_conf_level(conf.level)
  .check_boot_args(boot, replicates)
  boot_ci <- .resolve_boot_ci(boot, boot_ci, method, se_method)
  # without a test the interval is two-sided
  if (is.null(null.value)) {
    alternative <- "two.sided"
  }

  prep <- .reli_prepare(x = x, data = data, measure = measure, item = item,
                        id = id, col.names = col.names)
  form <- .icc_form(model, type, unit)
  .check_icc_null(null.value, form, prep$k)

  fit <- .reli_fit(prep, method = method, model = model)
  if (method == "reml" && !prep$balanced && !boot) {
    .reli_warn_unbalanced(prep)
  }

  estimate <- .icc_estimate(fit, form)
  label <- .icc_label(form)
  k_z <- if (.icc_is_average(form)) prep$k else fit$kbar
  se_z <- .icc_se_z(prep, fit, form, se_method)
  z_obs <- .icc_z(estimate, form, k_z)
  drho_dz <- .num_grad(function(z) .icc_z_inv(z[[1]], form, k_z), c(z = z_obs))
  stderr <- c(ICC = abs(drho_dz) * se_z, z = se_z)

  if (!boot) {
    test <- if (is.null(null.value)) {
      list(statistic = NULL, parameter = NULL, p.value = NULL)
    } else {
      .icc_ftest(fit, form, null.value, alternative)
    }
    conf.int <- .icc_ci(fit, form, conf.level, alternative)
    boot_info <- NULL
  } else {
    agreement <- form %in% c("A1", "Ak")
    stat_fun <- function(p, resample) {
      f <- .reli_fit(p, method = method, model = model, keep_fit = FALSE)
      if (resample && agreement) {
        f <- .hybrid_item_ms(f)
      }
      c(est = .icc_estimate(f, form),
        z = .icc_z_fit(f, form),
        se_z = if (boot_ci == "stud") .icc_se_z(p, f, form, se_method) else
          NA_real_)
    }
    bo <- .reli_boot(prep, stat_fun, R = replicates,
                     jackknife = boot_ci == "bca")
    .check_boot_failures(bo)
    bounds <- c(if (.icc_is_average(form)) -Inf else -1 / (prep$k - 1), 1)
    conf.int <- .boot_interval(bo, boot_ci, conf.level, alternative,
                               back = function(z) .icc_z_inv(z, form, k_z),
                               bounds = bounds)
    test <- list(statistic = NULL, parameter = NULL,
                 p.value = if (is.null(null.value)) NULL else
                   .boot_pvalue(bo, boot_ci, null.value, alternative,
                                fwd = function(r) .icc_z(r, form, k_z)))
    boot_info <- list(boot_ci = boot_ci,
                      replicates = replicates,
                      n_used = nrow(bo$t),
                      n_failed = bo$n_failed,
                      boot_se = stats::sd(bo$t[, "est"]),
                      estimates = bo$t[, "est"])
    stderr <- c(stderr, boot = boot_info$boot_se)
  }

  settings <- list(statistic = "icc",
                   form = form,
                   model = model,
                   type = type,
                   unit = unit,
                   method = method,
                   conf.level = conf.level,
                   boot = boot,
                   boot_ci = if (boot) boot_ci else NULL,
                   se_method = se_method,
                   label = label)

  structure(list(statistic = test$statistic,
                 parameter = test$parameter,
                 p.value = test$p.value,
                 conf.int = conf.int,
                 estimate = stats::setNames(estimate, label),
                 stderr = stderr,
                 null.value = if (is.null(null.value)) NULL else
                   c(ICC = null.value),
                 alternative = alternative,
                 method = .icc_method_text(form, method, boot, boot_ci),
                 data.name = prep$data.name,
                 fit = fit,
                 data = prep$df,
                 settings = settings,
                 boot = boot_info),
            class = c("reli_htest", "htest"))
}

# Helpers --------------------------------------------------------------------

.icc_label <- function(form) {
  switch(form,
         "1" = "ICC(1)", "k" = "ICC(k)",
         "C1" = "ICC(C,1)", "Ck" = "ICC(C,k)",
         "A1" = "ICC(A,1)", "Ak" = "ICC(A,k)")
}

.icc_shrout_fleiss <- function(form) {
  switch(form,
         "1" = "ICC(1,1)", "k" = "ICC(1,k)",
         "C1" = "ICC(3,1)", "Ck" = "ICC(3,k)",
         "A1" = "ICC(2,1)", "Ak" = "ICC(2,k)")
}

.icc_description <- function(form) {
  model <- if (form %in% c("1", "k")) "One-way" else "Two-way"
  type <- if (form %in% c("C1", "Ck")) "consistency" else "absolute agreement"
  unit <- if (.icc_is_average(form)) "average-measure" else "single-measure"
  paste0(model, ", ", type, ", ", unit, " ICC")
}

.icc_method_text <- function(form, method, boot, boot_ci) {
  base <- paste0(.icc_description(form), " [Shrout & Fleiss ",
                 .icc_shrout_fleiss(form), "]")
  fit <- if (method == "reml") " (REML)" else ""
  inference <- if (boot) {
    paste0(" with nonparametric (subject) bootstrap ",
           .boot_ci_name(boot_ci), " interval")
  } else ""
  paste0(base, fit, inference)
}

.boot_ci_name <- function(boot_ci) {
  switch(boot_ci, bca = "BCa", stud = "studentized",
         perc = "percentile", basic = "basic")
}

#' Resolve `boot_ci = "auto"` and check the studentized bootstrap settings
#'
#' "auto" was chosen from simulations: studentized (with a resample-based SE)
#' for complete data, percentile for REML fits. With `agreement_bca = TRUE`
#' (two-way agreement error statistics in `reli_err_test()`), complete data
#' use BCa, which covered best for those statistics.
#' @noRd
.resolve_boot_ci <- function(boot, boot_ci, method, se_method,
                             agreement_bca = FALSE) {
  if (boot_ci == "auto") {
    boot_ci <- if (method == "reml") {
      "perc"
    } else if (agreement_bca) {
      "bca"
    } else if (se_method != "normal") {
      "stud"
    } else {
      "perc"
    }
  }
  if (boot && boot_ci == "stud") {
    if (se_method == "normal") {
      stop("boot_ci = \"stud\" needs a standard error estimated from each ",
           "resample; the normal-theory SE depends only on the degrees of ",
           "freedom and cannot studentize. Use se_method = \"influence\" or ",
           "\"jackknife\".", call. = FALSE)
    }
    if (se_method == "jackknife" && method == "reml") {
      stop("boot_ci = \"stud\" with se_method = \"jackknife\" and ",
           "method = \"reml\" refits the mixed model n times in every ",
           "bootstrap replicate. Use se_method = \"influence\".",
           call. = FALSE)
    }
    if (method == "reml") {
      warning("The studentized bootstrap with method = \"reml\" can be ",
              "unreliable: in simulations with missing data it under-covered ",
              "at moderate ICCs (0.80 to 0.88 for 95% intervals). Consider ",
              "boot_ci = \"perc\".", call. = FALSE)
    }
  }
  boot_ci
}

.check_conf_level <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      is.na(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be a single number between 0 and 1.",
         call. = FALSE)
  }
}

.check_boot_args <- function(boot, replicates) {
  if (!is.logical(boot) || length(boot) != 1 || is.na(boot)) {
    stop("'boot' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(replicates) || length(replicates) != 1 ||
      replicates < 2 || replicates != round(replicates)) {
    stop("'replicates' must be a whole number of at least 2.", call. = FALSE)
  }
}

.check_icc_null <- function(null.value, form, k) {
  lower <- if (.icc_is_average(form)) -Inf else -1 / (k - 1)
  if (is.null(null.value)) {
    return(invisible())
  }
  if (!is.numeric(null.value) || length(null.value) != 1 ||
      is.na(null.value) || null.value >= 1 || null.value <= lower) {
    stop("'null.value' must be a single number below 1",
         if (is.finite(lower)) sprintf(" and above %s (-1/(k - 1))",
                                       format(signif(lower, 3))),
         ".", call. = FALSE)
  }
}

.check_boot_failures <- function(bo) {
  if (nrow(bo$t) < 2) {
    stop("Fewer than 2 bootstrap replicates could be computed.", call. = FALSE)
  }
  if (bo$n_failed > 0.1 * bo$R) {
    warning(sprintf(paste0("%d of %d bootstrap replicates failed and were ",
                           "dropped; the bootstrap interval may be unreliable."),
                    bo$n_failed, bo$R), call. = FALSE)
  }
}
