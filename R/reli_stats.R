#' @title Reliability Statistics
#'
#' @description
#'
#' `r lifecycle::badge('superseded')`
#'
#' `reli_stats()` and `reli_aov()` return the intraclass correlation
#' coefficients (ICCs), the coefficient of variation (CV), and the standard
#' errors of measurement (SEM), estimate (SEE) and prediction (SEP)
#' described by Weir (2005), all at once.
#'
#' These functions are superseded: they still work, but new code should use
#' [icc_test()] for an ICC (with a test against a meaningful null value and
#' optional bootstrap inference) and [reli_err_test()] for the SEM, SEE or
#' SEP, including the CV via `scale = "log"` (sympercents) or
#' `scale = "mean"`. See Details for how the two sets of functions differ.
#'
#' @param measure Name of column containing the measurement of interest.
#' @param item Name of column containing the items. If this is a test-retest
#'   reliability study then this would indicate the time point (e.g., time1,
#'   time2, time3, etc.).
#' @param id Column with subject identifier.
#' @param data Data frame with all data.
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a
#'   "wide" format. Default is FALSE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the
#'   column names containing the measurements for reliability analysis.
#' @param conf.level The confidence level required. Default is 95%. The ICC
#'   intervals are two-sided (see Details).
#' @param cv_calc Coefficient of variation (CV) calculation: `"MSE"`
#'   (default), the square root of the error mean square divided by the
#'   grand mean, or `"SEM"`, the SEM (see `se_type`) divided by the grand
#'   mean. `"residuals"` is no longer supported and is replaced by `"MSE"`
#'   with a warning.
#' @param other_ci Logical value (TRUE or FALSE) indicating whether to
#'   calculate confidence intervals for the CV, SEM, SEP, and SEE.
#' @param se_type Type of standard error calculation. The default, `"MSE"`,
#'   uses the error mean square for the SEM, and ICC3 (ICC(C,1)) for the SEE
#'   and SEP. Otherwise, the SD of all the measurements and the named ICC are
#'   used for all three (see Details).
#' @param type The type of confidence intervals for the CV, SEM, SEP, and SEE:
#'   `"chisq"` (default; analytic intervals, see Details), or a bootstrap
#'   interval, `"perc"` (percentile), `"norm"` (normal, with bias correction)
#'   or `"basic"`.
#' @param replicates The number of bootstrap replicates. Default is 1999.
#'
#' @details
#' **Estimation.** Both functions use the engine of [icc_test()]:
#' `reli_stats()` fits a linear mixed model by REML (crossed random effects
#' for subjects and items) and allows missing measurements; `reli_aov()`
#' uses the analysis of variance and keeps only subjects measured on every
#' item (incomplete subjects are dropped with a message). With complete data
#' the two give the same results (unless a variance component is estimated
#' at zero). With missing data, `reli_stats()` uses degrees of freedom based
#' on the observed data and warns that its intervals are approximate.
#'
#' **ICCs.** The six forms of Shrout and Fleiss (1979) are returned, with
#' the F-based intervals of McGraw and Wong (1996). The intervals are
#' two-sided `conf.level` intervals, as in those papers (and in the psych,
#' irr, SPSS and Stata implementations). Before version 0.4.0 the intervals
#' used `conf.level` in each tail, so each limit was a one-sided
#' `conf.level` bound and the pair covered only `1 - 2 * (1 - conf.level)`;
#' the old output at `conf.level = c` is reproduced by
#' `conf.level = 1 - 2 * (1 - c)` (e.g. old 0.95 = new 0.90). For a one-sided
#' lower bound on a single ICC, use `icc_test(alternative = "greater")`.
#'
#' **SEM, SEE and SEP.** With `se_type = "MSE"`, SEM = \eqn{\sqrt{MS_E}}
#' (Weir, 2005, preferred), and SEE = \eqn{SD\sqrt{ICC(1 - ICC)}} and
#' SEP = \eqn{SD\sqrt{1 - ICC^2}} with ICC3. With another `se_type`, the
#' named ICC is used for all three, and SEM = \eqn{SD\sqrt{1 - ICC}}. Here SD
#' is the SD of all the measurements, \eqn{\sqrt{SS_{total}/(N - 1)}}, as in
#' Weir (2005). [reli_err_test()] instead uses the SD matched to the ICC's
#' denominator, so its SEE and SEP differ slightly. A negative ICC is
#' treated as zero.
#'
#' **CV.** The CV is the SEM (or \eqn{\sqrt{MS_E}}) divided by the grand
#' mean, as a proportion (printed as a percentage).
#'
#' **Intervals for the CV, SEM, SEE and SEP** (`other_ci = TRUE`). With
#' `type = "chisq"`, the SEM from \eqn{MS_E} has the exact
#' \eqn{\chi^2} interval; every other statistic has a Wald interval on the
#' log scale with a delta-method standard error from the normal-theory
#' variances of the mean squares (and of the grand mean, for the CV). Before
#' version 0.4.0 these used \eqn{\chi^2} intervals with the error degrees of
#' freedom (and Vangel's interval for the CV), which do not apply to these
#' statistics. The bootstrap types resample subjects (as [icc_test()] does,
#' including its hybrid step for the item mean square); before version 0.4.0
#' `reli_stats()` used a parametric bootstrap.
#'
#' @return Returns single list with the results of the reliability analysis.
#'
#'   - `icc`: Table of ICC results
#'   - `lmer`: The fitted model: a linear mixed model from lme4
#'     (`reli_stats()`), or a linear model of subjects and items
#'     (`reli_aov()`)
#'   - `anova`: Analysis of Variance table (mean squares reconstructed from
#'     the variance components for `reli_stats()`)
#'   - `var_comp`: Table of Variance Components
#'   - `n.id`: Number of subjects/participants
#'   - `n.item`: Number of items/time points
#'   - `cv`: Coefficient of Variation (a one-row data frame)
#'   - `SEM`: Standard Error of Measurement (a one-row data frame)
#'   - `SEE`: Standard Error of the Estimate (a one-row data frame)
#'   - `SEP`: Standard Error of Prediction (a one-row data frame)
#'   - `call`: the matched call, with the settings used
#'
#' @examples
#' data('reps')
#' reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
#'
#' # The recommended replacements (reps has missing measurements, so REML)
#' icc_test(data = reps, col.names = c("x", "y"), method = "reml")
#' reli_err_test(data = reps, col.names = c("x", "y"), method = "reml")
#'
#' @references
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the
#' intraclass correlation coefficient and the SEM. Journal of Strength and
#' Conditioning Research, 19(1), 231-240. \doi{10.1519/15184.1}
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
#' assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
#' \doi{10.1037/0033-2909.86.2.420}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' @seealso [icc_test()], [reli_err_test()]
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var residuals aov
#' @import ggplot2
#' @import lme4
#' @export

reli_stats = function(measure = NULL,
                      item = NULL,
                      id = NULL,
                      data,
                      wide = FALSE,
                      col.names = NULL,
                      se_type = c("MSE","ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"),
                      cv_calc = c("MSE","residuals","SEM"),
                      conf.level = .95,
                      other_ci = FALSE,
                      type = c("chisq", "perc", "norm", "basic"),
                      replicates = 1999){
  se_type = match.arg(se_type)
  cv_calc = match.arg(cv_calc)
  type = match.arg(type)
  prep = .legacy_prepare(measure, item, id, data, wide, col.names)
  if (prep$k == 2) {
    message("Only 2 items in data. It is recommended to use reli_aov instead of reli_stats.")
  }
  result = .reli_legacy(prep, method = "reml", se_type = se_type,
                        cv_calc = cv_calc, conf.level = conf.level,
                        other_ci = other_ci, type = type,
                        replicates = replicates)
  result$call = .legacy_call(match.call(), result, conf.level, other_ci)
  result$prep = NULL
  structure(result, class = "simple_reli")
}

#' @rdname reli_stats
#' @export
#'
reli_aov = function(measure = NULL,
                    item = NULL,
                    id = NULL,
                    data,
                    wide = FALSE,
                    col.names = NULL,
                    se_type = c("MSE","ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"),
                    cv_calc = c("MSE","residuals","SEM"),
                    conf.level = .95,
                    other_ci = FALSE,
                    type = c("chisq", "perc", "norm", "basic"),
                    replicates = 1999) {
  se_type = match.arg(se_type)
  cv_calc = match.arg(cv_calc)
  type = match.arg(type)
  prep = .legacy_prepare(measure, item, id, data, wide, col.names)
  prep = .complete_subjects(prep)
  result = .reli_legacy(prep, method = "anova", se_type = se_type,
                        cv_calc = cv_calc, conf.level = conf.level,
                        other_ci = other_ci, type = type,
                        replicates = replicates)
  result$call = .legacy_call(match.call(), result, conf.level, other_ci)
  result$prep = NULL
  structure(result, class = "simple_reli")
}

# Legacy engine ----------------------------------------------------------------

.legacy_prepare = function(measure, item, id, data, wide, col.names) {
  if (isTRUE(wide)) {
    if (is.null(col.names)) {
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    return(.reli_prepare(data = data, col.names = col.names))
  }
  if (is.null(measure) || is.null(item) || is.null(id)) {
    stop("Must provide measure, item, and id column names if wide = FALSE")
  }
  .reli_prepare(data = data, measure = measure, item = item, id = id)
}

# Keep only subjects measured on every item (complete-case ANOVA).
.complete_subjects = function(prep) {
  if (prep$balanced) {
    return(prep)
  }
  counts = table(prep$df$id)
  keep = names(counts)[counts == prep$k]
  dropped = length(counts) - length(keep)
  if (length(keep) < 2) {
    stop("Fewer than 2 subjects were measured on every item. ",
         "Use reli_stats() or icc_test(method = \"reml\") for incomplete data.",
         call. = FALSE)
  }
  message(sprintf(paste0("%d subject(s) without a measurement on every item ",
                         "were dropped. Use reli_stats() or ",
                         "icc_test(method = \"reml\") to keep them."), dropped))
  .reli_prep_from_long(prep$df[prep$df$id %in% keep, , drop = FALSE],
                       data.name = prep$data.name)
}

# Store evaluated settings (not the code typed) so print() works when they
# were passed as variables, and the data the plot needs.
.legacy_call = function(call2, result, conf.level, other_ci) {
  df = result$prep$df
  call2$lm_mod = plot_frame(values = df$values, id = df$id, items = df$items)
  call2$conf.level = conf.level
  call2$other_ci = other_ci
  call2
}

.legacy_forms = c(ICC1 = "1", ICC2 = "A1", ICC3 = "C1",
                  ICC1k = "k", ICC2k = "Ak", ICC3k = "Ck")

.reli_legacy = function(prep, method, se_type, cv_calc, conf.level,
                        other_ci, type, replicates) {
  .check_conf_level(conf.level)
  if (cv_calc == "residuals") {
    warning("cv_calc = \"residuals\" is no longer supported (it was a ",
            "downward-biased version of \"MSE\"); using cv_calc = \"MSE\".",
            call. = FALSE)
    cv_calc = "MSE"
  }
  fit = .reli_fit(prep, method = method, model = "twoway")
  if (method == "reml" && !prep$balanced) {
    pct = 100 * prep$n_missing / (prep$n * prep$k)
    warning(sprintf(paste0(
      "%.1f%% of subject-by-item cells are missing. Intervals use ",
      "observed-data degrees of freedom and are approximate; consider ",
      "icc_test(method = \"reml\", boot = TRUE)."), pct), call. = FALSE)
  }
  ms = fit$ms
  dfs = fit$df

  # ANOVA table (subjects, items, residual)
  stats = matrix(NA, ncol = 3, nrow = 5)
  stats[1, ] = c(dfs[["dfB"]], dfs[["dfJ"]], dfs[["dfE"]])
  stats[3, ] = c(ms[["MSB"]], ms[["MSJ"]], ms[["MSE"]])
  stats[2, ] = stats[1, ] * stats[3, ]
  stats[4, 1:2] = stats[3, 1:2] / ms[["MSE"]]
  stats[5, 1:2] = stats::pf(stats[4, 1:2], stats[1, 1:2], dfs[["dfE"]],
                            lower.tail = FALSE)
  colnames(stats) = c("Subjects", "items", "Residual")
  rownames(stats) = c("df", "SumSq", "MS", "F", "p")

  # Variance components
  vc = fit$vc
  MS.df = data.frame(variance = c(vc[["id"]], vc[["items"]], vc[["resid"]],
                                  sum(vc)))
  rownames(MS.df) = c("ID", "Items", "Residual", "Total")
  MS.df$percent = MS.df$variance / sum(vc)

  # ICCs with two-sided F-based intervals
  results = data.frame(
    model = c("one-way random","two-way random", "two-way fixed",
              "one-way random","two-way random", "two-way fixed"),
    measures = c("Agreement", "Agreement", "Consistency",
                 "Avg. Agreement", "Avg. Agreement", "Avg. Consistency"),
    type = names(.legacy_forms),
    icc = NA_real_, lower.ci = NA_real_, upper.ci = NA_real_)
  for (i in seq_along(.legacy_forms)) {
    results$icc[i] = .icc_estimate(fit, .legacy_forms[[i]])
    results[i, c("lower.ci", "upper.ci")] =
      as.numeric(.icc_ci(fit, .legacy_forms[[i]], conf.level))
  }

  # CV, SEM, SEE, SEP
  est = .legacy_other(fit, se_type, cv_calc)
  res_other = data.frame(estimate = est, bias = NA_real_, se = NA_real_,
                         lower.ci = NA_real_, upper.ci = NA_real_,
                         row.names = names(est))
  if (isTRUE(other_ci)) {
    res_other = if (type == "chisq") {
      .legacy_other_analytic(fit, se_type, cv_calc, conf.level, res_other)
    } else {
      .legacy_other_boot(prep, fit, method, se_type, cv_calc, conf.level,
                         type, replicates, res_other)
    }
  }

  list(icc = results,
       lmer = fit$fit,
       anova = stats,
       var_comp = MS.df,
       n.id = prep$n,
       n.item = prep$k,
       call = NULL,
       cv = res_other["cv", ],
       SEM = res_other["SEM", ],
       SEE = res_other["SEE", ],
       SEP = res_other["SEP", ],
       prep = prep)
}

# The four legacy statistics from a reli_fit (MSW recomputed from MSJ and
# MSE, so the function depends only on MSB, MSJ, MSE and the grand mean).
.legacy_other = function(fit, se_type, cv_calc) {
  ms = fit$ms
  n = fit$n
  fit$ms[["MSW"]] = (ms[["MSJ"]] + (n - 1) * ms[["MSE"]]) / n
  # SD of all measurements: sqrt(SS_total / (N - 1))
  sd_tots = sqrt(sum(fit$df[c("dfB", "dfJ", "dfE")] *
                       fit$ms[c("MSB", "MSJ", "MSE")]) / (fit$N - 1))
  icc_of = function(nm) max(.icc_estimate(fit, .legacy_forms[[nm]]), 0)
  rho = icc_of(if (se_type == "MSE") "ICC3" else se_type)
  SEM = if (se_type == "MSE") sqrt(fit$ms[["MSE"]]) else
    sd_tots * sqrt(1 - rho)
  SEE = sd_tots * sqrt(rho * (1 - rho))
  SEP = sd_tots * sqrt(1 - rho^2)
  stddev = if (cv_calc == "MSE") sqrt(fit$ms[["MSE"]]) else SEM
  c(cv = stddev / fit$grand_mean, SEM = SEM, SEP = SEP, SEE = SEE)
}

# Analytic intervals: exact chi-square for sqrt(MSE), log-scale delta method
# for everything else.
.legacy_other_analytic = function(fit, se_type, cv_calc, conf.level, res) {
  alpha = 1 - conf.level
  names_ms = c("MSB", "MSJ", "MSE")
  theta = c(fit$ms[names_ms], mean = fit$grand_mean)
  var_theta = c(.ms_var(fit)[names_ms],
                mean = .grand_mean_var(fit, "twoway", "consistency"))
  for (stat in rownames(res)) {
    exact_sem = stat == "SEM" && se_type == "MSE"
    if (exact_sem) {
      ci = sqrt(.var_ci(fit$ms[["MSE"]], fit$df[["dfE"]], conf.level))
    } else {
      g = function(th) {
        f = fit
        f$ms[names_ms] = th[names_ms]
        f$grand_mean = th[["mean"]]
        log(.legacy_other(f, se_type, cv_calc)[[stat]])
      }
      se = .delta_se(g, theta, var_theta)
      ci = res[stat, "estimate"] * exp(c(-1, 1) * stats::qnorm(1 - alpha / 2) * se)
      res[stat, "se"] = res[stat, "estimate"] * se
    }
    res[stat, c("lower.ci", "upper.ci")] = as.numeric(ci)
  }
  res
}

# Subject bootstrap (hybrid step for the item mean square and grand mean),
# percentile, basic or normal intervals.
.legacy_other_boot = function(prep, fit, method, se_type, cv_calc,
                              conf.level, type, replicates, res) {
  alpha = 1 - conf.level
  stats = rownames(res)
  boots = t(vapply(seq_len(replicates), function(i) {
    p = tryCatch(.reli_resample(prep), error = function(e) NULL)
    if (is.null(p)) {
      return(rep(NA_real_, length(stats)))
    }
    tryCatch(suppressWarnings({
      f = .reli_fit(p, method = method, model = "twoway", keep_fit = FALSE)
      .legacy_other(.hybrid_item_ms(f, shift_mean = TRUE), se_type,
                    cv_calc)[stats]
    }), error = function(e) rep(NA_real_, length(stats)))
  }, numeric(length(stats))))
  colnames(boots) = stats
  ok = stats::complete.cases(boots)
  .check_boot_failures(list(t = boots[ok, , drop = FALSE], R = replicates,
                            n_failed = sum(!ok)))
  boots = boots[ok, , drop = FALSE]
  for (stat in stats) {
    b = boots[, stat]
    est = res[stat, "estimate"]
    res[stat, "bias"] = mean(b) - est
    res[stat, "se"] = stats::sd(b)
    ci = switch(type,
                perc = stats::quantile(b, c(alpha / 2, 1 - alpha / 2),
                                       names = FALSE),
                basic = 2 * est - stats::quantile(b, c(1 - alpha / 2, alpha / 2),
                                                  names = FALSE),
                norm = est - res[stat, "bias"] +
                  c(-1, 1) * stats::qnorm(1 - alpha / 2) * res[stat, "se"])
    res[stat, c("lower.ci", "upper.ci")] = ci
  }
  res
}
