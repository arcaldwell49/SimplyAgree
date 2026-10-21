#' Reliability Across Facets: Repeats and Occasions
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' **The name of this function is experimental and may change** (it was
#' briefly `gstudy_test()` during development); its arguments may change
#' too.
#'
#' Estimates one reliability coefficient or one standard error of
#' measurement (SEM) when each subject is measured more than once by each
#' item (rater, device): for example 3 operators each scanning 20
#' participants twice. The extra measurements separate error *within* a
#' rater from disagreement *between* raters, which [icc_test()] and
#' [reli_err_test()] cannot do with one measurement per subject and item.
#' The measurements can be exchangeable repeats within each subject-by-item
#' cell (the default), or shared occasions (`occasion`: every rater measured
#' every subject on day 1 and again on day 2). The model is the
#' generalizability-theory (G-study) model for that design. Returns an
#' object of class \code{"htest"} with a confidence interval and an optional
#' test.
#'
#' @inheritParams icc_test
#' @param x A formula of the form `values ~ item | id`, with several rows per
#'   subject and item. Leave `NULL` to use `measure`, `item` and `id`. Wide
#'   data are not supported.
#' @param occasion `NULL` (default) when the measurements within each
#'   subject-by-item cell are exchangeable repeats, or the name of the column
#'   identifying shared occasions (e.g. day 1 and day 2, the same for every
#'   item). See "Nested or crossed".
#' @param stat `"ICC"` (default), a reliability coefficient, or `"SEM"`, the
#'   standard error of measurement. See Details.
#' @param facet Which measurement varies: `"intra"` (default), the same item
#'   on another repeat or occasion (intra-rater, repeatability); `"inter"`,
#'   another item (on the same occasion, with `occasion`); or `"overall"`,
#'   another item on another occasion (the same as `"inter"` without
#'   `occasion`). See Details.
#' @param type `"consistency"` (default) sets systematic differences between
#'   items (and occasions) aside, to be reported separately; `"agreement"`
#'   counts them as error, as in [icc_test()]. Without `occasion`,
#'   `facet = "intra"` has no systematic effects and `type` makes no
#'   difference.
#' @param k_avg,m_avg,r_avg The planned score (D-study): the mean of `k_avg`
#'   items, `m_avg` occasions and `r_avg` repeats (defaults 1, a single
#'   measurement). `k_avg` must be 1 for `facet = "intra"` and `m_avg` must
#'   be 1 for `facet = "inter"` and without `occasion`; `r_avg` must be 1
#'   for crossed data with one measurement per subject, item and occasion.
#' @param scale For `stat = "SEM"`, the scale it is reported on: `"raw"`
#'   (default), `"log"` (sympercents) or `"mean"` (percentage of the grand
#'   mean), as in [reli_err_test()]. Must be `"raw"` for `stat = "ICC"`.
#' @param method `"anova"` (default) for balanced data (every subject
#'   measured by every item, on every occasion, the same number of times),
#'   or `"reml"` (a linear mixed model) for missing cells or unequal numbers
#'   of repeats.
#' @param null.value A value to test against, on the scale of the estimate.
#'   The default, `NULL`, gives the estimate and interval without a test.
#' @param alternative `"two.sided"` (default), `"greater"` or `"less"`. For
#'   an SEM, `"less"` tests whether the error is below an acceptable value.
#'   Used only when `null.value` is given.
#' @param ci_method The analytic interval when it is not exact: `"auto"`
#'   (default), `"mls"` (modified large-sample) or `"satterthwaite"`. See
#'   Details. Ignored when the interval is exact or `boot = TRUE`.
#' @param boot_ci Bootstrap interval type: `"auto"` (default), `"perc"`,
#'   `"bca"`, `"stud"` (studentized) or `"basic"`. With `method = "anova"`,
#'   `"auto"` uses `"bca"` for an agreement SEM that counts systematic
#'   differences between items or occasions and `"stud"` otherwise
#'   (`"perc"` when `se_method = "normal"`); with `method = "reml"` it uses
#'   `"perc"`. See Details. Used only when `boot = TRUE`.
#' @param se_method How the standard error on the working scale is
#'   estimated: `"jackknife"` (default; delete one subject at a time) or
#'   `"normal"` (normal theory, delta method). Reported in `stderr` and used
#'   by the studentized bootstrap.
#'
#' @details
#' **Nested or crossed.** Ask whether "measurement 2" is the same event for
#' every item. If each operator simply scanned each participant twice, in no
#' shared order, the measurements are exchangeable repeats nested in the
#' subject-by-item cell: leave `occasion = NULL`. If every operator measured
#' everyone on day 1 and again on day 2, the days are shared occasions:
#' give `occasion`. Treating occasions as repeats counts day-to-day shifts as
#' repeat error, so the intra-rater error looks larger and cannot be split
#' into a systematic shift (fixable) and noise. With `occasion`, the result
#' (in `nested`) and `summary()` also give what the repeat model would have
#' estimated.
#'
#' **Model.** All effects are random. Without `occasion`, with n subjects, k
#' items and r repeats per cell,
#' \deqn{y_{pir} = \mu + p + i + (pi) + e_{pir};}
#' with `occasion` (m occasions),
#' \deqn{y_{pio} = \mu + p + i + o + (pi) + (po) + (io) + (pio, e),}
#' where with one measurement per subject, item and occasion the three-way
#' interaction and the error are not separable (the residual). Further
#' repeats within each subject, item and occasion are allowed and add a
#' separate repeat error. The ANOVA estimators are unconstrained: a negative
#' variance component estimate is kept (as [icc_test()] keeps a negative
#' ICC), so a coefficient can be negative. The component table in
#' `components` (and `summary()`) shows each component with an interval and
#' its share of the variance of a single measurement. The model is the
#' unrestricted (random facets) one used by lme4 and by McGraw and Wong
#' (1996); for the consistency forms the items may be regarded as fixed,
#' which changes the interpretation but not the computation.
#'
#' **Estimands.** Following Brennan (2001), the facet that stays the same is
#' part of the subject's score, and the facets that vary are error. For a
#' planned score that is the mean of \eqn{k'} items (`k_avg`), \eqn{m'}
#' occasions (`m_avg`) and \eqn{r'} repeats (`r_avg`), the error variance
#' (SEM\eqn{^2}) is:
#'
#' | `facet` | Score | Consistency error | Agreement adds |
#' |---|---|---|---|
#' | intra, repeats | \eqn{\sigma^2_p + \sigma^2_{pi}} | \eqn{\sigma^2_e/r'} | (nothing) |
#' | intra, occasions | \eqn{\sigma^2_p + \sigma^2_{pi}} | \eqn{(\sigma^2_{po} + \sigma^2_{pio,e})/m'} | \eqn{(\sigma^2_o + \sigma^2_{io})/m'} |
#' | inter, repeats | \eqn{\sigma^2_p} | \eqn{\sigma^2_{pi}/k' + \sigma^2_e/(k'r')} | \eqn{\sigma^2_i/k'} |
#' | inter, occasions | \eqn{\sigma^2_p + \sigma^2_{po}} | \eqn{(\sigma^2_{pi} + \sigma^2_{pio,e})/k'} | \eqn{(\sigma^2_i + \sigma^2_{io})/k'} |
#' | overall, occasions | \eqn{\sigma^2_p} | \eqn{\sigma^2_{pi}/k' + \sigma^2_{po}/m' + \sigma^2_{pio,e}/(k'm')} | \eqn{\sigma^2_i/k' + \sigma^2_o/m' + \sigma^2_{io}/(k'm')} |
#'
#' (with further repeats within subject, item and occasion, the residual is
#' split and its error part is also divided by \eqn{r'}). The coefficient
#' (`stat = "ICC"`) is score / (score + error). The intra-rater coefficient
#' is the correlation between two measurements by the same item, pooled
#' over items (it assumes the same error for every item; `check()` shows the
#' within-cell SD by item). With occasions, the inter-rater coefficient is
#' for raters on the same occasion, what most inter-rater studies report;
#' `"overall"` is for any rater on any occasion, and its coefficients are
#' the generalizability (\eqn{E\rho^2}) and dependability (\eqn{\Phi})
#' coefficients of G theory. In gauge R&R terms (repeats), \eqn{\sigma^2_e}
#' is repeatability and \eqn{\sigma^2_i + \sigma^2_{pi}} reproducibility.
#'
#' **What averaging does.** Without `occasion` and with \eqn{r' = r} (the
#' score averages as many repeats as the study had), the inter-rater
#' consistency coefficient is the consistency ICC of the cell means,
#' \eqn{(F - 1)/(F - 1 + k/k')} with \eqn{F = MS_P/MS_{PI}}, identical to
#' [icc_test()] on the cell means; it is not the reliability of a *single*
#' measurement unless r = 1. Averaging the repeats before calling
#' [icc_test()] is therefore only right when the scores used in practice
#' are also means of r repeats.
#'
#' **Intervals and tests.** An estimand that is a multiple of one mean
#' square (an SEM) or a function of the ratio of two (a coefficient) has an
#' exact interval and test: without `occasion`, the intra SEM (\eqn{\chi^2}
#' on \eqn{MS_W}) and the inter-rater consistency SEM and coefficient with
#' \eqn{r' = r}. The others are approximate:
#'
#' - `"mls"`: modified large-sample intervals for linear combinations of
#'   mean squares (Graybill & Wang, 1980; Ting et al., 1990; Burdick &
#'   Graybill, 1992). For a coefficient \eqn{\rho}, the interval holds the
#'   values \eqn{\rho_0} for which the MLS interval of
#'   \eqn{\sigma^2_{score} - \rho_0 \sigma^2_{total}} contains 0. The
#'   p-value inverts the interval, so there is no test statistic.
#' - `"satterthwaite"`: the SEM uses a \eqn{\chi^2} distribution with
#'   Satterthwaite df; a coefficient uses an F test of
#'   \eqn{\sigma^2_{score} - \rho_0 \sigma^2_{total} = 0}, the positive and
#'   negative parts each with Satterthwaite df (McGraw & Wong's device for
#'   ICC(A,1)), and the interval inverts it.
#' - `"auto"` uses MLS, chosen in simulations (see "Simulations").
#'
#' SEMs with `scale = "mean"` use a Wald interval on the log scale with a
#' delta-method SE that includes the uncertainty of the grand mean.
#'
#' **Bootstrap.** `boot = TRUE` resamples subjects, each with all its
#' measurements, as in [icc_test()]. Resampling subjects keeps the same
#' items and occasions, so for statistics that count systematic differences
#' between them as error, the mean squares of the effects without subjects
#' (items, occasions, items by occasions) are also multiplied by independent
#' \eqn{\chi^2_{df}/df} draws (the hybrid step). The p-value inverts the
#' interval and is approximate.
#'
#' **Unbalanced data.** `method = "reml"` fits the model with lme4 and keeps
#' the analytic inference, with degrees of freedom from the observed data
#' and the harmonic mean number of repeats in place of r, and gives a
#' warning.
#'
#' **Simulations.** Repeats (no `occasion`): with 10 or 30 subjects, 2, 3
#' or 5 items, 2 or 3 repeats, four mixes of variance components and normal
#' data (2000 data sets per setting), the exact intervals covered 0.940 to
#' 0.951 and the MLS intervals at least 0.939 for every estimand (0.95 to
#' 0.96 on average; slightly conservative for \eqn{\Phi}), hence
#' `ci_method = "auto"` uses MLS. The Satterthwaite intervals were similar
#' for the intra-rater and consistency statistics but fell to 0.67 to 0.74
#' for the agreement SEM and coefficient with 2 items when systematic
#' differences between items were the main error. With REML and unbalanced
#' data (30 subjects, 20% of repeats missing, or 1 to 3 repeats per cell),
#' the MLS intervals covered 0.937 to 0.969.
#'
#' Occasions (2 or 3 items, 2 or 3 occasions, one measurement per cell,
#' four mixes including a large day effect and large rater bias, 2000 data
#' sets per setting): the MLS intervals covered at least 0.934 with 30
#' subjects and 0.928 with 10, for every facet, type and statistic; the
#' Satterthwaite intervals fell to 0.69 to 0.79 for the agreement
#' statistics. The MLS intervals for the agreement *coefficients* are
#' conservative (up to 0.97 to 0.99 with 30 subjects), so their tests reject
#' less often than the nominal level. With REML and 20% of cells missing,
#' MLS covered 0.922 to 0.979. Treating the occasions as exchangeable
#' repeats (leaving out `occasion`) gave intra-rater SEM intervals that
#' covered the true agreement SEM only 0.24 to 0.92 of the time (0.04 to
#' 0.87 for the consistency SEM, which it overstated by 30% on average).
#'
#' With skewed, heavy-tailed data every analytic interval under-covered
#' (0.78 to 0.90 with repeats). The studentized bootstrap was then the best
#' interval for every statistic except the agreement SEM, for which BCa was
#' best, hence `boot_ci = "auto"`: with repeats it covered 0.90 to 0.93 for
#' the intra-rater and consistency statistics with 30 subjects (2 or 3
#' items, 2 repeats, 300 data sets of 499 replicates per setting). As in
#' [reli_err_test()], agreement statistics with few items (or occasions)
#' and large systematic differences between them under-covered with every
#' method (0.69 to 0.85 with 2 or 3 items); more items help more than more
#' subjects. Resampling subjects without the hybrid step covered as little
#' as 0.15 there. With normal data the analytic intervals were better than
#' any bootstrap interval. With occasions (2 days), the bootstrap gave the
#' same `"auto"` choices and helped for the consistency statistics (0.92
#' to 0.93 with skewed data), but for the agreement statistics the MLS
#' interval was as good or better even with skewed data, and every interval
#' under-covered when a day effect on 2 days dominated the error.
#'
#' **Other software.** VCA (variance components for deeper nesting and
#' laboratory precision designs), rptR (repeatability for non-Gaussian
#' data), gtheoryr (crossed multi-facet G-studies and D-study design
#' optimisation), SixSigma (`ss.rr()`, gauge R&R summaries) and MethComp
#' (method comparison with linked replicates and method-specific variances)
#' fit related models. This function adds intervals and tests for the
#' coefficients and the intra- and inter-rater SEMs, and the link to
#' [mdc_test()].
#'
#' @return A list of class `c("reli_htest", "htest")` containing:
#'   \item{statistic, parameter}{The F (coefficients) or \eqn{\chi^2} (SEM)
#'   statistic and its df, when a test was requested with an exact or
#'   Satterthwaite method (z for SEMs with `scale = "mean"`).}
#'   \item{p.value}{The p-value, when a test was requested.}
#'   \item{conf.int}{The confidence interval.}
#'   \item{estimate}{The estimate.}
#'   \item{stderr}{Standard errors of the estimate and on the working scale
#'   (`z`: \eqn{-\log(1 - \rho)/2} for coefficients, the log for SEMs), from
#'   `se_method`; with `boot = TRUE`, also `boot`.}
#'   \item{null.value, alternative, method, data.name}{As for an `htest`.}
#'   \item{components}{The variance components with intervals and shares.}
#'   \item{nested}{With `occasion` and `facet = "intra"`: the estimate the
#'   repeat model (occasions ignored) would give.}
#'   \item{fit}{The mean squares, degrees of freedom and fitted model.}
#'   \item{data}{The data in long format.}
#'   \item{settings}{The settings used, for the methods.}
#'   \item{boot}{Bootstrap details (`NULL` unless `boot = TRUE`).}
#'
#' @examples
#' # 12 participants, 3 operators, 2 scans each (exchangeable repeats)
#' set.seed(2)
#' scans <- expand.grid(scan = 1:2, operator = c("A", "B", "C"), id = 1:12)
#' subj <- rnorm(12, 50, 5)
#' bias <- c(A = 0, B = 1.5, C = -1)
#' scans$thickness <- subj[scans$id] + bias[scans$operator] +
#'   rnorm(nrow(scans), 0, 1.5)
#'
#' # Intra-rater (repeatability) ICC and SEM
#' reli_facet_test(thickness ~ operator | id, data = scans)
#' reli_facet_test(thickness ~ operator | id, data = scans, stat = "SEM")
#'
#' # Inter-rater agreement for a single scan by any operator
#' res <- reli_facet_test(thickness ~ operator | id, data = scans,
#'                        facet = "inter", type = "agreement")
#' summary(res)
#'
#' # D-study: the mean of 2 scans by one operator
#' reli_facet_test(thickness ~ operator | id, data = scans, facet = "inter",
#'                 type = "agreement", r_avg = 2)
#'
#' # If scan 1 and scan 2 were shared days, the same for every operator
#' day1 <- reli_facet_test(thickness ~ operator | id, data = scans,
#'                         occasion = "scan", type = "agreement")
#' day1
#' day1$nested
#'
#' # Smallest difference between two operators beyond measurement error
#' mdc_test(reli_facet_test(thickness ~ operator | id, data = scans,
#'                          stat = "SEM", facet = "inter",
#'                          type = "agreement"))
#'
#' @references
#' Brennan, R. L. (2001). Generalizability Theory. Springer.
#' \doi{10.1007/978-1-4757-3456-0}
#'
#' Burdick, R. K., & Graybill, F. A. (1992). Confidence Intervals on
#' Variance Components. Marcel Dekker.
#'
#' Graybill, F. A., & Wang, C. M. (1980). Confidence intervals on nonnegative
#' linear combinations of variances. Journal of the American Statistical
#' Association, 75(372), 869-873. \doi{10.1080/01621459.1980.10477565}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' Ting, N., Burdick, R. K., Graybill, F. A., Jeyaratnam, S., & Lu, T.-F. C.
#' (1990). Confidence intervals on linear combinations of variance
#' components that are unrestricted in sign. Journal of Statistical
#' Computation and Simulation, 35(3-4), 135-143.
#' \doi{10.1080/00949659008811240}
#'
#' @seealso [icc_test()] and [reli_err_test()] for one measurement per
#'   subject and item; [mdc_test()]; [reli_htest-methods] for `summary()`,
#'   `plot()` and `check()`.
#' @export
reli_facet_test <- function(x = NULL,
                            data = NULL,
                            measure = NULL,
                            item = NULL,
                            id = NULL,
                            occasion = NULL,
                            stat = c("ICC", "SEM"),
                            facet = c("intra", "inter", "overall"),
                            type = c("consistency", "agreement"),
                            k_avg = 1,
                            m_avg = 1,
                            r_avg = 1,
                            scale = c("raw", "log", "mean"),
                            method = c("anova", "reml"),
                            conf.level = 0.95,
                            null.value = NULL,
                            alternative = c("two.sided", "greater", "less"),
                            ci_method = c("auto", "mls", "satterthwaite"),
                            boot = FALSE,
                            boot_ci = c("auto", "perc", "bca", "stud", "basic"),
                            replicates = 1999,
                            se_method = c("jackknife", "normal")) {
  stat <- match.arg(stat)
  facet <- match.arg(facet)
  type <- match.arg(type)
  scale <- match.arg(scale)
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  ci_method <- match.arg(ci_method)
  boot_ci <- match.arg(boot_ci)
  se_method <- match.arg(se_method)
  .check_conf_level(conf.level)
  .check_boot_args(boot, replicates)
  .check_whole(k_avg, "k_avg")
  .check_whole(m_avg, "m_avg")
  .check_whole(r_avg, "r_avg")
  crossed <- !is.null(occasion)
  if (!crossed && facet == "overall") {
    facet <- "inter"
  }
  if (facet == "intra" && k_avg != 1) {
    stop("facet = \"intra\" concerns a single item; 'k_avg' must be 1.",
         call. = FALSE)
  }
  if (m_avg != 1 && (!crossed || facet == "inter")) {
    stop("'m_avg' must be 1 ",
         if (!crossed) "without 'occasion'." else
           "for facet = \"inter\" (one occasion).", call. = FALSE)
  }
  if (stat == "ICC" && scale != "raw") {
    stop("'scale' applies to stat = \"SEM\" only.", call. = FALSE)
  }
  if (!is.null(null.value)) {
    ok <- is.numeric(null.value) && length(null.value) == 1 &&
      !is.na(null.value) && is.finite(null.value)
    if (stat == "ICC" && (!ok || null.value >= 1)) {
      stop("'null.value' must be NULL or a single number below 1.",
           call. = FALSE)
    }
    if (stat == "SEM" && (!ok || null.value <= 0)) {
      stop("'null.value' must be NULL or a single positive number.",
           call. = FALSE)
    }
  }
  if (is.null(null.value)) {
    alternative <- "two.sided"
  }

  prep <- .facet_prepare(x = x, data = data, measure = measure, item = item,
                         id = id, occasion = occasion)
  if (r_avg != 1 && !prep$replicated) {
    stop("'r_avg' must be 1: there is one measurement per subject, item ",
         "and occasion.", call. = FALSE)
  }
  prep_fit <- prep
  if (scale == "log") {
    if (any(prep$df$values <= 0)) {
      stop("scale = \"log\" needs all measurements to be positive.",
           call. = FALSE)
    }
    prep_fit$df$values <- log(prep$df$values)
  }
  if (scale == "mean" && mean(prep$df$values) <= 0) {
    stop("scale = \"mean\" needs a positive grand mean.", call. = FALSE)
  }
  fit <- .facet_fit(prep_fit, method = method)
  if (method == "reml" && !prep$balanced && !boot) {
    .facet_warn_unbalanced(prep)
  }

  spec <- list(stat = stat, facet = facet, type = type, k_avg = k_avg,
               m_avg = m_avg, r_avg = r_avg, scale = scale)
  boot_ci <- .facet_resolve_boot_ci(boot, boot_ci, method, se_method,
                                    fit, spec)
  if (stat == "SEM" && sum(.facet_ms_coef(fit, spec)$var * fit$ms) < 0) {
    warning("The estimated error variance is negative; the SEM is reported ",
            "as 0.", call. = FALSE)
  }
  ci_used <- .facet_resolve_ci(fit, spec, ci_method)
  estimate <- .facet_estimate(fit, spec)
  label <- .facet_label(spec, fit$design)
  se_z <- .facet_se_z(prep_fit, fit, spec, se_method)
  z_obs <- .facet_z(estimate, spec)
  d_dz <- .num_grad(function(z) .facet_z_inv(z[[1]], spec), c(z = z_obs))
  stderr <- stats::setNames(c(abs(d_dz) * se_z, se_z), c(label, "z"))

  if (!boot) {
    conf.int <- .facet_ci(fit, spec, conf.level, alternative, ci_used)
    test <- if (is.null(null.value)) {
      list(statistic = NULL, parameter = NULL, p.value = NULL)
    } else {
      .facet_test(fit, spec, null.value, alternative, ci_used)
    }
    boot_info <- NULL
  } else {
    hybrid <- length(.facet_nonsubject(fit, spec)) > 0
    stat_fun <- function(p, resample) {
      f <- .facet_fit(p, method = method, keep_fit = FALSE)
      if (resample && hybrid) {
        f <- .facet_hybrid(f, spec, shift_mean = scale == "mean")
      }
      est <- .facet_estimate(f, spec)
      c(est = est,
        z = .facet_z(est, spec),
        se_z = if (boot_ci == "stud") .facet_se_z(p, f, spec, se_method) else
          NA_real_)
    }
    bo <- .reli_boot(prep_fit, stat_fun, R = replicates,
                     jackknife = boot_ci == "bca",
                     rebuild = .facet_prep_from_long)
    .check_boot_failures(bo)
    bounds <- if (stat == "ICC") c(-Inf, 1) else c(0, Inf)
    conf.int <- .boot_interval(bo, boot_ci, conf.level, alternative,
                               back = function(z) .facet_z_inv(z, spec),
                               bounds = bounds)
    test <- list(statistic = NULL, parameter = NULL,
                 p.value = if (is.null(null.value)) NULL else
                   .boot_pvalue(bo, boot_ci, null.value, alternative,
                                fwd = function(v) .facet_z(v, spec)))
    boot_info <- list(boot_ci = boot_ci,
                      replicates = replicates,
                      n_used = nrow(bo$t),
                      n_failed = bo$n_failed,
                      boot_se = stats::sd(bo$t[, "est"]),
                      estimates = bo$t[, "est"])
    stderr <- c(stderr, boot = boot_info$boot_se)
  }

  nested <- if (crossed && facet == "intra") {
    .facet_nested_estimate(prep_fit, spec, method)
  } else NULL

  settings <- list(statistic = "facet",
                   stat = stat,
                   facet = facet,
                   type = type,
                   design = fit$design,
                   k_avg = k_avg,
                   m_avg = m_avg,
                   r_avg = r_avg,
                   scale = scale,
                   method = method,
                   conf.level = conf.level,
                   ci_method = ci_used,
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
                   stats::setNames(null.value, label),
                 alternative = alternative,
                 method = .facet_method_text(spec, fit, method, ci_used,
                                             boot, boot_ci),
                 data.name = prep$data.name,
                 components = .facet_components(fit, conf.level),
                 nested = nested,
                 fit = fit,
                 data = prep$df,
                 settings = settings,
                 boot = boot_info),
            class = c("reli_htest", "htest"))
}

# Helpers --------------------------------------------------------------------

.check_whole <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 ||
      x != round(x)) {
    stop("'", name, "' must be a whole number of at least 1.", call. = FALSE)
  }
}

#' Warning text for REML fits to unbalanced data
#' @noRd
.facet_warn_unbalanced <- function(prep) {
  observed <- prep$counts[prep$counts > 0]
  what <- c(
    if (prep$n_missing > 0) {
      sprintf("%d of %d cells are empty", prep$n_missing, length(prep$counts))
    },
    if (length(unique(observed)) > 1) {
      sprintf("the number of measurements per cell varies (%d to %d)",
              min(observed), max(observed))
    })
  warning(paste0(
    "The design is unbalanced (", paste(what, collapse = "; "), "). ",
    "Intervals and p-values use observed-data degrees of freedom and the ",
    "harmonic mean number of repeats, and are approximate. ",
    "Consider boot = TRUE."), call. = FALSE)
}

#' What the repeat model (occasions ignored) would estimate for an intra
#' statistic: the occasions become repeats within subject-by-item cells.
#' @noRd
.facet_nested_estimate <- function(prep, spec, method) {
  df <- prep$df
  df$occ <- NULL
  p <- .facet_prep_from_long(df, prep$data.name)
  f <- suppressWarnings(.facet_fit(p, method = method, keep_fit = FALSE))
  s <- spec
  s$facet <- "intra"
  s$m_avg <- 1
  s$r_avg <- spec$m_avg * spec$r_avg
  est <- .facet_estimate(f, s)
  list(estimate = stats::setNames(est, .facet_label(s, "nested")),
       components = .facet_components(f))
}

#' Resolve the analytic interval method
#'
#' "exact" when the estimand involves one mean square (SEM) or a ratio of
#' two (coefficient); "delta" for SEMs relative to the mean; otherwise
#' `ci_method`, with "auto" resolved by `.facet_auto_ci()`.
#' @noRd
.facet_resolve_ci <- function(fit, spec, ci_method) {
  if (spec$stat == "SEM" && spec$scale == "mean") {
    return("delta")
  }
  if (.facet_is_exact(fit, spec)) {
    return("exact")
  }
  if (ci_method == "auto") .facet_auto_ci(spec) else ci_method
}

#' Analytic default per estimand: MLS for every non-exact estimand, from the
#' simulations in references/general/reli_sims/gstudy_coverage.R and
#' occasion_coverage.R (reli_redesign.md, Sections 14.8 and 15.9).
#' Satterthwaite under-covered badly for the agreement statistics when the
#' item (or occasion) variance dominated.
#' @noRd
.facet_auto_ci <- function(spec) {
  "mls"
}

#' Resolve `boot_ci = "auto"`: studentized for ANOVA fits, BCa for an SEM
#' that counts effects without subjects (systematic differences between
#' items or occasions), percentile for REML or a normal-theory SE
#' (reli_sims/gstudy_boot.R; reli_redesign.md, Section 14.8)
#' @noRd
.facet_resolve_boot_ci <- function(boot, boot_ci, method, se_method,
                                   fit = NULL, spec = NULL) {
  if (boot_ci == "auto") {
    boot_ci <- if (method == "reml" || se_method == "normal") {
      "perc"
    } else if (!is.null(spec) && spec$stat == "SEM" &&
               length(.facet_nonsubject(fit, spec)) > 0) {
      "bca"
    } else {
      "stud"
    }
  }
  if (boot && boot_ci == "stud") {
    if (se_method == "normal") {
      stop("boot_ci = \"stud\" needs a standard error estimated from each ",
           "resample; use se_method = \"jackknife\".", call. = FALSE)
    }
    if (method == "reml") {
      stop("boot_ci = \"stud\" with method = \"reml\" refits the mixed model ",
           "n times in every bootstrap replicate. Use boot_ci = \"perc\".",
           call. = FALSE)
    }
  }
  boot_ci
}

.facet_label <- function(spec, design) {
  ty <- if (spec$type == "consistency") "C" else "A"
  if (spec$facet == "intra" && design == "nested") {
    stat_label <- paste0(spec$stat, "(intra)")
  } else if (spec$facet == "overall" && spec$stat == "ICC") {
    return(if (ty == "C") "Erho2" else "Phi")
  } else {
    stat_label <- paste0(spec$stat, "(", spec$facet, ",", ty, ")")
  }
  if (spec$stat == "ICC") stat_label else .err_label(stat_label, spec$scale)
}

.facet_description <- function(spec, design) {
  varies <- switch(spec$facet,
                   intra = if (design == "crossed") {
                     "same item, another occasion"
                   } else "same item, another repeat",
                   inter = if (design == "crossed") {
                     "another item, same occasion"
                   } else "another item",
                   overall = "another item and occasion")
  kind <- if (spec$facet == "intra" && design == "nested") "" else
    if (spec$type == "consistency") "consistency, " else "absolute agreement, "
  what <- if (spec$stat == "ICC") {
    switch(spec$facet,
           intra = "Intra-rater ICC",
           inter = "Inter-rater ICC",
           overall = if (spec$type == "consistency") {
             "Generalizability coefficient"
           } else "Dependability coefficient")
  } else {
    switch(spec$facet,
           intra = "Intra-rater SEM",
           inter = "Inter-rater SEM",
           overall = "Overall SEM")
  }
  paste0(what, " (", kind, varies, "), ", .facet_score_text(spec, design))
}

.facet_score_text <- function(spec, design) {
  parts <- c(if (spec$k_avg > 1) paste(spec$k_avg, "items"),
             if (spec$m_avg > 1) paste(spec$m_avg, "occasions"),
             if (spec$r_avg > 1) paste(spec$r_avg, "repeats"))
  if (length(parts) == 0) "for a single measurement" else
    paste0("for the mean of ", paste(parts, collapse = " x "))
}

.facet_ci_name <- function(ci_method) {
  switch(ci_method,
         exact = "exact",
         mls = "MLS",
         satterthwaite = "Satterthwaite",
         delta = "delta-method")
}

.facet_method_text <- function(spec, fit, method, ci_used, boot, boot_ci) {
  base <- .facet_description(spec, fit$design)
  base <- paste0(base, switch(spec$scale,
                              raw = "",
                              log = ", log scale (sympercents)",
                              mean = ", as a percentage of the grand mean"))
  fit_text <- if (method == "reml") " (REML)" else ""
  inference <- if (boot) {
    paste0(" with nonparametric (subject) bootstrap ",
           .boot_ci_name(boot_ci), " interval")
  } else {
    paste0(" with ", .facet_ci_name(ci_used), " interval")
  }
  paste0(base, fit_text, inference)
}
