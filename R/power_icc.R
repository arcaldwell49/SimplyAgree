#' Power and Sample Size for an Intraclass Correlation Test
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Computes the power of the F test of an intraclass correlation
#' coefficient (ICC) used by [icc_test()], or solves for the number of
#' subjects, the number of measurements per subject, the ICC under the
#' alternative, or the significance level that gives a target power. Exactly
#' one of `n`, `k`, `rho1`, `power` and `sig.level` must be `NULL`; that one
#' is computed from the others, as in [stats::power.t.test()].
#'
#' @param n Number of subjects.
#' @param k Number of measurements (items: trials, raters or occasions) per
#'   subject. When solved for, the smallest whole number of at least 2 that
#'   reaches the target is returned.
#' @param rho0 The ICC under the null hypothesis (default 0).
#' @param rho1 The ICC under the alternative hypothesis.
#' @param sig.level Significance level (Type I error rate; default 0.05).
#' @param power Power of the test (1 minus the Type II error rate).
#' @param model `"oneway"` (default) or `"twoway"`. `"twoway"` means the
#'   consistency ICC, ICC(C,1) of [icc_test()]; the absolute agreement ICC
#'   is not supported (see Details).
#' @param unit `"single"` (default) for the reliability of a single
#'   measurement, or `"average"` for the reliability of the mean of the `k`
#'   measurements (ICC(k) or ICC(C,k)), as in [icc_test()]. With
#'   `"average"`, the ICC values are on the average-measure scale and `k`
#'   cannot be solved for. See Details.
#' @param missing The expected proportion of measurements (subject-by-item
#'   cells) that will be missing, analysed with `icc_test(method = "reml")`.
#'   The default, 0, gives the exact complete-data calculation; values above
#'   0 give an approximation (see Details). For subjects who will be lost
#'   entirely, reduce `n` instead.
#' @param alternative `"two.sided"` (default), `"greater"`, or `"less"`, as
#'   in [icc_test()]. A design planned with the defaults matches the analysis
#'   run with the defaults.
#' @param method `"exact"` (default) computes power from the exact
#'   distribution of the F statistic. `"approximate"` uses the closed form of
#'   Walter, Eliasziw and Donner (1998), available only for
#'   `model = "oneway"`.
#'
#' @details
#' **Exact power.** For the one-way and the two-way consistency ICC with
#' complete data, the F ratio of the subject mean square to the error mean
#' square is a scaled central F variable whatever the true ICC \eqn{\rho}:
#' \deqn{F \sim \frac{1 + (k-1)\rho}{1 - \rho} F_{n-1, \nu_2},}
#' with \eqn{\nu_2 = n(k-1)} (one-way) or \eqn{(n-1)(k-1)} (two-way). The
#' test of \eqn{H_0: \rho = \rho_0} (McGraw & Wong, 1996) divides the F ratio
#' by the same factor at \eqn{\rho_0}, so its power is a single F
#' probability (Donner & Eliasziw, 1987). Non-integer `n` is allowed, as in
#' [stats::power.t.test()]; round up to the next whole number of subjects.
#'
#' **Approximate power** uses the closed form of Walter, Eliasziw and Donner
#' (1998),
#' \deqn{n = 1 + \frac{2k (z_{1-\alpha} + z_{1-\beta})^2}{(k-1)(\log C_0)^2},
#' \quad C_0 = \frac{1 + k\theta_0}{1 + k\theta_1}, \quad
#' \theta = \frac{\rho}{1 - \rho},}
#' with \eqn{\alpha/2} in place of \eqn{\alpha} for a two-sided test. Walter
#' et al. (and the PASS software) write n for the number of measurements per
#' subject and k for the number of subjects; here n is always the number of
#' subjects and k the number of measurements per subject. The exact method is
#' preferred; the approximation is provided to reproduce published tables.
#'
#' **Two-sided tests.** When solving for `rho1` with
#' `alternative = "two.sided"`, the solution above `rho0` is returned; use
#' `alternative = "less"` (with the significance level halved) for the one
#' below.
#'
#' **Average-measure ICCs.** The average-measure ICC is the Spearman-Brown
#' projection of the single-measure ICC to k measurements,
#' \eqn{k\rho / (1 + (k-1)\rho)}, so a test about ICC(k) or ICC(C,k) is the
#' same F test as the test about the corresponding single-measure ICC, and
#' its interval is the projection of the single-measure interval. With
#' `unit = "average"` the calculations are therefore exact, not
#' approximations; `k` must be given because it defines the quantity being
#' planned for.
#'
#' **Missing measurements (approximate).** With `missing = p > 0`, the
#' calculation uses the observed-data quantities that
#' `icc_test(method = "reml")` uses for incomplete data:
#' \eqn{N = nk(1 - p)} observations, \eqn{\bar{k} = N/n} measurements per
#' subject in the scale factor of the F ratio and in the interval, and error
#' degrees of freedom \eqn{N - n - k + 1} (two-way) or \eqn{N - n} (one-way).
#' This is an approximation: the F distribution is exact only for complete
#' data. In simulations of the REML analysis (one-way and two-way ICCs, 15
#' or 40 subjects, 3 or 5 items, 15% or 30% of measurements missing
#' completely at random or 30% at random depending on the first
#' measurement; 1000 replicates per setting), the approximate power was
#' within 0.035 of the simulated power (Monte Carlo SE 0.016) and the
#' approximate median interval width within 2.2% of the simulated median.
#' The REML test itself was slightly liberal with 30% missing (type I error
#' up to 0.069 missing completely at random and 0.073 at random, for a
#' nominal 0.05), which the simulated power includes. A warning is given
#' above 30% missing. The calculation assumes each subject keeps at least
#' one measurement.
#'
#' **Planning other designs.** These functions are exact only for the
#' one-way and two-way consistency ICCs with complete, normally distributed
#' data. They can still guide the planning of other ICC studies, as long as
#' the direction of the error is kept in mind:
#'
#' - *Absolute agreement ICC (ICC(A,1), and the CCC for two methods).*
#'   Not supported exactly: its test has no exact F distribution, and
#'   because its denominator degrees of freedom depend on the sample, its
#'   power "cannot be determined exactly" (McGraw & Wong, 1996, footnote 3).
#'   When the items (trials, raters, methods) differ little systematically,
#'   ICC(A,1) is close to ICC(C,1) and the consistency calculation is a good
#'   guide. When they differ, the agreement test and interval also depend on
#'   the item mean square, which has only k - 1 degrees of freedom: power is
#'   lower and intervals wider than the consistency calculation says, most
#'   of all with 2 or 3 items. Treat the consistency result as a lower bound
#'   on the number of subjects; simulation would be needed for a precise
#'   answer.
#' - *Missing measurements.* Use `missing` for measurements missing here and
#'   there (see below). For subjects who will be lost entirely, plan for the
#'   subjects who complete the study and recruit \eqn{n / (1 - d)} for an
#'   expected dropout d.
#' - *Non-normal data.* The exact calculations assume normality. With
#'   skewed or heavy-tailed data the F-based intervals are wider and cover
#'   less than planned (see [icc_test()] and [reli_err_test()] for
#'   simulations), so allow a margin or plan for the bootstrap with a larger
#'   sample.
#'
#' @return An object of class \code{"power.htest"}, printed by
#'   [stats::print.power.htest()], with elements `n`, `k`, `rho0`, `rho1`,
#'   `sig.level`, `power`, `alternative`, `method` and `note`.
#'
#' @examples
#' # Subjects needed to show an ICC above 0.6 when it is 0.8, with 2 trials
#' power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.8,
#'           alternative = "greater")
#'
#' # Power with 30 subjects and 3 trials
#' power_icc(n = 30, k = 3, rho0 = 0.5, rho1 = 0.75, alternative = "greater",
#'           model = "twoway")
#'
#' # Trials needed with 20 subjects
#' power_icc(n = 20, k = NULL, rho0 = 0.5, rho1 = 0.8, power = 0.9,
#'           alternative = "greater")
#'
#' @references
#' Donner, A., & Eliasziw, M. (1987). Sample size requirements for
#' reliability studies. Statistics in Medicine, 6(4), 441-448.
#' \doi{10.1002/sim.4780060404}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' Walter, S. D., Eliasziw, M., & Donner, A. (1998). Sample size and optimal
#' designs for reliability studies. Statistics in Medicine, 17(1), 101-110.
#' \doi{10.1002/(SICI)1097-0258(19980115)17:1<101::AID-SIM727>3.0.CO;2-E}
#'
#' @seealso [precision_icc()] for the sample size for a confidence interval
#'   of a given width; [icc_test()] for the analysis.
#' @export
power_icc <- function(n = NULL,
                      k = 2,
                      rho0 = 0,
                      rho1 = NULL,
                      sig.level = 0.05,
                      power = NULL,
                      model = c("oneway", "twoway"),
                      unit = c("single", "average"),
                      missing = 0,
                      alternative = c("two.sided", "greater", "less"),
                      method = c("exact", "approximate")) {
  model <- match.arg(model)
  unit <- match.arg(unit)
  alternative <- match.arg(alternative)
  method <- match.arg(method)
  .check_one_null(list(n = n, k = k, rho1 = rho1, power = power,
                       sig.level = sig.level))
  .check_approx_model(method, model)
  .check_n_k(n, k)
  .check_average_k(unit, k)
  .check_missing(missing, method)
  .check_unit(sig.level, "sig.level")
  .check_unit(power, "power")
  k_min <- if (is.null(k)) 2 else k
  .check_rho(rho0, k_min, "rho0", unit)
  .check_rho(rho1, k_min, "rho1", unit)
  if (!is.null(rho1)) {
    if (rho1 == rho0) {
      stop("'rho1' must differ from 'rho0'.", call. = FALSE)
    }
    if (alternative == "greater" && rho1 < rho0) {
      stop("With alternative = \"greater\", 'rho1' must be above 'rho0'.",
           call. = FALSE)
    }
    if (alternative == "less" && rho1 > rho0) {
      stop("With alternative = \"less\", 'rho1' must be below 'rho0'.",
           call. = FALSE)
    }
  }

  # Average-measure ICCs are the Spearman-Brown projection of the
  # single-measure ICC, so tests about them are tests about the single-measure
  # ICC: compute on that scale (exact, not an approximation).
  to_single <- function(r, k) {
    if (unit == "average") .spearman_brown_inv(r, k) else r
  }
  pfun_single <- function(n, k, r1, sig.level) {
    r0 <- to_single(rho0, k)
    if (method == "exact") {
      .icc_power_exact(n, k, r0, r1, sig.level, alternative, model, missing)
    } else {
      .icc_power_walter(n, k, r0, r1, sig.level, alternative)
    }
  }
  pfun <- function(n, k, rho1, sig.level) {
    pfun_single(n, k, to_single(rho1, k), sig.level)
  }
  notes <- character()

  if (is.null(power)) {
    power <- pfun(n, k, rho1, sig.level)
  } else if (is.null(n)) {
    n <- .solve_continuous(function(x) pfun(x, k, rho1, sig.level) - power,
                           c(2, 1e7), "n", lower_ok = TRUE)
    notes <- c(notes, "n is the number of subjects; round up to a whole number.")
  } else if (is.null(k)) {
    k <- .solve_integer(function(x) pfun(n, x, rho1, sig.level) >= power - 1e-8,
                        "k")
  } else if (is.null(rho1)) {
    # solve on the single-measure scale, then project back
    lower <- -1 / (k - 1)
    r0 <- to_single(rho0, k)
    eps <- 1e-10
    interval <- switch(alternative,
                       greater = c(r0 + eps, 1 - eps),
                       two.sided = c(r0 + eps, 1 - eps),
                       less = c(lower + eps, r0 - eps))
    rho1 <- .solve_continuous(function(r) pfun_single(n, k, r, sig.level) - power,
                              interval, "rho1")
    if (unit == "average") {
      rho1 <- .spearman_brown(rho1, k)
    }
    if (alternative == "two.sided") {
      notes <- c(notes, paste0("rho1 is the solution above rho0; use ",
                               "alternative = \"less\" for the one below."))
    }
  } else {
    sig.level <- .solve_continuous(function(a) pfun(n, k, rho1, a) - power,
                                   c(1e-10, 1 - 1e-10), "sig.level")
  }

  notes <- c(notes, .missing_note(missing))
  notes <- c(notes, paste0("n = number of subjects, k = measurements per ",
                           "subject"))
  structure(Filter(Negate(is.null), list(n = n,
                 k = k,
                 rho0 = rho0,
                 rho1 = rho1,
                 sig.level = sig.level,
                 power = power,
                 alternative = alternative,
                 missing = if (missing > 0) missing else NULL,
                 note = paste(notes, collapse = "\n      "),
                 method = .icc_design_method("Power calculation for the",
                                             model, method, unit, missing))),
            class = "power.htest")
}

#' Sample Size for the Precision of an Intraclass Correlation
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Plans a reliability study so that the confidence interval for an
#' intraclass correlation coefficient (ICC) from [icc_test()] is narrow
#' enough. Computes the assurance (the probability that the interval is no
#' wider than `width`), or solves for the number of subjects, the number of
#' measurements per subject, or the width. Exactly one of `n`, `k`, `width`
#' and `assurance` must be `NULL`.
#'
#' @inheritParams power_icc
#' @param rho The anticipated ICC.
#' @param width The full width of the confidence interval (upper minus
#'   lower limit). Zou (2012) uses the half-width \eqn{\omega}:
#'   `width = 2 * omega`.
#' @param conf.level Confidence level of the interval (default 0.95).
#' @param assurance The probability that the realised interval is no wider
#'   than `width`. The default, 0.5, plans for the median width.
#' @param method `"exact"` (default) computes the assurance from the exact
#'   distribution of the interval width. `"approximate"` uses the closed form
#'   of Zou (2012), available only for `model = "oneway"`.
#'
#' @details
#' **Exact assurance.** The two-sided F-based interval for the one-way or
#' two-way consistency ICC (Searle's interval; Shrout & Fleiss, 1979; McGraw
#' & Wong, 1996) is a function of the F ratio only, and the F ratio is a
#' scaled central F variable for any true ICC (see [power_icc()]). The
#' assurance is therefore the probability, under that distribution, of the F
#' values whose interval is no wider than `width`. The width is largest at
#' moderate F and shrinks towards both ends, so this set has two parts; the
#' lower part (estimates near \eqn{-1/(k-1)}) is negligible in realistic
#' designs. Mondal et al. (2024) recommend this interval for the one-way ICC.
#'
#' **Approximate sample size** (Zou, 2012) for a Wald interval with Fisher's
#' variance, half-width \eqn{\omega} and assurance \eqn{1 - \gamma}:
#' \deqn{n = 1 + \left[\frac{A z_{1-\alpha/2} + \sqrt{(A z_{1-\alpha/2})^2 +
#' 4 \omega A |B| z_{1-\gamma} z_{1-\alpha/2}}}{\omega\sqrt{2k(k-1)}}\right]^2,}
#' with \eqn{A = (1-\rho)(1 + (k-1)\rho)} and \eqn{B = k - 2 + 2\rho(1-k)}.
#' It requires `assurance` of at least 0.5. Mondal et al. (2024) note that
#' Zou used \eqn{n - 1} in deriving it and that another expression in his
#' paper (the variance of the log-F statistic, not used here) is incorrect;
#' the exact method avoids both issues. For a one-sided lower limit, the
#' assurance that the lower limit exceeds a value \eqn{\rho_0} is the power
#' of the one-sided test: use `power_icc(alternative = "greater")`.
#'
#' **Average-measure ICCs** (`unit = "average"`): the interval for ICC(k) or
#' ICC(C,k) has limits \eqn{1 - a/F} and \eqn{1 - 1/(bF)} (with a and b the
#' F quantiles), so its width, \eqn{(a - 1/b)/F}, decreases with F and the
#' assurance is a single F probability. `width` is on the average-measure
#' scale. The approximate method is for single measures only.
#'
#' **Missing measurements** (`missing > 0`): the interval width uses the
#' observed-data quantities described in [power_icc()], an approximation
#' whose median widths were within 2.2% of simulated REML intervals with up
#' to 30% of measurements missing.
#'
#' The absolute agreement ICC is not supported exactly; see "Planning other
#' designs" in [power_icc()] for using these calculations as a guide for it,
#' for missing data, and for non-normal data.
#'
#' @return An object of class \code{"power.htest"}, printed by
#'   [stats::print.power.htest()], with elements `n`, `k`, `rho`, `width`,
#'   `conf.level`, `assurance`, `method` and `note`.
#'
#' @examples
#' # Subjects for a median 95% CI width of 0.2 around an ICC of 0.8, 3 trials
#' precision_icc(rho = 0.8, k = 3, width = 0.2)
#'
#' # ... with 90% assurance
#' precision_icc(rho = 0.8, k = 3, width = 0.2, assurance = 0.9)
#'
#' # Zou (2012), Table 1: half-width 0.1, 80% assurance
#' precision_icc(rho = 0.6, k = 2, width = 0.2, assurance = 0.8,
#'               method = "approximate")
#'
#' @references
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
#' intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' Mondal, D., Vanbelle, S., Cassese, A., & Candel, M. J. J. M. (2024).
#' Review of sample size determination methods for the intraclass
#' correlation coefficient in the one-way analysis of variance model.
#' Statistical Methods in Medical Research, 33(3), 532-553.
#' \doi{10.1177/09622802231224657}
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
#' assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
#' \doi{10.1037/0033-2909.86.2.420}
#'
#' Zou, G. Y. (2012). Sample size formulas for estimating intraclass
#' correlation coefficients with precision and assurance. Statistics in
#' Medicine, 31(29), 3972-3981. \doi{10.1002/sim.5466}
#'
#' @seealso [power_icc()]; [icc_test()] for the analysis.
#' @export
precision_icc <- function(n = NULL,
                          k = 2,
                          rho = NULL,
                          width = NULL,
                          conf.level = 0.95,
                          assurance = 0.5,
                          model = c("oneway", "twoway"),
                          unit = c("single", "average"),
                          missing = 0,
                          method = c("exact", "approximate")) {
  model <- match.arg(model)
  unit <- match.arg(unit)
  method <- match.arg(method)
  if (is.null(rho)) {
    stop("'rho' (the anticipated ICC) must be given.", call. = FALSE)
  }
  .check_one_null(list(n = n, k = k, width = width, assurance = assurance))
  .check_approx_model(method, model)
  if (method == "approximate" && unit == "average") {
    stop("method = \"approximate\" (Zou, 2012) is only available for ",
         "unit = \"single\".", call. = FALSE)
  }
  .check_n_k(n, k)
  .check_average_k(unit, k)
  .check_missing(missing, method)
  .check_conf_level(conf.level)
  .check_unit(assurance, "assurance")
  .check_rho(rho, if (is.null(k)) 2 else k, "rho", unit)
  if (!is.null(width) && (!is.numeric(width) || length(width) != 1 ||
                          is.na(width) || width <= 0)) {
    stop("'width' must be a single positive number.", call. = FALSE)
  }
  if (method == "approximate" && !is.null(assurance) && assurance < 0.5) {
    stop("method = \"approximate\" needs assurance of at least 0.5.",
         call. = FALSE)
  }

  afun <- function(n, k, width) {
    if (method == "exact") {
      .icc_assurance_exact(n, k, rho, width, conf.level, model, unit, missing)
    } else {
      .icc_assurance_zou(n, k, rho, width, conf.level)
    }
  }
  notes <- character()

  if (is.null(assurance)) {
    assurance <- afun(n, k, width)
  } else if (is.null(n)) {
    if (method == "approximate") {
      n <- .zou_n(k, rho, width, conf.level, assurance)
    } else {
      n <- .solve_continuous(function(x) afun(x, k, width) - assurance,
                             c(2, 1e7), "n", lower_ok = TRUE)
    }
    notes <- c(notes, "n is the number of subjects; round up to a whole number.")
  } else if (is.null(k)) {
    reached <- if (method == "approximate") {
      # invert through n, which avoids Zou's formula below assurance 0.5
      function(x) .zou_n(x, rho, width, conf.level, assurance) <= n + 1e-8
    } else {
      function(x) afun(n, x, width) >= assurance - 1e-8
    }
    k <- .solve_integer(reached, "k")
  } else {
    gap <- if (method == "approximate") {
      function(w) n - .zou_n(k, rho, w, conf.level, assurance)
    } else {
      function(w) afun(n, k, w) - assurance
    }
    # an average-measure interval can be arbitrarily wide (no lower bound)
    kbar <- k * (1 - missing)
    max_width <- if (unit == "average" || kbar <= 1) 1e4 else 1 + 1 / (kbar - 1)
    width <- .solve_continuous(gap, c(1e-8, max_width), "width")
  }

  notes <- c(notes, .missing_note(missing))
  notes <- c(notes, paste0("n = number of subjects, k = measurements per ",
                           "subject; width = upper minus lower limit"))
  structure(Filter(Negate(is.null), list(n = n,
                 k = k,
                 rho = rho,
                 width = width,
                 conf.level = conf.level,
                 assurance = assurance,
                 missing = if (missing > 0) missing else NULL,
                 note = paste(notes, collapse = "\n      "),
                 method = .icc_design_method(
                   "Precision (confidence interval width) calculation for the",
                   model, method, unit, missing))),
            class = "power.htest")
}

# Helpers --------------------------------------------------------------------

.icc_design_method <- function(what, model, method, unit = "single",
                               missing = 0) {
  icc <- if (model == "oneway") {
    if (unit == "single") "one-way ICC, ICC(1)" else
      "one-way average-measure ICC, ICC(k)"
  } else {
    if (unit == "single") "two-way consistency ICC, ICC(C,1)" else
      "two-way consistency average-measure ICC, ICC(C,k)"
  }
  how <- if (missing > 0) {
    "F distribution with observed-data df; approximate"
  } else if (method == "exact") {
    "exact F distribution"
  } else {
    "approximate closed form"
  }
  paste0(what, " ", icc, " (", how, ")")
}

.check_missing <- function(missing, method) {
  if (!is.numeric(missing) || length(missing) != 1 || is.na(missing) ||
      missing < 0 || missing >= 1) {
    stop("'missing' must be a single proportion in [0, 1).", call. = FALSE)
  }
  if (missing > 0 && method == "approximate") {
    stop("'missing' is only available with method = \"exact\".",
         call. = FALSE)
  }
  if (missing > 0.3) {
    warning("missing = ", missing, " is beyond the 30% checked by ",
            "simulation (see Details); the calculation may be less accurate.",
            call. = FALSE)
  }
}

.missing_note <- function(missing) {
  if (missing == 0) {
    return(character())
  }
  paste0("approximate: ", format(100 * missing), "% of measurements missing ",
         "at random, analysed with icc_test(method = \"reml\")")
}

# The average-measure ICC is defined for the mean of k measurements, so k is
# part of what is being planned for and cannot be solved for.
.check_average_k <- function(unit, k) {
  if (unit == "average" && is.null(k)) {
    stop("'k' cannot be solved for with unit = \"average\": the ",
         "average-measure ICC is the reliability of the mean of k ",
         "measurements, so k must be given.", call. = FALSE)
  }
}

.icc_df2 <- function(n, k, model) {
  if (model == "oneway") n * (k - 1) else (n - 1) * (k - 1)
}

#' Design quantities for the F calculations
#'
#' With a proportion `missing` of measurements missing, the observed-data
#' quantities used by `icc_test(method = "reml")`: N = n k (1 - missing)
#' observations, kbar = N / n measurements per subject, and error df
#' N - n - k + 1 (two-way) or N - n (one-way). With `missing = 0` these are
#' the complete-data values (kbar = k, df2 = (n - 1)(k - 1) or n (k - 1)).
#' @noRd
.icc_design <- function(n, k, model, missing = 0) {
  N <- n * k * (1 - missing)
  list(kbar = N / n,
       df1 = n - 1,
       df2 = if (model == "oneway") N - n else N - n - k + 1)
}

# Scale factor of the F ratio: F ~ c(rho) * F(df1, df2)
.icc_f_scale <- function(rho, k) (1 + (k - 1) * rho) / (1 - rho)

#' Power of the McGraw & Wong F test of H0: rho = rho0
#'
#' Exact with complete data; with `missing > 0`, approximate (observed-data
#' design quantities, see `.icc_design()`).
#' @noRd
.icc_power_exact <- function(n, k, rho0, rho1, sig.level, alternative, model,
                             missing = 0) {
  d <- .icc_design(n, k, model, missing)
  df1 <- d$df1
  df2 <- d$df2
  if (df2 <= 0) {
    return(0)
  }
  r <- .icc_f_scale(rho0, d$kbar) / .icc_f_scale(rho1, d$kbar)
  upper <- function(a) stats::pf(stats::qf(1 - a, df1, df2) * r, df1, df2,
                                 lower.tail = FALSE)
  lower <- function(a) stats::pf(stats::qf(a, df1, df2) * r, df1, df2)
  switch(alternative,
         greater = upper(sig.level),
         less = lower(sig.level),
         two.sided = upper(sig.level / 2) + lower(sig.level / 2))
}

#' Approximate power from Walter, Eliasziw & Donner (1998)
#' @noRd
.icc_power_walter <- function(n, k, rho0, rho1, sig.level, alternative) {
  theta <- function(r) r / (1 - r)
  log_c0 <- log((1 + k * theta(rho0)) / (1 + k * theta(rho1)))
  a <- if (alternative == "two.sided") sig.level / 2 else sig.level
  stats::pnorm(abs(log_c0) * sqrt((n - 1) * (k - 1) / (2 * k)) -
                 stats::qnorm(1 - a))
}

#' Width of the two-sided F-based ICC interval as a function of the F ratio
#' @noRd
.icc_width_fun <- function(n, k, conf.level, model, missing = 0) {
  d <- .icc_design(n, k, model, missing)
  alpha <- 1 - conf.level
  a <- stats::qf(1 - alpha / 2, d$df1, d$df2)
  b <- stats::qf(1 - alpha / 2, d$df2, d$df1)
  g <- function(x) (x - 1) / (x + d$kbar - 1)
  function(F_ratio) g(F_ratio * b) - g(F_ratio / a)
}

#' Exact probability that the F-based interval is no wider than `width`
#'
#' The width is unimodal in log F, so {width <= w} is {F <= f_lo} or
#' {F >= f_hi}, with F ~ c(rho) * F(df1, df2).
#' @noRd
.icc_assurance_exact <- function(n, k, rho, width, conf.level, model,
                                 unit = "single", missing = 0) {
  d <- .icc_design(n, k, model, missing)
  df1 <- d$df1
  df2 <- d$df2
  if (df2 <= 0) {
    return(0)
  }
  if (unit == "average") {
    # Limits 1 - a/F and 1 - 1/(b F): width (a - 1/b) / F decreases in F,
    # so {width <= w} = {F >= (a - 1/b) / w}. The average-measure ICC is
    # the projection to all k items; F is scaled by the single-measure ICC
    # with kbar observed items per subject.
    alpha <- 1 - conf.level
    a <- stats::qf(1 - alpha / 2, df1, df2)
    b <- stats::qf(1 - alpha / 2, df2, df1)
    f_star <- (a - 1 / b) / width
    scale <- .icc_f_scale(.spearman_brown_inv(rho, k), d$kbar)
    return(stats::pf(f_star / scale, df1, df2, lower.tail = FALSE))
  }
  w_fun <- .icc_width_fun(n, k, conf.level, model, missing)
  w_log <- function(t) w_fun(exp(t))
  peak <- stats::optimize(w_log, c(-60, 60), maximum = TRUE, tol = 1e-10)
  if (peak$objective <= width) {
    return(1)
  }
  root <- function(interval) {
    stats::uniroot(function(t) w_log(t) - width, interval,
                   tol = 1e-12)$root
  }
  t_lo <- root(c(-60, peak$maximum))
  t_hi <- root(c(peak$maximum, 60))
  scale <- .icc_f_scale(rho, d$kbar)
  stats::pf(exp(t_lo) / scale, df1, df2) +
    stats::pf(exp(t_hi) / scale, df1, df2, lower.tail = FALSE)
}

#' Zou (2012) sample size for half-width width / 2 with assurance
#' @noRd
.zou_n <- function(k, rho, width, conf.level, assurance) {
  omega <- width / 2
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  z_g <- stats::qnorm(assurance)
  A <- (1 - rho) * (1 + (k - 1) * rho)
  B <- abs(k - 2 + 2 * rho * (1 - k))
  1 + ((A * z + sqrt((A * z)^2 + 4 * omega * A * B * z_g * z)) /
         (omega * sqrt(2 * k * (k - 1))))^2
}

#' Assurance implied by Zou's formula for given n (inverting `.zou_n()`)
#'
#' Returns 0.5 or more; values below the median-width sample size give 0.5
#' with a warning, since the formula does not cover assurance below 0.5.
#' @noRd
.icc_assurance_zou <- function(n, k, rho, width, conf.level) {
  omega <- width / 2
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  A <- (1 - rho) * (1 + (k - 1) * rho)
  B <- abs(k - 2 + 2 * rho * (1 - k))
  S <- sqrt(n - 1) * omega * sqrt(2 * k * (k - 1))
  if (S <= 2 * A * z) {
    warning("n is below the median-width sample size; Zou's formula does ",
            "not cover assurance below 0.5, so 0.5 is returned.",
            call. = FALSE)
    return(0.5)
  }
  if (B < 1e-12) {
    # the approximate width does not vary: the target is met with certainty
    return(1)
  }
  z_g <- ((S - A * z)^2 - (A * z)^2) / (4 * omega * A * B * z)
  stats::pnorm(z_g)
}

.solve_continuous <- function(f, interval, what, lower_ok = FALSE) {
  f_lo <- f(interval[1])
  f_hi <- f(interval[2])
  if (lower_ok && f_lo >= 0) {
    return(interval[1])
  }
  if (sign(f_lo) == sign(f_hi)) {
    stop("No value of '", what, "' in [", format(signif(interval[1], 3)),
         ", ", format(signif(interval[2], 3)), "] reaches the target.",
         call. = FALSE)
  }
  stats::uniroot(f, interval, tol = 1e-10)$root
}

.solve_integer <- function(reached, what, from = 2, to = 1000) {
  for (x in seq(from, to)) {
    if (reached(x)) {
      return(x)
    }
  }
  stop("No value of '", what, "' up to ", to, " reaches the target.",
       call. = FALSE)
}

.check_one_null <- function(args) {
  nulls <- vapply(args, is.null, logical(1))
  if (sum(nulls) != 1) {
    stop("Exactly one of ", paste0("'", names(args), "'", collapse = ", "),
         " must be NULL.", call. = FALSE)
  }
}

.check_approx_model <- function(method, model) {
  if (method == "approximate" && model != "oneway") {
    stop("method = \"approximate\" is only available for model = \"oneway\".",
         call. = FALSE)
  }
}

.check_n_k <- function(n, k) {
  if (!is.null(n) && (!is.numeric(n) || length(n) != 1 || is.na(n) ||
                      n < 2)) {
    stop("'n' must be a single number of at least 2.", call. = FALSE)
  }
  if (!is.null(k) && (!is.numeric(k) || length(k) != 1 || is.na(k) ||
                      k < 2 || k != round(k))) {
    stop("'k' must be a whole number of at least 2.", call. = FALSE)
  }
}

.check_unit <- function(x, name) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || is.na(x) ||
                      x <= 0 || x >= 1)) {
    stop("'", name, "' must be a single number between 0 and 1.",
         call. = FALSE)
  }
}

.check_rho <- function(rho, k, name, unit = "single") {
  if (is.null(rho)) {
    return(invisible())
  }
  if (unit == "average") {
    # the average-measure ICC has no lower bound
    if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) || rho >= 1) {
      stop(sprintf("'%s' must be a single number below 1.", name),
           call. = FALSE)
    }
    return(invisible())
  }
  lower <- -1 / (k - 1)
  if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) || rho <= lower ||
      rho >= 1) {
    stop(sprintf("'%s' must be a single number between %s and 1.", name,
                 format(signif(lower, 3))), call. = FALSE)
  }
}
