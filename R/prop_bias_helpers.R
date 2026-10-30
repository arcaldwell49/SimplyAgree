# Proportional bias diagnostics for check() ----
#
# cov(D, S) = var(x) - var(y) for D = x - y and S = x + y, so a slope of the
# differences on the means only tests whether the two methods have equal total
# variance (the Pitman-Morgan test). Unequal variance arises from a true
# proportional bias (y = a + b * x with b != 1), from unequal measurement
# error, or from a prior calibration of one method on the other. With
# replicates, the error variances are estimated from the within-subject
# variances and the test is corrected for them; with a known error variance
# ratio, Deming regression gives the slope directly.

# Subject-level summaries ----
# Means, counts, and within-subject sums of squares of x and y by id (NA
# allowed in either method). Subjects measured by only one method keep their
# within-subject sums of squares for the pooled error variances.
.pm_subjects = function(x, y, id) {
  id = factor(id)
  ss = function(v) {
    v = v[!is.na(v)]
    if (length(v) < 2) 0 else sum((v - mean(v))^2)
  }
  mn = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
  data.frame(
    mx = as.vector(tapply(!is.na(x), id, sum)),
    my = as.vector(tapply(!is.na(y), id, sum)),
    xb = as.vector(tapply(x, id, mn)),
    yb = as.vector(tapply(y, id, mn)),
    ssx = as.vector(tapply(x, id, ss)),
    ssy = as.vector(tapply(y, id, ss))
  )
}

# Moments of the subject means and their expected covariance under equal
# scales (b = 1). extra_cov and extra_var are fixed additions to the expected
# covariance and to the error part of var(S) (e.g., method-by-item variances
# from method_vc()).
.pm_moments = function(s,
                       extra_cov = 0,
                       extra_var = 0) {
  both = s$mx > 0 & s$my > 0
  sw2x = sum(s$ssx) / sum(pmax(s$mx - 1, 0))
  sw2y = sum(s$ssy) / sum(pmax(s$my - 1, 0))
  b = s[both, , drop = FALSE]
  d = b$xb - b$yb
  sm = b$xb + b$yb
  ex = sw2x * mean(1 / b$mx)
  ey = sw2y * mean(1 / b$my)
  e0 = ex - ey + extra_cov
  c(cov = stats::cov(d, sm),
    var_s = stats::var(sm),
    e0 = e0,
    err_s = ex + ey + extra_var,
    theta = stats::cov(d, sm) - e0,
    sw2x = sw2x,
    sw2y = sw2y)
}

# Slope of D on M (M = S/2) implied by a covariance and a variance of S
.pm_slope = function(cov_ds, var_s) {
  if (!is.finite(var_s) || var_s <= 0) NA_real_ else 2 * cov_ds / var_s
}

# Pitman-Morgan test on subject means ----
# x, y: measurements on the analysis scale (rows); id: subject. With one row
# per subject this is cor.test(x - y, x + y). With correct = TRUE (replicates
# of both methods required), cov(D, S) of the subject means is compared to its
# expected value under equal scales, sw2x * mean(1/m_x) - sw2y * mean(1/m_y),
# rather than to zero; the difference of the error variances does not depend
# on an occasion effect shared by both methods (linked replicates). The
# standard error is a jackknife over subjects (the error variances are
# re-estimated in each sample) with n - 1 df.
.pm_slope_test = function(x,
                          y,
                          id = seq_along(x),
                          correct = FALSE,
                          extra_cov = 0,
                          extra_var = 0) {
  s = .pm_subjects(x, y, id)
  s = s[s$mx > 0 | s$my > 0, , drop = FALSE]
  both = s$mx > 0 & s$my > 0
  d = s$xb[both] - s$yb[both]
  sm = s$xb[both] + s$yb[both]
  n = sum(both)
  plot_dat = data.frame(mean = sm / 2, diff = d)

  out = list(n = n,
             data = plot_dat,
             center = c(mean = mean(sm) / 2, diff = mean(d)),
             slope_obs = .pm_slope(stats::cov(d, sm), stats::var(sm)),
             corrected = FALSE,
             method = "Pitman-Morgan test (equal variances)",
             statistic = NA_real_,
             parameter = NA_real_,
             p.value = NA_real_,
             p.uncorrected = NA_real_,
             slope_adj = NA_real_)
  if (n >= 3 && stats::sd(d) > 0 && stats::sd(sm) > 0) {
    ct = stats::cor.test(d, sm)
    out$statistic = unname(ct$statistic)
    out$parameter = unname(ct$parameter)
    out$p.value = out$p.uncorrected = ct$p.value
  }

  has_reps = sum(pmax(s$mx - 1, 0)) > 0 && sum(pmax(s$my - 1, 0)) > 0
  if (!correct || !has_reps || n < 4) {
    return(out)
  }

  mom = .pm_moments(s, extra_cov = extra_cov, extra_var = extra_var)
  jk = vapply(seq_len(nrow(s)), function(i) {
    .pm_moments(s[-i, , drop = FALSE],
                extra_cov = extra_cov,
                extra_var = extra_var)[["theta"]]
  }, numeric(1))
  k = length(jk)
  se = sqrt((k - 1) / k * sum((jk - mean(jk))^2))

  out$corrected = TRUE
  out$method = "Error-corrected Pitman-Morgan test"
  out$theta = mom[["theta"]]
  out$se = se
  out$statistic = mom[["theta"]] / se
  out$parameter = n - 1
  out$p.value = 2 * stats::pt(-abs(out$statistic), n - 1)
  out$sw2 = c(x = mom[["sw2x"]], y = mom[["sw2y"]])
  out$slope_null = .pm_slope(mom[["e0"]], mom[["var_s"]])
  out$slope_adj = .pm_slope(mom[["theta"]], mom[["var_s"]] - mom[["err_s"]])
  # scale of y on x on the true values (y = a + beta * x) implied by the
  # error-adjusted slope
  out$beta = if (is.finite(out$slope_adj)) {
    (2 - out$slope_adj) / (2 + out$slope_adj)
  } else NA_real_
  out
}

# Results table attached to the check() output as attr(, "prop_bias"): one
# row per test (per pair of methods or condition, named in `group`).
.pm_table = function(res_list) {
  get = function(r, f) {
    if (is.null(r[[f]])) NA_real_ else unname(r[[f]])
  }
  rows = lapply(seq_along(res_list), function(j) {
    r = res_list[[j]]
    nm = names(res_list)[j]
    data.frame(group = if (is.null(nm) || nm == "") NA_character_ else nm,
               test = r$method,
               corrected = isTRUE(r$corrected),
               n = get(r, "n"),
               statistic = get(r, "statistic"),
               df = get(r, "parameter"),
               p.value = get(r, "p.value"),
               p.uncorrected = get(r, "p.uncorrected"),
               slope_obs = get(r, "slope_obs"),
               slope_null = get(r, "slope_null"),
               slope_adj = get(r, "slope_adj"),
               beta = get(r, "beta"),
               sw2_x = if (is.null(r$sw2)) NA_real_ else unname(r$sw2[1]),
               sw2_y = if (is.null(r$sw2)) NA_real_ else unname(r$sw2[2]),
               error.ratio = get(r, "error.ratio"))
  })
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

# Known error variance ratio ----
# Deming regression of y on x (y = a + b * x) with error.ratio =
# var(error of x) / var(error of y), as in dem_reg(), with the jackknife
# standard error and the t-test of b = 1 on n - 2 df that dem_reg() reports.
# The slope of D = x - y on the mean implied by b is 2 * (1 - b) / (1 + b).
.pm_known_ratio = function(x,
                           y,
                           error.ratio) {
  ok = stats::complete.cases(x, y)
  x = x[ok]
  y = y[ok]
  n = length(x)
  jd = jack_dem(x, y, w_i = rep(1, n), error.ratio = error.ratio)
  b = jd$df$coef[2]
  se = jd$df$se[2]
  stat = (b - 1) / se
  d = x - y
  sm = x + y
  # error variances implied by the fit: y - a - b * x has variance
  # var_y * (1 + error.ratio * b^2)
  e = y - jd$df$coef[1] - b * x
  sw2y = sum(e^2) / ((n - 2) * (1 + error.ratio * b^2))
  sw2x = error.ratio * sw2y
  list(n = n,
       data = data.frame(mean = sm / 2, diff = d),
       center = c(mean = mean(sm) / 2, diff = mean(d)),
       slope_obs = .pm_slope(stats::cov(d, sm), stats::var(sm)),
       corrected = TRUE,
       method = "Deming slope = 1 (known error ratio)",
       error.ratio = error.ratio,
       beta = b,
       statistic = stat,
       parameter = n - 2,
       p.value = 2 * stats::pt(-abs(stat), n - 2),
       p.uncorrected = stats::cor.test(d, sm)$p.value,
       sw2 = c(x = sw2x, y = sw2y),
       slope_null = .pm_slope(sw2x - sw2y, stats::var(sm)),
       slope_adj = 2 * (1 - b) / (1 + b))
}

# Which dashed line check() draws (bias_line) ----
# "adjusted": the error-adjusted slope (the slope without measurement error);
# "expected": the slope expected from unequal measurement error alone. NULL
# when the test is not corrected.
.pm_line = function(res,
                    bias_line) {
  if (!isTRUE(res$corrected)) {
    return(NULL)
  }
  s = if (bias_line == "adjusted") res$slope_adj else res$slope_null
  if (is.null(s) || !is.finite(s)) NULL else s
}

.pm_line_label = function(bias_line) {
  if (bias_line == "adjusted") "Error-adjusted" else
    "Expected from unequal error"
}

.pm_subtitle = function(bias_line,
                        has_line) {
  if (!has_line) {
    "Reference line should be horizontal"
  } else if (bias_line == "adjusted") {
    "Error-adjusted (dashed) line should be horizontal"
  } else {
    "Observed (solid) line should follow the dashed line"
  }
}

.pm_check_ratio = function(error.ratio) {
  if (!is.null(error.ratio) &&
      (!is.numeric(error.ratio) || length(error.ratio) != 1 ||
       is.na(error.ratio) || error.ratio <= 0)) {
    stop("error.ratio must be a single numeric value that is positive.",
         call. = FALSE)
  }
  invisible(error.ratio)
}

# Caption line for a test result
.pm_caption = function(res,
                       label = NULL,
                       bias_line = "adjusted") {
  lab = if (is.null(label)) "" else paste0(label, ": ")
  if (isTRUE(res$corrected)) {
    second = if (bias_line == "adjusted") {
      paste0(", error-adjusted ", signif(res$slope_adj, 3),
             " \nbeta (scale of y on x) = ", signif(res$beta, 3))
    } else {
      paste0(", expected ", signif(res$slope_null, 3))
    }
    paste0(lab, res$method, ": ", .fmt_test_p(res$p.value),
           " \nslope: observed ", signif(res$slope_obs, 3), second)
  } else {
    paste0(lab, res$method, ": ", .fmt_test_p(res$p.value))
  }
}

# Message when the slope is significant ----
# design: "single" (one measurement per method, or replicates that cannot be
# told apart from repeated measures), "reps" (corrected with replicates), or
# "ratio" (known error ratio). `which` names the pairs or conditions affected.
.pm_message = function(design = c("single", "reps", "ratio"),
                       which = NULL,
                       alt = NULL) {
  design = match.arg(design)
  where = if (is.null(which)) "" else
    paste0(" (", paste(which, collapse = ", "), ")")
  if (design == "single") {
    message("The differences change with the mean", where, " (p < 0.05). ",
            "This slope tests whether the two methods have equal variances ",
            "(Pitman-Morgan): cov(difference, sum) = var(x) - var(y). ",
            "Without replicate measurements of the same true value, it ",
            "cannot separate a proportional bias from unequal measurement ",
            "error (or a prior calibration of one method on the other). Use ",
            "replicate measurements or a known error variance ratio ",
            "(check(error.ratio = )) to separate them",
            if (is.null(alt)) "." else paste0(", or ", alt, "."))
  } else if (design == "reps") {
    message("After correcting for the measurement error variances estimated ",
            "from the replicates, the differences still change with the ",
            "mean", where, " (p < 0.05), which suggests a proportional bias",
            if (is.null(alt)) "." else paste0("; ", alt, "."))
  } else {
    message("With the given error variance ratio, the Deming slope differs ",
            "from 1", where, " (p < 0.05), which suggests a proportional bias.")
  }
}

# Message when the uncorrected slope is significant but the corrected one is
# not
.pm_message_explained = function(which = NULL) {
  where = if (is.null(which)) "" else
    paste0(" (", paste(which, collapse = ", "), ")")
  message("The differences change with the mean", where, " (uncorrected ",
          "p < 0.05), but the slope is consistent with unequal measurement ",
          "error alone (error-corrected p >= 0.05).")
}

# Emit the message(s) for one result
.pm_notify = function(res,
                      design,
                      which = NULL,
                      alt = NULL) {
  if (is.na(res$p.value)) {
    return(invisible(NULL))
  }
  if (res$p.value < 0.05) {
    .pm_message(design = design, which = which, alt = alt)
  } else if (design == "reps" && isTRUE(res$p.uncorrected < 0.05)) {
    .pm_message_explained(which = which)
  }
  invisible(NULL)
}
