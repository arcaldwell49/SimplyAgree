# Reliability engine ---------------------------------------------------------
#
# Internal data preparation, model fitting and inference helpers shared by the
# reliability functions (icc_test(), reli_err_test(), power_icc(),
# precision_icc()). Design notes and sources: references/working/reli_redesign.md
#
# Notation follows McGraw & Wong (1996): n subjects (rows), k items/trials
# (columns), mean squares MSB (subjects), MSJ (items), MSE (residual) and
# MSW (within subjects, one-way).

# Data preparation -----------------------------------------------------------

#' Prepare reliability data from a formula, long, or wide interface
#'
#' Exactly one interface must be used: a formula `values ~ item | id`, the
#' column names `measure`, `item` and `id` (long data), or `col.names` (wide
#' data, one row per subject).
#'
#' @return A list of class `reli_prep` with the long data (`values`, `items`,
#'   `id`), the number of subjects `n`, items `k`, observations `N`, missing
#'   cells `n_missing`, whether the design is `balanced`, and `data.name`.
#' @noRd
.reli_prepare <- function(x = NULL,
                          data = NULL,
                          measure = NULL,
                          item = NULL,
                          id = NULL,
                          col.names = NULL) {
  use_formula <- !is.null(x)
  use_long <- !is.null(measure) || !is.null(item) || !is.null(id)
  use_wide <- !is.null(col.names)
  n_interfaces <- sum(use_formula, use_long, use_wide)

  if (n_interfaces == 0) {
    stop("Specify the data with a formula (values ~ item | id), ",
         "with 'measure', 'item' and 'id', or with 'col.names' for wide data.",
         call. = FALSE)
  }
  if (n_interfaces > 1) {
    stop("Use only one of: a formula, 'measure'/'item'/'id', or 'col.names'.",
         call. = FALSE)
  }
  if (is.null(data)) {
    stop("'data' must be supplied.", call. = FALSE)
  }
  data <- as.data.frame(data)

  if (use_formula) {
    parsed <- .reli_parse_formula(x)
    measure <- parsed$measure
    item <- parsed$item
    id <- parsed$id
    data.name <- paste(deparse(x), collapse = " ")
  }

  if (use_formula || use_long) {
    if (is.null(measure) || is.null(item) || is.null(id)) {
      stop("'measure', 'item' and 'id' must all be supplied for long data.",
           call. = FALSE)
    }
    cols <- c(measure, item, id)
    if (!is.character(cols) || length(cols) != 3) {
      stop("'measure', 'item' and 'id' must each be a single column name.",
           call. = FALSE)
    }
    .reli_check_columns(cols, data)
    items <- data[[item]]
    items <- if (is.factor(items)) items else factor(items, levels = unique(items))
    df <- data.frame(values = data[[measure]],
                     items = items,
                     id = data[[id]])
    if (!use_formula) {
      data.name <- paste0(measure, " (items: ", item, ", subjects: ", id, ")")
    }
  } else {
    if (!is.character(col.names) || length(col.names) < 2) {
      stop("'col.names' must name at least 2 columns.", call. = FALSE)
    }
    .reli_check_columns(col.names, data)
    wide <- data[, col.names, drop = FALSE]
    n_rows <- nrow(wide)
    df <- data.frame(values = unlist(wide, use.names = FALSE),
                     items = factor(rep(col.names, each = n_rows),
                                    levels = col.names),
                     id = rep(seq_len(n_rows), times = length(col.names)))
    data.name <- paste(col.names, collapse = ", ")
  }

  if (!is.numeric(df$values)) {
    stop("The measurement column must be numeric.", call. = FALSE)
  }
  if (anyNA(df$items) || anyNA(df$id)) {
    stop("Missing values found in the item and/or id columns.", call. = FALSE)
  }

  .reli_prep_from_long(df, data.name = data.name)
}

#' Build a `reli_prep` object from long data with columns values, items, id
#'
#' Drops missing measurements, re-levels factors, and validates the design.
#' Also used for bootstrap and jackknife resamples.
#' @noRd
.reli_prep_from_long <- function(df, data.name = NULL) {
  df <- df[!is.na(df$values), , drop = FALSE]
  df$items <- droplevels(as.factor(df$items))
  df$id <- if (is.factor(df$id)) droplevels(df$id) else
    factor(df$id, levels = unique(df$id))
  rownames(df) <- NULL

  if (anyDuplicated(df[c("id", "items")])) {
    stop("Each subject must have at most one measurement per item; ",
         "aggregate replicate measurements first.", call. = FALSE)
  }

  n <- nlevels(df$id)
  k <- nlevels(df$items)
  N <- nrow(df)
  if (n < 2) {
    stop("At least 2 subjects with measurements are required.", call. = FALSE)
  }
  if (k < 2) {
    stop("At least 2 items (trials or raters) are required.", call. = FALSE)
  }

  structure(list(df = df,
                 n = n,
                 k = k,
                 N = N,
                 n_missing = n * k - N,
                 balanced = N == n * k,
                 data.name = data.name),
            class = "reli_prep")
}

.reli_parse_formula <- function(x) {
  bad_formula <- "The formula must have the form values ~ item | id."
  if (!inherits(x, "formula") || length(x) != 3) {
    stop(bad_formula, call. = FALSE)
  }
  rhs <- x[[3]]
  if (!is.call(rhs) || !identical(rhs[[1]], as.name("|"))) {
    stop(bad_formula, call. = FALSE)
  }
  vars <- list(measure = all.vars(x[[2]]),
               item = all.vars(rhs[[2]]),
               id = all.vars(rhs[[3]]))
  if (any(lengths(vars) != 1)) {
    stop(bad_formula, call. = FALSE)
  }
  vars
}

.reli_check_columns <- function(cols, data) {
  absent <- setdiff(cols, names(data))
  if (length(absent) > 0) {
    stop("Column(s) not found in 'data': ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
}

#' Warning text for REML fits to unbalanced data
#' @noRd
.reli_warn_unbalanced <- function(prep) {
  pct <- 100 * prep$n_missing / (prep$n * prep$k)
  warning(sprintf(paste0(
    "%.1f%% of subject-by-item cells are missing. Intervals and p-values ",
    "use observed-data degrees of freedom and are approximate; they may be ",
    "liberal as missingness increases. Consider boot = TRUE."), pct),
    call. = FALSE)
}

# Model fitting --------------------------------------------------------------

#' Fit the reliability model and return mean squares and degrees of freedom
#'
#' `method = "anova"` computes the two-way ANOVA mean squares in closed form
#' and requires balanced, complete data. `method = "reml"` fits a linear mixed
#' model (crossed subject and item random effects for `model = "twoway"`,
#' subject random effect only for `model = "oneway"`) and reconstructs the
#' mean squares from the variance components using the observed-data design
#' quantities: `kbar = N / n` items per subject, error df `N - n - k + 1`.
#' With balanced data these equal the ANOVA quantities.
#'
#' @return A list of class `reli_fit`.
#' @noRd
.reli_fit <- function(prep,
                      method = c("anova", "reml"),
                      model = c("twoway", "oneway"),
                      keep_fit = TRUE) {
  method <- match.arg(method)
  model <- match.arg(model)
  df <- prep$df
  n <- prep$n
  k <- prep$k
  N <- prep$N

  if (method == "anova") {
    if (!prep$balanced) {
      stop("method = \"anova\" requires complete, balanced data (",
           prep$n_missing, " subject-by-item cells are missing). ",
           "Use method = \"reml\" for incomplete data.", call. = FALSE)
    }
    Y <- matrix(NA_real_, n, k)
    Y[cbind(as.integer(df$id), as.integer(df$items))] <- df$values
    gm <- mean(Y)
    SSB <- k * sum((rowMeans(Y) - gm)^2)
    SSJ <- n * sum((colMeans(Y) - gm)^2)
    SST <- sum((Y - gm)^2)
    SSE <- SST - SSB - SSJ
    dfs <- c(dfB = n - 1, dfJ = k - 1, dfE = (n - 1) * (k - 1),
             dfW = n * (k - 1))
    ms <- c(MSB = SSB / dfs[["dfB"]],
            MSJ = SSJ / dfs[["dfJ"]],
            MSE = SSE / dfs[["dfE"]],
            MSW = (SSJ + SSE) / dfs[["dfW"]])
    kbar <- k
    singular <- FALSE
    fit <- if (keep_fit) {
      if (model == "twoway") stats::lm(values ~ id + items, data = df) else
        stats::lm(values ~ id, data = df)
    } else NULL
  } else {
    kbar <- N / n
    if (model == "twoway") {
      fit <- suppressMessages(lme4::lmer(values ~ 1 + (1 | id) + (1 | items),
                                         data = df, REML = TRUE))
      vc_df <- as.data.frame(lme4::VarCorr(fit))
      v_id <- vc_df$vcov[vc_df$grp == "id"]
      v_items <- vc_df$vcov[vc_df$grp == "items"]
      v_e <- stats::sigma(fit)^2
      ms <- c(MSB = kbar * v_id + v_e,
              MSJ = n * v_items + v_e,
              MSE = v_e,
              MSW = v_items + v_e)
      dfs <- c(dfB = n - 1, dfJ = k - 1, dfE = N - n - k + 1, dfW = N - n)
    } else {
      fit <- suppressMessages(lme4::lmer(values ~ 1 + (1 | id),
                                         data = df, REML = TRUE))
      vc_df <- as.data.frame(lme4::VarCorr(fit))
      v_id <- vc_df$vcov[vc_df$grp == "id"]
      v_w <- stats::sigma(fit)^2
      ms <- c(MSB = kbar * v_id + v_w,
              MSJ = NA_real_,
              MSE = NA_real_,
              MSW = v_w)
      dfs <- c(dfB = n - 1, dfJ = NA_real_, dfE = NA_real_, dfW = N - n)
    }
    singular <- lme4::isSingular(fit)
    if (!keep_fit) fit <- NULL
  }

  vc <- if (model == "twoway") {
    c(id = max((ms[["MSB"]] - ms[["MSE"]]) / kbar, 0),
      items = max((ms[["MSJ"]] - ms[["MSE"]]) / n, 0),
      resid = ms[["MSE"]])
  } else {
    c(id = max((ms[["MSB"]] - ms[["MSW"]]) / kbar, 0),
      resid = ms[["MSW"]])
  }

  structure(list(method = method,
                 model = model,
                 balanced = prep$balanced,
                 n = n,
                 k = k,
                 kbar = kbar,
                 N = N,
                 n_missing = prep$n_missing,
                 ms = ms,
                 df = dfs,
                 vc = vc,
                 grand_mean = mean(df$values),
                 singular = singular,
                 fit = fit),
            class = "reli_fit")
}

# ICC estimates, intervals and tests -----------------------------------------
#
# ICC forms (McGraw & Wong 1996; Shrout & Fleiss 1979 in brackets):
#   "1"  one-way, single        [ICC(1,1)]
#   "k"  one-way, average       [ICC(1,k)]
#   "C1" consistency, single    [ICC(3,1)]
#   "Ck" consistency, average   [ICC(3,k)]
#   "A1" agreement, single      [ICC(2,1)]
#   "Ak" agreement, average     [ICC(2,k)]

.icc_form <- function(model, type, unit) {
  single <- unit == "single"
  if (model == "oneway") {
    return(if (single) "1" else "k")
  }
  paste0(if (type == "consistency") "C" else "A", if (single) "1" else "k")
}

# Average-measure forms are the Spearman-Brown projection of the single-measure
# form to all k items. With balanced data this is algebraically identical to
# McGraw & Wong's average-measure estimates, intervals and tests; with missing
# data it keeps the estimand the reliability of the mean of all k items rather
# than of the average number observed per subject.
.icc_single_form <- function(form) {
  switch(form, k = "1", Ck = "C1", Ak = "A1", form)
}

.icc_is_average <- function(form) form %in% c("k", "Ck", "Ak")

.spearman_brown <- function(rho, k) k * rho / (1 + (k - 1) * rho)

.spearman_brown_inv <- function(rho_k, k) rho_k / (k - (k - 1) * rho_k)

.icc_estimate <- function(fit, form) {
  if (.icc_is_average(form)) {
    return(.spearman_brown(.icc_estimate(fit, .icc_single_form(form)), fit$k))
  }
  ms <- fit$ms
  n <- fit$n
  k <- fit$kbar
  MSB <- ms[["MSB"]]
  MSJ <- ms[["MSJ"]]
  MSE <- ms[["MSE"]]
  MSW <- ms[["MSW"]]
  switch(form,
         "1" = (MSB - MSW) / (MSB + (k - 1) * MSW),
         "C1" = (MSB - MSE) / (MSB + (k - 1) * MSE),
         "A1" = (MSB - MSE) / (MSB + (k - 1) * MSE + k * (MSJ - MSE) / n))
}

# Coefficients a and b of the ICC(A,1) statistics (McGraw & Wong 1996,
# Tables 7 and 8), evaluated at rho (the null value for tests, the estimate
# for intervals).
.icc_agreement_ab <- function(rho, n, k) {
  c(a = k * rho / (n * (1 - rho)),
    b = 1 + k * rho * (n - 1) / (n * (1 - rho)))
}

.icc_agreement_df <- function(fit, ab) {
  ms <- fit$ms
  .satterthwaite_df(ab, c(ms[["MSJ"]], ms[["MSE"]]),
                    c(fit$df[["dfJ"]], fit$df[["dfE"]]))
}

#' One-sided F-based limits for an ICC (McGraw & Wong 1996, Table 7)
#'
#' Each limit uses tail probability `a`; a two-sided (1 - alpha) interval uses
#' `a = alpha / 2` (Shrout & Fleiss 1979; McGraw & Wong 1996).
#' @noRd
.icc_limits <- function(fit, form, a) {
  if (.icc_is_average(form)) {
    return(.spearman_brown(.icc_limits(fit, .icc_single_form(form), a), fit$k))
  }
  ms <- fit$ms
  dfs <- fit$df
  n <- fit$n
  k <- fit$kbar
  MSB <- ms[["MSB"]]
  MSJ <- ms[["MSJ"]]
  MSE <- ms[["MSE"]]

  if (form %in% c("1", "C1")) {
    one_way <- form == "1"
    F_obs <- MSB / ms[[if (one_way) "MSW" else "MSE"]]
    df2 <- dfs[[if (one_way) "dfW" else "dfE"]]
    FL <- F_obs / stats::qf(1 - a, dfs[["dfB"]], df2)
    FU <- F_obs * stats::qf(1 - a, df2, dfs[["dfB"]])
    return(c((FL - 1) / (FL + k - 1), (FU - 1) / (FU + k - 1)))
  }

  rho <- .icc_estimate(fit, form)
  ab <- .icc_agreement_ab(rho, n, k)
  v <- .icc_agreement_df(fit, ab)
  Fl <- stats::qf(1 - a, dfs[["dfB"]], v)
  Fu <- stats::qf(1 - a, v, dfs[["dfB"]])
  c(n * (MSB - Fl * MSE) /
      (Fl * (k * MSJ + (k * n - k - n) * MSE) + n * MSB),
    n * (Fu * MSB - MSE) /
      (k * MSJ + (k * n - k - n) * MSE + n * Fu * MSB))
}

#' Confidence interval for an ICC
#'
#' Two-sided intervals use `alpha / 2` in each tail; one-sided alternatives
#' return a one-sided bound (as `t.test()` does): `[L, 1]` for `"greater"`,
#' `[-Inf, U]` for `"less"`.
#' @noRd
.icc_ci <- function(fit, form, conf.level = 0.95,
                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  alpha <- 1 - conf.level
  ci <- switch(alternative,
               two.sided = .icc_limits(fit, form, alpha / 2),
               greater = c(.icc_limits(fit, form, alpha)[1], 1),
               less = c(-Inf, .icc_limits(fit, form, alpha)[2]))
  attr(ci, "conf.level") <- conf.level
  ci
}

#' F test of H0: ICC = null.value (McGraw & Wong 1996, Table 8)
#' @noRd
.icc_ftest <- function(fit, form, null.value = 0,
                       alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (.icc_is_average(form)) {
    # H0: ICC(k) = r0 is H0: ICC(1) = r0 / (k - (k - 1) r0)
    return(.icc_ftest(fit, .icc_single_form(form),
                      .spearman_brown_inv(null.value, fit$k), alternative))
  }
  ms <- fit$ms
  dfs <- fit$df
  n <- fit$n
  k <- fit$kbar
  r0 <- null.value

  if (form %in% c("1", "C1")) {
    one_way <- form == "1"
    F_ratio <- ms[["MSB"]] / ms[[if (one_way) "MSW" else "MSE"]]
    statistic <- F_ratio * (1 - r0) / (1 + (k - 1) * r0)
    df1 <- dfs[["dfB"]]
    df2 <- dfs[[if (one_way) "dfW" else "dfE"]]
  } else {
    ab <- .icc_agreement_ab(r0, n, k)
    statistic <- ms[["MSB"]] / (ab[["a"]] * ms[["MSJ"]] + ab[["b"]] * ms[["MSE"]])
    df1 <- dfs[["dfB"]]
    df2 <- .icc_agreement_df(fit, ab)
  }

  list(statistic = c(F = statistic),
       parameter = c(df1 = df1, df2 = df2),
       p.value = .p_from_dist(statistic, alternative,
                              cdf = function(q) stats::pf(q, df1, df2)))
}

# p-value from a statistic whose distribution function is `cdf`; large values
# favour "greater".
.p_from_dist <- function(statistic, alternative, cdf) {
  lower <- cdf(statistic)
  switch(alternative,
         greater = 1 - lower,
         less = lower,
         two.sided = min(1, 2 * min(lower, 1 - lower)))
}

# Working scale for studentized bootstrap intervals: 0.5 * log of the F-ratio
# scale. For the plug-in one-way and consistency ICCs this is exactly
# 0.5 * log(F_obs).
.icc_z <- function(rho, form, k) {
  if (form %in% c("1", "C1", "A1")) {
    0.5 * log((1 + (k - 1) * rho) / (1 - rho))
  } else {
    -0.5 * log(1 - rho)
  }
}

.icc_z_inv <- function(z, form, k) {
  F_ratio <- exp(2 * z)
  if (form %in% c("1", "C1", "A1")) {
    (F_ratio - 1) / (F_ratio + k - 1)
  } else {
    1 - 1 / F_ratio
  }
}

#' Standard error of the ICC on the z working scale
#'
#' Exact for the one-way and consistency forms: Var(0.5 * log F) =
#' 0.25 * (trigamma(df1 / 2) + trigamma(df2 / 2)). Delta method over the mean
#' squares for the agreement forms.
#' @noRd
.icc_z_se <- function(fit, form) {
  dfs <- fit$df
  if (form %in% c("1", "k", "C1", "Ck")) {
    df2 <- dfs[[if (form %in% c("1", "k")) "dfW" else "dfE"]]
    return(0.5 * sqrt(trigamma(dfs[["dfB"]] / 2) + trigamma(df2 / 2)))
  }
  z_of_ms <- function(ms) {
    fit_ms <- fit
    fit_ms$ms[c("MSB", "MSJ", "MSE")] <- ms
    .icc_z(.icc_estimate(fit_ms, form), form, fit$kbar)
  }
  ms <- fit$ms[c("MSB", "MSJ", "MSE")]
  .delta_se(z_of_ms, ms, .ms_var(fit)[c("MSB", "MSJ", "MSE")])
}

# Measurement error (SEM) and total SD ---------------------------------------

#' Error variance and its degrees of freedom for the SEM
#'
#' consistency: MSE; agreement: (MSJ + (n - 1) MSE) / n with Satterthwaite
#' df; one-way: MSW.
#' @noRd
.sem_var <- function(fit, model, type) {
  ms <- fit$ms
  dfs <- fit$df
  n <- fit$n
  if (model == "oneway") {
    return(list(var = ms[["MSW"]], df = dfs[["dfW"]]))
  }
  if (type == "consistency") {
    return(list(var = ms[["MSE"]], df = dfs[["dfE"]]))
  }
  coef <- c(1 / n, (n - 1) / n)
  list(var = (ms[["MSJ"]] + (n - 1) * ms[["MSE"]]) / n,
       df = .satterthwaite_df(coef, c(ms[["MSJ"]], ms[["MSE"]]),
                              c(dfs[["dfJ"]], dfs[["dfE"]])))
}

#' Total SD matched to the denominator of the chosen ICC
#'
#' SD^2 is the observed-score variance the ICC is a proportion of, so that
#' SD^2 * (1 - ICC) equals the error variance of `.sem_var()` exactly.
#' @noRd
.sd_total <- function(fit, model, type) {
  ms <- fit$ms
  n <- fit$n
  k <- fit$kbar
  v <- if (model == "oneway") {
    (ms[["MSB"]] + (k - 1) * ms[["MSW"]]) / k
  } else if (type == "consistency") {
    (ms[["MSB"]] + (k - 1) * ms[["MSE"]]) / k
  } else {
    (ms[["MSB"]] + (k - 1) * ms[["MSE"]] + k * (ms[["MSJ"]] - ms[["MSE"]]) / n) / k
  }
  sqrt(v)
}

# Chi-square inference for a variance ---------------------------------------

#' Confidence interval for a variance estimated with `df` degrees of freedom
#' @noRd
.var_ci <- function(var, df, conf.level = 0.95,
                    alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  alpha <- 1 - conf.level
  lower <- function(a) df * var / stats::qchisq(1 - a, df)
  upper <- function(a) df * var / stats::qchisq(a, df)
  ci <- switch(alternative,
               two.sided = c(lower(alpha / 2), upper(alpha / 2)),
               greater = c(lower(alpha), Inf),
               less = c(0, upper(alpha)))
  attr(ci, "conf.level") <- conf.level
  ci
}

#' Chi-square test of H0: variance = null_var
#' @noRd
.var_test <- function(var, df, null_var,
                      alternative = c("less", "greater", "two.sided")) {
  alternative <- match.arg(alternative)
  statistic <- df * var / null_var
  list(statistic = c(`X-squared` = statistic),
       parameter = c(df = df),
       p.value = .p_from_dist(statistic, alternative,
                              cdf = function(q) stats::pchisq(q, df)))
}

#' Satterthwaite degrees of freedom for sum(coef * ms)
#' @noRd
.satterthwaite_df <- function(coef, ms, df) {
  sum(coef * ms)^2 / sum((coef * ms)^2 / df)
}

# Delta method ---------------------------------------------------------------

# Plug-in variances of the mean squares: Var(MS) = 2 MS^2 / df.
.ms_var <- function(fit) {
  ms <- fit$ms
  dfs <- fit$df
  stats::setNames(2 * ms^2 / dfs[c("dfB", "dfJ", "dfE", "dfW")], names(ms))
}

#' Delta-method standard error of f(theta) for independent components
#'
#' Central-difference gradient; `var_theta` holds the variance of each
#' component of `theta`.
#' @noRd
.delta_se <- function(f, theta, var_theta) {
  grad <- vapply(seq_along(theta), function(i) {
    h <- max(abs(theta[[i]]), 1e-8) * 1e-5
    up <- dn <- theta
    up[[i]] <- theta[[i]] + h
    dn[[i]] <- theta[[i]] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(1))
  sqrt(sum(grad^2 * var_theta))
}

# Nonparametric (subject) bootstrap -------------------------------------------

#' Resample subjects with replacement
#'
#' Keeps all of each drawn subject's observations and gives duplicated
#' subjects new ids. Returns `NULL` when fewer than 2 distinct subjects are
#' drawn.
#' @noRd
.reli_resample <- function(prep) {
  df <- prep$df
  rows <- split(seq_len(nrow(df)), df$id)
  draw <- sample(names(rows), length(rows), replace = TRUE)
  if (length(unique(draw)) < 2) {
    return(NULL)
  }
  idx <- unlist(rows[draw], use.names = FALSE)
  new_df <- df[idx, , drop = FALSE]
  new_df$id <- rep(seq_along(draw), lengths(rows[draw]))
  .reli_prep_from_long(new_df, data.name = prep$data.name)
}

#' Hybrid bootstrap step for random items
#'
#' Resampling subjects keeps the same items, so it leaves out the sampling
#' variation of the item mean square that comes from drawing the items. This
#' adds it back under normal theory: MSJ is multiplied by an independent
#' chi-square(k - 1) / (k - 1) draw. Applied to bootstrap replicates of the
#' two-way agreement forms only.
#' @noRd
.hybrid_item_ms <- function(fit) {
  df_j <- fit$df[["dfJ"]]
  fit$ms[["MSJ"]] <- fit$ms[["MSJ"]] * stats::rchisq(1, df_j) / df_j
  fit
}

#' Run the subject bootstrap (and optionally the jackknife)
#'
#' `stat_fun(prep, resample)` must return a named numeric vector with
#' elements `est`, `z` and `se_z` (`z` and `se_z` may be `NA` when not
#' needed). `resample` is `TRUE` for bootstrap replicates and `FALSE` for the
#' original data and the jackknife, so a statistic can add variation that
#' subject resampling does not capture (the hybrid bootstrap for agreement
#' ICCs). Replicates that error, warn, or draw fewer than 2 distinct subjects
#' are dropped and counted.
#'
#' @return A list with `t0` (statistics on the original data), `t` (matrix of
#'   usable replicates), `R`, `n_failed`, and `jack` (matrix of
#'   leave-one-subject-out statistics, or `NULL`).
#' @noRd
.reli_boot <- function(prep, stat_fun, R = 1999, jackknife = FALSE) {
  safe_stat <- function(p, resample) {
    if (is.null(p)) {
      return(c(est = NA_real_, z = NA_real_, se_z = NA_real_))
    }
    tryCatch(stat_fun(p, resample),
             error = function(e) c(est = NA_real_, z = NA_real_, se_z = NA_real_),
             warning = function(w) c(est = NA_real_, z = NA_real_, se_z = NA_real_))
  }

  t0 <- stat_fun(prep, FALSE)
  t_all <- t(vapply(seq_len(R),
                    function(i) safe_stat(.reli_resample(prep), TRUE),
                    numeric(3)))
  colnames(t_all) <- c("est", "z", "se_z")
  usable <- is.finite(t_all[, "est"])

  jack <- NULL
  if (jackknife) {
    ids <- levels(prep$df$id)
    jack <- t(vapply(ids, function(i) {
      kept <- prep$df[prep$df$id != i, , drop = FALSE]
      p <- tryCatch(.reli_prep_from_long(kept, prep$data.name),
                    error = function(e) NULL)
      safe_stat(p, FALSE)
    }, numeric(3)))
    colnames(jack) <- c("est", "z", "se_z")
  }

  list(t0 = t0,
       t = t_all[usable, , drop = FALSE],
       R = R,
       n_failed = sum(!usable),
       jack = jack)
}

#' BCa bias correction and acceleration (as in TOSTER::bca_params)
#' @noRd
.bca_params <- function(b, est, jack) {
  z0 <- stats::qnorm(mean(b < est))
  if (!is.finite(z0)) {
    stop("BCa bias correction is infinite (all bootstrap estimates are on one ",
         "side of the original estimate). Use boot_ci = \"perc\" instead.",
         call. = FALSE)
  }
  jack <- jack[is.finite(jack)]
  L <- mean(jack) - jack
  denom <- 6 * sum(L^2)^(3 / 2)
  list(z0 = z0, acc = if (denom != 0) sum(L^3) / denom else 0)
}

#' Bootstrap confidence interval
#'
#' `boot_ci` is one of "bca", "stud", "perc", "basic". For "stud", `back`
#' maps the working (z) scale back to the estimate scale. `bounds` gives the
#' parameter space, used for the open end of one-sided intervals and to clamp
#' the limits.
#' @noRd
.boot_interval <- function(bo, boot_ci, conf.level = 0.95,
                           alternative = c("two.sided", "greater", "less"),
                           back = identity, bounds = c(-Inf, Inf)) {
  alternative <- match.arg(alternative)
  alpha <- 1 - conf.level
  b <- bo$t[, "est"]
  est <- bo$t0[["est"]]

  if (boot_ci == "bca") {
    bca <- .bca_params(b, est, bo$jack[, "est"])
  }
  if (boot_ci == "stud") {
    tvec <- .boot_tvec(bo)
  }

  # Lower limit with tail probability a; the upper limit is lim(1 - a).
  lim <- function(p) {
    switch(boot_ci,
           perc = stats::quantile(b, p, names = FALSE),
           basic = 2 * est - stats::quantile(b, 1 - p, names = FALSE),
           bca = {
             zp <- bca$z0 + stats::qnorm(p)
             adj <- stats::pnorm(bca$z0 + zp / (1 - bca$acc * zp))
             stats::quantile(b, adj, names = FALSE)
           },
           stud = back(bo$t0[["z"]] -
                         stats::quantile(tvec, 1 - p, names = FALSE) *
                         bo$t0[["se_z"]]))
  }

  ci <- switch(alternative,
               two.sided = c(lim(alpha / 2), lim(1 - alpha / 2)),
               greater = c(lim(alpha), bounds[2]),
               less = c(bounds[1], lim(1 - alpha)))
  ci <- pmin(pmax(ci, bounds[1]), bounds[2])
  attr(ci, "conf.level") <- conf.level
  ci
}

.boot_tvec <- function(bo) {
  tvec <- (bo$t[, "z"] - bo$t0[["z"]]) / bo$t[, "se_z"]
  tvec <- tvec[is.finite(tvec) & bo$t[, "se_z"] > 0]
  if (length(tvec) == 0) {
    stop("No usable studentized bootstrap replicates. ",
         "Use boot_ci = \"perc\" instead.", call. = FALSE)
  }
  tvec
}

#' Bootstrap p-value by inverting the chosen interval
#'
#' Follows TOSTER::boot_pvalue() (TOSTER 0.9.0) with the same tie handling
#' in every branch. `fwd` maps the null value to the working (z) scale for
#' "stud".
#' @noRd
.boot_pvalue <- function(bo, boot_ci, null.value,
                         alternative = c("two.sided", "greater", "less"),
                         fwd = identity) {
  alternative <- match.arg(alternative)
  b <- bo$t[, "est"]
  B <- length(b)
  est <- bo$t0[["est"]]
  r0 <- null.value

  tail_p <- function(phat) {
    switch(alternative,
           two.sided = min(1, 2 * min(phat, 1 - phat)),
           greater = phat,
           less = 1 - phat)
  }

  switch(boot_ci,
         perc = tail_p((sum(b < r0) + 0.5 * sum(b == r0)) / B),
         basic = {
           reflected <- 2 * est - b
           tail_p((sum(reflected < r0) + 0.5 * sum(reflected == r0)) / B)
         },
         bca = {
           bca <- .bca_params(b, est, bo$jack[, "est"])
           p0 <- (sum(b < r0) + 0.5) / (B + 1)
           u <- stats::qnorm(p0) - bca$z0
           tail_p(stats::pnorm((u * (1 - bca$acc * bca$z0) - bca$z0) /
                                 (1 + bca$acc * u)))
         },
         stud = {
           tvec <- .boot_tvec(bo)
           t_obs <- (bo$t0[["z"]] - fwd(r0)) / bo$t0[["se_z"]]
           switch(alternative,
                  two.sided = min(1, 2 * min(mean(tvec >= t_obs),
                                             mean(tvec <= t_obs))),
                  greater = mean(tvec >= t_obs),
                  less = mean(tvec <= t_obs))
         })
}
