# Reliability engine ---------------------------------------------------------
#
# Internal data preparation, model fitting and inference helpers shared by the
# reliability functions (icc_test(), reli_err_test(), power_icc(),
# precision_icc()). Design notes and sources: references/general/reli_redesign.md
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
                          col.names = NULL,
                          allow_categorical = FALSE) {
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

  if (!allow_categorical && !is.numeric(df$values)) {
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
    effects <- NULL
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
    # intercept and item effects, used by the influence-function SE
    effects <- list(
      mu = unname(lme4::fixef(fit)[1]),
      items = if (model == "twoway") {
        stats::setNames(lme4::ranef(fit)$items[, 1],
                        rownames(lme4::ranef(fit)$items))
      } else NULL)
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
                 effects = effects,
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
  ms <- fit$ms[c("MSB", "MSJ", "MSE")]
  .delta_se(.icc_z_of_ms(fit, form, c("MSB", "MSJ", "MSE")), ms,
            .ms_var(fit)[c("MSB", "MSJ", "MSE")])
}

# z for a fit: the single-measure forms use the observed items per subject,
# the average-measure forms all k items (matching .icc_estimate()).
.icc_z_fit <- function(fit, form) {
  k_z <- if (.icc_is_average(form)) fit$k else fit$kbar
  .icc_z(.icc_estimate(fit, form), form, k_z)
}

# Function of the named mean squares returning z, for numerical derivatives.
.icc_z_of_ms <- function(fit, form, names) {
  function(ms) {
    fit$ms[names] <- ms
    .icc_z_fit(fit, form)
  }
}

# Mean squares whose subject-level influence drives each form; the agreement
# forms also depend on MSJ, handled separately (normal theory, random items).
.icc_ms_names <- function(form) {
  if (form %in% c("1", "k")) c("MSB", "MSW") else c("MSB", "MSE")
}

#' Standard error of the ICC on the z scale
#'
#' `"influence"`: per-subject influence (sandwich) SE, sqrt(sum(psi^2)) / n,
#' with psi the influence of each subject on z. `"jackknife"`: delete one
#' subject at a time. `"normal"`: normal-theory SE (`.icc_z_se()`). For the
#' two-way agreement forms, the influence and jackknife SEs cover the subject
#' part (MSB, MSE) and add the normal-theory variance of MSJ,
#' 2 MSJ^2 / (k - 1), treating the items as random (as the F interval and the
#' hybrid bootstrap do).
#' @noRd
.icc_se_z <- function(prep, fit, form,
                      se_method = c("jackknife", "influence", "normal")) {
  se_method <- match.arg(se_method)
  if (se_method == "normal") {
    return(.icc_z_se(fit, form))
  }
  .subject_se(prep, fit, g = function(f) .icc_z_fit(f, form),
              names = .icc_ms_names(form),
              random_items = form %in% c("A1", "Ak"),
              se_method = se_method)
}

#' Subject-level standard error of a statistic of the mean squares
#'
#' `g(fit)` returns the statistic on its working scale from a `reli_fit`
#' (using `fit$ms`, `fit$n`, `fit$kbar` and, for statistics relative to the
#' mean, `fit$grand_mean`). `names` are the mean squares whose subject-level
#' variation drives `g`. With `random_items = TRUE` the jackknife holds MSJ at
#' its full-data value and the normal-theory variance of MSJ,
#' 2 MSJ^2 / (k - 1), is added (items treated as random); if `g` depends on
#' the grand mean, the normal-theory variance of the item mean,
#' (MSJ - MSE) / (n k), is added too.
#' @noRd
.subject_se <- function(prep, fit, g, names, random_items,
                        se_method = c("jackknife", "influence")) {
  se_method <- match.arg(se_method)
  g_of_ms <- function(nm) {
    function(ms) {
      f <- fit
      f$ms[nm] <- ms
      g(f)
    }
  }
  g_of_mean <- function(m) {
    f <- fit
    f$grand_mean <- m[[1]]
    g(f)
  }
  # zero unless g depends on the grand mean
  g_m <- .num_grad(g_of_mean, c(mean = fit$grand_mean))
  if (se_method == "influence") {
    psi <- cbind(.icc_influence(prep, fit)[, names, drop = FALSE],
                 mean = .grand_mean_influence(prep))
    grad <- c(.num_grad(g_of_ms(names), fit$ms[names]), g_m)
    var_g <- sum((psi %*% grad)^2) / prep$n^2
  } else {
    var_g <- .jackknife_var(prep, fit, g, hold_msj = random_items)
  }
  if (random_items) {
    msj <- fit$ms[["MSJ"]]
    g_j <- .num_grad(g_of_ms("MSJ"), c(MSJ = msj))
    var_g <- var_g + g_j^2 * 2 * msj^2 / fit$df[["dfJ"]] +
      g_m^2 * max(msj - fit$ms[["MSE"]], 0) / (fit$n * fit$kbar)
  }
  sqrt(var_g)
}

#' Per-subject influence on the grand mean
#'
#' The grand mean is sum(n_i * mean_i) / N, so subject i's influence is
#' n * n_i * (mean_i - grand mean) / N (mean_i - grand mean with complete
#' data), on the same footing as `.icc_influence()`.
#' @noRd
.grand_mean_influence <- function(prep) {
  df <- prep$df
  n_i <- as.vector(table(df$id))
  m_i <- as.vector(tapply(df$values, df$id, mean))
  psi <- prep$n * n_i * (m_i - mean(df$values)) / prep$N
  psi - mean(psi)
}

.num_grad <- function(f, theta) {
  vapply(seq_along(theta), function(i) {
    h <- max(abs(theta[[i]]), 1e-8) * 1e-5
    up <- dn <- theta
    up[[i]] <- theta[[i]] + h
    dn[[i]] <- theta[[i]] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(1))
}

#' Per-subject influence on the mean squares
#'
#' Returns an n x 3 matrix with columns MSB, MSE and MSW, each centred to
#' mean zero. Complete data: exact influence of each subject on the ANOVA mean
#' squares. Missing data (REML): moment approximation from each subject's
#' observed cells, adjusted for the estimated intercept and item effects.
#' @noRd
.icc_influence <- function(prep, fit) {
  psi <- if (prep$balanced) .influence_balanced(prep, fit) else
    .influence_unbalanced(prep, fit)
  sweep(psi, 2, colMeans(psi))
}

.reli_matrix <- function(prep) {
  df <- prep$df
  Y <- matrix(NA_real_, prep$n, prep$k)
  Y[cbind(as.integer(df$id), as.integer(df$items))] <- df$values
  Y
}

.influence_balanced <- function(prep, fit) {
  Y <- .reli_matrix(prep)
  n <- nrow(Y)
  k <- ncol(Y)
  D <- sweep(Y, 2, colMeans(Y))   # remove item means
  m <- rowMeans(D)                # subject mean deviations
  r <- D - m                      # residuals after subject and item effects
  w <- rowSums((Y - rowMeans(Y))^2) / (k - 1)
  cbind(MSB = k * m^2 * n / (n - 1),
        MSE = rowSums(r^2) * n / ((n - 1) * (k - 1)),
        MSW = w)
}

# Moment approximation for REML fits to incomplete data. With a_ij the
# measurements adjusted for the intercept and item effects, subject i's mean
# m_i estimates sigma2_id + sigma2_e / n_i and its within-subject variance e_i
# estimates sigma2_e. MSB = kbar * sigma2_id + sigma2_e and MSE (or MSW) =
# sigma2_e, so their influences follow. With complete data this reduces to the
# exact influence (up to the item effects being REML predictions).
.influence_unbalanced <- function(prep, fit) {
  df <- prep$df
  eff <- fit$effects
  item_eff <- if (is.null(eff$items)) 0 else eff$items[as.character(df$items)]
  adj <- df$values - eff$mu - item_eff
  n_i <- as.vector(table(df$id))
  m_i <- as.vector(tapply(adj, df$id, mean))
  ss_i <- as.vector(tapply(adj, df$id, function(a) sum((a - mean(a))^2)))
  e_i <- ifelse(n_i > 1, ss_i / pmax(n_i - 1, 1), 0)

  v_e <- fit$vc[["resid"]]
  v_id <- fit$vc[["id"]]
  wts <- n_i - 1
  psi_e <- prep$n * wts * (e_i - v_e) / sum(wts)
  psi_id <- (m_i^2 - v_e / n_i - v_id) - mean(1 / n_i) * psi_e
  psi_b <- fit$kbar * psi_id + psi_e
  cbind(MSB = psi_b, MSE = psi_e, MSW = psi_e)
}

#' Jackknife variance of g(fit), deleting one subject at a time
#'
#' ANOVA fits use closed-form leave-one-out mean squares; REML fits are
#' refitted. With `hold_msj = TRUE` MSJ is held at its full-data value (its
#' variance is added separately in `.subject_se()`).
#' @noRd
.jackknife_var <- function(prep, fit, g, hold_msj = FALSE) {
  n <- prep$n
  if (fit$method == "anova") {
    Y <- .reli_matrix(prep)
    loo <- .loo_ms(Y)
    loo_mean <- (sum(Y) - rowSums(Y)) / ((n - 1) * ncol(Y))
    z_i <- vapply(seq_len(n), function(i) {
      f <- fit
      f$n <- n - 1
      f$ms <- loo[i, ]
      f$grand_mean <- loo_mean[i]
      if (hold_msj) f$ms[["MSJ"]] <- fit$ms[["MSJ"]]
      g(f)
    }, numeric(1))
  } else {
    ids <- levels(prep$df$id)
    z_i <- vapply(ids, function(i) {
      # deleting a subject can leave too few items (e.g. the only subject
      # observed on an item); such deletions are skipped
      tryCatch({
        p <- .reli_prep_from_long(prep$df[prep$df$id != i, , drop = FALSE])
        f <- .reli_fit(p, method = fit$method, model = fit$model,
                       keep_fit = FALSE)
        if (hold_msj) f$ms[["MSJ"]] <- fit$ms[["MSJ"]]
        g(f)
      }, error = function(e) NA_real_)
    }, numeric(1))
    z_i <- z_i[is.finite(z_i)]
    if (length(z_i) < n) {
      warning(sprintf(paste0("%d of %d leave-one-subject-out fits failed and ",
                             "were skipped in the jackknife standard error."),
                      n - length(z_i), n), call. = FALSE)
    }
    n <- length(z_i)
    if (n < 2) {
      return(NA_real_)
    }
  }
  (n - 1) / n * sum((z_i - mean(z_i))^2)
}

#' Leave-one-subject-out ANOVA mean squares for a complete n x k matrix
#'
#' Row i holds the mean squares with subject i removed, computed by
#' downdating the sums rather than refitting.
#' @noRd
.loo_ms <- function(Y) {
  n <- nrow(Y)
  k <- ncol(Y)
  n1 <- n - 1
  m <- rowMeans(Y)
  g <- (sum(Y) - rowSums(Y)) / (n1 * k)
  SSB <- k * (sum(m^2) - m^2 - n1 * g^2)
  SST <- (sum(Y^2) - rowSums(Y^2)) - n1 * k * g^2
  col_means <- (matrix(colSums(Y), n, k, byrow = TRUE) - Y) / n1
  SSJ <- n1 * (rowSums(col_means^2) - k * g^2)
  SSE <- SST - SSB - SSJ
  cbind(MSB = SSB / (n1 - 1),
        MSJ = SSJ / (k - 1),
        MSE = SSE / ((n1 - 1) * (k - 1)),
        MSW = (SSJ + SSE) / (n1 * (k - 1)))
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

# Error statistics: SEM, SEE, SEP --------------------------------------------
#
# SEM: sqrt of the error variance (.sem_var()). SEE and SEP use the
# single-measure ICC of the same model and type with the matched total SD:
#   SEE = SD * sqrt(ICC * (1 - ICC)),  SEP = SD * sqrt(1 - ICC^2)
# (Weir 2005, eqs. 11 and 15). A negative ICC estimate is truncated at 0 (no
# true-score variance), giving SEE = 0 and SEP = SD.

.err_icc_form <- function(model, type) .icc_form(model, type, "single")

#' SEM, SEE or SEP from a reli_fit, on the reported scale
#'
#' `scale = "raw"`: measurement units. `"log"`: the fit is to log(values),
#' and the statistic is reported in sympercents, 100 times the log-scale
#' value. `"mean"`: 100 times the statistic divided by the grand mean.
#' @noRd
.err_estimate <- function(fit, stat, model, type, scale = "raw") {
  est <- if (stat == "SEM") {
    sqrt(.sem_var(fit, model, type)$var)
  } else {
    rho <- max(.icc_estimate(fit, .err_icc_form(model, type)), 0)
    sd <- .sd_total(fit, model, type)
    switch(stat,
           SEE = sd * sqrt(rho * (1 - rho)),
           SEP = sd * sqrt(1 - rho^2))
  }
  switch(scale,
         raw = est,
         log = 100 * est,
         mean = 100 * est / fit$grand_mean)
}

# Mean squares whose subject-level variation drives the error statistics.
.err_ms_names <- function(model) {
  if (model == "oneway") c("MSB", "MSW") else c("MSB", "MSE")
}

.err_random_items <- function(model, type) {
  model == "twoway" && type == "agreement"
}

#' Normal-theory variance of the grand mean
#'
#' Items fixed (consistency): MSB / (n k). Items random (two-way agreement):
#' (MSB + MSJ - MSE) / (n k), adding the sampling variance of the item mean.
#' One-way: MSB / (n k). With missing data, n k is the number of
#' observations N.
#' @noRd
.grand_mean_var <- function(fit, model, type) {
  ms <- fit$ms
  v <- ms[["MSB"]]
  if (.err_random_items(model, type)) {
    v <- v + max(ms[["MSJ"]] - ms[["MSE"]], 0)
  }
  v / (fit$n * fit$kbar)
}

#' Standard error of the log of an error statistic
#'
#' `"normal"`: for the SEM, 0.5 * sqrt(trigamma(nu / 2)), exact for a variance
#' with nu degrees of freedom (Satterthwaite nu for the agreement SEM); for
#' SEE and SEP, the delta method over the mean squares with
#' Var(MS) = 2 MS^2 / df. `"jackknife"` and `"influence"`: subject-level SEs
#' as for the ICC, adding the normal-theory variance of MSJ for the two-way
#' agreement forms. The log scale (`scale = "log"`) only multiplies by 100,
#' so the SE of the log is unchanged. With `scale = "mean"` the normal-theory
#' SE adds Var(grand mean) / mean^2 (the grand mean is independent of the
#' mean squares under normality); the subject-level SEs include the grand
#' mean through each subject's influence or deletion.
#' @noRd
.err_se_log <- function(prep, fit, stat, model, type,
                        se_method = c("jackknife", "influence", "normal"),
                        scale = "raw") {
  se_method <- match.arg(se_method)
  g <- function(f) log(.err_estimate(f, stat, model, type, scale))
  if (se_method == "normal") {
    if (stat == "SEM") {
      nu <- .sem_var(fit, model, type)$df
      se <- 0.5 * sqrt(trigamma(nu / 2))
    } else {
      names <- c(.err_ms_names(model),
                 if (.err_random_items(model, type)) "MSJ")
      g_ms <- function(ms) {
        f <- fit
        f$ms[names] <- ms
        g(f)
      }
      se <- .delta_se(g_ms, fit$ms[names], .ms_var(fit)[names])
    }
    if (scale == "mean") {
      se <- sqrt(se^2 + .grand_mean_var(fit, model, type) / fit$grand_mean^2)
    }
    return(se)
  }
  .subject_se(prep, fit, g = g,
              names = .err_ms_names(model),
              random_items = .err_random_items(model, type),
              se_method = se_method)
}

#' Analytic confidence interval for an error statistic
#'
#' SEM on the raw or log scale: square root of the chi-square interval for
#' the error variance (exact, or Satterthwaite for the agreement SEM), times
#' 100 on the log scale. Otherwise (SEE, SEP, and every statistic with
#' `scale = "mean"`): Wald interval on the log scale with the normal-theory
#' delta-method SE, back-transformed.
#' @noRd
.err_ci <- function(fit, stat, model, type, conf.level = 0.95,
                    alternative = c("two.sided", "greater", "less"),
                    scale = "raw") {
  alternative <- match.arg(alternative)
  if (stat == "SEM" && scale != "mean") {
    sv <- .sem_var(fit, model, type)
    mult <- if (scale == "log") 100 else 1
    ci <- mult * sqrt(.var_ci(sv$var, sv$df, conf.level, alternative))
    attr(ci, "conf.level") <- conf.level
    return(ci)
  }
  est <- .err_estimate(fit, stat, model, type, scale)
  se <- .err_se_log(NULL, fit, stat, model, type, "normal", scale)
  alpha <- 1 - conf.level
  lim <- function(p) est * exp(stats::qnorm(p) * se)
  ci <- switch(alternative,
               two.sided = c(lim(alpha / 2), lim(1 - alpha / 2)),
               greater = c(lim(alpha), Inf),
               less = c(0, lim(1 - alpha)))
  attr(ci, "conf.level") <- conf.level
  ci
}

#' Test of H0: error statistic = null.value (on the reported scale)
#'
#' SEM on the raw or log scale: chi-square test on the variance, statistic
#' nu * SEM^2 / null^2. Otherwise: z test on the log scale with the
#' normal-theory delta SE.
#' @noRd
.err_test <- function(fit, stat, model, type, null.value,
                      alternative = c("less", "greater", "two.sided"),
                      scale = "raw") {
  alternative <- match.arg(alternative)
  if (stat == "SEM" && scale != "mean") {
    sv <- .sem_var(fit, model, type)
    null_raw <- if (scale == "log") null.value / 100 else null.value
    return(.var_test(sv$var, sv$df, null_raw^2, alternative))
  }
  est <- .err_estimate(fit, stat, model, type, scale)
  se <- .err_se_log(NULL, fit, stat, model, type, "normal", scale)
  z <- (log(est) - log(null.value)) / se
  list(statistic = c(z = z),
       parameter = NULL,
       p.value = .p_from_dist(z, alternative, cdf = stats::pnorm))
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
#' two-way agreement forms only. With `shift_mean = TRUE` (statistics
#' relative to the grand mean) the grand mean also gets an independent
#' normal draw with the item mean's variance, (MSJ - MSE) / (n k).
#' @noRd
.hybrid_item_ms <- function(fit, shift_mean = FALSE) {
  if (shift_mean) {
    v_item_mean <- max(fit$ms[["MSJ"]] - fit$ms[["MSE"]], 0) / (fit$n * fit$kbar)
    fit$grand_mean <- fit$grand_mean + stats::rnorm(1, 0, sqrt(v_item_mean))
  }
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
  # a resample with missing data can lose items; .reli_resample() then errors
  # and the replicate is counted as failed
  t_all <- t(vapply(seq_len(R), function(i) {
    p <- tryCatch(.reli_resample(prep), error = function(e) NULL)
    safe_stat(p, TRUE)
  }, numeric(3)))
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
