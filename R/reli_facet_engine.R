# Facet (generalizability) engine ---------------------------------------------
#
# Internal data preparation, model fitting and inference for
# reli_facet_test(). Two designs, both with subjects (p) as the object of
# measurement and all effects random:
#
#   nested:  items (i) crossed with subjects, repeat measurements nested in
#            each subject-by-item cell, r:(p x i);
#   crossed: items crossed with occasions (o) shared across items (every
#            item measures every subject on each occasion), p x i x o,
#            optionally with repeats within each p x i x o cell.
#
# The design is data: effects are named by their facets ("p", "i", "o",
# "pi", "po", "io", "pio"), plus "e" for repeats within cells. Expected mean
# squares follow the all-random rule and estimand weights follow Brennan's
# (2001) rules, so every estimand is a linear combination of mean squares
# (an error variance) or a ratio of two (a coefficient), and one set of
# interval routines (exact, MLS, Satterthwaite) serves all of them.
# Design notes and simulations: references/general/reli_redesign.md,
# Sections 14 and 15.

# Data preparation -----------------------------------------------------------

#' Prepare facet data (formula or long interface only)
#' @noRd
.facet_prepare <- function(x = NULL, data = NULL, measure = NULL,
                           item = NULL, id = NULL, occasion = NULL) {
  .reli_prepare(x = x, data = data, measure = measure, item = item, id = id,
                repeats = TRUE, occasion = occasion)
}

#' Build a `facet_prep` object from long data with columns values, items, id
#' and, for the crossed design, occ
#'
#' Cells (subject x item, or subject x item x occasion) may hold several
#' measurements. Also used for bootstrap and jackknife resamples.
#' @noRd
.facet_prep_from_long <- function(df, data.name = NULL) {
  df <- df[!is.na(df$values), , drop = FALSE]
  df$items <- droplevels(as.factor(df$items))
  df$id <- if (is.factor(df$id)) droplevels(df$id) else
    factor(df$id, levels = unique(df$id))
  crossed <- "occ" %in% names(df)
  if (crossed) {
    df$occ <- droplevels(as.factor(df$occ))
  }
  rownames(df) <- NULL

  n <- nlevels(df$id)
  k <- nlevels(df$items)
  if (n < 2) {
    stop("At least 2 subjects with measurements are required.", call. = FALSE)
  }
  if (k < 2) {
    stop("At least 2 items (raters or devices) are required.", call. = FALSE)
  }
  levels <- c(p = n, i = k)
  if (crossed) {
    levels <- c(levels, o = nlevels(df$occ))
    if (levels[["o"]] < 2) {
      stop("At least 2 occasions are required.", call. = FALSE)
    }
  }
  factors <- names(levels)
  cols <- .facet_columns(factors)
  counts <- unclass(table(df[cols]))
  if (!crossed && max(counts) < 2) {
    stop("No subject-by-item cell has more than one measurement, so repeat ",
         "measurement error cannot be separated from the subject-by-item ",
         "interaction. Use icc_test() or reli_err_test() for one measurement ",
         "per cell, or give 'occasion' if the measurements were made on ",
         "shared occasions.", call. = FALSE)
  }
  observed <- counts[counts > 0]
  balanced <- all(counts == counts[1])
  effects <- .facet_effects(factors, replicated = max(counts) >= 2)
  # observed level combinations of each effect's factors
  n_T <- vapply(setdiff(effects, "e"), function(eff) {
    nrow(unique(df[.facet_columns(.facet_letters(eff))]))
  }, numeric(1))

  structure(list(df = df,
                 n = n,
                 k = k,
                 levels = levels,
                 factors = factors,
                 design = if (crossed) "crossed" else "nested",
                 effects = effects,
                 counts = counts,
                 n_T = n_T,
                 N = nrow(df),
                 n_cells = length(observed),
                 n_missing = length(counts) - length(observed),
                 # repeats per cell; the harmonic mean when they vary
                 r = if (balanced) counts[1] else
                   length(observed) / sum(1 / observed),
                 replicated = max(counts) >= 2,
                 balanced = balanced,
                 data.name = data.name),
            class = c("facet_prep", "reli_prep"))
}

# Effects and their facets -----------------------------------------------------

# All non-empty subsets of the factors, by order (p, i, o, pi, po, io, pio),
# plus "e" (repeats within cells) when there is replication.
.facet_effects <- function(factors, replicated) {
  subs <- unlist(lapply(seq_along(factors), function(s) {
    utils::combn(factors, s, paste, collapse = "")
  }), use.names = FALSE)
  if (replicated) c(subs, "e") else subs
}

# Facets of an effect; "e" involves every factor and the repeat facet "r".
.facet_letters <- function(eff, factors = c("p", "i", "o")) {
  if (eff == "e") c(factors, "r") else strsplit(eff, "")[[1]]
}

.facet_columns <- function(letters) {
  c(p = "id", i = "items", o = "occ")[intersect(letters, c("p", "i", "o"))]
}

#' Expected-mean-square matrix (rows: mean squares, columns: components)
#'
#' All-random rule: E(MS_S) = sum over effects T containing S of c_T
#' sigma2_T, with c_T = r times the number of cells per level combination of
#' T (the product of the levels of the factors not in T with balanced data;
#' observed cells / observed combinations otherwise), and c_e = 1.
#' @noRd
.facet_ems <- function(fit) {
  eff <- fit$effects
  E <- matrix(0, length(eff), length(eff), dimnames = list(eff, eff))
  cells <- if (fit$balanced) prod(fit$levels) else fit$n_cells
  for (s in eff) {
    for (t in eff) {
      if (t == "e") {
        E[s, t] <- 1
      } else if (s != "e" && all(.facet_letters(s) %in% .facet_letters(t))) {
        n_t <- if (fit$balanced) prod(fit$levels[.facet_letters(t)]) else
          fit$n_T[[t]]
        E[s, t] <- fit$r * cells / n_t
      }
    }
  }
  E
}

# Components as linear combinations of the mean squares: solve(EMS).
.facet_refresh <- function(fit) {
  fit$comp_coef <- solve(.facet_ems(fit))
  fit$vc <- drop(fit$comp_coef %*% fit$ms)
  fit
}

# Model fitting --------------------------------------------------------------

#' Balanced ANOVA mean squares from the array of cell means
#'
#' `A` has one dimension per factor (subjects first); `ssw` is the
#' within-cell sum of squares (used when there is replication).
#' @noRd
.facet_anova_ms <- function(A, r, ssw, effects) {
  dims <- dim(A)
  nd <- length(dims)
  letters <- c("p", "i", "o")[seq_len(nd)]
  margin <- function(sub) {
    if (length(sub) == 0) return(array(mean(A), dims))
    if (length(sub) == nd) return(A)
    M <- apply(A, sub, mean)
    rest <- setdiff(seq_len(nd), sub)
    aperm(array(M, dims[c(sub, rest)]), order(c(sub, rest)))
  }
  subsets <- unlist(lapply(0:nd, function(s) {
    utils::combn(seq_len(nd), s, simplify = FALSE)
  }), recursive = FALSE)
  means <- lapply(subsets, margin)
  keys <- vapply(subsets, function(s) paste(letters[s], collapse = ""), "")
  ms <- dfs <- stats::setNames(numeric(length(effects)), effects)
  for (eff in setdiff(effects, "e")) {
    S <- match(strsplit(eff, "")[[1]], letters)
    est <- 0
    for (j in seq_along(subsets)) {
      Tj <- subsets[[j]]
      if (all(Tj %in% S)) {
        est <- est + (-1)^(length(S) - length(Tj)) * means[[j]]
      }
    }
    dfs[[eff]] <- prod(dims[S] - 1)
    ms[[eff]] <- r * sum(est^2) / dfs[[eff]]
  }
  if ("e" %in% effects) {
    dfs[["e"]] <- prod(dims) * (r - 1)
    ms[["e"]] <- ssw / dfs[["e"]]
  }
  list(ms = ms, df = dfs)
}

# Array of cell means (subjects x items [x occasions]) and the within-cell
# sums of squares per subject.
.facet_cells <- function(prep) {
  df <- prep$df
  cols <- .facet_columns(prep$factors)
  A <- tapply(df$values, unname(as.list(df[cols])), mean)
  A <- array(as.numeric(A), dim(A))
  idx <- vapply(cols, function(cl) as.integer(df[[cl]]), integer(nrow(df)))
  if (!is.matrix(idx)) idx <- matrix(idx, nrow = 1)
  resid2 <- (df$values - A[idx])^2
  list(A = A, ssw_i = as.vector(tapply(resid2, df$id, sum)))
}

#' Fit the facet model
#'
#' `method = "anova"`: balanced data (every cell with the same number of
#' repeats), mean squares in closed form. `method = "reml"`: lme4 with a
#' random intercept for every effect except the residual, and mean squares
#' reconstructed from the components with the observed-data design
#' quantities (harmonic mean repeats, observed cells per level combination,
#' observed-data df). With balanced data the two coincide unless REML
#' truncates a component at 0.
#'
#' @return A list of class `facet_fit`.
#' @noRd
.facet_fit <- function(prep, method = c("anova", "reml"), keep_fit = TRUE) {
  method <- match.arg(method)
  df <- prep$df
  fit <- list(method = method,
              model = "facet",
              design = prep$design,
              factors = prep$factors,
              effects = prep$effects,
              levels = prep$levels,
              n = prep$n,
              k = prep$k,
              r = prep$r,
              n_T = prep$n_T,
              N = prep$N,
              n_cells = prep$n_cells,
              n_missing = prep$n_missing,
              replicated = prep$replicated,
              balanced = prep$balanced,
              grand_mean = mean(df$values),
              singular = FALSE)

  if (method == "anova") {
    if (!prep$balanced) {
      stop("method = \"anova\" requires every cell to have the same number ",
           "of measurements. Use method = \"reml\" for empty cells or ",
           "unequal repeats.", call. = FALSE)
    }
    cells <- .facet_cells(prep)
    am <- .facet_anova_ms(cells$A, prep$r, sum(cells$ssw_i), prep$effects)
    fit$ms <- am$ms
    fit$df <- am$df
    # every effect except the residual one, so the residuals are the error
    fit$fit <- if (keep_fit) {
      stats::lm(if (prep$design == "nested") values ~ id * items else
        if (prep$replicated) values ~ id * items * occ else
          values ~ (id + items + occ)^2, data = df)
    } else NULL
    return(structure(.facet_refresh(fit), class = "facet_fit"))
  }

  resid_eff <- if (prep$replicated) "e" else
    paste(prep$factors, collapse = "")
  terms <- vapply(setdiff(prep$effects, resid_eff), function(eff) {
    paste0("(1 | ", paste(.facet_columns(.facet_letters(eff)),
                          collapse = ":"), ")")
  }, "")
  form <- stats::as.formula(paste("values ~ 1 +", paste(terms, collapse = " + ")))
  mod <- suppressMessages(lme4::lmer(form, data = df, REML = TRUE))
  vc_df <- as.data.frame(lme4::VarCorr(mod))
  vc <- stats::setNames(numeric(length(prep$effects)), prep$effects)
  for (eff in setdiff(prep$effects, resid_eff)) {
    grp <- paste(.facet_columns(.facet_letters(eff)), collapse = ":")
    vc[[eff]] <- vc_df$vcov[vc_df$grp == grp]
  }
  vc[[resid_eff]] <- stats::sigma(mod)^2
  fit$ms <- drop(.facet_ems(fit) %*% vc)
  fit$df <- .facet_observed_df(prep)
  fit$singular <- lme4::isSingular(mod)
  fit$fit <- if (keep_fit) mod else NULL
  structure(.facet_refresh(fit), class = "facet_fit")
}

# Observed-data df: for each effect, observed level combinations minus 1
# minus the df of the effects it contains; N - cells within cells.
.facet_observed_df <- function(prep) {
  eff <- prep$effects
  dfs <- stats::setNames(numeric(length(eff)), eff)
  for (s in setdiff(eff, "e")) {
    lower <- setdiff(eff, c(s, "e"))
    lower <- lower[vapply(lower, function(t) {
      all(.facet_letters(t) %in% .facet_letters(s))
    }, logical(1))]
    dfs[[s]] <- prep$n_T[[s]] - 1 - sum(dfs[lower])
  }
  if ("e" %in% eff) {
    dfs[["e"]] <- prep$N - prep$n_cells
  }
  dfs
}

# Estimands ------------------------------------------------------------------
#
# `spec` holds stat ("ICC" or "SEM"), facet ("intra", "inter", "overall"),
# type ("consistency", "agreement"), the D-study sizes k_avg (items), m_avg
# (occasions) and r_avg (repeats), and scale ("raw", "log", "mean").
#
# Which facets are fixed (part of the subject's universe score) and which
# are generalised over (random):
#   intra:   the item fixed; occasions and repeats random
#   inter:   the occasion fixed (crossed design); items and repeats random
#   overall: items, occasions and repeats random
# With the nested design "overall" is "inter".

.facet_roles <- function(spec, design) {
  fixed <- switch(spec$facet,
                  intra = "i",
                  inter = if (design == "crossed") "o" else character(0),
                  overall = character(0))
  list(fixed = fixed,
       random = setdiff(c("i", "o", "r"), fixed),
       nprime = c(i = spec$k_avg, o = spec$m_avg, r = spec$r_avg))
}

#' Component weights of the universe score and error (Brennan, 2001)
#'
#' Universe score: effects with p and only fixed facets. Relative error:
#' effects with p and a random facet. Absolute error adds the effects
#' without p that have a random facet. Each effect is divided by the D-study
#' sizes of its facets other than p.
#' @noRd
.facet_weights <- function(fit, spec) {
  roles <- .facet_roles(spec, fit$design)
  eff <- fit$effects
  tau <- delta <- extra <- stats::setNames(numeric(length(eff)), eff)
  for (t in eff) {
    L <- .facet_letters(t, fit$factors)
    others <- setdiff(L, "p")
    w <- 1 / prod(roles$nprime[others])
    if ("p" %in% L) {
      if (all(others %in% roles$fixed)) tau[[t]] <- w else
        if (any(others %in% roles$random)) delta[[t]] <- w
    } else if (any(L %in% roles$random)) {
      extra[[t]] <- w
    }
  }
  err <- if (spec$type == "agreement") delta + extra else delta
  if (spec$stat == "SEM") {
    return(list(var = err))
  }
  list(num = tau, den = tau + err)
}

#' Mean-square coefficients of the estimand
#'
#' SEM: `var`, the error variance. ICC: `a` (numerator) and `t` (total).
#' @noRd
.facet_ms_coef <- function(fit, spec) {
  w <- .facet_weights(fit, spec)
  clean <- function(v) {
    v <- drop(v %*% fit$comp_coef)
    v[abs(v) < 1e-12 * max(abs(v))] <- 0
    v
  }
  if (spec$stat == "SEM") {
    return(list(var = clean(w$var)))
  }
  list(a = clean(w$num), t = clean(w$den))
}

# Effects without subjects that enter the estimand (random items or
# occasions counted as error): their mean squares get the hybrid bootstrap
# step and the normal-theory term in the jackknife SE.
.facet_nonsubject <- function(fit, spec) {
  cf <- .facet_ms_coef(fit, spec)
  used <- if (spec$stat == "SEM") cf$var != 0 else cf$a != 0 | cf$t != 0
  eff <- fit$effects[used]
  eff[!grepl("p", eff, fixed = TRUE) & eff != "e"]
}

#' Is the analytic inference exact (one mean square for an SEM, a ratio of
#' two for a coefficient)?
#' @noRd
.facet_is_exact <- function(fit, spec) {
  cf <- .facet_ms_coef(fit, spec)
  if (spec$stat == "SEM") {
    return(sum(cf$var != 0) == 1)
  }
  sum(cf$a != 0 | cf$t != 0) <= 2
}

#' Point estimate on the reported scale (unconstrained; a negative error
#' variance gives an SEM of 0)
#' @noRd
.facet_estimate <- function(fit, spec) {
  cf <- .facet_ms_coef(fit, spec)
  ms <- fit$ms
  if (spec$stat == "ICC") {
    return(sum(cf$a * ms) / sum(cf$t * ms))
  }
  est <- sqrt(max(sum(cf$var * ms), 0))
  switch(spec$scale,
         raw = est,
         log = 100 * est,
         mean = 100 * est / fit$grand_mean)
}

# Working scale: -0.5 * log(1 - rho) for coefficients, the log for SEMs.
.facet_z <- function(est, spec) {
  if (spec$stat == "ICC") -0.5 * log(1 - est) else log(est)
}

.facet_z_inv <- function(z, spec) {
  if (spec$stat == "ICC") 1 - exp(-2 * z) else exp(z)
}

.facet_z_fit <- function(fit, spec) .facet_z(.facet_estimate(fit, spec), spec)

# Intervals for linear combinations of mean squares --------------------------

#' Modified large-sample (MLS) one-sided bounds for sum(coef * E(MS))
#'
#' Graybill & Wang (1980) when every coefficient is positive; Ting, Burdick,
#' Graybill, Jeyaratnam & Lu (1990), intervals (3.1) and (3.2), when some
#' are negative (checked against the paper, 2026-10-21; their F(d, ., .) is
#' the value with area d to the right, so F(a1) here is qf(1 - a)). Their
#' refinements (3.3) and (3.4) add cross-products for pairs of same-signed
#' mean squares; these are omitted: they widened our intervals by at most
#' about 0.002 (reli_redesign.md, Section 15.10). Each bound has tail
#' probability `a`.
#'
#' TODO (possible, not planned): a generalized confidence interval (GCI;
#' Weerahandi, 1993; Hamada & Weerahandi, 2000) as a third `ci_method`.
#' Motivation: for agreement statistics with 2-3 items or occasions the MLS
#' tails are uneven (the bound driven by the 1-2 df component is very
#' conservative, the other up to about 6% instead of 2.5%). Not done
#' because (1) the unevenness mostly reflects how little 1-2 df say about
#' that variance, so a GCI faces the same limit (its pivotal R = s^2 /
#' chi^2_1 is just as right-skewed) and published comparisons find GCI and
#' MLS similar; (2) the conservative side is the side of the usual tests
#' (coefficient above a threshold, SEM below an acceptable error); (3) it
#' adds Monte Carlo output (seed-dependent), test inversion and another
#' round of simulations. Ting et al.'s (1989) cross terms were also tried
#' and rejected: liberal (to 0.17) with a 1-df mean square. Before building
#' it, pilot a GCI on the cells of reli_sims/ting1989_check.R and proceed
#' only if both tails clearly move toward nominal.
#' @noRd
.mls_bounds <- function(coef, ms, dfs, a) {
  keep <- coef != 0
  coef <- coef[keep]
  ms <- ms[keep]
  dfs <- dfs[keep]
  est <- sum(coef * ms)
  pos <- coef > 0
  cp <- coef[pos] * ms[pos]
  np <- dfs[pos]
  cn <- -coef[!pos] * ms[!pos]
  nn <- dfs[!pos]
  G <- function(nu) 1 - nu / stats::qchisq(1 - a, nu)
  H <- function(nu) nu / stats::qchisq(a, nu) - 1
  v_lower <- sum(G(np)^2 * cp^2) + sum(H(nn)^2 * cn^2)
  v_upper <- sum(H(np)^2 * cp^2) + sum(G(nn)^2 * cn^2)
  for (q in seq_along(cp)) {
    for (s in seq_along(cn)) {
      f1 <- stats::qf(1 - a, np[q], nn[s])
      f2 <- stats::qf(a, np[q], nn[s])
      g_qs <- ((f1 - 1)^2 - G(np[q])^2 * f1^2 - H(nn[s])^2) / f1
      h_qs <- ((1 - f2)^2 - H(np[q])^2 * f2^2 - G(nn[s])^2) / f2
      v_lower <- v_lower + g_qs * cp[q] * cn[s]
      v_upper <- v_upper + h_qs * cp[q] * cn[s]
    }
  }
  c(est - sqrt(max(v_lower, 0)), est + sqrt(max(v_upper, 0)))
}

#' Satterthwaite F statistic for H0: sum(d * E(MS)) = 0
#'
#' The positive and negative parts of the combination are each treated as a
#' scaled chi-square with Satterthwaite df, so their ratio is approximately
#' F (the device of McGraw & Wong's ICC(A,1) test). Large values favour a
#' positive combination.
#' @noRd
.satterthwaite_f <- function(d, ms, dfs) {
  pos <- d > 0
  neg <- d < 0
  num <- sum(d[pos] * ms[pos])
  den <- sum(-d[neg] * ms[neg])
  df1 <- if (any(pos)) .satterthwaite_df(d[pos], ms[pos], dfs[pos]) else 1
  df2 <- if (any(neg)) .satterthwaite_df(-d[neg], ms[neg], dfs[neg]) else 1
  stat <- if (den == 0) Inf else num / den
  list(statistic = stat, df1 = df1, df2 = df2,
       p_greater = stats::pf(stat, df1, df2, lower.tail = FALSE),
       p_less = stats::pf(stat, df1, df2))
}

# Root in rho of a function that changes sign once on (-Inf, 1): the bracket
# is widened downwards as needed. Returns `at_lower` / `at_upper` when there
# is no sign change at that end.
.facet_root <- function(f, start, at_lower = -Inf, at_upper = 1) {
  hi <- 1 - 1e-10
  f_hi <- f(hi)
  lo <- min(start, 0) - 1
  f_lo <- f(lo)
  while (sign(f_lo) == sign(f_hi) && lo > -1e6) {
    lo <- 2 * lo - 1
    f_lo <- f(lo)
  }
  if (sign(f_lo) == sign(f_hi)) {
    return(if (f_hi > 0) at_upper else at_lower)
  }
  stats::uniroot(f, c(lo, hi), f.lower = f_lo, f.upper = f_hi,
                 tol = 1e-12, maxiter = 1000)$root
}

#' One-sided limits for a coefficient by inverting a test of
#' H0: rho = rho0, i.e. sum((a - rho0 t) * E(MS)) = 0
#'
#' `ci_method = "satterthwaite"`: Satterthwaite F test (exact when only two
#' mean squares are involved). `"mls"`: the MLS interval for the
#' combination, which excludes 0 exactly when rho0 is outside the interval.
#' @noRd
.facet_icc_limits <- function(fit, spec, a, ci_method) {
  cf <- .facet_ms_coef(fit, spec)
  ms <- fit$ms
  dfs <- fit$df
  est <- .facet_estimate(fit, spec)
  d <- function(rho0) cf$a - rho0 * cf$t
  if (ci_method == "mls") {
    total <- sum(cf$t * ms)
    lower_f <- function(rho0) .mls_bounds(d(rho0), ms, dfs, a)[1] / total
    upper_f <- function(rho0) .mls_bounds(d(rho0), ms, dfs, a)[2] / total
    return(c(.facet_root(lower_f, est), .facet_root(upper_f, est)))
  }
  lower_f <- function(rho0) a - .satterthwaite_f(d(rho0), ms, dfs)$p_greater
  upper_f <- function(rho0) .satterthwaite_f(d(rho0), ms, dfs)$p_less - a
  c(.facet_root(lower_f, est), .facet_root(upper_f, est))
}

#' One-sided limits for the error variance (SEM^2)
#' @noRd
.facet_var_limits <- function(fit, spec, a, ci_method) {
  cf <- .facet_ms_coef(fit, spec)$var
  ms <- fit$ms
  dfs <- fit$df
  v <- max(sum(cf * ms), 0)
  if (ci_method == "mls") {
    return(pmax(.mls_bounds(cf, ms, dfs, a), 0))
  }
  # exact (one mean square) or Satterthwaite chi-square
  nu <- .facet_sem_df(fit, spec)
  c(nu * v / stats::qchisq(1 - a, nu), nu * v / stats::qchisq(a, nu))
}

.facet_sem_df <- function(fit, spec) {
  cf <- .facet_ms_coef(fit, spec)$var
  keep <- cf != 0
  .satterthwaite_df(cf[keep], fit$ms[keep], fit$df[keep])
}

#' Analytic interval on the reported scale
#'
#' `ci_method` is the resolved method ("exact", "mls", "satterthwaite" or
#' "delta"). SEMs with `scale = "mean"` use a Wald interval on the log scale
#' with the normal-theory delta-method SE, including the grand mean.
#' @noRd
.facet_ci <- function(fit, spec, conf.level = 0.95,
                      alternative = c("two.sided", "greater", "less"),
                      ci_method = "mls") {
  alternative <- match.arg(alternative)
  alpha <- 1 - conf.level
  a <- if (alternative == "two.sided") alpha / 2 else alpha
  if (ci_method == "exact") {
    ci_method <- "satterthwaite"
  }

  if (spec$stat == "ICC") {
    lim <- .facet_icc_limits(fit, spec, a, ci_method)
    bounds <- c(-Inf, 1)
  } else if (spec$scale != "mean") {
    lim <- sqrt(.facet_var_limits(fit, spec, a, ci_method))
    if (spec$scale == "log") lim <- 100 * lim
    bounds <- c(0, Inf)
  } else {
    est <- .facet_estimate(fit, spec)
    se <- .facet_se_normal(fit, spec)
    lim <- est * exp(c(-1, 1) * stats::qnorm(1 - a) * se)
    bounds <- c(0, Inf)
  }
  ci <- switch(alternative,
               two.sided = lim,
               greater = c(lim[1], bounds[2]),
               less = c(bounds[1], lim[2]))
  attr(ci, "conf.level") <- conf.level
  ci
}

#' Test of H0: estimand = null.value (on the reported scale)
#'
#' Exact and Satterthwaite: F test (coefficients) or chi-square test (SEM).
#' MLS: p-value from inverting the MLS interval (no statistic). SEMs with
#' `scale = "mean"`: z test on the log scale.
#' @noRd
.facet_test <- function(fit, spec, null.value,
                        alternative = c("two.sided", "greater", "less"),
                        ci_method = "mls") {
  alternative <- match.arg(alternative)
  ms <- fit$ms
  dfs <- fit$df
  if (ci_method == "exact") {
    ci_method <- "satterthwaite"
  }
  combine <- function(p_greater, p_less) {
    switch(alternative,
           greater = p_greater,
           less = p_less,
           two.sided = min(1, 2 * min(p_greater, p_less)))
  }

  if (spec$stat == "SEM" && spec$scale == "mean") {
    est <- .facet_estimate(fit, spec)
    z <- (log(est) - log(null.value)) / .facet_se_normal(fit, spec)
    return(list(statistic = c(z = z), parameter = NULL,
                p.value = .p_from_dist(z, alternative, cdf = stats::pnorm)))
  }

  if (spec$stat == "ICC") {
    cf <- .facet_ms_coef(fit, spec)
    d <- cf$a - null.value * cf$t
    if (ci_method == "satterthwaite") {
      st <- .satterthwaite_f(d, ms, dfs)
      return(list(statistic = c(F = st$statistic),
                  parameter = c(df1 = st$df1, df2 = st$df2),
                  p.value = combine(st$p_greater, st$p_less)))
    }
    p <- .mls_pvalues(d, ms, dfs, 0)
    return(list(statistic = NULL, parameter = NULL,
                p.value = combine(p[["greater"]], p[["less"]])))
  }

  # SEM, raw or log scale: H0 on the error variance
  null_var <- (if (spec$scale == "log") null.value / 100 else null.value)^2
  cf <- .facet_ms_coef(fit, spec)$var
  if (ci_method == "satterthwaite") {
    v <- max(sum(cf * ms), 0)
    return(.var_test(v, .facet_sem_df(fit, spec), null_var, alternative))
  }
  p <- .mls_pvalues(cf, ms, dfs, null_var)
  list(statistic = NULL, parameter = NULL,
       p.value = combine(p[["greater"]], p[["less"]]))
}

#' One-sided p-values for H0: sum(coef * E(MS)) = target from the MLS bounds
#'
#' "greater": the tail probability a at which the lower bound equals
#' `target`; "less": at which the upper bound does. The MLS bounds are
#' defined for a <= 0.5, so a p-value above 0.5 is taken as one minus the
#' opposite one-sided p-value.
#' @noRd
.mls_pvalues <- function(coef, ms, dfs, target) {
  solve_a <- function(which) {
    f <- function(a) {
      b <- .mls_bounds(coef, ms, dfs, a)
      if (which == 1) b[1] - target else target - b[2]
    }
    lo <- 1e-12
    hi <- 0.5
    f_lo <- f(lo)
    f_hi <- f(hi)
    if (f_lo >= 0) return(0)
    if (f_hi <= 0) return(NA_real_)
    stats::uniroot(f, c(lo, hi), f.lower = f_lo, f.upper = f_hi,
                   tol = 1e-12)$root
  }
  p <- c(greater = solve_a(1), less = solve_a(2))
  if (all(is.na(p))) {
    # target inside both median bounds: no evidence either way
    return(c(greater = 0.5, less = 0.5))
  }
  p[is.na(p)] <- 1 - p[!is.na(p)]
  p
}

# Standard errors -------------------------------------------------------------

#' Normal-theory variance of the grand mean
#'
#' E(MS_p) / (n c_p) collects every effect with subjects and the repeats;
#' each effect without subjects that is generalised over (random items or
#' occasions counted as error) adds sigma2_T / (its observed level
#' combinations), with negative estimates set to 0.
#' @noRd
.facet_grand_mean_var <- function(fit, spec) {
  E <- .facet_ems(fit)
  v <- fit$ms[["p"]] / (fit$n * E["p", "p"])
  for (t in .facet_nonsubject(fit, spec)) {
    n_t <- if (fit$balanced) prod(fit$levels[.facet_letters(t)]) else
      fit$n_T[[t]]
    v <- v + max(fit$vc[[t]], 0) / n_t
  }
  v
}

#' Normal-theory (delta method) SE on the working scale
#' @noRd
.facet_se_normal <- function(fit, spec) {
  g_ms <- function(ms) {
    f <- fit
    f$ms[] <- ms
    .facet_z_fit(f, spec)
  }
  se <- .delta_se(g_ms, fit$ms, 2 * fit$ms^2 / fit$df)
  if (spec$stat == "SEM" && spec$scale == "mean") {
    se <- sqrt(se^2 + .facet_grand_mean_var(fit, spec) / fit$grand_mean^2)
  }
  se
}

#' Standard error on the working scale
#'
#' `"normal"`: delta method. `"jackknife"`: delete one subject at a time
#' (ANOVA mean squares recomputed, REML refitted). Mean squares of effects
#' without subjects that enter the estimand are held at their full-data
#' values in the jackknife, and their normal-theory variance,
#' 2 MS^2 / df, is added (as `.subject_se()` does for MSJ).
#' @noRd
.facet_se_z <- function(prep, fit, spec,
                        se_method = c("jackknife", "normal")) {
  se_method <- match.arg(se_method)
  if (se_method == "normal") {
    return(.facet_se_normal(fit, spec))
  }
  g <- function(f) .facet_z_fit(f, spec)
  held <- .facet_nonsubject(fit, spec)
  var_g <- .facet_jackknife_var(prep, fit, g, hold = held)
  if (length(held) > 0) {
    for (t in held) {
      g_t <- .num_grad(function(m) {
        f <- fit
        f$ms[[t]] <- m[[1]]
        g(f)
      }, stats::setNames(fit$ms[[t]], t))
      var_g <- var_g + g_t^2 * 2 * fit$ms[[t]]^2 / fit$df[[t]]
    }
    g_m <- .num_grad(function(m) {
      f <- fit
      f$grand_mean <- m[[1]]
      g(f)
    }, c(mean = fit$grand_mean))
    var_g <- var_g + g_m^2 * (.facet_grand_mean_var(fit, spec) -
                                fit$ms[["p"]] / (fit$n * .facet_ems(fit)["p", "p"]))
  }
  sqrt(var_g)
}

#' Jackknife variance of g(fit), deleting one subject at a time
#' @noRd
.facet_jackknife_var <- function(prep, fit, g, hold = character(0)) {
  n <- prep$n
  if (fit$method == "anova") {
    cells <- .facet_cells(prep)
    A <- cells$A
    nd <- length(dim(A))
    ssw <- sum(cells$ssw_i)
    z_i <- vapply(seq_len(n), function(i) {
      Ai <- if (nd == 2) A[-i, , drop = FALSE] else A[-i, , , drop = FALSE]
      am <- .facet_anova_ms(Ai, fit$r, ssw - cells$ssw_i[i], fit$effects)
      f <- fit
      f$n <- n - 1
      f$levels[["p"]] <- n - 1
      f$ms <- am$ms
      f$df <- am$df
      f$grand_mean <- mean(Ai)
      f <- .facet_refresh(f)
      if (length(hold) > 0) f$ms[hold] <- fit$ms[hold]
      g(f)
    }, numeric(1))
  } else {
    ids <- levels(prep$df$id)
    z_i <- vapply(ids, function(i) {
      tryCatch({
        p <- .facet_prep_from_long(prep$df[prep$df$id != i, , drop = FALSE])
        f <- .facet_fit(p, method = fit$method, keep_fit = FALSE)
        if (length(hold) > 0) f$ms[hold] <- fit$ms[hold]
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

# Bootstrap ---------------------------------------------------------------------

#' Hybrid bootstrap step for effects without subjects
#'
#' Resampling subjects keeps the same items and occasions, so it leaves out
#' the sampling variation of the mean squares of effects without subjects.
#' Each such mean square that enters the estimand is multiplied by an
#' independent chi-square(df) / df draw and, for statistics relative to the
#' grand mean, the mean is shifted by a normal draw with those effects'
#' share of its variance.
#' @noRd
.facet_hybrid <- function(fit, spec, shift_mean = FALSE) {
  held <- .facet_nonsubject(fit, spec)
  if (shift_mean) {
    v <- .facet_grand_mean_var(fit, spec) -
      fit$ms[["p"]] / (fit$n * .facet_ems(fit)["p", "p"])
    fit$grand_mean <- fit$grand_mean + stats::rnorm(1, 0, sqrt(v))
  }
  for (t in held) {
    df_t <- fit$df[[t]]
    fit$ms[[t]] <- fit$ms[[t]] * stats::rchisq(1, df_t) / df_t
  }
  fit
}

# Variance components table -----------------------------------------------------

.facet_effect_label <- function(eff, design, replicated) {
  base <- c(p = "subjects", i = "items", o = "occasions")
  if (eff == "e") {
    return("repeats (within cells)")
  }
  L <- strsplit(eff, "")[[1]]
  label <- paste(base[L], collapse = " x ")
  if (!replicated && length(L) == length(c("p", "i", if (design == "crossed") "o"))) {
    label <- paste0(label, " (residual)")
  }
  label
}

#' Variance components with intervals and shares of the total
#'
#' Estimates are unconstrained ANOVA estimators (or REML estimates).
#' Intervals: MLS (Graybill & Wang, 1980; Ting et al., 1990), which is the
#' exact chi-square interval for a component that is one mean square, with
#' negative limits set to 0. Shares are of the total variance of a single
#' measurement, with negative estimates counted as 0.
#' @noRd
.facet_components <- function(fit, conf.level = 0.95) {
  C <- fit$comp_coef
  a <- (1 - conf.level) / 2
  est <- drop(C %*% fit$ms)
  ci <- t(vapply(rownames(C), function(nm) {
    pmax(.mls_bounds(C[nm, ], fit$ms, fit$df, a), 0)
  }, numeric(2)))
  share <- pmax(est, 0) / sum(pmax(est, 0))
  out <- data.frame(
    component = vapply(fit$effects, .facet_effect_label, "",
                       design = fit$design, replicated = fit$replicated),
    variance = unname(est),
    lower = unname(ci[, 1]),
    upper = unname(ci[, 2]),
    sd = sqrt(pmax(unname(est), 0)),
    percent = 100 * unname(share))
  rownames(out) <- NULL
  attr(out, "conf.level") <- conf.level
  out
}
