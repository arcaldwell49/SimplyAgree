#' @title Model-Based Kappa for Ordinal Ratings from Many Raters
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Estimates the model-based kappa \eqn{\kappa_m} of Nelson and Edwards
#' (2015) and Nelson, Mitani and Edwards (2017), and please cite these papers if this function is utilized.
#' This is a chance-corrected measure of
#' exact agreement between raters who classify subjects on an ordinal scale.
#' It is computed from an ordinal (cumulative probit) generalized linear mixed
#' model with crossed random effects for subjects and raters, so it describes
#' the populations of raters and subjects that were sampled, and it is not
#' affected by the prevalence of the categories.
#'
#' Requires the \pkg{ordinal} package.
#'
#' @inheritParams cac_test
#' @param x A formula of the form `rating ~ rater | id`. Leave `NULL` to use
#'   `measure`, `item` and `id` (long data) or `col.names` (wide data).
#' @param categories All categories of the ordinal scale, in order. Defaults to
#'   the levels of a factor, and otherwise to the sorted observed ratings.
#'   Character ratings need `categories` to set their order. The number of
#'   categories, C, enters \eqn{\kappa_m} through the chance agreement 1/C.
#' @param null.value The hypothesized value of \eqn{\kappa_m}, in \[0, 1).
#'   `NULL` (default) gives no test. Not used with `group`.
#' @param group Optional name of a column with a two-level rater or subject
#'   characteristic (e.g. experienced or inexperienced raters, younger or
#'   older patients), constant within each rater (or subject). The two groups
#'   are then compared; see Details. The order of the levels is that of a
#'   factor, and otherwise sorted. Requires long data.
#' @param group_type Whether `group` describes the `"rater"`s (default) or
#'   the `"subject"`s.
#'
#' @details
#' **Model.** The rating of subject i by rater j falls in category c when a
#' latent variable falls between thresholds \eqn{\alpha_{c-1}} and
#' \eqn{\alpha_c}:
#' \deqn{P(Y_{ij} \le c \mid u_i, v_j) = \Phi(\alpha_c - u_i - v_j),}
#' with subject effects \eqn{u_i \sim N(0, \sigma^2_u)} and rater effects
#' \eqn{v_j \sim N(0, \sigma^2_v)}. It is fitted by the Laplace
#' approximation with [ordinal::clmm()].
#'
#' **Kappa.** Observed agreement \eqn{p_0} is the probability that two raters
#' put the same subject in the same category. Chance agreement is taken at
#' its minimum, \eqn{p_c = 1/C}, reached when the thresholds split the
#' latent scale into C equally likely categories, and \eqn{p_0} is evaluated
#' at those thresholds. Then
#' \deqn{\kappa_m = \frac{p_0 - 1/C}{1 - 1/C},}
#' which depends only on C and
#' \eqn{\rho = \sigma^2_u / (\sigma^2_u + \sigma^2_v + 1)}, the share of the
#' latent variance due to differences between subjects. \eqn{\kappa_m} is 0
#' when \eqn{\rho = 0} and approaches 1 as \eqn{\rho} approaches 1. Because
#' the thresholds drop out, \eqn{\kappa_m} does not depend on how common each
#' category is. The observed agreement at the fitted thresholds is also
#' returned (`fit$p0`).
#'
#' **Inference.** The standard error is from the delta method with the
#' covariance matrix of the variance components from the model Hessian
#' (Nelson et al., 2017). The interval and test are formed on the logit of
#' \eqn{\rho} and mapped back to \eqn{\kappa_m}, so the interval stays within
#' \[0, 1\]; Nelson et al. (2017) used the Wald interval
#' \eqn{\hat\kappa_m \pm 1.96\, se}, which can be found from `estimate` and
#' `stderr`. The p-value is from a z test of \eqn{\rho = \rho_0}, where
#' \eqn{\rho_0} gives the `null.value` of \eqn{\kappa_m}.
#'
#' **Design.** Raters and subjects are treated as random samples from their
#' populations. Missing ratings are allowed. At least three raters are
#' needed to estimate the rater variance (Nelson et al., 2017); with few
#' raters or subjects the variance components, and so the interval, are
#' imprecise.
#'
#' **Comparing groups.** With `group`, the group enters the model as a fixed
#' effect (shifting the thresholds, e.g. for a higher prevalence of severe
#' ratings in older patients), and the rater (or subject) variance may differ
#' between the two groups (Nelson et al., 2017). The result gives
#' \eqn{\kappa_m} within each group, with an interval for their difference,
#' and the test is the likelihood ratio test of a common variance. For rater
#' groups, `fit$subgroups` also gives \eqn{\kappa_m} between a rater from
#' each group; chance agreement is then minimised numerically over common
#' thresholds, and the fixed effect matters. Subgroup intervals are formed
#' on the logit scale.
#'
#' Nelson et al. (2017) add a random slope for the group indicator and test
#' its variance and correlation against a 50:50 mixture of chi-square
#' distributions with 1 and 2 df. Because the group is constant within each
#' rater (or subject), only the variance in each group is identified, not
#' the intercept-slope covariance, and the Hessian of that model is singular.
#' The equivalent model with a separate variance for each group is fitted
#' instead: it has the same likelihood and the same kappas, with one
#' parameter fewer. Equal variances lie inside its parameter space, so the
#' likelihood ratio statistic is compared with a chi-square distribution
#' with 1 df.
#'
#' @return A list of class `c("cac_htest", "htest")` containing:
#'   \item{statistic}{The z statistic (only with a `null.value`), or with
#'   `group`, the likelihood ratio statistic.}
#'   \item{p.value}{The p-value (only with a `null.value` or `group`).}
#'   \item{conf.int}{The confidence interval for \eqn{\kappa_m}, or with
#'   `group`, for the difference between the groups (second minus first).}
#'   \item{estimate}{The estimated \eqn{\kappa_m}, or with `group`, one per
#'   group.}
#'   \item{stderr}{The delta-method standard error(s).}
#'   \item{null.value}{The hypothesized value.}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{A description of the method.}
#'   \item{data.name}{A description of the data.}
#'   \item{fit}{The fitted model (`model`), the variance components
#'   (`sigma2_subject`, `sigma2_rater`), `rho` and its standard error, the
#'   thresholds, the observed agreement at the fitted thresholds (`p0`), the
#'   number of categories `C`, the rater conditional modes and their
#'   conditional variances (`rater_effects`), and the numbers of subjects and
#'   raters. With `group`, `fit` also has the null model (`null_model`), the
#'   likelihood ratio test (`lrt`), the fixed effect of the group (`beta`),
#'   one variance per group, and a table of subgroup kappas with standard
#'   errors and intervals (`subgroups`).}
#'   \item{data}{The ratings in long format.}
#'   \item{settings}{The settings used, for the methods.}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ordinal", quietly = TRUE)) {
#'   # Simulated ratings of 60 subjects by 8 raters on a 5-point scale
#'   set.seed(42)
#'   d <- expand.grid(id = 1:60, rater = paste0("R", 1:8))
#'   latent <- rnorm(60, sd = 1.5)[d$id] + rnorm(8, sd = 0.5)[d$rater] +
#'     rnorm(nrow(d))
#'   d$rating <- cut(latent, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE)
#'   res <- ord_kappa_test(rating ~ rater | id, data = d)
#'   res
#'   summary(res)
#'   plot(res, type = "raters")
#'
#'   # Do the first 4 raters agree better than the other 4?
#'   d$exp <- ifelse(d$rater %in% c("R1", "R2", "R3", "R4"), "exp", "inexp")
#'   res_g <- ord_kappa_test(rating ~ rater | id, data = d, group = "exp")
#'   res_g
#'   res_g$fit$subgroups
#' }
#' }
#'
#' @references
#' Nelson, K. P., & Edwards, D. (2015). Measures of agreement between many
#' raters for ordinal classifications. Statistics in Medicine, 34(23),
#' 3116-3132. \doi{10.1002/sim.6546}
#'
#' Nelson, K. P., Mitani, A. A., & Edwards, D. (2017). Assessing the influence
#' of rater and subject characteristics on measures of agreement for ordinal
#' ratings. Statistics in Medicine, 36(20), 3181-3199. \doi{10.1002/sim.7323}
#'
#' @seealso [cac_htest-methods] for `summary()` and `plot()`; [cac_test()] for
#'   Gwet's AC1/AC2, Fleiss' kappa and Krippendorff's alpha.
#' @export
ord_kappa_test <- function(x = NULL,
                           data = NULL,
                           measure = NULL,
                           item = NULL,
                           id = NULL,
                           col.names = NULL,
                           categories = NULL,
                           conf.level = 0.95,
                           null.value = NULL,
                           alternative = c("two.sided", "greater", "less"),
                           group = NULL,
                           group_type = c("rater", "subject")) {
  if (!requireNamespace("ordinal", quietly = TRUE)) {
    stop("ord_kappa_test() requires the 'ordinal' package: ",
         "install.packages(\"ordinal\")", call. = FALSE)
  }
  alternative <- match.arg(alternative)
  group_type <- match.arg(group_type)
  .check_conf_level(conf.level)
  if (!is.null(group)) {
    if (!is.character(group) || length(group) != 1) {
      stop("'group' must be a single column name.", call. = FALSE)
    }
    if (!is.null(null.value)) {
      stop("With 'group', the test is the likelihood ratio test of no ",
           "difference between the groups; leave 'null.value' as NULL.",
           call. = FALSE)
    }
    if (!is.null(col.names)) {
      stop("'group' requires long data (a formula, or 'measure', 'item' ",
           "and 'id').", call. = FALSE)
    }
  }
  if (is.null(null.value)) {
    alternative <- "two.sided"
  } else if (!is.numeric(null.value) || length(null.value) != 1 ||
             is.na(null.value) || null.value < 0 || null.value >= 1) {
    stop("'null.value' must be a single number in [0, 1).", call. = FALSE)
  }

  prep <- .reli_prepare(x = x, data = data, measure = measure, item = item,
                        id = id, col.names = col.names,
                        allow_categorical = TRUE)
  df <- prep$df
  categories <- .ordk_categories(df$values, categories)
  C <- length(categories)
  df$values <- factor(as.character(df$values),
                      levels = as.character(categories), ordered = TRUE)
  if (nlevels(droplevels(df$values)) < 2) {
    stop("At least 2 categories must be used.", call. = FALSE)
  }
  if (prep$k < 3) {
    warning("Fewer than 3 raters: the rater variance, and so kappa_m, ",
            "cannot be estimated reliably (Nelson et al., 2017).",
            call. = FALSE)
  }

  if (!is.null(group)) {
    cols <- if (!is.null(x)) .reli_parse_formula(x) else
      list(item = item, id = id)
    key_col <- if (group_type == "rater") cols$item else cols$id
    return(.ord_kappa_group_test(df, prep, data, group, key_col, group_type,
                                 categories, conf.level))
  }

  fit <- .ordk_fit(df)
  rho <- fit$rho
  if (!is.finite(fit$se_rho) || fit$se_rho <= 0) {
    stop("The standard error of the variance components could not be ",
         "estimated (a variance may be estimated at zero).", call. = FALSE)
  }
  estimate <- .kappa_m(rho, C)
  dk_drho <- .num_grad(function(r) .kappa_m(r[[1]], C), c(rho = rho))
  stderr <- abs(dk_drho) * fit$se_rho

  # inference on logit(rho); kappa_m increases with rho
  se_l <- fit$se_rho / (rho * (1 - rho))
  l_hat <- stats::qlogis(rho)
  alpha <- 1 - conf.level
  l_ci <- switch(alternative,
                 two.sided = l_hat + c(-1, 1) * stats::qnorm(1 - alpha / 2) * se_l,
                 greater = c(l_hat - stats::qnorm(1 - alpha) * se_l, Inf),
                 less = c(-Inf, l_hat + stats::qnorm(1 - alpha) * se_l))
  conf.int <- vapply(stats::plogis(l_ci), .kappa_m, numeric(1), C = C)
  attr(conf.int, "conf.level") <- conf.level

  statistic <- p.value <- NULL
  if (!is.null(null.value)) {
    rho0 <- .kappa_m_inverse(null.value, C)
    z <- (l_hat - stats::qlogis(rho0)) / se_l
    statistic <- c(z = z)
    p.value <- switch(alternative,
                      two.sided = 2 * stats::pnorm(-abs(z)),
                      greater = stats::pnorm(z, lower.tail = FALSE),
                      less = stats::pnorm(z))
  }

  label <- "kappa_m"
  fit$C <- C
  fit$categories <- categories
  fit$p0 <- .ordk_p0(fit$thresholds, fit$sigma2_subject, fit$sigma2_rater)
  fit$n <- prep$n
  fit$k <- prep$k
  fit$N <- prep$N
  fit$n_missing <- prep$n_missing

  structure(list(statistic = statistic,
                 parameter = NULL,
                 p.value = p.value,
                 conf.int = conf.int,
                 estimate = stats::setNames(estimate, label),
                 stderr = stderr,
                 null.value = if (is.null(null.value)) NULL else
                   stats::setNames(null.value, label),
                 alternative = alternative,
                 method = paste0("Model-based kappa (ordinal probit GLMM) for ",
                                 prep$k, " raters and ", C, " categories"),
                 data.name = prep$data.name,
                 fit = fit,
                 data = df,
                 settings = list(statistic = "kappa_m",
                                 conf.level = conf.level,
                                 label = label)),
            class = c("cac_htest", "htest"))
}

# Helpers --------------------------------------------------------------------

.ordk_categories <- function(values, categories) {
  if (!is.null(categories)) {
    if (anyNA(categories) || anyDuplicated(categories)) {
      stop("'categories' must not contain missing or duplicated values.",
           call. = FALSE)
    }
  } else if (is.factor(values)) {
    categories <- levels(values)
  } else if (is.numeric(values)) {
    categories <- sort(unique(values))
  } else {
    stop("Give the order of character ratings in 'categories', or use an ",
         "ordered factor.", call. = FALSE)
  }
  absent <- setdiff(as.character(unique(values)), as.character(categories))
  if (length(absent) > 0) {
    stop("Ratings not listed in 'categories': ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  if (length(categories) < 2) {
    stop("At least 2 categories are required.", call. = FALSE)
  }
  categories
}

#' Fit the cumulative probit GLMM with crossed subject and rater effects
#' @noRd
.ordk_fit <- function(df) {
  df$values <- droplevels(df$values)
  model <- ordinal::clmm(values ~ 1 + (1 | id) + (1 | items), data = df,
                         link = "probit", Hess = TRUE)
  if (model$optRes$convergence != 0) {
    warning("The ordinal mixed model may not have converged.", call. = FALSE)
  }
  sd_u <- unname(model$ST$id[1, 1])
  sd_v <- unname(model$ST$items[1, 1])
  # vcov() ends with the random-effect standard deviations, in the order of
  # the random terms
  V <- stats::vcov(model)
  st <- utils::tail(seq_len(nrow(V)), length(model$ST))
  V_sd <- V[st, st, drop = FALSE]
  dimnames(V_sd) <- list(names(model$ST), names(model$ST))
  V_sd <- V_sd[c("id", "items"), c("id", "items")]

  total <- sd_u^2 + sd_v^2 + 1
  rho <- sd_u^2 / total
  g <- c(2 * sd_u * (sd_v^2 + 1), -2 * sd_v * sd_u^2) / total^2
  var_rho <- drop(t(g) %*% V_sd %*% g)

  re <- ordinal::ranef(model, condVar = TRUE)
  rater_effects <- data.frame(rater = rownames(re$items),
                              mode = re$items[, 1],
                              cond_var = attr(re$items, "condVar")[, 1])
  list(model = model,
       sigma2_subject = sd_u^2,
       sigma2_rater = sd_v^2,
       vcov_sd = V_sd,
       rho = rho,
       se_rho = if (is.finite(var_rho) && var_rho > 0) sqrt(var_rho) else NA,
       thresholds = model$alpha,
       rater_effects = rater_effects,
       converged = model$optRes$convergence == 0)
}

#' Probability that two raters agree on a subject, given the thresholds on
#' the latent scale standardised by the total SD
#' @noRd
.ordk_agreement <- function(q, rho) {
  q <- c(-Inf, q, Inf)
  f <- function(z) {
    vapply(z, function(zz) {
      p <- diff(stats::pnorm((q - zz * sqrt(rho)) / sqrt(1 - rho)))
      sum(p^2)
    }, numeric(1)) * stats::dnorm(z)
  }
  stats::integrate(f, -Inf, Inf, rel.tol = 1e-10)$value
}

#' Model-based kappa (Nelson & Edwards, 2015) from rho and the number of
#' categories, with the thresholds that minimise chance agreement
#' @noRd
.kappa_m <- function(rho, C) {
  if (rho <= 0) return(0)
  if (rho >= 1) return(1)
  q <- stats::qnorm(seq_len(C - 1) / C)
  (.ordk_agreement(q, rho) - 1 / C) / (1 - 1 / C)
}

.kappa_m_inverse <- function(kappa, C) {
  if (kappa <= 0) return(0)
  stats::uniroot(function(r) .kappa_m(r, C) - kappa, c(1e-10, 1 - 1e-10),
                 tol = 1e-12)$root
}

#' Observed agreement at the fitted thresholds
#' @noRd
.ordk_p0 <- function(thresholds, s2u, s2v) {
  total <- s2u + s2v + 1
  .ordk_agreement(unname(thresholds) / sqrt(total), s2u / total)
}

# Rater or subject groups (Nelson, Mitani & Edwards, 2017) ---------------------

#' Indicator (0/1) of the second group level for each rating
#' @noRd
.ordk_group_indicator <- function(data, df, group, key_col, group_type) {
  .reli_check_columns(group, data)
  g <- data[[group]]
  key <- as.character(data[[key_col]])
  if (anyNA(g)) {
    stop("Missing values found in the 'group' column.", call. = FALSE)
  }
  levs <- if (is.factor(g)) levels(droplevels(g)) else
    as.character(sort(unique(g)))
  if (length(levs) != 2) {
    stop("'group' must have exactly 2 levels.", call. = FALSE)
  }
  g <- as.character(g)
  n_levels <- tapply(g, key, function(v) length(unique(v)))
  if (any(n_levels > 1)) {
    unit <- if (group_type == "rater") "rater" else "subject"
    stop("'group' must be constant within each ", unit, ".", call. = FALSE)
  }
  map <- stats::setNames(g, key)[!duplicated(key)]
  unit_col <- if (group_type == "rater") "items" else "id"
  list(x = as.integer(map[as.character(df[[unit_col]])] == levs[2]),
       levels = levs,
       counts = table(factor(map, levels = levs)))
}

#' Covariance matrices of the random effects from the clmm parameters
#'
#' Each term's parameters are the diagonal and then the lower triangle of a
#' factor L with covariance L L'.
#' @noRd
.ordk_cov_from_par <- function(st_par, ST) {
  out <- list()
  i <- 0
  for (nm in names(ST)) {
    d <- ncol(ST[[nm]])
    L <- matrix(0, d, d)
    diag(L) <- st_par[i + seq_len(d)]
    i <- i + d
    if (d > 1) {
      lo <- lower.tri(L)
      L[lo] <- st_par[i + seq_len(sum(lo))]
      i <- i + sum(lo)
    }
    out[[nm]] <- L %*% t(L)
  }
  out
}

#' Chance agreement minimised over common thresholds for two raters whose
#' latent variables have means k1, k2 and SDs s1, s2 (Nelson et al., 2017)
#' @noRd
.ordk_pc_min <- function(C, k1, k2, s1, s2) {
  probs <- function(a, k, s) diff(stats::pnorm(c(-Inf, (a - k) / s, Inf)))
  pc <- function(a) sum(probs(a, k1, s1) * probs(a, k2, s2))
  grad <- function(a) {
    g1 <- probs(a, k1, s1)
    g2 <- probs(a, k2, s2)
    m <- seq_len(C - 1)
    stats::dnorm((a - k1) / s1) / s1 * (g2[m] - g2[m + 1]) +
      stats::dnorm((a - k2) / s2) / s2 * (g1[m] - g1[m + 1])
  }
  start <- stats::qnorm(seq_len(C - 1) / C) * (s1 + s2) / 2 + (k1 + k2) / 2
  o <- stats::optim(start, pc, grad, method = "BFGS",
                    control = list(reltol = 1e-15, maxit = 1000))
  list(thresholds = o$par, pc = o$value)
}

#' Model-based kappa between two raters from different groups: shared
#' subject variance s2u, rater variances s2v1 and s2v2, latent means k1, k2
#' @noRd
.kappa_m_between <- function(C, s2u, s2v1, s2v2, k1 = 0, k2 = 0) {
  s1 <- sqrt(s2u + s2v1 + 1)
  s2 <- sqrt(s2u + s2v2 + 1)
  m <- .ordk_pc_min(C, k1, k2, s1, s2)
  r1 <- s2u / s1^2
  r2 <- s2u / s2^2
  q1 <- c(-Inf, (m$thresholds - k1) / s1, Inf)
  q2 <- c(-Inf, (m$thresholds - k2) / s2, Inf)
  f <- function(z) {
    vapply(z, function(zz) {
      sum(diff(stats::pnorm((q1 - zz * sqrt(r1)) / sqrt(1 - r1))) *
            diff(stats::pnorm((q2 - zz * sqrt(r2)) / sqrt(1 - r2))))
    }, numeric(1)) * stats::dnorm(z)
  }
  p0 <- stats::integrate(f, -Inf, Inf, rel.tol = 1e-10)$value
  (p0 - m$pc) / (1 - m$pc)
}

#' Group variances from the random-effect parameters. The grouped effect has
#' one random intercept per group (terms `_a` and `_b`); the other effect has
#' a single variance.
#' @noRd
.ordk_group_vars <- function(st_par, ST, group_type) {
  S <- vapply(.ordk_cov_from_par(st_par, ST), function(m) m[1, 1], numeric(1))
  if (group_type == "rater") {
    list(subject = S[["id"]], rater = c(S[["items_a"]], S[["items_b"]]))
  } else {
    list(subject = c(S[["id_a"]], S[["id_b"]]), rater = S[["items"]])
  }
}

#' Subgroup kappas from the fixed effect of the group (beta) and the
#' random-effect parameters: within each group, and (rater groups) between
#' a rater from each group
#' @noRd
.ordk_group_kappas <- function(beta, st_par, ST, C, group_type) {
  v <- .ordk_group_vars(st_par, ST, group_type)
  s2u <- v$subject
  s2v <- v$rater
  if (group_type == "rater") {
    c(.kappa_m(s2u / (s2u + s2v[1] + 1), C),
      .kappa_m(s2u / (s2u + s2v[2] + 1), C),
      .kappa_m_between(C, s2u, s2v[1], s2v[2], 0, beta))
  } else {
    c(.kappa_m(s2u[1] / (s2u[1] + s2v + 1), C),
      .kappa_m(s2u[2] / (s2u[2] + s2v + 1), C))
  }
}

#' Fit the group model and its null model, and compute the subgroup kappas
#'
#' Nelson et al. (2017) add a random slope for the group indicator. When the
#' group is constant within each rater (or subject), only the variance of the
#' effect in each group is identified, not the intercept-slope covariance, so
#' the equivalent model with a separate random-intercept variance for each
#' group is fitted (same likelihood, one parameter fewer).
#' @noRd
.ordk_group_fit <- function(df, C, group_type, conf.level) {
  df$values <- droplevels(df$values)
  df$g0 <- 1 - df$x
  df$g1 <- df$x
  if (group_type == "rater") {
    df$items_a <- df$items_b <- df$items
    f_full <- values ~ x + (1 | id) + (0 + g0 | items_a) + (0 + g1 | items_b)
  } else {
    df$id_a <- df$id_b <- df$id
    f_full <- values ~ x + (0 + g0 | id_a) + (0 + g1 | id_b) + (1 | items)
  }
  model <- ordinal::clmm(f_full, data = df, link = "probit", Hess = TRUE)
  null_model <- ordinal::clmm(values ~ x + (1 | id) + (1 | items), data = df,
                              link = "probit")
  if (model$optRes$convergence != 0 || null_model$optRes$convergence != 0) {
    warning("The ordinal mixed model may not have converged.", call. = FALSE)
  }

  # likelihood ratio test of equal variances in the two groups (1 df)
  lrt <- max(0, 2 * (as.numeric(stats::logLik(model)) -
                       as.numeric(stats::logLik(null_model))))
  p_lrt <- stats::pchisq(lrt, 1, lower.tail = FALSE)

  # optRes$par holds the thresholds, the fixed effect, then the random-effect
  # SDs in the order of names(model$ST)
  par <- model$optRes$par
  n_theta <- length(model$alpha)
  i_beta <- n_theta + 1
  i_st <- seq.int(n_theta + 2, length(par))
  ST <- model$ST
  theta <- c(unname(par[i_beta]), unname(par[i_st]))
  kfun <- function(th) {
    .ordk_group_kappas(th[[1]], th[-1], ST, C, group_type)
  }
  kappas <- kfun(theta)

  V <- tryCatch(stats::vcov(model), error = function(e) NULL)
  if (!is.null(V) && nrow(V) == length(par)) {
    V <- V[c(i_beta, i_st), c(i_beta, i_st)]
    J <- vapply(seq_along(theta), function(i) {
      h <- max(abs(theta[[i]]), 1e-3) * 1e-4
      up <- dn <- theta
      up[[i]] <- theta[[i]] + h
      dn[[i]] <- theta[[i]] - h
      (kfun(up) - kfun(dn)) / (2 * h)
    }, numeric(length(kappas)))
    J <- matrix(J, nrow = length(kappas))
    Vk <- J %*% V %*% t(J)
    se <- sqrt(pmax(diag(Vk), 0))
    se_diff <- sqrt(max(Vk[1, 1] + Vk[2, 2] - 2 * Vk[1, 2], 0))
    se_beta <- sqrt(V[1, 1])
  } else {
    warning("The Hessian of the ordinal mixed model is not positive ",
            "definite (a variance may be estimated at zero), so standard ",
            "errors and intervals are not available. The likelihood ratio ",
            "test is still valid.", call. = FALSE)
    se <- rep(NA_real_, length(kappas))
    se_diff <- se_beta <- NA_real_
  }

  # subgroup intervals on the logit scale, so they stay within [0, 1]
  zc <- stats::qnorm(1 - (1 - conf.level) / 2)
  se_logit <- se / (kappas * (1 - kappas))
  sub <- data.frame(kappa_m = kappas, se = se,
                    lower = stats::plogis(stats::qlogis(kappas) - zc * se_logit),
                    upper = stats::plogis(stats::qlogis(kappas) + zc * se_logit))

  # rater conditional modes: from the term of the rater's own group
  re <- ordinal::ranef(model, condVar = TRUE)
  if (group_type == "rater") {
    x_r <- tapply(df$x, df$items, function(v) v[1])
    raters <- names(x_r)
    pick <- function(term, col) {
      r <- re[[term]]
      stats::setNames(if (col == "mode") r[, 1] else attr(r, "condVar")[, 1],
                      rownames(r))[raters]
    }
    mode <- ifelse(x_r == 1, pick("items_b", "mode"), pick("items_a", "mode"))
    cv <- ifelse(x_r == 1, pick("items_b", "cv"), pick("items_a", "cv"))
  } else {
    x_r <- NULL
    raters <- rownames(re$items)
    mode <- re$items[, 1]
    cv <- attr(re$items, "condVar")[, 1]
  }
  rater_effects <- data.frame(rater = raters, mode = unname(mode),
                              cond_var = unname(cv))

  vars <- .ordk_group_vars(par[i_st], ST, group_type)
  list(model = model,
       null_model = null_model,
       lrt = c(statistic = lrt, p.value = p_lrt),
       beta = c(estimate = unname(par[i_beta]), se = se_beta),
       sigma2_subject = vars$subject,
       sigma2_rater = vars$rater,
       subgroups = sub,
       difference = c(estimate = kappas[2] - kappas[1], se = se_diff),
       thresholds = model$alpha,
       rater_effects = rater_effects,
       rater_x = x_r,
       converged = model$optRes$convergence == 0)
}

#' ord_kappa_test() with a rater or subject group: the htest
#' @noRd
.ord_kappa_group_test <- function(df, prep, data, group, key_col, group_type,
                                  categories, conf.level) {
  gi <- .ordk_group_indicator(data, df, group, key_col, group_type)
  df$x <- gi$x
  levs <- gi$levels
  if (group_type == "rater" && any(gi$counts < 3)) {
    warning("Fewer than 3 raters in a group: the group's rater variance ",
            "cannot be estimated reliably.", call. = FALSE)
  }
  C <- length(categories)
  gf <- .ordk_group_fit(df, C, group_type, conf.level)

  rows <- if (group_type == "rater") {
    c(levs, paste(levs[1], "vs", levs[2]))
  } else levs
  sub <- cbind(subgroup = rows, gf$subgroups)
  rownames(sub) <- NULL

  zc <- stats::qnorm(1 - (1 - conf.level) / 2)
  d <- gf$difference
  conf.int <- pmin(pmax(d[["estimate"]] + c(-1, 1) * zc * d[["se"]], -1), 1)
  attr(conf.int, "conf.level") <- conf.level

  rater_effects <- gf$rater_effects
  if (group_type == "rater") {
    rater_effects$group <- factor(levs[gf$rater_x + 1], levels = levs)
  }

  fit <- c(gf[setdiff(names(gf), c("subgroups", "rater_effects", "rater_x"))],
           list(subgroups = sub,
                rater_effects = rater_effects,
                group = group,
                group_type = group_type,
                levels = levs,
                group_sizes = gi$counts,
                C = C,
                categories = categories,
                n = prep$n,
                k = prep$k,
                N = prep$N,
                n_missing = prep$n_missing))
  labels <- paste0("kappa_m (", levs, ")")
  unit <- if (group_type == "rater") "rater" else "subject"
  structure(list(statistic = c(LRT = unname(gf$lrt[["statistic"]])),
                 parameter = NULL,
                 p.value = unname(gf$lrt[["p.value"]]),
                 conf.int = conf.int,
                 estimate = stats::setNames(gf$subgroups$kappa_m[1:2], labels),
                 stderr = c(stats::setNames(gf$subgroups$se[1:2], labels),
                            difference = d[["se"]]),
                 null.value = c("difference in kappa_m" = 0),
                 alternative = "two.sided",
                 method = paste0("Model-based kappa by ", unit, " group (",
                                 group, "): likelihood ratio test of a ",
                                 "group-specific ", unit, " variance"),
                 data.name = paste0(prep$data.name, ", ", unit, " group: ",
                                    group),
                 fit = fit,
                 data = df,
                 settings = list(statistic = "kappa_m_group",
                                 conf.level = conf.level,
                                 label = "kappa_m")),
            class = c("cac_htest", "htest"))
}
