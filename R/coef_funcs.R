# Chance-corrected agreement coefficients for categorical ratings (Gwet, 2014),
# used by cac_test() and agree_coef(). The point estimates and variances follow
# Gwet (2008, 2014) and match the irrCAC package.

.cac_weight_types <- c("unweighted", "quadratic", "linear", "ordinal",
                       "radical", "ratio", "circular", "bipolar")

#' Weight matrix for a set of categories (Gwet, 2014, ch. 3)
#'
#' Numeric categories use their values; other categories use their positions
#' 1, ..., q in the order given.
#' @noRd
.cac_weights <- function(type, categories) {
  q <- length(categories)
  if (type == "unweighted" || q == 1) {
    return(diag(q))
  }
  x <- if (is.numeric(categories)) categories else seq_len(q)
  if (type == "ratio" && any(x < 0)) {
    stop("Ratio weights require non-negative categories.", call. = FALSE)
  }
  d <- outer(x, x, "-")
  rng <- max(x) - min(x)
  w <- switch(type,
              quadratic = 1 - d^2 / rng^2,
              linear = 1 - abs(d) / rng,
              radical = 1 - sqrt(abs(d)) / sqrt(rng),
              ratio = 1 - (d / outer(x, x, "+"))^2 /
                (rng / (max(x) + min(x)))^2,
              ordinal = {
                m <- abs(outer(seq_len(q), seq_len(q), "-")) + 1
                m <- m * (m - 1) / 2
                1 - m / max(m)
              },
              circular = {
                s <- sin(pi * d / (rng + 1))^2
                1 - s / max(s)
              },
              bipolar = {
                s <- outer(x, x, "+")
                b <- d^2 / ((s - 2 * min(x)) * (2 * max(x) - s))
                diag(b) <- 0
                1 - b / max(b)
              })
  diag(w) <- 1
  w
}

#' Set up the agreement matrices for the coefficient functions
#'
#' @param ratings An n x r matrix of ratings (subjects by raters), numeric or
#'   character, with NA for missing ratings.
#' @param categories The rating categories, or NULL for the sorted observed
#'   values. Numeric categories are sorted.
#' @param weights One of `.cac_weight_types`, or a q x q numeric matrix.
#' @return A list of class `cac_prep`.
#' @noRd
.cac_prep <- function(ratings, categories = NULL, weights = "unweighted") {
  ratings <- as.matrix(ratings)
  # subjects with no ratings carry no information (and would divide by zero)
  ratings <- ratings[rowSums(!is.na(ratings)) > 0, , drop = FALSE]
  observed <- unique(as.vector(ratings[!is.na(ratings)]))
  if (is.null(categories)) {
    categories <- sort(observed)
  } else {
    if (anyNA(categories) || anyDuplicated(categories)) {
      stop("'categories' must not contain missing or duplicated values.",
           call. = FALSE)
    }
    if (is.numeric(categories)) {
      categories <- sort(categories)
    }
    absent <- setdiff(as.character(observed), as.character(categories))
    if (length(absent) > 0) {
      stop("Ratings not listed in 'categories': ",
           paste(absent, collapse = ", "), call. = FALSE)
    }
  }
  q <- length(categories)

  if (is.character(weights)) {
    w <- .cac_weights(weights, categories)
    w_name <- weights
  } else {
    w <- as.matrix(weights)
    if (!is.numeric(w) || !all(dim(w) == q) || anyNA(w) ||
        any(w < 0 | w > 1) || any(diag(w) != 1)) {
      stop("A custom weight matrix must be ", q, " x ", q, " (one row and ",
           "column per category), with values in [0, 1] and 1 on the ",
           "diagonal.", call. = FALSE)
    }
    w_name <- if (all(w == diag(q))) "unweighted" else "custom"
  }

  n <- nrow(ratings)
  agree <- matrix(0, nrow = n, ncol = q)
  for (k in seq_len(q)) {
    agree[, k] <- rowSums(ratings == categories[k], na.rm = TRUE)
  }
  ri <- rowSums(agree)
  if (sum(ri >= 2) == 0) {
    stop("At least one subject must have 2 or more ratings.", call. = FALSE)
  }
  agree_w <- agree %*% t(w)
  # numerator of each subject's pairwise (weighted) agreement
  sum_q <- rowSums(agree * (agree_w - 1))
  den <- ri * (ri - 1)
  structure(list(ratings = ratings,
                 agree = agree,
                 agree_w = agree_w,
                 weights = w,
                 weights_name = w_name,
                 categories = categories,
                 n = n,
                 r = ncol(ratings),
                 q = q,
                 ri = ri,
                 sum_q = sum_q,
                 # subjects with fewer than 2 ratings contribute 0 below
                 pa_ivec = sum_q / ifelse(den == 0, -1, den),
                 two = ri >= 2),
            class = "cac_prep")
}

# Variance of a coefficient from its subject-level linearisation (Gwet, 2008).
.cac_se <- function(ivec, centre, n) {
  if (n < 2) {
    return(NA_real_)
  }
  sqrt(sum((ivec - centre)^2) / (n * (n - 1)))
}

# Percent agreement among the subjects with 2 or more ratings.
.cac_observed <- function(p) {
  sum(p$sum_q[p$two] / (p$ri * (p$ri - 1))[p$two]) / sum(p$two)
}

.cac_pa <- function(p) {
  pa <- .cac_observed(p)
  ivec <- (p$n / sum(p$two)) * p$pa_ivec
  list(est = pa, se = .cac_se(ivec, pa, p$n), df = p$n - 1, pa = pa, pe = 0)
}

.cac_gwet <- function(p) {
  n <- p$n
  q <- p$q
  pa <- .cac_observed(p)
  pi_k <- colMeans(p$agree / p$ri)
  tw <- sum(p$weights) / (q * (q - 1))
  pe <- tw * sum(pi_k * (1 - pi_k))
  est <- (pa - pe) / (1 - pe)
  ivec <- (n / sum(p$two)) * (p$pa_ivec - pe * p$two) / (1 - pe)
  pe_ivec <- tw * drop(p$agree %*% (1 - pi_k)) / p$ri
  ivec <- ivec - 2 * (1 - est) * (pe_ivec - pe) / (1 - pe)
  list(est = est, se = .cac_se(ivec, est, n), df = n - 1, pa = pa, pe = pe)
}

.cac_fleiss <- function(p) {
  n <- p$n
  w <- p$weights
  pa <- .cac_observed(p)
  pi_k <- colMeans(p$agree / p$ri)
  pe <- sum(w * outer(pi_k, pi_k))
  est <- (pa - pe) / (1 - pe)
  ivec <- (n / sum(p$two)) * (p$pa_ivec - pe * p$two) / (1 - pe)
  pi_w <- drop(w %*% pi_k + t(w) %*% pi_k) / 2
  pe_ivec <- drop(p$agree %*% pi_w) / p$ri
  ivec <- ivec - 2 * (1 - est) * (pe_ivec - pe) / (1 - pe)
  list(est = est, se = .cac_se(ivec, est, n), df = n - 1, pa = pa, pe = pe)
}

.cac_kripp <- function(p) {
  w <- p$weights
  a <- p$agree[p$two, , drop = FALSE]
  aw <- p$agree_w[p$two, , drop = FALSE]
  r_i <- p$ri[p$two]
  r_bar <- mean(r_i)
  n <- nrow(a)
  eps <- 1 / sum(r_i)
  sum_q <- rowSums(a * (aw - 1))
  pa_prime <- sum(sum_q / (r_bar * (r_i - 1))) / n
  pa <- (1 - eps) * pa_prime + eps
  pi_k <- colMeans(a) / r_bar
  pe <- sum(w * outer(pi_k, pi_k))
  est <- (pa - pe) / (1 - pe)
  est_prime <- (pa_prime - pe) / (1 - pe)
  pa_ivec <- sum_q / (r_bar * (r_i - 1)) - pa_prime * (r_i - r_bar) / r_bar
  ivec <- (pa_ivec - pe) / (1 - pe)
  pi_w <- drop(w %*% pi_k + t(w) %*% pi_k) / 2
  pe_ivec <- drop(a %*% pi_w) / r_bar - pe * (r_i - r_bar) / r_bar
  ivec <- ivec - 2 * (1 - est_prime) * (pe_ivec - pe) / (1 - pe)
  list(est = est, se = .cac_se(ivec, est_prime, n), df = n - 1, pa = pa,
       pe = pe)
}

#' Compute one coefficient
#'
#' With a single category the chance-corrected coefficients are 0/0 and are
#' returned as NA.
#' @noRd
.cac_coef <- function(p, coef = c("pa", "gwet", "fleiss", "krippendorff")) {
  coef <- match.arg(coef)
  if (coef != "pa" && p$q < 2) {
    return(list(est = NA_real_, se = NA_real_, df = p$n - 1, pa = 1,
                pe = NA_real_))
  }
  switch(coef,
         pa = .cac_pa(p),
         gwet = .cac_gwet(p),
         fleiss = .cac_fleiss(p),
         krippendorff = .cac_kripp(p))
}

#' Label of a coefficient for printing
#' @noRd
.cac_label <- function(coef, weighted) {
  switch(coef,
         pa = "percent agreement",
         gwet = if (weighted) "Gwet's AC2" else "Gwet's AC1",
         fleiss = "Fleiss' kappa",
         krippendorff = "Krippendorff's alpha")
}

#' Range a coefficient can take, for clipping confidence limits
#' @noRd
.cac_bounds <- function(coef) {
  if (coef == "pa") c(0, 1) else c(-1, 1)
}

#' t-based interval and test (Gwet, 2014), limits clipped to the range of the
#' coefficient
#' @noRd
.cac_inference <- function(fit, coef, conf.level, alternative,
                           null.value = NULL) {
  bounds <- .cac_bounds(coef)
  est <- fit$est
  se <- fit$se
  df <- fit$df
  alpha <- 1 - conf.level
  ci <- switch(alternative,
               two.sided = est + c(-1, 1) * stats::qt(1 - alpha / 2, df) * se,
               greater = c(est - stats::qt(1 - alpha, df) * se, Inf),
               less = c(-Inf, est + stats::qt(1 - alpha, df) * se))
  ci <- pmin(pmax(ci, bounds[1]), bounds[2])
  attr(ci, "conf.level") <- conf.level
  test <- list(statistic = NULL, parameter = NULL, p.value = NULL)
  if (!is.null(null.value)) {
    t_stat <- (est - null.value) / se
    test <- list(statistic = c(t = t_stat),
                 parameter = c(df = df),
                 p.value = switch(alternative,
                                  two.sided = 2 * stats::pt(-abs(t_stat), df),
                                  greater = stats::pt(t_stat, df,
                                                      lower.tail = FALSE),
                                  less = stats::pt(t_stat, df)))
  }
  list(conf.int = ci, test = test)
}

#' Ratings matrix (subjects by raters) from long data with columns values,
#' items and id
#' @noRd
.cac_ratings_matrix <- function(df) {
  values <- df$values
  if (is.factor(values)) {
    values <- as.character(values)
  }
  m <- matrix(if (is.numeric(values)) NA_real_ else NA_character_,
              nrow = nlevels(df$id), ncol = nlevels(df$items),
              dimnames = list(levels(df$id), levels(df$items)))
  m[cbind(as.integer(df$id), as.integer(df$items))] <- values
  m
}
