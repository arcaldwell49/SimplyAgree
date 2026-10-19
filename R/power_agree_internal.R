# Internal helpers for the agreement power / sample size functions
# (agree_expected_half, agree_assurance, power_agreement_exact,
# blandPowerCurve).
#
# All of the exact methods integrate over the sampling distribution of
# s = S^2 / sigma^2, the scaled sample variance of the paired differences, and
# treat the mean difference as normal with variance sigma^2 / n_eff.
#
# Independent pairs (m = 1): s ~ chisq(n - 1) / (n - 1) and n_eff = n.
#
# Clustered pairs (n subjects, m pairs each, balanced compound symmetry with
# intraclass correlation icc of the differences): the ANOVA estimate of the
# total variance is S^2 = MSB / m + (m - 1) / m * MSW, so
#   s = a * X1 / nu1 + b * X2 / nu2,
#   X1 ~ chisq(nu1 = n - 1), X2 ~ chisq(nu2 = n * (m - 1)), independent,
#   a = (1 + (m - 1) * icc) / m, b = (m - 1) * (1 - icc) / m,
# and the grand mean has variance sigma^2 * (1 + (m - 1) * icc) / (n * m),
# independent of S^2.

#' @importFrom stats dnorm pnorm qnorm pchisq qchisq
NULL

# Design effect for the mean
.deff_mean <- function(m, icc) {
  1 + (m - 1) * icc
}

# Effective sample size for the mean
.n_eff <- function(n, m, icc) {
  n * m / .deff_mean(m, icc)
}

# Components of the distribution of s
.s2_pars <- function(n, m, icc) {
  list(
    nu1 = n - 1,
    nu2 = n * (m - 1),
    a = (1 + (m - 1) * icc) / m,
    b = (m - 1) * (1 - icc) / m
  )
}

# Satterthwaite df for s (exact chi-square df when m = 1)
.s2_df <- function(n, m, icc) {
  if (m == 1) {
    return(n - 1)
  }
  p <- .s2_pars(n, m, icc)
  1 / (p$a^2 / p$nu1 + p$b^2 / p$nu2)
}

# Chi-square quantile at normal score z, accurate in both tails
.qchisq_z <- function(z, df) {
  out <- numeric(length(z))
  lo <- z <= 0
  out[lo] <- qchisq(pnorm(z[lo]), df)
  out[!lo] <- qchisq(pnorm(z[!lo], lower.tail = FALSE), df,
                     lower.tail = FALSE)
  out
}

# CDF of s at t
.s2_cdf <- function(t, n, m = 1, icc = 0, nz = 401) {
  if (m == 1) {
    return(pchisq(t * (n - 1), n - 1))
  }
  p <- .s2_pars(n, m, icc)
  # Simpson's rule over the normal score of X1
  zz <- seq(-8, 8, length.out = nz)
  coe <- c(1, rep(c(4, 2), (nz - 3) / 2), 4, 1)
  wz <- coe * dnorm(zz)
  wz <- wz / sum(wz)
  u <- p$a * .qchisq_z(zz, p$nu1) / p$nu1
  cdf <- outer(t, u, function(tt, uu) {
    pchisq(pmax(tt - uu, 0) * p$nu2 / p$b, p$nu2)
  })
  as.vector(cdf %*% wz)
}

# Quadrature nodes (s) and probability weights (w) for s = S^2 / sigma^2.
# Bins are equally spaced on the normal-score scale of (approximately) s, the
# weight of each bin is its exact probability, and its node is its
# (approximate) median.
.s2_grid <- function(n, m = 1, icc = 0, nbin = if (m == 1) 2000 else 1000) {
  zb <- seq(-8, 8, length.out = nbin + 1)
  zm <- (zb[-1] + zb[-(nbin + 1)]) / 2
  if (m == 1) {
    df <- n - 1
    s <- .qchisq_z(zm, df) / df
    w <- diff(pnorm(zb))
  } else {
    nus <- .s2_df(n, m, icc)
    brk <- .qchisq_z(zb, nus) / nus
    brk[1] <- 0
    brk[nbin + 1] <- Inf
    s <- .qchisq_z(zm, nus) / nus
    w <- pmax(diff(.s2_cdf(brk, n, m, icc)), 0)
  }
  list(s = s, w = w / sum(w))
}

# E[S / sigma]
.s_mean <- function(n, m = 1, icc = 0, grid = NULL) {
  if (m == 1) {
    df <- n - 1
    return(exp(lgamma(n / 2) - lgamma(df / 2) - log(sqrt(df / 2))))
  }
  if (is.null(grid)) grid <- .s2_grid(n, m, icc)
  sum(grid$w * sqrt(grid$s))
}

# Bisection for a monotone function. Finds the root of f(x) = target in
# [lo, hi], expanding hi (doubling) when needed. `increasing` is the direction
# of f. Returns the end of the final bracket on the side where
# f(x) >= target (side = "above") or f(x) <= target (side = "below").
.bisect <- function(f, target, lo, hi, increasing = TRUE,
                    side = c("above", "below"), rel_tol = 1e-10,
                    max_iter = 200, max_expand = 60) {
  side <- match.arg(side)
  past_hi <- function(v) if (increasing) v >= target else v <= target
  k <- 0
  while (!past_hi(f(hi))) {
    lo <- hi
    hi <- hi * 2
    k <- k + 1
    if (k > max_expand) {
      stop("Could not bracket the solution; check the inputs.",
           call. = FALSE)
    }
  }
  iter <- 0
  while ((hi - lo) > rel_tol * max(abs(hi), 1e-300)) {
    iter <- iter + 1
    if (iter > max_iter) {
      message("Maximum iterations reached in bisection")
      break
    }
    mid <- (lo + hi) / 2
    if (past_hi(f(mid))) {
      hi <- mid
    } else {
      lo <- mid
    }
  }
  # hi is past the target in the direction of increase
  if ((side == "above") == increasing) hi else lo
}

# Equal-tailed tolerance factor g: the smallest g such that
# Pr(xbar - g S < theta_(1-p) and theta_p < xbar + g S) >= coverp.
.tol_g <- function(n_eff, grid, zp, coverp) {
  cover <- function(g) {
    b <- sqrt(n_eff) * (-zp + g * sqrt(grid$s))
    sum(grid$w * pmax(2 * pnorm(b) - 1, 0))
  }
  .bisect(cover, coverp, lo = 0, hi = 2 * zp + 2, increasing = TRUE,
          side = "above")
}

# Critical value gamma of the exact agreement test (Shieh 2019): the size at
# the boundary of H0 equals alpha (on the conservative side).
.exact_gamma <- function(n_eff, grid, z_p, alpha, max_iter = 200) {
  size <- function(gam) {
    h <- z_p * sqrt(n_eff) - gam * sqrt(grid$s)
    sum(grid$w * pmax(2 * pnorm(h) - 1, 0))
  }
  .bisect(size, alpha, lo = 0, hi = 2 * z_p * sqrt(n_eff) + 2,
          increasing = FALSE, side = "below", max_iter = max_iter)
}

# Power of the exact agreement test
.exact_power <- function(n_eff, grid, gam, delta, mu, sigma) {
  std <- sigma / sqrt(n_eff)
  hel <- (-delta - mu) / std + gam * sqrt(grid$s)
  heu <- (delta - mu) / std - gam * sqrt(grid$s)
  sum(grid$w * pmax(pnorm(heu) - pnorm(hel), 0))
}

# Smallest integer n in [lo, hi] with ok(n) TRUE, assuming ok is monotone.
# Doubles n from lo until ok(n) holds, then bisects. Warns and returns hi when
# even hi does not satisfy ok.
.search_n <- function(ok, lo = 2, hi = 10000) {
  if (ok(lo)) return(lo)
  cur <- lo
  repeat {
    prev <- cur
    cur <- min(cur * 2, hi)
    if (ok(cur)) break
    if (cur >= hi) {
      warning("Target not reached at n = ", hi,
              "; the sample size returned is the upper search limit.",
              call. = FALSE)
      return(hi)
    }
  }
  lo <- prev
  hi <- cur
  while (hi - lo > 1) {
    mid <- floor((lo + hi) / 2)
    if (ok(mid)) {
      hi <- mid
    } else {
      lo <- mid
    }
  }
  hi
}

# Shared input checks for the clustering arguments
.check_cluster_args <- function(m, icc) {
  if (length(m) != 1 || !is.numeric(m) || is.na(m) || m < 1 ||
      m != round(m)) {
    stop("'m' must be a single integer >= 1", call. = FALSE)
  }
  if (length(icc) != 1 || !is.numeric(icc) || is.na(icc) || icc < 0 ||
      icc >= 1) {
    stop("'icc' must be a single value in [0, 1)", call. = FALSE)
  }
  invisible(TRUE)
}

.agree_n_note <- function(m, icc) {
  if (m == 1) {
    "n is the number of pairs. Two measurements per unit; one for each method."
  } else {
    paste0("n is the number of subjects, each with m = ", m,
           " paired measurements (n.obs pairs in total).\n",
           "     Assumes balanced compound symmetry with ICC of the ",
           "differences = ", icc, ".")
  }
}

.check_n <- function(n) {
  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 2 ||
      n != round(n)) {
    stop("'n' must be an integer >= 2", call. = FALSE)
  }
  invisible(TRUE)
}
