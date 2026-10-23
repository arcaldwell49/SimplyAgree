# Record of the 2026-10-21 check of Ting et al. (1989) cross-product terms
# (reli_redesign.md, Section 15.10). Run from the package root; ~1 minute.
# Quick check: does Ting et al. (1989) (2.5b) - cross-products among the
# positive terms in the UPPER bound of a sum - reduce the conservative upper
# tail of the agreement SEM? Uses package internals; the bounds function
# is local to this script.
suppressMessages(devtools::load_all(quiet = TRUE))

# one-sided bounds of a positive sum: GW, and Ting 1989 (2.5a)/(2.5b)
bounds89 <- function(coef, ms, dfs, a, cross = TRUE) {
  keep <- coef != 0; c_ <- coef[keep] * ms[keep]; nu <- dfs[keep]; Q <- length(c_)
  stopifnot(all(c_ > 0))
  G <- function(v) 1 - v / qchisq(1 - a, v); H <- function(v) v / qchisq(a, v) - 1
  vl <- sum(G(nu)^2 * c_^2); vu <- sum(H(nu)^2 * c_^2)
  if (cross && Q > 1) for (q in 1:(Q - 1)) for (t in (q + 1):Q) {
    s <- nu[q] + nu[t]; w <- s^2 / (nu[q] * nu[t])
    vl <- vl + (G(s)^2 * w - G(nu[q])^2 * nu[q] / nu[t] - G(nu[t])^2 * nu[t] / nu[q]) / (Q - 1) * c_[q] * c_[t]
    vu <- vu + (H(s)^2 * w - H(nu[q])^2 * nu[q] / nu[t] - H(nu[t])^2 * nu[t] / nu[q]) / (Q - 1) * c_[q] * c_[t]
  }
  est <- sum(c_)
  c(est - sqrt(max(vl, 0)), est + sqrt(max(vu, 0)))
}

# nested design, agreement SEM^2 = i + pi + e (all positive in MS)
set.seed(1)
cells <- expand.grid(k = c(2, 3), mix = c("I", "R", "B"), stringsAsFactors = FALSE)
mixes <- list(R = c(i = .05, pi = .05, e = .30), I = c(i = .05, pi = .30, e = .05),
              B = c(i = .30, pi = .05, e = .10))
n <- 30; r <- 2; nsim <- 4000; a <- 0.025
for (ci in seq_len(nrow(cells))) {
  k <- cells$k[ci]; v <- mixes[[cells$mix[ci]]]; truth <- v[["i"]] + v[["pi"]] + v[["e"]]
  res <- t(replicate(nsim, {
    it <- rnorm(k, 0, sqrt(v[["i"]])); p <- rnorm(n)
    pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
    Y <- array(rep(outer(p, it, "+") + pi, r), c(n, k, r)) + rnorm(n * k * r, 0, sqrt(v[["e"]]))
    M <- rowMeans(Y, dims = 2)
    am <- .facet_anova_ms(M, r, sum((Y - c(M))^2), c("p", "i", "pi", "e"))
    f <- structure(.facet_refresh(list(method = "anova", design = "nested", factors = c("p", "i"),
      effects = c("p", "i", "pi", "e"), levels = c(p = n, i = k), n = n, k = k, r = r,
      balanced = TRUE, replicated = TRUE, ms = am$ms, df = am$df, grand_mean = 0)), class = "facet_fit")
    cf <- .facet_ms_coef(f, list(stat = "SEM", facet = "inter", type = "agreement",
                                 k_avg = 1, m_avg = 1, r_avg = 1, scale = "raw"))$var
    c(bounds89(cf, f$ms, f$df, a, cross = FALSE), bounds89(cf, f$ms, f$df, a, cross = TRUE))
  }))
  cat(sprintf("k=%d mix %s | GW: below lower %.3f, above upper %.3f | Ting89: below %.3f, above %.3f | median upper GW %.3f vs T89 %.3f\n",
              k, cells$mix[ci], mean(truth < res[, 1]), mean(truth > res[, 2]),
              mean(truth < res[, 3]), mean(truth > res[, 4]), median(res[, 2]), median(res[, 4])))
}
