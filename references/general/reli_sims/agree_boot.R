# Phase 2: subject-only vs pigeonhole vs hybrid bootstrap for ICC(A,1), quick run.
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

library(parallel)
cl <- makeCluster(12)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
B <- 499; nsim <- 250; n <- 30; v_id <- 1; v_it <- 0.1; icc_c <- 0.9
cells <- expand.grid(k = c(3, 6), dist = c("normal", "non-normal"), stringsAsFactors = FALSE)
one_sim <- function(seed, k, dist) {
  set.seed(seed)
  v_e <- (1 - icc_c) / icc_c
  truth <- v_id / (v_id + v_it + v_e)
  draw <- function(m, v, kind) if (dist == "normal") rnorm(m, 0, sqrt(v)) else
    if (kind == "e") rt(m, 5) * sqrt(v * 3 / 5) else (rchisq(m, 3) - 3) / sqrt(6) * sqrt(v)
  Y <- outer(draw(n, v_id, "s"), rep(1, k)) + outer(rep(1, n), draw(k, v_it, "i")) + matrix(draw(n * k, v_e, "e"), n)
  ms_of <- function(M) { p <- .reli_prep_from_long(data.frame(values = as.vector(M), items = rep(seq_len(ncol(M)), each = nrow(M)), id = rep(seq_len(nrow(M)), ncol(M))))
    .reli_fit(p, keep_fit = FALSE) }
  f0 <- ms_of(Y)
  est <- function(f) .icc_estimate(f, "A1")
  subj <- pig <- hyb <- numeric(B)
  for (b in 1:B) {
    rs <- sample(n, n, TRUE); cs <- sample(k, k, TRUE)
    fs <- ms_of(Y[rs, , drop = FALSE]); subj[b] <- est(fs)
    fh <- fs; fh$ms[["MSJ"]] <- fs$ms[["MSJ"]] * rchisq(1, k - 1) / (k - 1); hyb[b] <- est(fh)
    pig[b] <- tryCatch(est(ms_of(Y[rs, cs, drop = FALSE])), error = function(e) NA)
  }
  cover <- function(x) { q <- quantile(x, c(.025, .975), na.rm = TRUE); c(cov = q[[1]] <= truth && truth <= q[[2]], lo = truth < q[[1]], w = diff(q)) }
  ciF <- .icc_ci(f0, "A1")
  c(F = c(cov = ciF[1] <= truth && truth <= ciF[2], lo = truth < ciF[1], w = diff(ciF)),
    subject = cover(subj), pigeonhole = cover(pig), hybrid = cover(hyb))
}
clusterExport(cl, c("one_sim", "B", "n", "v_id", "v_it", "icc_c"))
res <- lapply(seq_len(nrow(cells)), function(i) {
  m <- do.call(rbind, parLapply(cl, seq_len(nsim) + 5000 * i, function(s, k, dist) one_sim(s, k, dist), k = cells$k[i], dist = cells$dist[i]))
  colMeans(m, na.rm = TRUE)
})
stopCluster(cl)
tab <- cbind(cells, round(do.call(rbind, res), 3))
for (w in c("cov", "lo", "w")) { cat("\n==", w, "==\n"); print(tab[, c("k", "dist", names(tab)[endsWith(names(tab), paste0(".", w))])], row.names = FALSE) }
cat("MC SE at .95:", round(sqrt(.95 * .05 / nsim), 3), "\n")
