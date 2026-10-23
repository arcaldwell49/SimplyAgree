# Phase 2: first ICC bootstrap coverage study (bca/stud/perc/basic vs F).
# Note: its 'stud' used the old constant normal-theory SE (degenerate).
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

library(parallel)
cl <- makeCluster(12)
clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE)))
k <- 3; v_it <- 0.1; B <- 499; nsim <- 250
cells <- expand.grid(n = c(10, 30), icc = c(0.5, 0.9), dist = c("normal", "skewed"),
                     stringsAsFactors = FALSE)
one_sim <- function(seed, n, icc, dist) {
  set.seed(seed)
  v_e <- (1 - icc) / icc            # sigma2_id = 1, ICC(C,1) = icc
  truth <- c(C1 = icc, A1 = 1 / (1 + v_it + v_e))
  draw <- function(m, v) if (dist == "normal") rnorm(m, 0, sqrt(v)) else
    if (v == v_e) rt(m, 5) * sqrt(v * 3 / 5) else (rchisq(m, 3) - 3) / sqrt(6) * sqrt(v)
  Y <- outer(draw(n, 1), rep(1, k)) + outer(rep(1, n), rnorm(k, 0, sqrt(v_it))) +
    matrix(draw(n * k, v_e), n)
  prep <- .reli_prep_from_long(data.frame(values = as.vector(Y), items = rep(1:k, each = n), id = rep(1:n, k)))
  stats <- function(p) {
    f <- .reli_fit(p, keep_fit = FALSE)
    unlist(lapply(c("C1", "A1"), function(fm) { r <- .icc_estimate(f, fm)
      c(r, .icc_z(r, fm, f$kbar), .icc_z_se(f, fm)) }))
  }
  t0 <- stats(prep)
  tb <- t(replicate(B, { p <- .reli_resample(prep); if (is.null(p)) rep(NA, 6) else
    tryCatch(stats(p), error = function(e) rep(NA, 6)) }))
  tb <- tb[stats::complete.cases(tb), , drop = FALSE]
  jk <- t(vapply(levels(prep$df$id), function(i) stats(.reli_prep_from_long(prep$df[prep$df$id != i, ])), numeric(6)))
  f0 <- .reli_fit(prep, keep_fit = FALSE)
  out <- c()
  for (j in 1:2) {
    fm <- c("C1", "A1")[j]; cols <- (3 * j - 2):(3 * j)
    bo <- list(t0 = setNames(t0[cols], c("est", "z", "se_z")),
               t = setNames(as.data.frame(tb[, cols]), c("est", "z", "se_z")),
               jack = setNames(as.data.frame(jk[, cols]), c("est", "z", "se_z")))
    bo$t <- as.matrix(bo$t); bo$jack <- as.matrix(bo$jack)
    for (ty in c("bca", "stud", "perc", "basic")) {
      ci <- tryCatch(.boot_interval(bo, ty, back = function(z) .icc_z_inv(z, fm, k), bounds = c(-1 / (k - 1), 1)),
                     error = function(e) c(NA, NA))
      out[paste(fm, ty, "cov", sep = "_")] <- ci[1] <= truth[[fm]] && truth[[fm]] <= ci[2]
      out[paste(fm, ty, "lo", sep = "_")] <- truth[[fm]] < ci[1]
      out[paste(fm, ty, "w", sep = "_")] <- diff(ci)
    }
    ci <- .icc_ci(f0, fm)
    out[paste(fm, "F", "cov", sep = "_")] <- ci[1] <= truth[[fm]] && truth[[fm]] <= ci[2]
    out[paste(fm, "F", "lo", sep = "_")] <- truth[[fm]] < ci[1]
    out[paste(fm, "F", "w", sep = "_")] <- diff(ci)
  }
  out
}
clusterExport(cl, c("one_sim", "k", "v_it", "B"))
res <- lapply(seq_len(nrow(cells)), function(i) {
  c0 <- cells[i, ]
  sims <- parLapply(cl, seq_len(nsim) + 1000 * i, function(s, n, icc, dist) one_sim(s, n, icc, dist),
                    n = c0$n, icc = c0$icc, dist = c0$dist)
  m <- do.call(rbind, sims)
  colMeans(m, na.rm = TRUE)
})
stopCluster(cl)
tab <- cbind(cells, do.call(rbind, res))
for (fm in c("C1", "A1")) for (what in c("cov", "lo", "w")) {
  cat("\n==", fm, switch(what, cov = "coverage (nominal .95)", lo = "share with truth below interval (nominal .025)", w = "mean width"), "==\n")
  cols <- paste(fm, c("F", "bca", "stud", "perc", "basic"), what, sep = "_")
  x <- tab[, c("n", "icc", "dist", cols)]; names(x)[4:8] <- c("F", "bca", "stud", "perc", "basic")
  print(format(x, digits = 3), row.names = FALSE)
}
cat("\nMC SE of coverage at .95 with", nsim, "sims:", round(sqrt(.95 * .05 / nsim), 3), "\n")
