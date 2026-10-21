# Phase 2: combined bootstrap coverage study that set boot_ci = "auto".
# Usage: Rscript boot_coverage2.R <progress_log_file> [start_cell]
# (writes boot_coverage2_partial.rds next to the log; start_cell > 1 resumes).
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n", file = log_file, append = TRUE)
cl <- makeCluster(12)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
B <- 499; k <- 3; v_it <- 0.1
cells <- expand.grid(n = c(10, 30), icc = c(0.5, 0.9), dist = c("normal", "non-normal"), miss = c(0, 0.2),
                     stringsAsFactors = FALSE)
one_sim <- function(seed, n, icc, dist, miss) {
  set.seed(seed)
  v_e <- (1 - icc) / icc
  truth <- c(C1 = icc, A1 = 1 / (1 + v_it + v_e))
  s <- if (dist == "normal") rnorm(n) else (rchisq(n, 3) - 3) / sqrt(6)
  it <- if (dist == "normal") rnorm(k, 0, sqrt(v_it)) else (rchisq(k, 3) - 3) / sqrt(6) * sqrt(v_it)
  e <- if (dist == "normal") rnorm(n * k, 0, sqrt(v_e)) else rt(n * k, 5) * sqrt(v_e * 3 / 5)
  Y <- outer(s, rep(1, k)) + outer(rep(1, n), it) + matrix(e, n)
  if (miss > 0) { M <- matrix(FALSE, n, k); M[, -1] <- runif(n * (k - 1)) < miss * k / (k - 1); Y[M] <- NA }
  prep <- tryCatch(.reli_prep_from_long(data.frame(values = as.vector(Y), items = rep(1:k, each = n), id = rep(1:n, k))), error = function(e) NULL)
  if (is.null(prep)) return(NULL)
  method <- if (prep$balanced) "anova" else "reml"
  stats <- function(p, resample, ses = TRUE) {
    f <- .reli_fit(p, method, "twoway", keep_fit = FALSE)
    out <- c()
    for (fm in c("C1", "A1")) {
      ff <- if (resample && fm == "A1") .hybrid_item_ms(f) else f
      si <- if (ses) .icc_se_z(p, ff, fm, "influence") else NA
      sj <- if (ses && method == "anova") .icc_se_z(p, ff, fm, "jackknife") else NA
      out <- c(out, .icc_estimate(ff, fm), .icc_z_fit(ff, fm), si, sj)
    }
    out
  }
  t0 <- stats(prep, FALSE)
  tb <- t(replicate(B, { p <- tryCatch(.reli_resample(prep), error = function(e) NULL)
    if (is.null(p)) rep(NA, 8) else tryCatch(suppressWarnings(stats(p, TRUE)), error = function(e) rep(NA, 8)) }))
  tb <- tb[is.finite(tb[, 1]) & is.finite(tb[, 5]), , drop = FALSE]
  jk <- t(vapply(levels(prep$df$id), function(i) {
    tryCatch(suppressWarnings(stats(.reli_prep_from_long(prep$df[prep$df$id != i, ]), FALSE, ses = FALSE)), error = function(e) rep(NA, 8))
  }, numeric(8)))
  f0 <- suppressWarnings(.reli_fit(prep, method, "twoway", keep_fit = FALSE))
  res <- c()
  for (j in 1:2) {
    fm <- c("C1", "A1")[j]; base <- 4 * (j - 1)
    mk <- function(secol) {
      list(t0 = c(est = t0[base + 1], z = t0[base + 2], se_z = t0[base + secol]),
           t = cbind(est = tb[, base + 1], z = tb[, base + 2], se_z = tb[, base + secol]),
           jack = cbind(est = jk[, base + 1], z = NA, se_z = NA))
    }
    back <- function(z) .icc_z_inv(z, fm, if (method == "anova") k else f0$kbar)
    ints <- list(F = .icc_ci(f0, fm),
                 perc = tryCatch(.boot_interval(mk(3), "perc"), error = function(e) c(NA, NA)),
                 bca = tryCatch(.boot_interval(mk(3), "bca"), error = function(e) c(NA, NA)),
                 stud_infl = tryCatch(.boot_interval(mk(3), "stud", back = back, bounds = c(-0.5, 1)), error = function(e) c(NA, NA)),
                 stud_jack = if (method == "anova") tryCatch(.boot_interval(mk(4), "stud", back = back, bounds = c(-0.5, 1)), error = function(e) c(NA, NA)) else c(NA, NA))
    for (nm in names(ints)) {
      ci <- ints[[nm]]; tr <- truth[[fm]]
      res[paste(fm, nm, "cov", sep = "_")] <- ci[1] <= tr && tr <= ci[2]
      res[paste(fm, nm, "lo", sep = "_")] <- tr < ci[1]
      res[paste(fm, nm, "w", sep = "_")] <- ci[2] - ci[1]
    }
  }
  res
}
clusterExport(cl, c("one_sim", "B", "k", "v_it"))
logp("start coverage study:", nrow(cells), "cells")
start <- if (length(args) > 1) as.integer(args[2]) else 1
rows <- if (start > 1) readRDS(file.path(dirname(log_file), "boot_coverage2_partial.rds"))$rows else list()
for (i in seq(start, nrow(cells))) {
  c0 <- cells[i, ]
  nsim <- if (c0$miss > 0) 150 else 200
  sims <- parLapply(cl, seq_len(nsim) + 3000 * i, function(s, n, icc, dist, miss) one_sim(s, n, icc, dist, miss),
                    n = c0$n, icc = c0$icc, dist = c0$dist, miss = c0$miss)
  sims <- Filter(Negate(is.null), sims)
  rows[[i]] <- colMeans(do.call(rbind, sims), na.rm = TRUE)
  logp("cell", i, "of", nrow(cells), "done (", nsim, "sims ):", paste(c0, collapse = " "))
  saveRDS(list(cells = cells[seq_len(i), ], rows = rows), file.path(dirname(log_file), "boot_coverage2_partial.rds"))
}
stopCluster(cl)
tab <- cbind(cells, do.call(rbind, rows))
for (fm in c("C1", "A1")) for (w in c("cov", "lo", "w")) {
  cat("\n==", fm, switch(w, cov = "coverage (nominal .95)", lo = "truth below interval (nominal .025)", w = "mean width"), "==\n")
  cols <- paste(fm, c("F", "perc", "bca", "stud_infl", "stud_jack"), w, sep = "_")
  x <- tab[, c("n", "icc", "dist", "miss", cols)]
  names(x)[5:9] <- c("F", "perc", "bca", "stud_infl", "stud_jack")
  print(format(x, digits = 3), row.names = FALSE)
}
cat("\nMC SE at .95: 200 sims", round(sqrt(.95 * .05 / 200), 3), "; 150 sims", round(sqrt(.95 * .05 / 150), 3), "\n")
logp("finished")
