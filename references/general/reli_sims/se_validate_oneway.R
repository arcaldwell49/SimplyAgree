# Phase 2: one-way SE validation with correctly simulated one-way data.
# Usage: Rscript se_validate_oneway.R <progress_log_file>
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n", file = log_file, append = TRUE)
cl <- makeCluster(12)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
nsim <- 300; n <- 20; k <- 3
cells <- expand.grid(miss = c(0, 0.15, 0.30), mech = c("MCAR", "MAR"), dist = c("normal", "non-normal"), stringsAsFactors = FALSE)
cells <- cells[!(cells$miss == 0 & cells$mech == "MAR"), ]
one <- function(seed, n, k, miss, mech, dist) {
  set.seed(seed)
  s <- if (dist == "normal") rnorm(n) else (rchisq(n, 3) - 3) / sqrt(6)
  # one-way design: each subject rated by different raters, so rater effects are unique to each cell
  rater <- if (dist == "normal") rnorm(n * k, 0, .3) else (rchisq(n * k, 3) - 3) / sqrt(6) * .3
  e <- if (dist == "normal") rnorm(n * k, 0, .6) else rt(n * k, 5) * sqrt(.36 * 3 / 5)
  Y <- outer(s, rep(1, k)) + matrix(rater + e, n)
  if (miss > 0) {
    M <- matrix(FALSE, n, k)
    p <- if (mech == "MCAR") rep(miss * k / (k - 1), n) else plogis(qlogis(miss * k / (k - 1)) + 1.5 * as.vector(scale(Y[, 1])))
    M[, -1] <- runif(n * (k - 1)) < p
    Y[M] <- NA
  }
  prep <- .reli_prep_from_long(data.frame(values = as.vector(Y), items = rep(1:k, each = n), id = rep(1:n, k)))
  method <- if (prep$balanced) "anova" else "reml"
  f <- suppressWarnings(.reli_fit(prep, method, "oneway"))
  c(z = .icc_z_fit(f, "1"),
    vapply(c("influence", "jackknife", "normal"), function(m) suppressWarnings(.icc_se_z(prep, f, "1", m)), 1))
}
clusterExport(cl, "one")
logp("start one-way validation:", nrow(cells), "cells x", nsim, "sims")
rows <- list()
for (i in seq_len(nrow(cells))) {
  c0 <- cells[i, ]
  m <- do.call(rbind, parLapply(cl, seq_len(nsim) + 7000 * i, function(s, n, k, miss, mech, dist) one(s, n, k, miss, mech, dist),
                                n = n, k = k, miss = c0$miss, mech = c0$mech, dist = c0$dist))
  sdz <- sd(m[, "z"])
  rows[[i]] <- c(trueSD = sdz, colMeans(m[, c("influence", "jackknife", "normal")]) / sdz)
  logp("cell", i, "of", nrow(cells), "done:", paste(c0, collapse = " "))
}
stopCluster(cl)
tab <- cbind(cells, round(do.call(rbind, rows), 3))
cat("== ICC(1), one-way data, n = 20, k = 3: mean SE / true SD of z ==\n"); print(tab, row.names = FALSE)
logp("finished")
