# NOTE (2026-10-20): written for the development version of reli_facet_test(),
# then called gstudy_test(), with internal .gstudy_*() functions and the old
# `form` argument. Kept as the record of the Section 14 results; to rerun,
# map .gstudy_* to .facet_* and form = "intra" / "consistency" /
# "agreement" to facet = "intra" / facet = "inter", type = "consistency" /
# facet = "inter", type = "agreement".
# Phase G4: coverage of the analytic gstudy_test() intervals with
# method = "reml" and unbalanced data (observed-data df, harmonic-mean r).
# Usage: Rscript gstudy_reml.R <progress_log_file> [nsim]
# (writes gstudy_reml.rds next to the log). Run from the package root.
# Written 2026-10-20 against R/gstudy_engine.R; results in reli_redesign.md,
# Section 9 (phase G4). Criterion 5 (Section 14.5): coverage >= .91 at
# n = 30 with normal data.
#
# Patterns: "mcar20", 20% of repeats deleted completely at random, keeping
# at least one measurement per cell; "unequal", 1, 2 or 3 repeats per cell
# (equally likely), no empty cells.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 1000
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

mixes <- list(R = c(i = 0.05, pi = 0.05, e = 0.30),
              I = c(i = 0.05, pi = 0.30, e = 0.05),
              B = c(i = 0.30, pi = 0.05, e = 0.10),
              L = c(i = 0.10, pi = 0.50, e = 0.50))
cells <- rbind(
  expand.grid(n = 30, k = c(2, 3), r = c(2, 3), mix = names(mixes),
              pattern = "mcar20", stringsAsFactors = FALSE),
  expand.grid(n = 30, k = c(2, 3), r = 3, mix = names(mixes),
              pattern = "unequal", stringsAsFactors = FALSE))
estimands <- list(
  SEM_rep = list(stat = "SEM", form = "intra", k_avg = 1, r_avg = 1),
  SEM_C = list(stat = "SEM", form = "consistency", k_avg = 1, r_avg = 1),
  SEM_A = list(stat = "SEM", form = "agreement", k_avg = 1, r_avg = 1),
  Erho_11 = list(stat = "ICC", form = "consistency", k_avg = 1, r_avg = 1),
  Phi_11 = list(stat = "ICC", form = "agreement", k_avg = 1, r_avg = 1),
  rho_intra = list(stat = "ICC", form = "intra", k_avg = 1, r_avg = 1))

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; r <- cl$r
  v <- c(p = 1, mixes[[cl$mix]])[c("p", "i", "pi", "e")]
  specs <- lapply(estimands, function(s) { s$scale <- "raw"; s })
  truth <- vapply(specs, function(s) {
    w <- .gstudy_weights(s)
    if (s$stat == "SEM") sqrt(sum(w$var * v)) else sum(w$num * v) / sum(w$den * v)
  }, numeric(1))
  set.seed(9000 + ci)
  rows <- list()
  for (sim in seq_len(nsim)) {
    d <- expand.grid(rep = seq_len(r), items = seq_len(k), id = seq_len(n))
    if (cl$pattern == "unequal") {
      reps <- sample(1:3, n * k, replace = TRUE)
      d <- d[d$rep <= reps[(d$id - 1) * k + d$items], ]
    }
    p <- rnorm(n, 0, sqrt(v[["p"]]))
    it <- rnorm(k, 0, sqrt(v[["i"]]))
    pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
    d$values <- p[d$id] + it[d$items] + pi[cbind(d$id, d$items)] +
      rnorm(nrow(d), 0, sqrt(v[["e"]]))
    if (cl$pattern == "mcar20") {
      drop <- runif(nrow(d)) < 0.2
      # keep at least one measurement per cell
      cell <- paste(d$id, d$items)
      keep_one <- ave(seq_len(nrow(d)), cell, FUN = function(x) x == x[sample.int(length(x), 1)])
      drop[keep_one == 1] <- FALSE
      d <- d[!drop, ]
    }
    prep <- tryCatch(.gstudy_prep_from_long(d[c("values", "items", "id")]),
                     error = function(e) NULL)
    if (is.null(prep)) next
    fit <- tryCatch(suppressWarnings(.gstudy_fit(prep, "reml", keep_fit = FALSE)),
                    error = function(e) NULL)
    if (is.null(fit)) next
    for (nm in names(specs)) {
      s <- specs[[nm]]
      est <- .gstudy_estimate(fit, s)
      methods <- if (.gstudy_is_exact(fit, s)) "exact" else c("mls", "satterthwaite")
      for (m in methods) {
        ci2 <- tryCatch(as.numeric(.gstudy_ci(fit, s, 0.95, "two.sided", m)),
                        error = function(e) c(NA, NA))
        rows[[length(rows) + 1]] <- data.frame(estimand = nm, ci_method = m,
                                               est = est, lower = ci2[1],
                                               upper = ci2[2], truth = truth[[nm]])
      }
    }
  }
  res <- do.call(rbind, rows)
  agg <- do.call(rbind, lapply(split(res, list(res$estimand, res$ci_method), drop = TRUE), function(d) {
    ok <- is.finite(d$lower) & is.finite(d$upper)
    data.frame(estimand = d$estimand[1], ci_method = d$ci_method[1], truth = d$truth[1],
               coverage = mean(d$lower[ok] <= d$truth[1] & d$truth[1] <= d$upper[ok]),
               miss_low = mean(d$truth[1] < d$lower[ok]),
               miss_high = mean(d$truth[1] > d$upper[ok]),
               bias = mean(d$est) - d$truth[1],
               n_sims = nrow(d), n_fail = sum(!ok))
  }))
  cbind(cl[rep(1, nrow(agg)), ], agg, row.names = NULL)
}

cl <- makeCluster(14)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
clusterExport(cl, c("cells", "mixes", "estimands", "run_cell", "nsim",
                    "log_file", "logp"))
logp("start", nrow(cells), "cells, nsim =", nsim)
out <- parLapplyLB(cl, seq_len(nrow(cells)), function(ci) {
  t0 <- Sys.time()
  res <- run_cell(ci)
  logp("cell", ci, "done in", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
  res
})
stopCluster(cl)
out <- do.call(rbind, out)
saveRDS(out, file.path(dirname(log_file), "gstudy_reml.rds"))
logp("finished")
