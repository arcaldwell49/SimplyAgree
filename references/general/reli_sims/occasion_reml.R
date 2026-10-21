# Phase C4: coverage of the analytic reli_facet_test() intervals with
# crossed occasions, method = "reml" and 20% of subject x item x occasion
# cells missing completely at random (criterion C5: >= .91 at n = 30 with
# normal data). Uses the package's prep, fit and interval code.
# Usage: Rscript occasion_reml.R <progress_log_file> [nsim] [workers]
# (writes occasion_reml.rds next to the log). Run from the package root.
# Written 2026-10-20; results in reli_redesign.md, Section 15.9.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 1000
workers <- if (length(args) > 2) as.integer(args[3]) else 14
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

src <- readLines("references/general/reli_sims/occasion_coverage.R")
eval(parse(text = src[grep("^mixes", src):(grep("^run_cell", src) - 1)]))
cells <- expand.grid(n = 30, k = c(2, 3), m = c(2, 3), mix = names(mixes),
                     dist = "normal", stringsAsFactors = FALSE)
estimands <- estimands[c("ICC_intra_C", "ICC_intra_A", "ICC_inter_A", "Phi_11",
                         "SEM_intra_C", "SEM_intra_A", "SEM_overall_A")]

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; m <- cl$m
  v <- mixes[[cl$mix]]
  truth <- vapply(estimands, truth_of, numeric(1), v = v)
  set.seed(11000 + ci)
  rows <- list()
  for (sim in seq_len(nsim)) {
    Y <- sim_array(n, k, m, v, cl$dist)
    d <- expand.grid(id = seq_len(n), items = seq_len(k), occ = seq_len(m))
    d$values <- as.vector(Y)
    d <- d[runif(nrow(d)) >= 0.2, ]
    prep <- tryCatch(.facet_prep_from_long(d[c("values", "items", "id", "occ")]),
                     error = function(e) NULL)
    if (is.null(prep)) next
    fit <- tryCatch(suppressWarnings(suppressMessages(
      .facet_fit(prep, "reml", keep_fit = FALSE))), error = function(e) NULL)
    if (is.null(fit)) next
    for (nm in names(estimands)) {
      s <- estimands[[nm]]
      est <- .facet_estimate(fit, s)
      methods <- if (.facet_is_exact(fit, s)) "exact" else c("mls", "satterthwaite")
      for (meth in methods) {
        ci2 <- tryCatch(as.numeric(.facet_ci(fit, s, 0.95, "two.sided", meth)),
                        error = function(e) c(NA, NA))
        rows[[length(rows) + 1]] <- data.frame(estimand = nm, ci_method = meth,
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
               rel_bias = (mean(d$est) - d$truth[1]) / abs(d$truth[1]),
               n_sims = nrow(d), n_fail = sum(!ok))
  }))
  cbind(cl[rep(1, nrow(agg)), ], agg, row.names = NULL)
}

cl <- makeCluster(workers)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
clusterExport(cl, c("cells", "mixes", "estimands", "draw", "draw_e", "sim_array",
                    "truth_of", "run_cell", "nsim", "log_file", "logp"))
logp("start", nrow(cells), "cells, nsim =", nsim)
out <- parLapplyLB(cl, seq_len(nrow(cells)), function(ci) {
  t0 <- Sys.time()
  res <- run_cell(ci)
  logp("cell", ci, "done in", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
  res
})
stopCluster(cl)
out <- do.call(rbind, out)
saveRDS(out, file.path(dirname(log_file), "occasion_reml.rds"))
logp("finished")
