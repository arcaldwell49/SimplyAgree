# NOTE (2026-10-20): written for the development version of reli_facet_test(),
# then called gstudy_test(), with internal .gstudy_*() functions and the old
# `form` argument. Kept as the record of the Section 14 results; to rerun,
# map .gstudy_* to .facet_* and form = "intra" / "consistency" /
# "agreement" to facet = "intra" / facet = "inter", type = "consistency" /
# facet = "inter", type = "agreement".
# Phase G2: coverage of the analytic gstudy_test() intervals (exact, MLS,
# Satterthwaite) for the r:(p x i) design. Sets ci_method = "auto".
# Usage: Rscript gstudy_coverage.R <progress_log_file> [nsim]
# (writes gstudy_coverage.rds next to the log). Run from the package root
# (it calls devtools::load_all(".")). Written 2026-10-20 against the engine in
# R/gstudy_engine.R; results and criteria in reli_redesign.md, Section 14.5
# and Section 9 (phase G2).
#
# Data are generated as an n x k x r array and the mean squares computed in
# closed form (the same formulas as .gstudy_fit(), checked against it in
# tests/testthat/test-gstudy_test.R); the interval code is the package's.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 2000
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

mixes <- list(R = c(i = 0.05, pi = 0.05, e = 0.30),
              I = c(i = 0.05, pi = 0.30, e = 0.05),
              B = c(i = 0.30, pi = 0.05, e = 0.10),
              L = c(i = 0.10, pi = 0.50, e = 0.50))
cells <- expand.grid(n = c(10, 30), k = c(2, 3, 5), r = c(2, 3),
                     mix = names(mixes), dist = c("normal", "nonnormal"),
                     stringsAsFactors = FALSE)

# estimands: stat, form, k_avg, r_avg ("r" = the design's r, "k" = its k)
estimands <- list(
  SEM_rep = list(stat = "SEM", form = "intra", k_avg = 1, r_avg = 1),
  SEM_C = list(stat = "SEM", form = "consistency", k_avg = 1, r_avg = 1),
  SEM_A = list(stat = "SEM", form = "agreement", k_avg = 1, r_avg = 1),
  Erho_11 = list(stat = "ICC", form = "consistency", k_avg = 1, r_avg = 1),
  Erho_1r = list(stat = "ICC", form = "consistency", k_avg = 1, r_avg = "r"),
  Phi_11 = list(stat = "ICC", form = "agreement", k_avg = 1, r_avg = 1),
  Phi_kr = list(stat = "ICC", form = "agreement", k_avg = "k", r_avg = "r"),
  rho_intra = list(stat = "ICC", form = "intra", k_avg = 1, r_avg = 1))

draw <- function(m, v, dist) {
  if (dist == "normal") return(rnorm(m, 0, sqrt(v)))
  sqrt(v) * (rchisq(m, 3) - 3) / sqrt(6)
}
draw_e <- function(m, v, dist) {
  if (dist == "normal") return(rnorm(m, 0, sqrt(v)))
  sqrt(v * 3 / 5) * rt(m, 5)
}

ms_from_array <- function(Y) {
  d <- dim(Y)
  n <- d[1]; k <- d[2]; r <- d[3]
  M <- rowMeans(Y, dims = 2)
  gm <- mean(M)
  rm <- rowMeans(M); cmn <- colMeans(M)
  c(MSP = k * r * sum((rm - gm)^2) / (n - 1),
    MSI = n * r * sum((cmn - gm)^2) / (k - 1),
    MSPI = r * sum((M - outer(rm, cmn, "+") + gm)^2) / ((n - 1) * (k - 1)),
    MSW = sum((Y - c(M))^2) / (n * k * (r - 1)))
}

mk_fit <- function(ms, n, k, r, gm = 0) {
  structure(list(method = "anova", balanced = TRUE, n = n, k = k, r = r,
                 kc = k, nc = n, ms = ms,
                 df = c(dfP = n - 1, dfI = k - 1, dfPI = (n - 1) * (k - 1),
                        dfW = n * k * (r - 1)),
                 grand_mean = gm), class = "gstudy_fit")
}

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; r <- cl$r
  v <- c(p = 1, mixes[[cl$mix]])[c("p", "i", "pi", "e")]
  specs <- lapply(estimands, function(s) {
    if (identical(s$r_avg, "r")) s$r_avg <- r
    if (identical(s$k_avg, "k")) s$k_avg <- k
    s$scale <- "raw"
    s
  })
  truth <- vapply(specs, function(s) {
    w <- .gstudy_weights(s)
    if (s$stat == "SEM") sqrt(sum(w$var * v)) else sum(w$num * v) / sum(w$den * v)
  }, numeric(1))
  set.seed(1000 + ci)
  res <- lapply(seq_len(nsim), function(sim) {
    p <- draw(n, v[["p"]], cl$dist)
    it <- draw(k, v[["i"]], cl$dist)
    pi <- draw(n * k, v[["pi"]], cl$dist)
    Y <- array(rep(outer(p, it, "+") + pi, r), c(n, k, r)) +
      array(draw_e(n * k * r, v[["e"]], cl$dist), c(n, k, r))
    fit <- mk_fit(ms_from_array(Y), n, k, r)
    out <- list()
    for (nm in names(specs)) {
      s <- specs[[nm]]
      est <- .gstudy_estimate(fit, s)
      methods <- if (.gstudy_is_exact(fit, s)) "exact" else c("mls", "satterthwaite")
      for (m in methods) {
        ci2 <- tryCatch(as.numeric(.gstudy_ci(fit, s, 0.95, "two.sided", m)),
                        error = function(e) c(NA, NA))
        out[[length(out) + 1]] <- data.frame(estimand = nm, ci_method = m, est = est,
                                             lower = ci2[1], upper = ci2[2])
      }
    }
    do.call(rbind, out)
  })
  res <- do.call(rbind, res)
  res$truth <- truth[res$estimand]
  agg <- do.call(rbind, lapply(split(res, list(res$estimand, res$ci_method), drop = TRUE), function(d) {
    ok <- is.finite(d$lower) & is.finite(d$upper)
    data.frame(estimand = d$estimand[1], ci_method = d$ci_method[1],
               truth = d$truth[1],
               coverage = mean(d$lower[ok] <= d$truth[1] & d$truth[1] <= d$upper[ok]),
               miss_low = mean(d$truth[1] < d$lower[ok]),
               miss_high = mean(d$truth[1] > d$upper[ok]),
               median_width = median(d$upper[ok] - d$lower[ok]),
               bias = mean(d$est) - d$truth[1],
               rel_bias = (mean(d$est) - d$truth[1]) / abs(d$truth[1]),
               n_fail = sum(!ok))
  }))
  cbind(cl[rep(1, nrow(agg)), ], agg, row.names = NULL)
}

cl <- makeCluster(14)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
clusterExport(cl, c("cells", "mixes", "estimands", "draw", "draw_e", "run_cell",
                    "ms_from_array", "mk_fit", "nsim", "log_file", "logp"))
logp("start", nrow(cells), "cells, nsim =", nsim)
out <- parLapplyLB(cl, seq_len(nrow(cells)), function(ci) {
  t0 <- Sys.time()
  res <- run_cell(ci)
  logp("cell", ci, "done in", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
  res
})
stopCluster(cl)
out <- do.call(rbind, out)
saveRDS(out, file.path(dirname(log_file), "gstudy_coverage.rds"))
logp("finished")
