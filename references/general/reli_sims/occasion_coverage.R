# Phase C2: coverage of the analytic reli_facet_test() intervals with
# crossed occasions (p x i x o, one measurement per cell), and the cost of
# ignoring the occasions (criterion C7).
# Usage: Rscript occasion_coverage.R <progress_log_file> [nsim]
# (writes occasion_coverage.rds next to the log). Run from the package root.
# Written 2026-10-20 against R/reli_facet_engine.R; criteria in
# reli_redesign.md, Section 15.7; results in Section 15.9.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 2000
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

mixes <- list(N = c(i = .05, o = .05, pi = .05, po = .05, io = .02, res = .30),
              D = c(i = .05, o = .30, pi = .05, po = .05, io = .05, res = .10),
              R = c(i = .30, o = .05, pi = .05, po = .05, io = .02, res = .10),
              X = c(i = .05, o = .05, pi = .25, po = .25, io = .10, res = .10))
cells <- expand.grid(n = c(10, 30), k = c(2, 3), m = c(2, 3),
                     mix = names(mixes), dist = c("normal", "nonnormal"),
                     stringsAsFactors = FALSE)

sp <- function(stat, facet, type, k_avg = 1, m_avg = 1) {
  list(stat = stat, facet = facet, type = type, k_avg = k_avg, m_avg = m_avg,
       r_avg = 1, scale = "raw")
}
estimands <- list(
  ICC_intra_C = sp("ICC", "intra", "consistency"),
  ICC_intra_A = sp("ICC", "intra", "agreement"),
  ICC_inter_C = sp("ICC", "inter", "consistency"),
  ICC_inter_A = sp("ICC", "inter", "agreement"),
  Erho_11 = sp("ICC", "overall", "consistency"),
  Phi_11 = sp("ICC", "overall", "agreement"),
  Erho_km = sp("ICC", "overall", "consistency", "k", "m"),
  Phi_km = sp("ICC", "overall", "agreement", "k", "m"),
  SEM_intra_C = sp("SEM", "intra", "consistency"),
  SEM_intra_A = sp("SEM", "intra", "agreement"),
  SEM_inter_C = sp("SEM", "inter", "consistency"),
  SEM_inter_A = sp("SEM", "inter", "agreement"),
  SEM_overall_C = sp("SEM", "overall", "consistency"),
  SEM_overall_A = sp("SEM", "overall", "agreement"))

draw <- function(len, v, dist) {
  if (dist == "normal") return(rnorm(len, 0, sqrt(v)))
  sqrt(v) * (rchisq(len, 3) - 3) / sqrt(6)
}
draw_e <- function(len, v, dist) {
  if (dist == "normal") return(rnorm(len, 0, sqrt(v)))
  sqrt(v * 3 / 5) * rt(len, 5)
}

# n x k x m array of measurements
sim_array <- function(n, k, m, v, dist) {
  p <- draw(n, 1, dist); it <- draw(k, v[["i"]], dist); o <- draw(m, v[["o"]], dist)
  pi <- matrix(draw(n * k, v[["pi"]], dist), n, k)
  po <- matrix(draw(n * m, v[["po"]], dist), n, m)
  io <- matrix(draw(k * m, v[["io"]], dist), k, m)
  Y <- array(0, c(n, k, m))
  for (l in seq_len(m)) {
    Y[, , l] <- outer(p, it, "+") + o[l] + pi + po[, l] +
      matrix(io[, l], n, k, byrow = TRUE)
  }
  Y + array(draw_e(n * k * m, v[["res"]], dist), c(n, k, m))
}

crossed_fit <- function(Y) {
  d <- dim(Y)
  eff <- .facet_effects(c("p", "i", "o"), replicated = FALSE)
  am <- .facet_anova_ms(Y, 1, 0, eff)
  f <- list(method = "anova", design = "crossed", factors = c("p", "i", "o"),
            effects = eff, levels = c(p = d[1], i = d[2], o = d[3]),
            n = d[1], k = d[2], r = 1, balanced = TRUE, replicated = FALSE,
            grand_mean = mean(Y), ms = am$ms, df = am$df)
  structure(.facet_refresh(f), class = "facet_fit")
}

# the repeat model fitted to the same data (occasions as repeats)
nested_fit <- function(Y) {
  d <- dim(Y)
  A <- rowMeans(Y, dims = 2)
  ssw <- sum((Y - c(A))^2)
  eff <- c("p", "i", "pi", "e")
  am <- .facet_anova_ms(A, d[3], ssw, eff)
  f <- list(method = "anova", design = "nested", factors = c("p", "i"),
            effects = eff, levels = c(p = d[1], i = d[2]), n = d[1], k = d[2],
            r = d[3], balanced = TRUE, replicated = TRUE, grand_mean = mean(Y),
            ms = am$ms, df = am$df)
  structure(.facet_refresh(f), class = "facet_fit")
}

truth_of <- function(s, v) {
  vc <- c(p = 1, i = v[["i"]], o = v[["o"]], pi = v[["pi"]], po = v[["po"]],
          io = v[["io"]], pio = v[["res"]])
  f <- list(design = "crossed", factors = c("p", "i", "o"), effects = names(vc))
  w <- .facet_weights(f, s)
  if (s$stat == "SEM") sqrt(sum(w$var * vc)) else sum(w$num * vc) / sum(w$den * vc)
}

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; m <- cl$m
  v <- mixes[[cl$mix]]
  specs <- lapply(estimands, function(s) {
    if (identical(s$k_avg, "k")) s$k_avg <- k
    if (identical(s$m_avg, "m")) s$m_avg <- m
    s
  })
  truth <- vapply(specs, truth_of, numeric(1), v = v)
  set.seed(2000 + ci)
  rows <- vector("list", nsim)
  for (sim in seq_len(nsim)) {
    Y <- sim_array(n, k, m, v, cl$dist)
    fit <- crossed_fit(Y)
    out <- list()
    for (nm in names(specs)) {
      s <- specs[[nm]]
      est <- .facet_estimate(fit, s)
      methods <- if (.facet_is_exact(fit, s)) "exact" else c("mls", "satterthwaite")
      for (meth in methods) {
        ci2 <- tryCatch(as.numeric(.facet_ci(fit, s, 0.95, "two.sided", meth)),
                        error = function(e) c(NA, NA))
        out[[length(out) + 1]] <- c(est, ci2, match(meth, c("exact", "mls", "satterthwaite")),
                                    match(nm, names(specs)))
      }
    }
    # C7: the repeat model's intra SEM and ICC, against the crossed truths
    nf <- nested_fit(Y)
    for (s2 in list(list(nm = "nested_SEM_intra", stat = "SEM"),
                    list(nm = "nested_ICC_intra", stat = "ICC"))) {
      ns <- list(stat = s2$stat, facet = "intra", type = "consistency", k_avg = 1,
                 m_avg = 1, r_avg = 1, scale = "raw")
      ci2 <- as.numeric(.facet_ci(nf, ns, 0.95, "two.sided", "mls"))
      out[[length(out) + 1]] <- c(.facet_estimate(nf, ns), ci2, 4,
                                  100 + match(s2$nm, c("nested_SEM_intra", "nested_ICC_intra")))
    }
    rows[[sim]] <- do.call(rbind, out)
  }
  res <- as.data.frame(do.call(rbind, rows))
  names(res) <- c("est", "lower", "upper", "meth", "j")
  res$ci_method <- c("exact", "mls", "satterthwaite", "nested")[res$meth]
  res$estimand <- ifelse(res$j > 100, c("nested_SEM_intra", "nested_ICC_intra")[pmax(res$j - 100, 1)],
                         names(specs)[pmin(res$j, length(specs))])
  # the nested estimates are judged against the crossed agreement and
  # consistency truths (what a user would take them to estimate)
  truth_all <- c(truth,
                 nested_SEM_intra_A = truth[["SEM_intra_A"]],
                 nested_SEM_intra_C = truth[["SEM_intra_C"]],
                 nested_ICC_intra_A = truth[["ICC_intra_A"]],
                 nested_ICC_intra_C = truth[["ICC_intra_C"]])
  agg <- list()
  for (key in unique(paste(res$estimand, res$ci_method))) {
    d <- res[paste(res$estimand, res$ci_method) == key, ]
    ok <- is.finite(d$lower) & is.finite(d$upper)
    targets <- if (d$estimand[1] %in% c("nested_SEM_intra", "nested_ICC_intra")) {
      paste0(d$estimand[1], c("_A", "_C"))
    } else d$estimand[1]
    for (tg in targets) {
      tr <- truth_all[[tg]]
      agg[[length(agg) + 1]] <- data.frame(
        estimand = tg, ci_method = d$ci_method[1], truth = tr,
        coverage = mean(d$lower[ok] <= tr & tr <= d$upper[ok]),
        miss_low = mean(tr < d$lower[ok]), miss_high = mean(tr > d$upper[ok]),
        median_width = stats::median(d$upper[ok] - d$lower[ok]),
        rel_bias = (mean(d$est) - tr) / abs(tr), n_fail = sum(!ok))
    }
  }
  agg <- do.call(rbind, agg)
  cbind(cl[rep(1, nrow(agg)), ], agg, row.names = NULL)
}

cl <- makeCluster(14)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
clusterExport(cl, c("cells", "mixes", "estimands", "sp", "draw", "draw_e", "sim_array",
                    "crossed_fit", "nested_fit", "truth_of", "run_cell", "nsim",
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
saveRDS(out, file.path(dirname(log_file), "occasion_coverage.rds"))
logp("finished")
