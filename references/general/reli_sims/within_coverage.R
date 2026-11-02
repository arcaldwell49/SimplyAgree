# Coverage of reli_facet_test(facet = "within") (trending_agreement_plan.md,
# "Follow-up: a within-subject facet"): crossed p x i x o data, one
# measurement per cell, ANOVA (balanced) and REML (20% of measurements
# missing). Written 2026-11-02.
# Usage, from the package root:
#   Rscript references/general/reli_sims/within_coverage.R [nsim]
# Writes within_coverage.csv next to this script.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
nsim <- if (length(args) > 0) as.integer(args[1]) else 2000
out_dir <- "references/general/reli_sims"

# variance components (p = 1): T = trending (real within-subject change
# large), S = small change, Z = no change (po at the boundary), I = large
# item-by-occasion disagreement
mixes <- list(T = c(i = .1, o = .1, pi = .1, po = .5, io = .02, res = .2),
              S = c(i = .1, o = .1, pi = .1, po = .05, io = .02, res = .2),
              Z = c(i = .1, o = .1, pi = .1, po = 0, io = .02, res = .2),
              I = c(i = .1, o = .1, pi = .1, po = .3, io = .2, res = .2))
cells <- expand.grid(n = c(10, 30), k = c(2, 3), m = c(3, 6),
                     mix = names(mixes), method = c("anova", "reml"),
                     stringsAsFactors = FALSE)

sp <- function(stat, type, k_avg = 1) {
  list(stat = stat, facet = "within", type = type, k_avg = k_avg,
       m_avg = 1, r_avg = 1, scale = "raw")
}
estimands <- list(ICC_C = sp("ICC", "consistency"),
                  ICC_A = sp("ICC", "agreement"),
                  ICC_C_k = sp("ICC", "consistency", "k"),
                  SEM_C = sp("SEM", "consistency"),
                  SEM_A = sp("SEM", "agreement"))

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".",
                                                               quiet = TRUE))))

run_cell <- function(ci, cells, mixes, estimands, nsim) {
  ns <- asNamespace("SimplyAgree")
  cel <- cells[ci, ]
  n <- cel$n; k <- cel$k; m <- cel$m
  v <- mixes[[cel$mix]]
  specs <- lapply(estimands, function(s) {
    if (identical(s$k_avg, "k")) s$k_avg <- k
    s
  })
  vc <- c(p = 1, i = v[["i"]], o = v[["o"]], pi = v[["pi"]], po = v[["po"]],
          io = v[["io"]], pio = v[["res"]])
  f0 <- list(design = "crossed", factors = c("p", "i", "o"),
             effects = names(vc))
  truth <- vapply(specs, function(s) {
    w <- ns$.facet_weights(f0, s)
    if (s$stat == "SEM") sqrt(sum(w$var * vc)) else
      sum(w$num * vc) / sum(w$den * vc)
  }, numeric(1))
  set.seed(5000 + ci)
  res <- matrix(NA, nsim, length(specs) * 3)
  for (s in seq_len(nsim)) {
    d <- expand.grid(occ = seq_len(m), items = seq_len(k), id = seq_len(n))
    p <- rnorm(n); it <- rnorm(k, 0, sqrt(v[["i"]]))
    o <- rnorm(m, 0, sqrt(v[["o"]]))
    pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
    po <- matrix(rnorm(n * m, 0, sqrt(v[["po"]])), n, m)
    io <- matrix(rnorm(k * m, 0, sqrt(v[["io"]])), k, m)
    d$values <- p[d$id] + it[d$items] + o[d$occ] + pi[cbind(d$id, d$items)] +
      po[cbind(d$id, d$occ)] + io[cbind(d$items, d$occ)] +
      rnorm(nrow(d), 0, sqrt(v[["res"]]))
    if (cel$method == "reml") {
      d <- d[-sample(nrow(d), round(0.2 * nrow(d))), ]
    }
    d$items <- factor(d$items); d$id <- factor(d$id); d$occ <- factor(d$occ)
    prep <- ns$.facet_prepare(values ~ items | id, data = d, occasion = "occ")
    fit <- tryCatch(suppressWarnings(suppressMessages(
      ns$.facet_fit(prep, method = cel$method, keep_fit = FALSE))),
      error = function(e) NULL)
    if (is.null(fit)) next
    out <- unlist(lapply(specs, function(sp_) {
      est <- ns$.facet_estimate(fit, sp_)
      meth <- if (ns$.facet_is_exact(fit, sp_)) "exact" else "mls"
      ci2 <- tryCatch(as.numeric(ns$.facet_ci(fit, sp_, 0.95, "two.sided",
                                              meth)),
                      error = function(e) c(NA, NA))
      c(est, ci2)
    }))
    res[s, ] <- out
  }
  do.call(rbind, lapply(seq_along(specs), function(j) {
    e <- res[, 3 * j - 2]; lo <- res[, 3 * j - 1]; hi <- res[, 3 * j]
    ok <- !is.na(lo) & !is.na(hi)
    data.frame(cel, estimand = names(specs)[j], truth = truth[[j]],
               n_ok = sum(ok), mean_est = mean(e, na.rm = TRUE),
               cover = mean(lo[ok] <= truth[[j]] & truth[[j]] <= hi[ok]),
               miss_low = mean(hi[ok] < truth[[j]]),
               miss_high = mean(lo[ok] > truth[[j]]))
  }))
}
clusterExport(cl, "run_cell")
out <- do.call(rbind, parLapplyLB(cl, seq_len(nrow(cells)), run_cell,
                                  cells = cells, mixes = mixes,
                                  estimands = estimands, nsim = nsim))
write.csv(out, file.path(out_dir, "within_coverage.csv"), row.names = FALSE)
print(stats::aggregate(cover ~ estimand + method + mix, data = out,
                       FUN = function(x) sprintf("%.3f-%.3f", min(x), max(x))))
