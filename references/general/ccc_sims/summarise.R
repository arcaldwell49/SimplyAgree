# Tabulate sim_ci.R output against the pass criteria in README.md.
# Run from the package root with CCC_SIMS_OUT set as for sim_ci.R. Writes
# references/general/ccc_sims/summary.csv (one row per type x cell x method).
#
# Criteria (set before running):
#   coverage of the 95% interval in [.925, .975]
#   each tail's miss rate in [.0125, .0375]
#   failures (no interval) < 1% of data sets
out_dir <- Sys.getenv("CCC_SIMS_OUT", file.path(tempdir(), "ccc_sims_out"))
files <- list.files(out_dir, "^(reps|nest)_(main|stud)_cell\\d+\\.rds$",
                    full.names = TRUE)

with_seed <- function(x) {
  m <- x$res
  if (!"seed" %in% colnames(m)) m <- cbind(m, seed = seq_len(nrow(m)))
  m
}

rows <- list()
for (f in files) {
  x <- readRDS(f)
  m <- with_seed(x)
  methods <- if (x$stage == "stud") "stud" else c("jk", "perc", "bca")
  for (meth in methods) {
    rows[[length(rows) + 1]] <- data.frame(
      type = x$type, as.list(x$cell), method = meth, seed = m[, "seed"],
      truth = m[, "truth"], est = m[, "est"],
      lo = m[, paste0(meth, "_lo")], hi = m[, paste0(meth, "_hi")])
  }
  # main-stage percentile/BCa/jackknife for the seeds the stud stage covers
  # (reps main skips them to save time)
  if (x$stage == "stud") {
    for (meth in c("jk", "perc", "bca")) {
      rows[[length(rows) + 1]] <- data.frame(
        type = x$type, as.list(x$cell), method = meth, seed = m[, "seed"],
        truth = m[, "truth"], est = m[, "est"],
        lo = m[, paste0(meth, "_lo")], hi = m[, paste0(meth, "_hi")])
    }
  }
}
all <- do.call(rbind, rows)
all <- all[!duplicated(all[, c("type", "cell", "method", "seed")]), ]

summ <- do.call(rbind, lapply(
  split(all, list(all$type, all$cell, all$method), drop = TRUE),
  function(d) {
    ok <- is.finite(d$lo) & is.finite(d$hi)
    n <- sum(ok)
    cover <- mean(d$lo[ok] <= d$truth[ok] & d$truth[ok] <= d$hi[ok])
    above <- mean(d$truth[ok] > d$hi[ok])   # interval entirely below truth
    below <- mean(d$truth[ok] < d$lo[ok])   # interval entirely above truth
    data.frame(d[1, c("type", "cell", "g", "k", "rho", "diff", "miss",
                      "dist", "method")],
               nsim = nrow(d), fail = mean(!ok), coverage = cover,
               mcse = sqrt(cover * (1 - cover) / n),
               miss_hi = above, miss_lo = below,
               width = median(d$hi[ok] - d$lo[ok]),
               bias = mean(d$est - d$truth, na.rm = TRUE),
               pass = cover >= .925 & cover <= .975 &
                 above >= .0125 & above <= .0375 &
                 below >= .0125 & below <= .0375 & mean(!ok) < .01,
               pass_cov = cover >= .925 & cover <= .975 & mean(!ok) < .01)
  }))
rownames(summ) <- NULL
write.csv(summ, "references/general/ccc_sims/summary.csv", row.names = FALSE)

pct <- function(v) sprintf("%.1f", 100 * v)
show <- function(by, s = summ) {
  a <- aggregate(cbind(coverage, miss_hi, miss_lo, pass_cov, pass) ~ .,
                 data = s[, c(by, "method", "coverage", "miss_hi", "miss_lo",
                              "pass_cov", "pass")], FUN = mean)
  a$cells <- aggregate(coverage ~ ., data = s[, c(by, "method", "coverage")],
                       FUN = length)$coverage
  a$min_cov <- aggregate(coverage ~ ., data = s[, c(by, "method", "coverage")],
                         FUN = min)$coverage
  a[order(a$type, a$method), ]
}
options(width = 160)
cat("\n== By type x g x method (mean over cells; pass = share of cells) ==\n")
print(show(c("type", "g")), digits = 3, row.names = FALSE)
for (f in c("k", "rho", "diff", "miss", "dist")) {
  cat("\n== By type x", f, "x method ==\n")
  print(show(c("type", f)), digits = 3, row.names = FALSE)
}
