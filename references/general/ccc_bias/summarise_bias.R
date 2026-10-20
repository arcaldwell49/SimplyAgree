# Tabulate sim_bias.R output against the criteria in README.md. Run from the
# package root with CCC_BIAS_OUT set as for sim_bias.R. Writes
# references/general/ccc_bias/summary_bias.csv (one row per type x cell).
out_dir <- Sys.getenv("CCC_BIAS_OUT", file.path(tempdir(), "ccc_bias_out"))
files <- list.files(out_dir, "^(reps|nest)_bias_cell\\d+\\.rds$",
                    full.names = TRUE)

cover <- function(lo, hi, truth) {
  ok <- is.finite(lo) & is.finite(hi)
  c(cov = mean(lo[ok] <= truth[ok] & truth[ok] <= hi[ok]),
    miss_hi = mean(truth[ok] > hi[ok]), miss_lo = mean(truth[ok] < lo[ok]),
    fail = mean(!ok))
}
pass <- function(v) {
  v[["cov"]] >= .925 & v[["cov"]] <= .975 & v[["fail"]] < .01
}
pass_tails <- function(v) {
  pass(v) & v[["miss_hi"]] >= .0125 & v[["miss_hi"]] <= .0375 &
    v[["miss_lo"]] >= .0125 & v[["miss_lo"]] <= .0375
}

rows <- lapply(files, function(f) {
  x <- readRDS(f)
  m <- x$res
  tr <- m[, "truth"]
  est_stats <- unlist(lapply(c("raw", "jk_r", "jk_z", "n1"), function(e) {
    d <- m[, e] - tr
    stats::setNames(c(mean(d, na.rm = TRUE), sqrt(mean(d^2, na.rm = TRUE)),
                      median(abs(d), na.rm = TRUE)),
                    paste0(e, c("_bias", "_rmse", "_mae")))
  }))
  jk <- cover(m[, "jk_lo"], m[, "jk_hi"], tr)
  bc <- cover(m[, "bc_lo"], m[, "bc_hi"], tr)
  data.frame(type = x$type, x$cell, nsim = nrow(m),
             as.list(est_stats), clamp = mean(m[, "clamp"], na.rm = TRUE),
             jk_cov = jk[["cov"]], jk_hi = jk[["miss_hi"]],
             jk_lo = jk[["miss_lo"]], jk_pass = pass(jk),
             jk_pass_tails = pass_tails(jk),
             bc_cov = bc[["cov"]], bc_hi = bc[["miss_hi"]],
             bc_lo = bc[["miss_lo"]], bc_pass = pass(bc),
             bc_pass_tails = pass_tails(bc),
             width_ratio = median(m[, "bc_hi"] - m[, "bc_lo"], na.rm = TRUE) /
               median(m[, "jk_hi"] - m[, "jk_lo"], na.rm = TRUE))
})
summ <- do.call(rbind, rows)
write.csv(summ, "references/general/ccc_bias/summary_bias.csv",
          row.names = FALSE)

options(width = 160)
show <- function(by, cols) {
  a <- aggregate(summ[cols], summ[by], mean)
  a[order(a$type), ]
}
est_cols <- c("raw_bias", "jk_r_bias", "jk_z_bias", "raw_rmse", "jk_r_rmse",
              "jk_z_rmse", "clamp")
int_cols <- c("jk_cov", "jk_hi", "jk_lo", "jk_pass", "jk_pass_tails",
              "bc_cov", "bc_hi", "bc_lo", "bc_pass", "bc_pass_tails")
for (by in list("g", c("g", "dist"), "rho", "k")) {
  cat("\n== Estimates by type x", paste(by, collapse = " x "), "==\n")
  print(show(c("type", by), est_cols), digits = 3, row.names = FALSE)
  cat("\n== Intervals by type x", paste(by, collapse = " x "), "==\n")
  print(show(c("type", by), int_cols), digits = 3, row.names = FALSE)
}
cat("\n== nest: n - 1 form ==\n")
print(show(c("type", "g"), c("raw_bias", "n1_bias", "raw_rmse", "n1_rmse"))[
  show(c("type", "g"), "raw_bias")$type == "nest", ], digits = 3,
  row.names = FALSE)

cat("\n== Criteria ==\n")
for (ty in unique(summ$type)) {
  s <- summ[summ$type == ty, ]
  for (e in c("jk_r", "jk_z")) {
    cut <- sapply(c(10, 20), function(g) {
      1 - mean(abs(s[s$g == g, paste0(e, "_bias")])) /
        mean(abs(s[s$g == g, "raw_bias"]))
    })
    worst <- max(s[s$g == 10, paste0(e, "_rmse")] / s[s$g == 10, "raw_rmse"])
    cat(ty, e, ": |bias| cut g=10", sprintf("%.0f%%", 100 * cut[1]),
        " g=20", sprintf("%.0f%%", 100 * cut[2]),
        "; worst g=10 RMSE ratio", sprintf("%.3f", worst),
        if (all(cut >= .5) && worst <= 1.1) "-> PASS" else "-> fail", "\n")
  }
}
