# Interval coverage of ccc_test() for data_type = "reps" and "nest":
# the current delete-one-subject jackknife-t against a subject (cluster)
# bootstrap: percentile, BCa and studentized. See README.md for the pass
# criteria (set before running) and the results.
#
# Run from the package root:
#   Rscript references/general/ccc_sims/sim_ci.R <type> <stage> [nsim]
#   type  = reps | nest
#   stage = pilot | main | stud
#     main: all 216 cells; jackknife-t, percentile, BCa (B = 999)
#     stud: the studentized cells (nest: all; reps: g = 10 and 20, plus 12
#           cells at g = 50), studentized with an inner jackknife on the
#           first 499 resamples. Uses the same seeds as main, so the data
#           sets are the first nsim of main's.
# Per-data-set results go to $CCC_SIMS_OUT (default: tempdir), one RDS per
# cell; summarise.R tabulates them.
#
# Estimators (same as R/ccc_test.R):
#   reps: variance-components CCC (fast_vc.R, a C++ REML that matches
#         .ccc_vc() / lme4 to 5e-5, see verify_fast_vc.R)
#   nest: Lin's CCC over the complete pairs
# A bootstrap resample draws g subjects with replacement; a subject drawn
# twice counts as two subjects (fresh ids), so a resample is a vector of
# counts w. The inner jackknife of a resample deletes one drawn copy at a
# time (copies of one subject give the same value, so it is weighted).

args <- commandArgs(trailingOnly = TRUE)
type <- args[1]; stage <- args[2]
nsim <- if (length(args) >= 3) as.integer(args[3]) else
  switch(stage, pilot = 20, main = 1000, stud = 500)
B <- 999; Bs <- 499; level <- 0.95
dir <- "references/general/ccc_sims"
out_dir <- Sys.getenv("CCC_SIMS_OUT", file.path(tempdir(), "ccc_sims_out"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

grid <- expand.grid(g = c(10, 20, 50), k = c(2, 3, 6), rho = c(.5, .8, .95),
                    diff = c(FALSE, TRUE), miss = c(FALSE, TRUE),
                    dist = c("normal", "nonnormal"),
                    stringsAsFactors = FALSE)
grid$cell <- seq_len(nrow(grid))
# cells that also get the studentized interval
stud_cell <- function(gr, type) {
  type == "nest" | gr$g < 50 | (gr$k == 3 & gr$miss)
}
if (stage == "stud") grid <- grid[stud_cell(grid, type), ]
if (stage == "pilot") {
  grid <- grid[grid$k == 3 & grid$rho == .8 & grid$diff & grid$miss &
                 grid$dist == "normal", ]
}

source(file.path(dir, "sim_funs.R"))

# ---- driver --------------------------------------------------------------
library(parallel)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterCall(cl, worker_init, dir = dir))
clusterExport(cl, c("make_estimator", "jack_se", "one_data_set", "run_chunk",
                    "B", "Bs"))

chunk <- 10
tasks <- list()
for (r in seq_len(nrow(grid))) {
  cell <- as.list(grid[r, ])
  base <- (if (type == "reps") 1e6 else 2e6) + cell$cell * 1e4
  # reps main skips the seeds the stud stage runs (stud also returns the
  # jackknife-t, percentile and BCa intervals for them)
  first <- if (stage == "main" && type == "reps" &&
               stud_cell(grid[r, ], type)) 501 else 1
  sims <- seq.int(first, nsim)
  for (s in split(sims, ceiling(seq_along(sims) / chunk))) {
    tasks[[length(tasks) + 1]] <- list(cell = cell, base = base,
                                       seeds = base + s)
  }
}
t0 <- Sys.time()
res <- parLapplyLB(cl, tasks, run_chunk, type = type,
                   stud = stage %in% c("stud", "pilot"))
cat(type, stage, nrow(grid), "cells x", nsim, "sims:",
    format(round(Sys.time() - t0, 1)), "\n")
cells <- vapply(tasks, function(t) t$cell$cell, numeric(1))
for (r in seq_len(nrow(grid))) {
  cid <- grid$cell[r]
  m <- do.call(rbind, res[cells == cid])
  saveRDS(list(cell = grid[r, ], type = type, stage = stage, res = m),
          file.path(out_dir, sprintf("%s_%s_cell%03d.rds", type, stage, cid)))
}
stopCluster(cl)
