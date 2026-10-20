# Part A: type I error of joint_test() (and marginal CI coverage) for
# dem_reg() and pb_reg() with H0 true (intercept 0, slope 1) and the data
# generated under the fitted model's assumptions. See README.md.
#
# Run from the package root:
#   Rscript references/general/eiv_sims/sim_type1.R <block> [nsim]
#   block = dem | reps | pb
# One RDS per cell in $EIV_SIMS_OUT (default references/working/eiv_out).

args <- commandArgs(trailingOnly = TRUE)
block <- args[1]
nsim <- if (length(args) >= 2) as.integer(args[2]) else
  switch(block, dem = 5000, reps = 5000, pb = 1000)
dir <- "references/general/eiv_sims"
out_dir <- Sys.getenv("EIV_SIMS_OUT", "references/working/eiv_out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ranges <- list(narrow = c(50, 100), wide = c(5, 100))
add_range <- function(g) {
  g$lo <- vapply(g$range, function(r) ranges[[r]][1], numeric(1))
  g$hi <- vapply(g$range, function(r) ranges[[r]][2], numeric(1))
  g
}

grid <- switch(block,
  dem = expand.grid(n = c(10, 20, 50, 100), range = names(ranges),
                    lambda = c(1, 4), err = c(0.05, 0.20),
                    var_type = c("const", "prop"), stringsAsFactors = FALSE),
  reps = expand.grid(n = c(10, 20, 50, 100), range = names(ranges),
                     lambda = c(1, 4), err = 0.20, stringsAsFactors = FALSE),
  pb = rbind(
    expand.grid(n = c(20, 50, 100), range = names(ranges), err = c(0.05, 0.20),
                lambda = c(1, 4), method = "scissors", stringsAsFactors = FALSE),
    expand.grid(n = 50, range = names(ranges), err = 0.20, lambda = 1,
                method = c("symmetric", "invariant"), stringsAsFactors = FALSE)))
grid <- add_range(grid)
if (block == "pb") grid$B <- 999
grid$cell <- seq_len(nrow(grid))
fun <- switch(block, dem = "one_dem", reps = "one_reps", pb = "one_pb")

library(parallel)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterCall(cl, function(dir) {
  source(file.path(dir, "sim_funs.R")); worker_init(dir)
}, dir = dir))

chunk <- if (block == "pb") 5 else 50
base0 <- switch(block, dem = 1e7, reps = 2e7, pb = 3e7)
tasks <- list()
for (r in seq_len(nrow(grid))) {
  cell <- as.list(grid[r, ])
  base <- base0 + cell$cell * 1e5
  for (s in split(seq_len(nsim), ceiling(seq_len(nsim) / chunk))) {
    tasks[[length(tasks) + 1]] <- list(cell = cell, seeds = base + s)
  }
}
# biggest PB cells first so the load balances
if (block == "pb") tasks <- tasks[order(-vapply(tasks, function(t) t$cell$n, 1))]

t0 <- Sys.time()
for (i in seq_along(tasks)) tasks[[i]]$fun <- fun
res <- parLapplyLB(cl, tasks, function(t) run_chunk(t, t$fun))
cat(block, nrow(grid), "cells x", nsim, "sims:",
    format(round(Sys.time() - t0, 1)), "\n")
cells <- vapply(tasks, function(t) t$cell$cell, numeric(1))
for (r in seq_len(nrow(grid))) {
  cid <- grid$cell[r]
  saveRDS(list(cell = grid[r, ], block = block,
               res = do.call(rbind, res[cells == cid])),
          file.path(out_dir, sprintf("type1_%s_cell%03d.rds", block, cid)))
}
stopCluster(cl)
