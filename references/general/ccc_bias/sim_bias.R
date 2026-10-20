# Bias of the ccc_test() estimate for replicate and nested data, and the
# jackknife bias corrections (see README.md for the candidates and the
# criteria set before running).
#
# Run from the package root:
#   Rscript references/general/ccc_bias/sim_bias.R <type> [nsim]
#   type = reps | nest
# Uses the generators, estimators and seeds of ../ccc_sims/ (sim_ci.R), so
# data set s of cell c is the same data set as there. One RDS per cell goes
# to $CCC_BIAS_OUT (default: tempdir).

args <- commandArgs(trailingOnly = TRUE)
type <- args[1]
nsim <- if (length(args) >= 2) as.integer(args[2]) else 1000
sims_dir <- "references/general/ccc_sims"
out_dir <- Sys.getenv("CCC_BIAS_OUT", file.path(tempdir(), "ccc_bias_out"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

grid <- expand.grid(g = c(10, 20, 50), k = c(2, 3, 6), rho = c(.5, .8, .95),
                    diff = c(FALSE, TRUE), miss = c(FALSE, TRUE),
                    dist = c("normal", "nonnormal"),
                    stringsAsFactors = FALSE)
grid$cell <- seq_len(nrow(grid))

# Lin's CCC with n - 1 variances and covariance (nested data only)
ccc_n1 <- function(dat) {
  d <- dat[complete.cases(dat), ]
  2 * cov(d$x, d$y) / (var(d$x) + var(d$y) + (mean(d$x) - mean(d$y))^2)
}

bias_one <- function(seed, cell, type) {
  set.seed(seed)
  sim <- gen_data(type, cell$g, cell$k, cell$rho, cell$diff, cell$miss,
                  cell$dist)
  e <- make_estimator(sim$dat, type)
  g <- e$g
  out <- c(truth = sim$truth, g = g, raw = NA, jk_r = NA, jk_z = NA, n1 = NA,
           clamp = NA, se_z = NA, jk_lo = NA, jk_hi = NA, bc_lo = NA,
           bc_hi = NA)
  est <- e$est_w(matrix(1, g, 1))
  if (!is.finite(est) || abs(est) >= 1) return(out)
  loo <- e$est_w(1 - diag(g))
  if (!all(is.finite(loo)) || any(abs(loo) >= 1)) return(out)
  z0 <- atanh(est)
  z_loo <- atanh(loo)
  se <- sqrt((g - 1) / g * sum((z_loo - mean(z_loo))^2))
  jk_r <- g * est - (g - 1) * mean(loo)
  z_bc <- g * z0 - (g - 1) * mean(z_loo)
  q <- qt(0.975, g - 1)
  out[c("raw", "jk_r", "jk_z", "clamp", "se_z", "jk_lo", "jk_hi", "bc_lo",
        "bc_hi")] <-
    c(est, min(max(jk_r, -1), 1), tanh(z_bc), abs(jk_r) > 1, se,
      tanh(z0 - q * se), tanh(z0 + q * se), tanh(z_bc - q * se),
      tanh(z_bc + q * se))
  if (type == "nest") out["n1"] <- ccc_n1(sim$dat)
  out
}

run_chunk <- function(task, type) {
  m <- t(vapply(task$seeds, bias_one, numeric(12), cell = task$cell,
                type = type))
  cbind(m, seed = task$seeds - task$base)
}

library(parallel)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterCall(cl, function(dir) {
  source(file.path(dir, "sim_funs.R"))
  worker_init(dir)
}, dir = sims_dir))
clusterExport(cl, c("bias_one", "ccc_n1", "run_chunk"))

tasks <- list()
for (r in seq_len(nrow(grid))) {
  cell <- as.list(grid[r, ])
  # the seeds of ../ccc_sims/sim_ci.R
  base <- (if (type == "reps") 1e6 else 2e6) + cell$cell * 1e4
  for (s in split(seq_len(nsim), ceiling(seq_len(nsim) / 50))) {
    tasks[[length(tasks) + 1]] <- list(cell = cell, base = base,
                                       seeds = base + s)
  }
}
t0 <- Sys.time()
res <- parLapplyLB(cl, tasks, run_chunk, type = type)
cat(type, nrow(grid), "cells x", nsim, "sims:",
    format(round(Sys.time() - t0, 1)), "\n")
cells <- vapply(tasks, function(t) t$cell$cell, numeric(1))
for (r in seq_len(nrow(grid))) {
  cid <- grid$cell[r]
  saveRDS(list(cell = grid[r, ], type = type,
               res = do.call(rbind, res[cells == cid])),
          file.path(out_dir, sprintf("%s_bias_cell%03d.rds", type, cid)))
}
stopCluster(cl)
