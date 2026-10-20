# Part B: accuracy of deming_power_sim() and deming_sample_size().
#
# Run from the package root:
#   Rscript references/general/eiv_sims/sim_power.R <stage> [reps]
#   stage = truth      10,000 data sets per scenario x n, fitted the way a
#                      user would (dem_reg() + joint_test(), F and chi-sq)
#           powersim   [reps = 20] runs of deming_power_sim(n_sims = 1000)
#                      per scenario x n
#           samplesize [reps = 50] runs of deming_sample_size() (defaults:
#                      target 0.90, n_sims 500, step 5) per scenario P1-P4
# Results go to $EIV_SIMS_OUT (default references/working/eiv_out).

args <- commandArgs(trailingOnly = TRUE)
stage <- args[1]
dir <- "references/general/eiv_sims"
out_dir <- Sys.getenv("EIV_SIMS_OUT", "references/working/eiv_out")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
source(file.path(dir, "gen.R"))

n_truth <- c(10, 15, 20, 30, 40, 50, 60, 80, 100, 125, 150, 200, 250, 300)
n_check <- c(20, 50, 100, 200)

library(parallel)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterCall(cl, function(dir) {
  source(file.path(dir, "sim_funs.R")); worker_init(dir)
}, dir = dir))

t0 <- Sys.time()
if (stage == "truth") {
  nsim <- if (length(args) >= 2) as.integer(args[2]) else 10000
  tasks <- list()
  for (k in seq_along(power_scenarios)) for (n in n_truth) {
    base <- 4e7 + (k * 1000 + n) * 1e4   # one seed stream per scenario x n
    for (s in split(seq_len(nsim), ceiling(seq_len(nsim) / 250))) {
      tasks[[length(tasks) + 1]] <- list(scenario = names(power_scenarios)[k],
                                         sc = power_scenarios[[k]], n = n,
                                         seeds = base + s)
    }
  }
  tasks <- tasks[order(-vapply(tasks, function(t) t$n, 1))]
  res <- parLapplyLB(cl, tasks, function(t) {
    m <- run_truth_chunk(t)
    data.frame(scenario = t$scenario, n = t$n, m)
  })
  out <- do.call(rbind, res)
} else if (stage == "powersim") {
  reps <- if (length(args) >= 2) as.integer(args[2]) else 20
  tasks <- list()
  for (k in seq_along(power_scenarios)) for (n in n_check) for (r in seq_len(reps)) {
    tasks[[length(tasks) + 1]] <- list(scenario = names(power_scenarios)[k],
                                       sc = power_scenarios[[k]], n = n,
                                       n_sims = 1000,
                                       seed = 5e7 + (k * 1000 + n) * 100 + r)
  }
  tasks <- tasks[order(-vapply(tasks, function(t) t$n, 1))]
  res <- parLapplyLB(cl, tasks, function(t) {
    data.frame(scenario = t$scenario, t(one_powersim(t)))
  })
  out <- do.call(rbind, res)
} else if (stage == "samplesize") {
  reps <- if (length(args) >= 2) as.integer(args[2]) else 50
  tasks <- list()
  for (k in 1:4) for (r in seq_len(reps)) {
    tasks[[length(tasks) + 1]] <- list(scenario = names(power_scenarios)[k],
                                       sc = power_scenarios[[k]], target = 0.90,
                                       seed = 6e7 + k * 1000 + r)
  }
  res <- parLapplyLB(cl, tasks, function(t) {
    data.frame(scenario = t$scenario, t(one_samplesize(t)))
  })
  out <- do.call(rbind, res)
} else {
  stop("stage must be truth, powersim or samplesize")
}
cat(stage, nrow(out), "rows:", format(round(Sys.time() - t0, 1)), "\n")
saveRDS(out, file.path(out_dir, sprintf("power_%s.rds", stage)))
stopCluster(cl)
