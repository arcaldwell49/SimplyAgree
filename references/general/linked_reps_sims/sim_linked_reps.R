# Size and power of linked_reps_test() (README.md).
# Run from the package root:
#   Rscript references/general/linked_reps_sims/sim_linked_reps.R
# Settings (environment variables):
#   LRT_NSIM_NULL  data sets per cell with omega = 0 (default 1000)
#   LRT_NSIM_ALT   data sets per cell with omega > 0 (default 300)
#   LRT_NPERM      permutations per test (default 49)
#   LRT_CORES      worker processes (default: all cores but 2)
#   LRT_CELLS      optional comma-separated cell numbers to run (e.g. "1,2")
# Each finished cell is saved to raw/cell_<i>.rds, and cells already saved
# are skipped, so the script can be stopped and restarted. summary.csv is
# rebuilt from all saved cells at the end.
library(parallel)

out_dir <- "references/general/linked_reps_sims"
raw_dir <- file.path(out_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

n_sim_null <- as.numeric(Sys.getenv("LRT_NSIM_NULL", "1000"))
n_sim_alt <- as.numeric(Sys.getenv("LRT_NSIM_ALT", "300"))
n_perm <- as.numeric(Sys.getenv("LRT_NPERM", "49"))
n_cores <- as.numeric(Sys.getenv("LRT_CORES",
                                 max(1, detectCores() - 2)))

# tau = 0.3 (method-by-item), sigma = 0.4 for every method (residual);
# omega (item-by-replicate, shared by the methods on an occasion) varies.
# Two methods in all designs; three methods in the 30-item, 3-replicate
# designs only.
grid <- expand.grid(omega = c(0, 0.2, 0.4), n_items = c(15, 30),
                    n_reps = c(2, 3), n_methods = 2,
                    stringsAsFactors = FALSE)
grid <- rbind(grid, data.frame(omega = c(0, 0.2, 0.4), n_items = 30,
                               n_reps = 3, n_methods = 3))
grid$cell <- seq_len(nrow(grid))

cells <- Sys.getenv("LRT_CELLS", "")
run_cells <- if (nzchar(cells)) {
  as.integer(strsplit(cells, ",")[[1]])
} else {
  grid$cell
}

cl <- makeCluster(n_cores)
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".",
                                                               quiet = TRUE))))

# one data set: returns the test result and, for the first pair, the SD of
# the differences from the linked fit, the permuted fits (mean) and an
# exchangeable fit
one_rep <- function(s, g, n_perm) {
  meth <- c("A", "B", "C")[seq_len(g$n_methods)]
  dat <- sim_methods(n_items = g$n_items, n_reps = g$n_reps,
                     methods = meth, alpha = c(0, 0.5, -0.3)[seq_along(meth)],
                     tau = 0.3, omega = g$omega,
                     sigma = rep(0.4, length(meth)),
                     replicates = "linked", seed = 100000 * g$cell + s)
  out <- c(p = NA, omega_hat = NA, omega_perm_mean = NA, sd_linked = NA,
           sd_perm = NA, sd_exch = NA, n_ok = NA)
  fit <- tryCatch(suppressWarnings(suppressMessages(
    method_vc(dat, y = "y", method = "method", id = "id",
              replicate = "repl", replicates = "linked"))),
    error = function(e) NULL)
  if (is.null(fit)) return(out)
  res <- tryCatch(suppressWarnings(
    linked_reps_test(fit, n_perm = n_perm, seed = s)),
    error = function(e) NULL)
  if (is.null(res)) return(out)
  fe <- tryCatch(suppressWarnings(suppressMessages(
    method_vc(dat, y = "y", method = "method", id = "id",
              replicate = "repl", replicates = "exchangeable",
              keep_model = FALSE))),
    error = function(e) NULL)
  lim <- res$limits[res$limits$quantity == "sd", ][1, ]
  c(p = res$p.value, omega_hat = unname(res$statistic),
    omega_perm_mean = unname(res$estimate), sd_linked = lim$observed,
    sd_perm = lim$perm_mean,
    sd_exch = if (is.null(fe)) NA else fe$limits$sd[1],
    n_ok = unname(res$parameter))
}
clusterExport(cl, "one_rep")

for (i in intersect(run_cells, grid$cell)) {
  f <- file.path(raw_dir, sprintf("cell_%02d.rds", i))
  if (file.exists(f)) next
  g <- grid[i, ]
  n_sim <- if (g$omega == 0) n_sim_null else n_sim_alt
  t0 <- Sys.time()
  res <- parLapplyLB(cl, seq_len(n_sim), one_rep, g = g, n_perm = n_perm)
  res <- as.data.frame(do.call(rbind, res))
  saveRDS(list(grid = g, n_perm = n_perm, results = res), f)
  message(sprintf("cell %d done (%d sims, %.1f min)", i, n_sim,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# summary over all saved cells
files <- list.files(raw_dir, "^cell_.*\\.rds$", full.names = TRUE)
summ <- do.call(rbind, lapply(files, function(f) {
  r <- readRDS(f)
  d <- r$results
  ok <- !is.na(d$p)
  data.frame(r$grid, n_perm = r$n_perm, n_sim = nrow(d), n_ok = sum(ok),
             reject_05 = mean(d$p[ok] <= 0.05),
             reject_10 = mean(d$p[ok] <= 0.10),
             mc_se_05 = sqrt(mean(d$p[ok] <= 0.05) *
                               (1 - mean(d$p[ok] <= 0.05)) / sum(ok)),
             omega_hat = mean(d$omega_hat[ok]),
             omega_perm = mean(d$omega_perm_mean[ok]),
             sd_linked = mean(d$sd_linked[ok]),
             sd_perm = mean(d$sd_perm[ok]),
             sd_exch = mean(d$sd_exch[ok], na.rm = TRUE))
}))
summ <- summ[order(summ$cell), ]
write.csv(summ, file.path(out_dir, "summary.csv"), row.names = FALSE)
print(summ, digits = 3)
