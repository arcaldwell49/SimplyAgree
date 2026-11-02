# Coverage of the within-item reliability of method_vc(replicates = "linked")
# (README.md): omega^2 / (omega^2 + sigma_m^2) per method and for the average
# SD, with the "logit" (delta method) and "F" intervals of
# mc_vc_reliability().
# Run from the package root:
#   Rscript references/general/method_vc_reliability_sims/sim_reliability.R
# Settings (environment variables):
#   MVREL_NSIM   data sets per cell (default 1000)
#   MVREL_CORES  worker processes (default: all cores but 2)
#   MVREL_CELLS  optional comma-separated cell numbers to run
# Each finished cell is saved to raw/cell_<i>.rds and skipped on a restart;
# summary.csv is rebuilt from all saved cells at the end.
library(parallel)

out_dir <- "references/general/method_vc_reliability_sims"
raw_dir <- file.path(out_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

n_sim <- as.numeric(Sys.getenv("MVREL_NSIM", "1000"))
n_cores <- as.numeric(Sys.getenv("MVREL_CORES", max(1, detectCores() - 2)))

# two methods: sigma 0.25 for both ("equal", also fitted with a common SD)
# or 0.25 and 0.6; three methods: 0.25, 0.4, 0.6. tau = 0.3, biases 0, 0.5
# (and -0.3).
grid <- expand.grid(n_items = c(20, 50), n_reps = c(3, 6),
                    omega = c(0.3, 1), sigma_type = c("equal", "unequal"),
                    n_methods = 2, stringsAsFactors = FALSE)
grid <- rbind(grid,
              data.frame(n_items = 20, n_reps = 3, omega = c(0.3, 1),
                         sigma_type = "unequal", n_methods = 3))
grid$cell <- seq_len(nrow(grid))

cells <- Sys.getenv("MVREL_CELLS", "")
run_cells <- if (nzchar(cells)) {
  as.integer(strsplit(cells, ",")[[1]])
} else {
  grid$cell
}

sigmas <- function(g) {
  if (g$n_methods == 3) return(c(0.25, 0.4, 0.6))
  if (g$sigma_type == "equal") c(0.25, 0.25) else c(0.25, 0.6)
}

cl <- makeCluster(n_cores)
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".",
                                                               quiet = TRUE))))

# one data set: a row per reliability (per method, average, and with equal
# SDs the common-SD fit), with the estimate and whether each CI covers
one_rep <- function(s, g, sig) {
  M <- g$n_methods
  lv <- c("A", "B", "C")[seq_len(M)]
  dat <- sim_methods(n_items = g$n_items, n_reps = g$n_reps, methods = lv,
                     alpha = c(0, 0.5, -0.3)[seq_len(M)], tau = 0.3,
                     omega = g$omega, sigma = sig, replicates = "linked",
                     seed = 100000 * g$cell + s)
  truth <- c(stats::setNames(g$omega^2 / (g$omega^2 + sig^2), lv),
             average = g$omega^2 / (g$omega^2 + mean(sig^2)),
             common = g$omega^2 / (g$omega^2 + mean(sig^2)))
  rows <- function(fit, fit_type) {
    if (is.null(fit)) return(NULL)
    out <- NULL
    for (ci in c("logit", "F")) {
      r <- mc_vc_reliability(fit$varcomp, fit$vc_cov, fit$design,
                             conf_level = 0.95, ci = ci)
      key <- ifelse(is.na(r$method), "common", r$method)
      tr <- unname(truth[key])
      out <- rbind(out, data.frame(sim = s, fit = fit_type, row = key,
                                   ci = ci, est = r$reliability,
                                   truth = tr, lower = r$lower,
                                   upper = r$upper,
                                   cover = r$lower <= tr & r$upper >= tr,
                                   miss_low = r$upper < tr,
                                   miss_high = r$lower > tr))
    }
    out
  }
  fit_one <- function(sigma) {
    tryCatch(suppressWarnings(suppressMessages(
      method_vc(dat, y = "y", method = "method", id = "id",
                replicate = "repl", replicates = "linked", sigma = sigma,
                keep_model = FALSE))),
      error = function(e) NULL)
  }
  res <- rows(fit_one("method"), "method")
  if (g$sigma_type == "equal") {
    res <- rbind(res, rows(fit_one("common"), "common"))
  }
  res
}
clusterExport(cl, c("one_rep", "sigmas"))

for (i in intersect(run_cells, grid$cell)) {
  f <- file.path(raw_dir, sprintf("cell_%02d.rds", i))
  if (file.exists(f)) next
  g <- grid[i, ]
  t0 <- Sys.time()
  res <- parLapplyLB(cl, seq_len(n_sim), one_rep, g = g, sig = sigmas(g))
  res <- do.call(rbind, res)
  saveRDS(list(grid = g, results = res), f)
  message(sprintf("cell %d done (%d sims, %.1f min)", i, n_sim,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# summary over all saved cells: one row per cell, fit, reliability and CI
files <- list.files(raw_dir, "^cell_.*\\.rds$", full.names = TRUE)
summ <- do.call(rbind, lapply(files, function(f) {
  r <- readRDS(f)
  d <- r$results
  g <- r$grid
  keys <- unique(d[, c("fit", "row", "ci")])
  do.call(rbind, lapply(seq_len(nrow(keys)), function(k) {
    x <- d[d$fit == keys$fit[k] & d$row == keys$row[k] &
             d$ci == keys$ci[k], ]
    ok <- !is.na(x$cover)
    data.frame(g, keys[k, ], truth = x$truth[1], n = nrow(x),
               n_ci = sum(ok), mean_est = mean(x$est),
               cover = mean(x$cover[ok]), miss_low = mean(x$miss_low[ok]),
               miss_high = mean(x$miss_high[ok]),
               width = stats::median((x$upper - x$lower)[ok]))
  }))
}))
summ <- summ[order(summ$cell, summ$fit, summ$row, summ$ci), ]
write.csv(summ, file.path(out_dir, "summary.csv"), row.names = FALSE)
print(summ, digits = 3)
