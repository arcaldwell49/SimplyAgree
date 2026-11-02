# Coverage of method_vc(methods = "random") (README.md).
# Run from the package root:
#   Rscript references/general/method_vc_random_sims/sim_method_vc_random.R
# Settings (environment variables):
#   MVR_NSIM    data sets per cell (default 1000)
#   MVR_CORES   worker processes (default: all cores but 2)
#   MVR_CELLS   optional comma-separated cell numbers to run (e.g. "1,2")
# Each finished cell is saved to raw/cell_<i>.rds, and cells already saved
# are skipped, so the script can be stopped and restarted. summary.csv is
# rebuilt from all saved cells at the end.
library(parallel)

out_dir <- "references/general/method_vc_random_sims"
raw_dir <- file.path(out_dir, "raw")
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

n_sim <- as.numeric(Sys.getenv("MVR_NSIM", "1000"))
n_cores <- as.numeric(Sys.getenv("MVR_CORES", max(1, detectCores() - 2)))

# tau = 0.3, omega = 0.3 (linked), sigma = 0.4 for every method unless
# sigma_type = "unequal" (0.3 to 0.6 across the methods, fitted with
# sigma = "method"); rater SD 0.2 or 0.5 (0.1, 0.05 in cells 17-18); 2 replicates.
grid <- expand.grid(rater = c(0.2, 0.5), n_methods = c(3, 5, 10),
                    n_items = c(15, 30), replicates = "linked",
                    sigma_type = "common", stringsAsFactors = FALSE)
grid <- rbind(grid,
              data.frame(rater = c(0.2, 0.5), n_methods = 5, n_items = 30,
                         replicates = "exchangeable", sigma_type = "common"),
              data.frame(rater = c(0.2, 0.5), n_methods = 5, n_items = 30,
                         replicates = "linked", sigma_type = "unequal"),
              # small rater SDs, for the rater SD interval (added 2026-11-02)
              data.frame(rater = c(0.1, 0.05), n_methods = 5, n_items = 30,
                         replicates = "linked", sigma_type = "common"))
grid$cell <- seq_len(nrow(grid))

cells <- Sys.getenv("MVR_CELLS", "")
run_cells <- if (nzchar(cells)) {
  as.integer(strsplit(cells, ",")[[1]])
} else {
  grid$cell
}

cl <- makeCluster(n_cores)
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".",
                                                               quiet = TRUE))))

# one data set: new rater effects drawn each time; returns whether the
# bounds of the limits, the rater SD interval, and the GPQ bound of tdi()
# (p = 0.95 and 0.8) cover the true values
one_rep <- function(s, g) {
  M <- g$n_methods
  set.seed(100000 * g$cell + s)
  alpha <- stats::rnorm(M, 0, g$rater)
  sig <- if (g$sigma_type == "unequal") seq(0.3, 0.6, length.out = M) else
    rep(0.4, M)
  omega <- if (g$replicates == "linked") 0.3 else 0
  dat <- sim_methods(n_items = g$n_items, n_reps = 2,
                     methods = sprintf("M%02d", seq_len(M)), alpha = alpha,
                     tau = 0.3, omega = omega, sigma = sig,
                     replicates = g$replicates, seed = 100000 * g$cell + s)
  # true limits for two randomly chosen raters (on the same occasion)
  sd_true <- sqrt(2 * (g$rater^2 + 0.3^2 + mean(sig^2)))
  upper_true <- stats::qnorm(0.975) * sd_true
  out <- c(ok = 0, sd = NA, cover_upper = NA, cover_lower = NA,
           cover_both = NA, cover_rater = NA, rater_hat = NA,
           rater_zero = NA, df_sd = NA, cover_tdi95 = NA, cover_tdi80 = NA)
  fit <- tryCatch(suppressWarnings(suppressMessages(
    method_vc(dat, y = "y", method = "method", id = "id",
              replicate = "repl", replicates = g$replicates,
              methods = "random",
              sigma = if (g$sigma_type == "unequal") "method" else "common",
              keep_model = FALSE))),
    error = function(e) NULL)
  if (is.null(fit)) return(out)
  l <- fit$limits
  r <- fit$varcomp[fit$varcomp$component == "rater", ]
  up <- l$upper_loa_ci >= upper_true
  lo <- l$lower_loa_ci <= -upper_true
  # GPQ bound of tdi() (bias 0, so the true TDI is qnorm((1 + p) / 2) * SD)
  tdi_cover <- function(p) {
    td <- tryCatch(suppressWarnings(tdi(fit, p = p, B = 2000, seed = s)),
                   error = function(e) NULL)
    if (is.null(td)) return(NA)
    td$conf.int[2] >= stats::qnorm((1 + p) / 2) * sd_true
  }
  c(ok = as.numeric(!is.na(l$upper_loa_ci)), sd = l$sd,
    cover_upper = up, cover_lower = lo, cover_both = up & lo,
    cover_rater = r$lower <= g$rater & r$upper >= g$rater,
    rater_hat = r$sd, rater_zero = as.numeric(r$sd < 1e-4 * 0.4),
    df_sd = l$df_sd, cover_tdi95 = tdi_cover(0.95),
    cover_tdi80 = tdi_cover(0.8))
}
clusterExport(cl, "one_rep")

for (i in intersect(run_cells, grid$cell)) {
  f <- file.path(raw_dir, sprintf("cell_%02d.rds", i))
  if (file.exists(f)) next
  g <- grid[i, ]
  t0 <- Sys.time()
  res <- parLapplyLB(cl, seq_len(n_sim), one_rep, g = g)
  res <- as.data.frame(do.call(rbind, res))
  saveRDS(list(grid = g, results = res), f)
  message(sprintf("cell %d done (%d sims, %.1f min)", i, n_sim,
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

# summary over all saved cells
files <- list.files(raw_dir, "^cell_.*\\.rds$", full.names = TRUE)
summ <- do.call(rbind, lapply(files, function(f) {
  r <- readRDS(f)
  d <- r$results
  ok <- d$ok == 1 & !is.na(d$ok)
  g <- r$grid
  sd_true <- sqrt(2 * (g$rater^2 + 0.3^2 +
                         mean((if (g$sigma_type == "unequal")
                           seq(0.3, 0.6, length.out = g$n_methods) else
                             rep(0.4, g$n_methods))^2)))
  data.frame(g, n_sim = nrow(d), n_ok = sum(ok),
             cover_upper = mean(d$cover_upper[ok]),
             cover_lower = mean(d$cover_lower[ok]),
             cover_both = mean(d$cover_both[ok]),
             cover_rater = mean(d$cover_rater[ok]),
             cover_tdi95 = mean(d$cover_tdi95[ok]),
             cover_tdi80 = mean(d$cover_tdi80[ok]),
             rater_zero = mean(d$rater_zero[ok]),
             sd_true = sd_true,
             sd_mean = mean(d$sd[ok]),
             df_sd_median = stats::median(d$df_sd[ok]))
}))
summ <- summ[order(summ$cell), ]
write.csv(summ, file.path(out_dir, "summary.csv"), row.names = FALSE)
print(summ, digits = 3)
