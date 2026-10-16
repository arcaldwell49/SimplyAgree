# Phase 4: coverage of reli_err_test(scale = "mean") intervals (analytic log-Wald
# with and without the grand-mean variance, Wald with the jackknife SE, bootstrap
# perc / bca / stud-jackknife). Population mean mu = 5.
# Usage: Rscript err_mean_coverage.R <progress_log_file>
# (writes err_mean_coverage_partial.rds next to the log). Run from the package
# root (it calls devtools::load_all(".")). Written against the engine as of
# 2026-10-16 on branch reli-update. Results are in
# references/general/reli_redesign.md (Section 9, phase 4).

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n", file = log_file, append = TRUE)
cl <- makeCluster(max(1, detectCores() - 2))
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
B <- 499; k <- 3; v_it <- 0.1; nsim <- 300; mu <- 5
if (nzchar(Sys.getenv("SIM_QUICK"))) { B <- 49; nsim <- 4 }  # smoke test
cells <- expand.grid(n = c(10, 30), icc = c(0.5, 0.9), dist = c("normal", "non-normal"),
                     stringsAsFactors = FALSE)
# statistic, model, type, data ("two" = crossed items, "one" = one-way data)
specs <- data.frame(name = c("SEM_C", "SEM_A", "SEP_C", "SEM_1"),
                    stat = c("SEM", "SEM", "SEP", "SEM"),
                    model = c(rep("twoway", 3), "oneway"),
                    type = c("consistency", "agreement", "consistency", "agreement"),
                    stringsAsFactors = FALSE)

one_sim <- function(seed, n, icc, dist) {
  set.seed(seed)
  v_e <- (1 - icc) / icc
  T_c <- 1 + v_e
  T_a <- 1 + v_it + v_e
  truth <- c(SEM_C = sqrt(v_e), SEM_A = sqrt(v_it + v_e),
             SEE_C = sqrt(v_e / T_c), SEP_C = sqrt(T_c * (1 - (1 / T_c)^2)),
             SEP_A = sqrt(T_a * (1 - (1 / T_a)^2)), SEM_1 = sqrt(v_it + v_e))
  truth <- 100 * truth / mu
  rs <- function(m) if (dist == "normal") rnorm(m) else (rchisq(m, 3) - 3) / sqrt(6)
  re <- function(m, v) if (dist == "normal") rnorm(m, 0, sqrt(v)) else rt(m, 5) * sqrt(v * 3 / 5)
  s <- rs(n)
  Y2 <- mu + outer(s, rep(1, k)) + outer(rep(1, n), rs(k) * sqrt(v_it)) + matrix(re(n * k, v_e), n)
  # one-way data: a different (random) item effect in every cell
  Y1 <- mu + outer(s, rep(1, k)) + matrix(rs(n * k) * sqrt(v_it) + re(n * k, v_e), n)
  to_prep <- function(Y) .reli_prep_from_long(data.frame(values = as.vector(Y),
                                                         items = rep(1:k, each = n),
                                                         id = rep(1:n, k)))
  res <- c()
  for (dat in c("two", "one")) {
    prep <- to_prep(if (dat == "two") Y2 else Y1)
    sp <- specs[(specs$model == "oneway") == (dat == "one"), ]
    model <- sp$model[1]
    # per statistic: estimate, jackknife SE of log, and (agreement) subject-only estimate
    stats <- function(p, resample, ses = TRUE) {
      f <- .reli_fit(p, "anova", model, keep_fit = FALSE)
      fh <- if (resample) .hybrid_item_ms(f, shift_mean = TRUE) else f
      out <- c()
      for (j in seq_len(nrow(sp))) {
        agr <- .err_random_items(model, sp$type[j])
        ff <- if (agr) fh else f
        est <- .err_estimate(ff, sp$stat[j], model, sp$type[j], "mean")
        se <- if (ses) .err_se_log(p, ff, sp$stat[j], model, sp$type[j], "jackknife", "mean") else NA
        est_subj <- .err_estimate(f, sp$stat[j], model, sp$type[j], "mean")
        out <- c(out, est, log(est), se, est_subj)
      }
      out
    }
    m <- 4 * nrow(sp)
    t0 <- stats(prep, FALSE)
    tb <- t(replicate(B, {
      p <- tryCatch(.reli_resample(prep), error = function(e) NULL)
      if (is.null(p)) rep(NA, m) else
        tryCatch(suppressWarnings(stats(p, TRUE)), error = function(e) rep(NA, m))
    }))
    tb <- tb[is.finite(tb[, 1]), , drop = FALSE]
    jk <- t(vapply(levels(prep$df$id), function(i) {
      tryCatch(stats(.reli_prep_from_long(prep$df[prep$df$id != i, ]), FALSE, ses = FALSE),
               error = function(e) rep(NA, m))
    }, numeric(m)))
    f0 <- .reli_fit(prep, "anova", model, keep_fit = FALSE)
    for (j in seq_len(nrow(sp))) {
      b <- 4 * (j - 1)
      nm <- sp$name[j]
      mk <- function(estcol) {
        list(t0 = c(est = t0[b + 1], z = t0[b + 2], se_z = t0[b + 3]),
             t = cbind(est = tb[, b + estcol], z = tb[, b + 2], se_z = tb[, b + 3]),
             jack = cbind(est = jk[, b + 1], z = NA, se_z = NA))
      }
      safe <- function(expr) tryCatch(expr, error = function(e) c(NA, NA))
      se_nogm <- .err_se_log(NULL, f0, sp$stat[j], model, sp$type[j], "normal", "raw")
      ints <- list(an = .err_ci(f0, sp$stat[j], model, sp$type[j], 0.95, "two.sided", "mean"),
                   an_nogm = t0[b + 1] * exp(c(-1, 1) * qnorm(0.975) * se_nogm),
                   waldjk = t0[b + 1] * exp(c(-1, 1) * qnorm(0.975) * t0[b + 3]),
                   perc = safe(.boot_interval(mk(1), "perc", bounds = c(0, Inf))),
                   bca = safe(.boot_interval(mk(1), "bca", bounds = c(0, Inf))),
                   stud = safe(.boot_interval(mk(1), "stud", back = exp, bounds = c(0, Inf))))
      if (.err_random_items(model, sp$type[j])) {
        ints$perc_subj <- safe(.boot_interval(mk(4), "perc", bounds = c(0, Inf)))
      }
      tr <- truth[[nm]]
      for (meth in names(ints)) {
        ci <- ints[[meth]]
        res[paste(nm, meth, "cov", sep = "_")] <- ci[1] <= tr && tr <= ci[2]
        res[paste(nm, meth, "lo", sep = "_")] <- tr < ci[1]
        res[paste(nm, meth, "hi", sep = "_")] <- tr > ci[2]
      }
    }
  }
  res
}

clusterExport(cl, c("one_sim", "B", "k", "v_it", "specs", "mu"))
logp("start err mean-scale coverage study:", nrow(cells), "cells,", nsim, "sims each")
rows <- list()
for (i in seq_len(nrow(cells))) {
  c0 <- cells[i, ]
  sims <- parLapply(cl, seq_len(nsim) + 5000 * i,
                    function(s, n, icc, dist) tryCatch(one_sim(s, n, icc, dist), error = function(e) NULL),
                    n = c0$n, icc = c0$icc, dist = c0$dist)
  sims <- Filter(Negate(is.null), sims)
  rows[[i]] <- colMeans(do.call(rbind, sims), na.rm = TRUE)
  logp("cell", i, "of", nrow(cells), "done (", length(sims), "sims ):", paste(c0, collapse = " "))
  saveRDS(list(cells = cells[seq_len(i), ], rows = rows),
          file.path(dirname(log_file), "err_mean_coverage_partial.rds"))
}
stopCluster(cl)
tab <- cbind(cells, do.call(rbind, rows))
saveRDS(tab, file.path(dirname(log_file), "err_mean_coverage.rds"))
meths <- c("an", "an_nogm", "waldjk", "perc", "bca", "stud", "perc_subj")
for (nm in specs$name) for (w in c("cov", "lo", "hi")) {
  cols <- intersect(paste(nm, meths, w, sep = "_"), names(tab))
  x <- tab[, c("n", "icc", "dist", cols)]
  names(x) <- c("n", "icc", "dist", sub(paste0("^", nm, "_(.*)_", w, "$"), "\\1", cols))
  cat("\n==", nm, switch(w, cov = "coverage (nominal .95)",
                         lo = "truth below interval (nominal .025)",
                         hi = "truth above interval (nominal .025)"), "==\n")
  print(format(x, digits = 3), row.names = FALSE)
}
cat("\nMC SE at .95:", round(sqrt(.95 * .05 / nsim), 3), "\n")
logp("finished")
