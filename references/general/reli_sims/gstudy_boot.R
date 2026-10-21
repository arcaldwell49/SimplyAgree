# NOTE (2026-10-20): written for the development version of reli_facet_test(),
# then called gstudy_test(), with internal .gstudy_*() functions and the old
# `form` argument. Kept as the record of the Section 14 results; to rerun,
# map .gstudy_* to .facet_* and form = "intra" / "consistency" /
# "agreement" to facet = "intra" / facet = "inter", type = "consistency" /
# facet = "inter", type = "agreement".
# Phase G3: coverage of the gstudy_test() bootstrap intervals (perc, basic,
# BCa, studentized with jackknife SE), hybrid vs subject-only resampling for
# the agreement statistics. Sets boot_ci = "auto".
# Usage: Rscript gstudy_boot.R <progress_log_file> [nsim] [B]
# (writes gstudy_boot.rds next to the log). Run from the package root.
# Written 2026-10-20 against R/gstudy_engine.R; results in reli_redesign.md,
# Section 9 (phase G3).
#
# Subjects are resampled from an n x k x r array (each drawn subject keeps
# all its cells and repeats), as .reli_resample() does; the estimates,
# hybrid step, jackknife SE and interval code mirror or call the package's.

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 300
B <- if (length(args) > 2) as.integer(args[3]) else 499
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

mixes <- list(R = c(i = 0.05, pi = 0.05, e = 0.30),
              I = c(i = 0.05, pi = 0.30, e = 0.05),
              B = c(i = 0.30, pi = 0.05, e = 0.10),
              L = c(i = 0.10, pi = 0.50, e = 0.50))
cells <- expand.grid(n = c(10, 30), k = c(2, 3), r = 2,
                     mix = names(mixes), dist = c("normal", "nonnormal"),
                     stringsAsFactors = FALSE)
estimands <- list(
  SEM_rep = list(stat = "SEM", form = "intra", k_avg = 1, r_avg = 1, scale = "raw"),
  SEM_C = list(stat = "SEM", form = "consistency", k_avg = 1, r_avg = 1, scale = "raw"),
  SEM_A = list(stat = "SEM", form = "agreement", k_avg = 1, r_avg = 1, scale = "raw"),
  Erho_11 = list(stat = "ICC", form = "consistency", k_avg = 1, r_avg = 1, scale = "raw"),
  Phi_11 = list(stat = "ICC", form = "agreement", k_avg = 1, r_avg = 1, scale = "raw"),
  rho_intra = list(stat = "ICC", form = "intra", k_avg = 1, r_avg = 1, scale = "raw"))

draw <- function(m, v, dist) {
  if (dist == "normal") return(rnorm(m, 0, sqrt(v)))
  sqrt(v) * (rchisq(m, 3) - 3) / sqrt(6)
}
draw_e <- function(m, v, dist) {
  if (dist == "normal") return(rnorm(m, 0, sqrt(v)))
  sqrt(v * 3 / 5) * rt(m, 5)
}

ms_from_array <- function(Y) {
  d <- dim(Y)
  n <- d[1]; k <- d[2]; r <- d[3]
  M <- rowMeans(Y, dims = 2)
  gm <- mean(M)
  rm <- rowMeans(M); cmn <- colMeans(M)
  c(MSP = k * r * sum((rm - gm)^2) / (n - 1),
    MSI = n * r * sum((cmn - gm)^2) / (k - 1),
    MSPI = r * sum((M - outer(rm, cmn, "+") + gm)^2) / ((n - 1) * (k - 1)),
    MSW = sum((Y - c(M))^2) / (n * k * (r - 1)))
}

mk_fit <- function(ms, n, k, r) {
  structure(list(method = "anova", balanced = TRUE, n = n, k = k, r = r,
                 kc = k, nc = n, ms = ms,
                 df = c(dfP = n - 1, dfI = k - 1, dfPI = (n - 1) * (k - 1),
                        dfW = n * k * (r - 1)),
                 grand_mean = 0), class = "gstudy_fit")
}

# leave-one-subject-out mean squares, as in .gstudy_jackknife_var()
loo_ms_array <- function(Y) {
  d <- dim(Y)
  n <- d[1]; k <- d[2]; r <- d[3]
  M <- rowMeans(Y, dims = 2)
  loo <- .loo_ms(M)
  ssw_i <- rowSums((Y - c(M))^2, dims = 1)
  cbind(MSP = r * loo[, "MSB"], MSI = r * loo[, "MSJ"], MSPI = r * loo[, "MSE"],
        MSW = (sum(ssw_i) - ssw_i) / ((n - 1) * k * (r - 1)))
}

# estimate, z and jackknife SE of z (with the random-items term) per estimand
stats_of <- function(Y, fit, with_se = TRUE) {
  n <- fit$n
  loo <- if (with_se) loo_ms_array(Y) else NULL
  t(vapply(estimands, function(s) {
    est <- .gstudy_estimate(fit, s)
    z <- .gstudy_z(est, s)
    if (!with_se) return(c(est, z, NA))
    ri <- s$form == "agreement"
    zi <- vapply(seq_len(n), function(i) {
      f <- fit; f$n <- n - 1; f$nc <- n - 1
      f$ms <- loo[i, ]
      if (ri) f$ms[["MSI"]] <- fit$ms[["MSI"]]
      .gstudy_z_fit(f, s)
    }, numeric(1))
    v <- (n - 1) / n * sum((zi - mean(zi))^2)
    if (ri) {
      gi <- .num_grad(function(m) { f <- fit; f$ms[["MSI"]] <- m[[1]]; .gstudy_z_fit(f, s) },
                      c(MSI = fit$ms[["MSI"]]))
      v <- v + gi^2 * 2 * fit$ms[["MSI"]]^2 / fit$df[["dfI"]]
    }
    c(est, z, sqrt(v))
  }, numeric(3)))
}

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; r <- cl$r
  v <- c(p = 1, mixes[[cl$mix]])[c("p", "i", "pi", "e")]
  truth <- vapply(estimands, function(s) {
    w <- .gstudy_weights(s)
    if (s$stat == "SEM") sqrt(sum(w$var * v)) else sum(w$num * v) / sum(w$den * v)
  }, numeric(1))
  set.seed(5000 + ci)
  rows <- list()
  for (sim in seq_len(nsim)) {
    p <- draw(n, v[["p"]], cl$dist)
    it <- draw(k, v[["i"]], cl$dist)
    pi <- draw(n * k, v[["pi"]], cl$dist)
    Y <- array(rep(outer(p, it, "+") + pi, r), c(n, k, r)) +
      array(draw_e(n * k * r, v[["e"]], cl$dist), c(n, k, r))
    fit0 <- mk_fit(ms_from_array(Y), n, k, r)
    t0 <- stats_of(Y, fit0)
    jack_ms <- loo_ms_array(Y)
    jack <- t(vapply(seq_len(n), function(i) {
      f <- fit0; f$n <- n - 1; f$nc <- n - 1; f$ms <- jack_ms[i, ]
      vapply(estimands, function(s) .gstudy_estimate(f, s), numeric(1))
    }, numeric(length(estimands))))
    # replicates: subject-only and hybrid (same resample, MSI multiplied)
    tb <- array(NA_real_, c(B, length(estimands), 3))
    tb_plain <- matrix(NA_real_, B, length(estimands))
    for (b in seq_len(B)) {
      idx <- sample.int(n, n, replace = TRUE)
      if (length(unique(idx)) < 2) next
      Yb <- Y[idx, , , drop = FALSE]
      fb <- mk_fit(ms_from_array(Yb), n, k, r)
      tb_plain[b, ] <- vapply(estimands, function(s) .gstudy_estimate(fb, s), numeric(1))
      fh <- fb
      fh$ms[["MSI"]] <- fb$ms[["MSI"]] * rchisq(1, k - 1) / (k - 1)
      sp <- stats_of(Yb, fb)
      sh <- stats_of(Yb, fh)
      agree <- vapply(estimands, function(s) s$form == "agreement", logical(1))
      sp[agree, ] <- sh[agree, ]
      tb[b, , ] <- sp
    }
    for (j in seq_along(estimands)) {
      s <- estimands[[j]]
      nm <- names(estimands)[j]
      bounds <- if (s$stat == "ICC") c(-Inf, 1) else c(0, Inf)
      ok <- is.finite(tb[, j, 1])
      bo <- list(t0 = c(est = t0[[j, 1]], z = t0[[j, 2]], se_z = t0[[j, 3]]),
                 t = cbind(est = tb[ok, j, 1], z = tb[ok, j, 2], se_z = tb[ok, j, 3]),
                 jack = cbind(est = jack[, j], z = NA, se_z = NA))
      bo_plain <- bo
      okp <- is.finite(tb_plain[, j])
      bo_plain$t <- cbind(est = tb_plain[okp, j], z = NA, se_z = NA)
      types <- list(perc = list(bo, "perc"), basic = list(bo, "basic"),
                    bca = list(bo, "bca"), stud = list(bo, "stud"))
      if (s$form == "agreement") {
        types$perc_subject <- list(bo_plain, "perc")
        types$bca_subject <- list(bo_plain, "bca")
      }
      for (tn in names(types)) {
        ci2 <- tryCatch(as.numeric(.boot_interval(types[[tn]][[1]], types[[tn]][[2]],
                                                   back = function(z) .gstudy_z_inv(z, s),
                                                   bounds = bounds)),
                        error = function(e) { if (nzchar(Sys.getenv("GS_DEBUG"))) print(conditionCall(e)); c(NA, NA) })
        rows[[length(rows) + 1]] <- c(sim = sim, j = j, type = match(tn, c("perc", "basic", "bca", "stud", "perc_subject", "bca_subject")),
                                      lower = ci2[1], upper = ci2[2], truth = truth[[nm]])
      }
    }
  }
  res <- as.data.frame(do.call(rbind, rows))
  res$estimand <- names(estimands)[res$j]
  res$boot_ci <- c("perc", "basic", "bca", "stud", "perc_subject", "bca_subject")[res$type]
  agg <- do.call(rbind, lapply(split(res, list(res$estimand, res$boot_ci), drop = TRUE), function(d) {
    ok <- is.finite(d$lower) & is.finite(d$upper)
    data.frame(estimand = d$estimand[1], boot_ci = d$boot_ci[1],
               coverage = mean(d$lower[ok] <= d$truth[ok] & d$truth[ok] <= d$upper[ok]),
               miss_low = mean(d$truth[ok] < d$lower[ok]),
               miss_high = mean(d$truth[ok] > d$upper[ok]),
               n_fail = sum(!ok))
  }))
  cbind(cl[rep(1, nrow(agg)), ], agg, row.names = NULL)
}

cl <- makeCluster(14)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
clusterExport(cl, c("cells", "mixes", "estimands", "draw", "draw_e", "ms_from_array", "run_cell",
                    "mk_fit", "loo_ms_array", "stats_of", "nsim", "B", "log_file", "logp"))
logp("start", nrow(cells), "cells, nsim =", nsim, "B =", B)
out <- parLapplyLB(cl, seq_len(nrow(cells)), function(ci) {
  t0 <- Sys.time()
  res <- run_cell(ci)
  logp("cell", ci, "done in", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
  res
})
stopCluster(cl)
out <- do.call(rbind, out)
saveRDS(out, file.path(dirname(log_file), "gstudy_boot.rds"))
logp("finished")
