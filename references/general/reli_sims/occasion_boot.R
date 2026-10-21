# Phase C3: coverage of the reli_facet_test() bootstrap intervals with
# crossed occasions; the generalised hybrid step (every mean square without
# subjects that enters the estimand scaled by chi-square(df)/df) against
# resampling subjects only (criterion C4); sets boot_ci = "auto".
# Usage: Rscript occasion_boot.R <progress_log_file> [nsim] [B]
# (writes occasion_boot.rds next to the log). Run from the package root.
# Written 2026-10-20 against R/reli_facet_engine.R; results in
# reli_redesign.md, Section 15.9.
#
# For speed the mean squares of the p x i x o array (one measurement per
# cell) are computed in closed form here (checked against
# .facet_anova_ms()), and each estimand's mean-square coefficients come
# from the package's .facet_ms_coef(), so estimates for many fits are one
# matrix product. The jackknife SE, hybrid step and intervals mirror or call
# .facet_se_z(), .facet_hybrid() and .boot_interval().

library(parallel)
args <- commandArgs(trailingOnly = TRUE)
log_file <- args[1]
nsim <- if (length(args) > 1) as.integer(args[2]) else 300
B <- if (length(args) > 2) as.integer(args[3]) else 499
logp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n",
                          file = log_file, append = TRUE)

src <- readLines("references/general/reli_sims/occasion_coverage.R")
eval(parse(text = src[grep("^mixes", src):(grep("^run_cell", src) - 1)]))
cells <- expand.grid(n = c(10, 30), k = c(2, 3), m = 2,
                     mix = names(mixes), dist = c("normal", "nonnormal"),
                     stringsAsFactors = FALSE)
estimands <- estimands[c("ICC_intra_C", "ICC_intra_A", "ICC_inter_A", "Phi_11",
                         "SEM_intra_C", "SEM_intra_A", "SEM_overall_A")]
types <- c("perc", "basic", "bca", "stud", "perc_subject", "bca_subject")
eff7 <- c("p", "i", "o", "pi", "po", "io", "pio")

# closed-form mean squares of an n x k x m array
fast_ms <- function(Y) {
  d <- dim(Y); n <- d[1]; k <- d[2]; m <- d[3]
  g <- mean(Y)
  mp <- rowMeans(Y); mi <- colMeans(aperm(Y, c(1, 3, 2)), dims = 2)
  mo <- colMeans(Y, dims = 2)
  mpi <- rowMeans(Y, dims = 2)
  mpo <- apply(Y, c(1, 3), mean); mio <- colMeans(Y)
  ss_p <- k * m * sum((mp - g)^2)
  ss_i <- n * m * sum((mi - g)^2)
  ss_o <- n * k * sum((mo - g)^2)
  ss_pi <- m * sum((mpi - outer(mp, mi, "+") + g)^2)
  ss_po <- k * sum((mpo - outer(mp, mo, "+") + g)^2)
  ss_io <- n * sum((mio - outer(mi, mo, "+") + g)^2)
  ss_t <- sum((Y - g)^2)
  ss_pio <- ss_t - ss_p - ss_i - ss_o - ss_pi - ss_po - ss_io
  df <- c(n - 1, k - 1, m - 1, (n - 1) * (k - 1), (n - 1) * (m - 1),
          (k - 1) * (m - 1), (n - 1) * (k - 1) * (m - 1))
  stats::setNames(c(ss_p, ss_i, ss_o, ss_pi, ss_po, ss_io, ss_pio) / df, eff7)
}

# coefficient vectors for each estimand at a given n (from the package)
coefs_at <- function(n, k, m) {
  f <- list(method = "anova", design = "crossed", factors = c("p", "i", "o"),
            effects = eff7, levels = c(p = n, i = k, o = m), n = n, k = k, r = 1,
            balanced = TRUE, replicated = FALSE, ms = stats::setNames(rep(1, 7), eff7),
            df = stats::setNames(rep(1, 7), eff7))
  f <- .facet_refresh(f)
  lapply(estimands, function(s) {
    cf <- .facet_ms_coef(f, s)
    c(cf, list(held = .facet_nonsubject(f, s), stat = s$stat))
  })
}

# estimate and z for rows of a mean-square matrix
est_z <- function(MS, cf) {
  if (cf$stat == "ICC") {
    est <- drop(MS %*% cf$a) / drop(MS %*% cf$t)
    list(est = est, z = -0.5 * log(1 - est))
  } else {
    est <- sqrt(pmax(drop(MS %*% cf$var), 0))
    list(est = est, z = log(est))
  }
}

# derivative of z with respect to one mean square (analytic)
dz_dms <- function(ms, cf, t) {
  if (cf$stat == "ICC") {
    A <- sum(cf$a * ms); Tt <- sum(cf$t * ms); est <- A / Tt
    0.5 / (1 - est) * (cf$a[[t]] * Tt - cf$t[[t]] * A) / Tt^2
  } else {
    cf$var[[t]] / (2 * sum(cf$var * ms))
  }
}

# estimate, z and jackknife SE (non-subject mean squares held, plus their
# normal-theory variance), as .facet_se_z()
stats_one <- function(ms, df, loo, cf, cf1) {
  ez <- est_z(matrix(ms, 1), cf)
  L <- loo
  if (length(cf$held) > 0) L[, cf$held] <- matrix(ms[cf$held], nrow(L), length(cf$held), byrow = TRUE)
  zi <- est_z(L, cf1)$z
  n <- nrow(L)
  v <- (n - 1) / n * sum((zi - mean(zi))^2)
  for (t in cf$held) v <- v + dz_dms(ms, cf, t)^2 * 2 * ms[[t]]^2 / df[[t]]
  c(ez$est, ez$z, sqrt(v))
}

run_cell <- function(ci) {
  cl <- cells[ci, ]
  n <- cl$n; k <- cl$k; m <- cl$m
  v <- mixes[[cl$mix]]
  truth <- vapply(estimands, truth_of, numeric(1), v = v)
  cfs <- coefs_at(n, k, m)
  cfs1 <- coefs_at(n - 1, k, m)
  df7 <- stats::setNames(c(n - 1, k - 1, m - 1, (n - 1) * (k - 1), (n - 1) * (m - 1),
                           (k - 1) * (m - 1), (n - 1) * (k - 1) * (m - 1)), eff7)
  loo_of <- function(Y) t(vapply(seq_len(n), function(i) fast_ms(Y[-i, , , drop = FALSE]), numeric(7)))
  set.seed(7000 + ci)
  rows <- list()
  for (sim in seq_len(nsim)) {
    Y <- sim_array(n, k, m, v, cl$dist)
    ms0 <- fast_ms(Y)
    loo0 <- loo_of(Y)
    J <- length(estimands)
    t0 <- t(vapply(seq_len(J), function(j) stats_one(ms0, df7, loo0, cfs[[j]], cfs1[[j]]), numeric(3)))
    jack <- vapply(seq_len(J), function(j) est_z(loo0, cfs1[[j]])$est, numeric(n))
    tb <- array(NA_real_, c(B, J, 3))
    tb_plain <- matrix(NA_real_, B, J)
    for (b in seq_len(B)) {
      idx <- sample.int(n, n, replace = TRUE)
      if (length(unique(idx)) < 2) next
      Yb <- Y[idx, , , drop = FALSE]
      msb <- fast_ms(Yb)
      lb <- loo_of(Yb)
      for (j in seq_len(J)) {
        tb_plain[b, j] <- est_z(matrix(msb, 1), cfs[[j]])$est
        msh <- msb
        for (t in cfs[[j]]$held) msh[[t]] <- msh[[t]] * stats::rchisq(1, df7[[t]]) / df7[[t]]
        tb[b, j, ] <- stats_one(msh, df7, lb, cfs[[j]], cfs1[[j]])
      }
    }
    for (j in seq_len(J)) {
      s <- estimands[[j]]
      bounds <- if (s$stat == "ICC") c(-Inf, 1) else c(0, Inf)
      ok <- is.finite(tb[, j, 1])
      bo <- list(t0 = c(est = t0[[j, 1]], z = t0[[j, 2]], se_z = t0[[j, 3]]),
                 t = cbind(est = tb[ok, j, 1], z = tb[ok, j, 2], se_z = tb[ok, j, 3]),
                 jack = cbind(est = jack[, j], z = NA, se_z = NA))
      okp <- is.finite(tb_plain[, j])
      bo_plain <- bo
      bo_plain$t <- cbind(est = tb_plain[okp, j], z = NA, se_z = NA)
      todo <- list(perc = list(bo, "perc"), basic = list(bo, "basic"),
                   bca = list(bo, "bca"), stud = list(bo, "stud"))
      if (length(cfs[[j]]$held) > 0) {
        todo$perc_subject <- list(bo_plain, "perc")
        todo$bca_subject <- list(bo_plain, "bca")
      }
      for (tn in names(todo)) {
        ci2 <- tryCatch(as.numeric(.boot_interval(todo[[tn]][[1]], todo[[tn]][[2]],
                                                   back = function(z) .facet_z_inv(z, s),
                                                   bounds = bounds)),
                        error = function(e) c(NA, NA))
        rows[[length(rows) + 1]] <- c(j = j, type = match(tn, types),
                                      lower = ci2[1], upper = ci2[2], truth = truth[[j]])
      }
    }
  }
  res <- as.data.frame(do.call(rbind, rows))
  res$estimand <- names(estimands)[res$j]
  res$boot_ci <- types[res$type]
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

if (!identical(Sys.getenv("OCC_BOOT_SOURCE_ONLY"), "true")) {
  cl <- makeCluster(14)
  invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
  clusterExport(cl, c("cells", "mixes", "estimands", "types", "eff7", "draw", "draw_e",
                      "sim_array", "truth_of", "fast_ms", "coefs_at", "est_z", "dz_dms",
                      "stats_one", "run_cell", "nsim", "B", "log_file", "logp"))
  logp("start", nrow(cells), "cells, nsim =", nsim, "B =", B)
  out <- parLapplyLB(cl, seq_len(nrow(cells)), function(ci) {
    t0 <- Sys.time()
    res <- run_cell(ci)
    logp("cell", ci, "done in", round(as.numeric(Sys.time() - t0, units = "secs")), "s")
    res
  })
  stopCluster(cl)
  out <- do.call(rbind, out)
  saveRDS(out, file.path(dirname(log_file), "occasion_boot.rds"))
  logp("finished")
}
