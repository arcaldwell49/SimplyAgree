# Worker functions for sim_ci.R (data set -> intervals). Globals B and Bs
# (bootstrap and studentized resamples) must be set by the caller.

worker_init <- function(dir) {
  source(file.path(dir, "gen.R"))
  options(ccc_sims.dir = dir)
  source(file.path(dir, "fast_vc.R"))
  NULL
}

# Subject statistics and an estimator of z = atanh(CCC) given weights.
# est_w(W) takes a g x m matrix of weights and returns m values of the CCC.
make_estimator <- function(dat, type) {
  if (type == "nest") {
    dat <- dat[complete.cases(dat), ]
    id <- factor(dat$id)
    s <- function(v) as.vector(tapply(v, id, sum))
    st <- cbind(n = s(rep(1, nrow(dat))), Sx = s(dat$x), Sy = s(dat$y),
                Sxx = s(dat$x^2), Syy = s(dat$y^2), Sxy = s(dat$x * dat$y))
    est_w <- function(W) {
      T <- crossprod(W, st)
      N <- T[, "n"]; mx <- T[, "Sx"] / N; my <- T[, "Sy"] / N
      sxy <- T[, "Sxy"] / N - mx * my
      2 * sxy / (T[, "Sxx"] / N - mx^2 + T[, "Syy"] / N - my^2 + (mx - my)^2)
    }
  } else {
    st <- vc_stats(dat$id, dat$x, dat$y)
    st <- st[st[, "nx"] + st[, "ny"] > 0, , drop = FALSE]
    est_w <- function(W) {
      apply(W, 2, function(w) {
        r <- tryCatch(vc_ccc(st, w), error = function(e) NA_real_)
        as.numeric(r)
      })
    }
  }
  list(g = nrow(st), est_w = est_w)
}

jack_se <- function(z_loo, wts = rep(1, length(z_loo))) {
  # jackknife SE over units; wts = number of identical units per value
  g <- sum(wts)
  m <- sum(wts * z_loo) / g
  sqrt((g - 1) / g * sum(wts * (z_loo - m)^2))
}

one_data_set <- function(seed, cell, type, stud) {
  set.seed(seed)
  sim <- gen_data(type, cell$g, cell$k, cell$rho, cell$diff, cell$miss,
                  cell$dist)
  e <- make_estimator(sim$dat, type)
  g <- e$g
  alpha <- 1 - 0.95
  out <- c(truth = sim$truth, g = g, est = NA, se_z = NA,
           jk_lo = NA, jk_hi = NA, perc_lo = NA, perc_hi = NA,
           bca_lo = NA, bca_hi = NA, stud_lo = NA, stud_hi = NA,
           boot_fail = NA, stud_fail = NA, secs = NA)
  t_start <- proc.time()[[3]]
  I <- diag(g)
  est <- e$est_w(matrix(1, g, 1))
  if (!is.finite(est) || abs(est) >= 1) return(out)
  z0 <- atanh(est)
  loo <- e$est_w(1 - I)
  z_loo <- atanh(loo)
  se0 <- jack_se(z_loo)
  q <- qt(1 - alpha / 2, g - 1)
  out[c("est", "se_z", "jk_lo", "jk_hi")] <-
    c(est, se0, tanh(z0 - q * se0), tanh(z0 + q * se0))

  # bootstrap: same resamples for every interval
  W <- rmultinom(B, g, rep(1, g))
  bt <- e$est_w(W)
  ok <- is.finite(bt) & abs(bt) < 1
  out["boot_fail"] <- sum(!ok)
  b <- bt[ok]
  out[c("perc_lo", "perc_hi")] <- quantile(b, c(alpha / 2, 1 - alpha / 2),
                                           names = FALSE)
  # BCa as .bca_params() in R/reli_engine.R (CCC scale, jackknife values)
  z0b <- qnorm(mean(b < est))
  if (is.finite(z0b)) {
    L <- mean(loo) - loo
    acc <- sum(L^3) / (6 * sum(L^2)^1.5)
    zp <- z0b + qnorm(c(alpha / 2, 1 - alpha / 2))
    adj <- pnorm(z0b + zp / (1 - acc * zp))
    out[c("bca_lo", "bca_hi")] <- quantile(b, adj, names = FALSE)
  }

  if (stud) {
    tv <- vapply(seq_len(Bs), function(j) {
      if (!ok[j]) return(NA_real_)
      w <- W[, j]
      drawn <- which(w > 0)
      Wl <- matrix(w, g, length(drawn))
      Wl[cbind(drawn, seq_along(drawn))] <- w[drawn] - 1
      zl <- atanh(e$est_w(Wl))
      if (!all(is.finite(zl))) return(NA_real_)
      se_b <- jack_se(zl, w[drawn])
      if (!(se_b > 0)) return(NA_real_)
      (atanh(bt[j]) - z0) / se_b
    }, numeric(1))
    out["stud_fail"] <- sum(!is.finite(tv))
    tv <- tv[is.finite(tv)]
    if (length(tv) > 0) {
      out[c("stud_lo", "stud_hi")] <-
        tanh(z0 - quantile(tv, c(1 - alpha / 2, alpha / 2), names = FALSE) *
               se0)
    }
  }
  out["secs"] <- proc.time()[[3]] - t_start
  out
}

run_chunk <- function(task, type, stud) {
  cell <- task$cell
  m <- t(vapply(task$seeds, one_data_set, numeric(15), cell = cell,
                type = type, stud = stud))
  cbind(m, seed = task$seeds - task$base)
}
