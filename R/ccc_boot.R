# Subject bootstrap for ccc_test(data_type = "reps") ---------------------------
#
# The studentized interval needs a jackknife SE inside every bootstrap
# replicate (g fits per replicate), too slow with lme4. The REML fit of the
# variance-components model is therefore computed here from each subject's
# sufficient statistics, so a resample is a vector of subject counts w and a
# jackknife sample lowers one count by 1. It matches .ccc_vc() (lme4) to
# about 5e-5, lme4's optimizer tolerance (references/general/ccc_sims).

#' Subject sufficient statistics for the variance-components CCC
#'
#' One row per subject with any data: `nx`, `ny` (non-missing counts), `Sx`,
#' `Sy` (sums) and `Q` (sum of squares of all of the subject's measurements).
#' `df` has columns id, x and y.
#' @noRd
.ccc_vc_stats <- function(df) {
  id <- factor(df$id)
  f <- function(v) as.vector(tapply(v, id, sum, default = 0))
  ox <- !is.na(df$x)
  oy <- !is.na(df$y)
  x0 <- ifelse(ox, df$x, 0)
  y0 <- ifelse(oy, df$y, 0)
  st <- cbind(nx = f(ox), ny = f(oy), Sx = f(x0), Sy = f(y0),
              Q = f(x0^2 + y0^2))
  st[st[, "nx"] + st[, "ny"] > 0, , drop = FALSE]
}

#' Pieces of the REML criterion of measure ~ method + (1 | id) +
#' (1 | id:method) at relative SDs th = (t1, t2), with subject weights w
#' @noRd
.ccc_vc_pieces <- function(th, st, w) {
  t1 <- th[1]
  t2 <- th[2]
  nx <- st[, "nx"]
  ny <- st[, "ny"]
  n <- nx + ny
  Sx <- st[, "Sx"]
  Sy <- st[, "Sy"]
  # per-subject M = I + Lambda Z'Z Lambda (3 x 3: subject, x, y) and inverse
  M11 <- 1 + t1^2 * n
  M12 <- t1 * t2 * nx
  M13 <- t1 * t2 * ny
  M22 <- 1 + t2^2 * nx
  M33 <- 1 + t2^2 * ny
  det <- M11 * M22 * M33 - M12^2 * M33 - M13^2 * M22
  i11 <- M22 * M33 / det
  i12 <- -M12 * M33 / det
  i13 <- -M13 * M22 / det
  i22 <- (M11 * M33 - M13^2) / det
  i23 <- M12 * M13 / det
  i33 <- (M11 * M22 - M12^2) / det
  q <- function(u, v) {
    u[[1]] * (i11 * v[[1]] + i12 * v[[2]] + i13 * v[[3]]) +
      u[[2]] * (i12 * v[[1]] + i22 * v[[2]] + i23 * v[[3]]) +
      u[[3]] * (i13 * v[[1]] + i23 * v[[2]] + i33 * v[[3]])
  }
  # Lambda Z'X (columns intercept and method y) and Lambda Z'y
  a1 <- list(t1 * n, t2 * nx, t2 * ny)
  a2 <- list(t1 * ny, 0, t2 * ny)
  cc <- list(t1 * (Sx + Sy), t2 * Sx, t2 * Sy)
  x12 <- sum(w * (ny - q(a1, a2)))
  list(XtX = matrix(c(sum(w * (n - q(a1, a1))), x12,
                      x12, sum(w * (ny - q(a2, a2)))), 2),
       Xty = c(sum(w * (Sx + Sy - q(a1, cc))), sum(w * (Sy - q(a2, cc)))),
       yty = sum(w * (st[, "Q"] - q(cc, cc))),
       logdet = sum(w * log(det)),
       N = sum(w * n))
}

#' REML deviance (up to a constant) at th
#' @noRd
.ccc_vc_dev <- function(th, st, w) {
  p <- .ccc_vc_pieces(th, st, w)
  ch <- tryCatch(chol(p$XtX), error = function(e) NULL)
  if (is.null(ch)) {
    return(Inf)
  }
  beta <- backsolve(ch, forwardsolve(t(ch), p$Xty))
  rss <- p$yty - sum(beta * p$Xty)
  if (!(rss > 0)) {
    return(Inf)
  }
  df <- p$N - 2
  p$logdet + 2 * sum(log(diag(ch))) + df * (1 + log(2 * pi * rss / df))
}

#' Variance-components CCC from subject statistics with weights w
#'
#' As .ccc_vc(): the subject:method term only when some subject has
#' replicates of a method, s2_method = (b^2 - var(b)) / 2 truncated at 0.
#' The REML criterion is minimised over th >= 0 with BOBYQA from `start`
#' (lme4's optimizer and default start). Returns the CCC with the optimum
#' `theta` as an attribute.
#' @noRd
.ccc_vc_fit <- function(st, w = rep(1, nrow(st)), start = c(1, 1)) {
  keep <- w > 0
  st <- st[keep, , drop = FALSE]
  w <- w[keep]
  if (any(st[, "nx"] > 1 | st[, "ny"] > 1)) {
    o <- minqa::bobyqa(start, .ccc_vc_dev, lower = c(0, 0),
                       control = list(rhobeg = 0.2, rhoend = 1e-7),
                       st = st, w = w)
    th <- o$par
  } else {
    o <- stats::optimize(function(a) .ccc_vc_dev(c(a, 0), st, w), c(0, 100),
                         tol = 1e-10)
    th <- c(o$minimum, 0)
  }
  p <- .ccc_vc_pieces(th, st, w)
  XtXi <- solve(p$XtX)
  beta <- XtXi %*% p$Xty
  s2 <- (p$yty - sum(beta * p$Xty)) / (p$N - 2)
  s2_id <- th[1]^2 * s2
  s2_int <- th[2]^2 * s2
  s2_met <- max((beta[2]^2 - s2 * XtXi[2, 2]) / 2, 0)
  structure(s2_id / (s2_id + s2_int + s2_met + s2), theta = th)
}

#' Subject bootstrap of the replicate-data CCC
#'
#' Returns a list in the form .boot_interval() and .boot_pvalue() expect:
#' `t0` and the rows of `t` hold `est` (CCC), `z` (Fisher z) and `se_z` (the
#' delete-one-subject jackknife SE of z, only for `boot_ci = "stud"`), `jack`
#' holds the leave-one-subject-out estimates (for BCa). A subject drawn
#' twice counts as two subjects. The inner jackknife of a replicate deletes
#' one drawn copy at a time, weighted by the number of copies.
#' @noRd
.ccc_boot_reps <- function(df, R = 1999, boot_ci = "stud") {
  st <- .ccc_vc_stats(df)
  g <- nrow(st)
  fit <- function(w, start = c(1, 1)) {
    tryCatch(.ccc_vc_fit(st, w, start), error = function(e) NA_real_)
  }
  # jackknife over the drawn subjects of weights w, warm-started at th
  jack_z <- function(w, th) {
    drawn <- which(w > 0)
    z <- vapply(drawn, function(i) {
      wl <- w
      wl[i] <- wl[i] - 1
      atanh(as.numeric(fit(wl, th)))
    }, numeric(1))
    list(z = z, wts = w[drawn])
  }
  jack_se <- function(j) {
    if (!all(is.finite(j$z))) {
      return(NA_real_)
    }
    m <- sum(j$wts * j$z) / sum(j$wts)
    sqrt((sum(j$wts) - 1) / sum(j$wts) * sum(j$wts * (j$z - m)^2))
  }
  one <- function(w, studentize) {
    est <- fit(w)
    if (!is.finite(est) || abs(est) >= 1) {
      return(c(est = NA_real_, z = NA_real_, se_z = NA_real_))
    }
    se_z <- if (studentize) {
      jack_se(jack_z(w, pmax(attr(est, "theta"), 1e-4)))
    } else {
      NA_real_
    }
    c(est = as.numeric(est), z = atanh(as.numeric(est)), se_z = se_z)
  }

  ones <- rep(1, g)
  est0 <- fit(ones)
  j0 <- jack_z(ones, pmax(attr(est0, "theta"), 1e-4))
  t0 <- c(est = as.numeric(est0), z = atanh(as.numeric(est0)),
          se_z = jack_se(j0))
  stud <- boot_ci == "stud"
  t_all <- t(vapply(seq_len(R), function(r) {
    w <- tabulate(sample.int(g, g, replace = TRUE), g)
    if (sum(w > 0) < 3) {
      return(c(est = NA_real_, z = NA_real_, se_z = NA_real_))
    }
    one(w, stud)
  }, numeric(3)))
  usable <- is.finite(t_all[, "est"]) & (!stud | is.finite(t_all[, "se_z"]))
  list(t0 = t0,
       t = t_all[usable, , drop = FALSE],
       R = R,
       n_failed = sum(!usable),
       jack = cbind(est = tanh(j0$z)))
}
