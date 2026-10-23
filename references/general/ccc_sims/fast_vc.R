# Fast REML for the variance-components CCC of replicate data (simulation
# helper only; the package uses lme4 in .ccc_vc()).
#
# Model: measure ~ method + (1 | id) + (1 | id:method), with the id:method
# term dropped when no subject has replicates of a method (as .ccc_vc()).
# The REML criterion is profiled over the relative SDs theta = (t1, t2)
# (random-effect SD / residual SD) as in lme4, and each subject's
# contribution needs only its sufficient statistics, so a bootstrap resample
# is a vector of subject weights (counts) and a jackknife sample sets one
# weight to 0.
#
# Subject statistics (one row per subject): nx, ny (non-missing counts),
# Sx, Sy (sums), Q (sum of squares of all of the subject's measurements).

vc_stats <- function(id, x, y) {
  id <- factor(id)
  f <- function(v, fun) as.vector(tapply(v, id, fun, default = 0))
  ox <- !is.na(x); oy <- !is.na(y)
  cbind(nx = f(ox, sum), ny = f(oy, sum),
        Sx = f(ifelse(ox, x, 0), sum), Sy = f(ifelse(oy, y, 0), sum),
        Q = f(ifelse(ox, x, 0)^2 + ifelse(oy, y, 0)^2, sum))
}

# Pieces of the REML criterion at theta for weights w (counts per subject)
.vc_pieces <- function(th, st, w) {
  t1 <- th[1]; t2 <- th[2]
  nx <- st[, "nx"]; ny <- st[, "ny"]; n <- nx + ny
  Sx <- st[, "Sx"]; Sy <- st[, "Sy"]; S <- Sx + Sy
  M11 <- 1 + t1^2 * n; M12 <- t1 * t2 * nx; M13 <- t1 * t2 * ny
  M22 <- 1 + t2^2 * nx; M33 <- 1 + t2^2 * ny
  det <- M11 * M22 * M33 - M12^2 * M33 - M13^2 * M22
  i11 <- M22 * M33 / det; i12 <- -M12 * M33 / det; i13 <- -M13 * M22 / det
  i22 <- (M11 * M33 - M13^2) / det; i23 <- M12 * M13 / det
  i33 <- (M11 * M22 - M12^2) / det
  qf <- function(u1, u2, u3, v1, v2, v3) {
    u1 * (i11 * v1 + i12 * v2 + i13 * v3) +
      u2 * (i12 * v1 + i22 * v2 + i23 * v3) +
      u3 * (i13 * v1 + i23 * v2 + i33 * v3)
  }
  # A = Lambda Z'X (columns intercept, method y), c = Lambda Z'y
  a1 <- list(t1 * n, t2 * nx, t2 * ny)
  a2 <- list(t1 * ny, 0, t2 * ny)
  cc <- list(t1 * S, t2 * Sx, t2 * Sy)
  q <- function(u, v) qf(u[[1]], u[[2]], u[[3]], v[[1]], v[[2]], v[[3]])
  XtX <- matrix(c(sum(w * (n - q(a1, a1))), sum(w * (ny - q(a1, a2))),
                  sum(w * (ny - q(a1, a2))), sum(w * (ny - q(a2, a2)))), 2)
  Xty <- c(sum(w * (S - q(a1, cc))), sum(w * (Sy - q(a2, cc))))
  yty <- sum(w * (st[, "Q"] - q(cc, cc)))
  list(XtX = XtX, Xty = Xty, yty = yty, logdet = sum(w * log(det)),
       N = sum(w * n))
}

.vc_dev <- function(th, st, w) {
  p <- .vc_pieces(th, st, w)
  ch <- tryCatch(chol(p$XtX), error = function(e) NULL)
  if (is.null(ch)) return(Inf)
  beta <- backsolve(ch, forwardsolve(t(ch), p$Xty))
  rss <- p$yty - sum(beta * p$Xty)
  if (!(rss > 0)) return(Inf)
  df <- p$N - 2
  p$logdet + 2 * sum(log(diag(ch))) + df * (1 + log(2 * pi * rss / df))
}

# Variance-components CCC from subject statistics st with weights w.
# Returns NA if the fit is not usable.
vc_ccc <- function(st, w = rep(1, nrow(st)), start = NULL) {
  keep <- w > 0
  st <- st[keep, , drop = FALSE]; w <- w[keep]
  inter <- any(st[, "nx"] > 1 | st[, "ny"] > 1)
  # BOBYQA on theta with lower bound 0, as lme4 (start at 1)
  ctrl <- list(rhobeg = 0.2, rhoend = 1e-9)
  if (inter) {
    if (is.null(start)) start <- c(1, 1)
    o <- minqa::bobyqa(start, function(th) vc_dev_fun(th, st, w),
                       lower = c(0, 0), control = ctrl)
    th <- o$par
  } else {
    o <- stats::optimize(function(a) vc_dev_fun(c(a, 0), st, w), c(0, 100),
                         tol = 1e-10)
    th <- c(o$minimum, 0)
  }
  p <- .vc_pieces(th, st, w)
  XtXi <- solve(p$XtX)
  beta <- XtXi %*% p$Xty
  df <- p$N - 2
  s2 <- (p$yty - sum(beta * p$Xty)) / df
  s2_id <- th[1]^2 * s2
  s2_int <- th[2]^2 * s2
  s2_met <- max((beta[2]^2 - s2 * XtXi[2, 2]) / 2, 0)
  out <- s2_id / (s2_id + s2_int + s2_met + s2)
  attr(out, "theta") <- th
  out
}

# Use the C++ criterion (vc_dev.cpp, same formulas) when it compiles; set
# options(ccc_sims.cpp = FALSE) before sourcing to use the R version.
vc_dev_fun <- .vc_dev
if (isTRUE(getOption("ccc_sims.cpp", TRUE))) {
  # path relative to the package root (scripts are run from there)
  Rcpp::sourceCpp(file.path(getOption("ccc_sims.dir", "references/general/ccc_sims"),
                            "vc_dev.cpp"))
  vc_dev_fun <- vc_dev_cpp
}
