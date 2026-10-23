# Data generators for the ccc_test() interval simulations.
# Total variance 1 in both designs; the true CCC is rho.
#   reps: x_ij = a_i + b_ix + e, y_ij = d + a_i + b_iy + e (b = subject x
#         method, k replicates per method); truth (Carrasco & Jover) =
#         s_a / (s_a + s_ab + d^2/2 + s_e).
#         s_a = rho; the rest is split s_ab : s_e = 1 : 3 (d = 0) or
#         s_ab : d^2/2 : s_e = 1 : 1 : 2 (d != 0).
#   nest: true value t_ij = a_i + g_ij varies within subject (k pairs);
#         x = t + e, y = d + t + e; truth (Lin, over pairs) =
#         2v / (2(v + s_e) + d^2), v = s_a + s_g = rho, s_a : s_g = 2 : 1,
#         s_e = 1 - rho (d = 0) or s_e = .75(1 - rho), d^2/2 = .25(1 - rho).
# dist "nonnormal": subject effects from a standardized chi-square(3),
# within-subject terms and errors from a standardized t(5).
# miss: drop 15% of the measurements at random (reps), or one member of
# 15% of the pairs (nest).

rz <- function(n, kind, dist) {
  if (dist == "normal") return(rnorm(n))
  if (kind == "subject") (rchisq(n, 3) - 3) / sqrt(6) else rt(n, 5) / sqrt(5 / 3)
}

gen_data <- function(type, g, k, rho, diff, miss, dist) {
  id <- rep(seq_len(g), each = k)
  if (type == "reps") {
    rest <- 1 - rho
    if (diff) { sab <- rest / 4; d <- sqrt(2 * rest / 4); se <- rest / 2 }
    else { sab <- rest / 4; d <- 0; se <- 3 * rest / 4 }
    a <- sqrt(rho) * rz(g, "subject", dist)
    x <- a[id] + sqrt(sab) * rz(g, "w", dist)[id] + sqrt(se) * rz(g * k, "w", dist)
    y <- d + a[id] + sqrt(sab) * rz(g, "w", dist)[id] + sqrt(se) * rz(g * k, "w", dist)
    if (miss) {
      drop <- sample(2 * g * k, round(0.15 * 2 * g * k))
      x[drop[drop <= g * k]] <- NA
      y[drop[drop > g * k] - g * k] <- NA
    }
    truth <- rho / (rho + sab + d^2 / 2 + se)
  } else {
    rest <- 1 - rho
    if (diff) { se <- 0.75 * rest; d <- sqrt(2 * 0.25 * rest) }
    else { se <- rest; d <- 0 }
    t <- sqrt(2 * rho / 3) * rz(g, "subject", dist)[id] +
      sqrt(rho / 3) * rz(g * k, "w", dist)
    x <- t + sqrt(se) * rz(g * k, "w", dist)
    y <- d + t + sqrt(se) * rz(g * k, "w", dist)
    if (miss) {
      pr <- sample(g * k, round(0.15 * g * k))
      half <- runif(length(pr)) < 0.5
      x[pr[half]] <- NA; y[pr[!half]] <- NA
    }
    truth <- 2 * rho / (2 * (rho + se) + d^2)
  }
  list(dat = data.frame(id = id, x = x, y = y), truth = truth)
}
