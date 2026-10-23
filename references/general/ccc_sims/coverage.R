# Coverage of ccc_test() 95% intervals for replicate and nested data
# (delete-one-subject jackknife SE of Fisher z; normal vs t(g - 1) quantiles).
# Pre-set criterion: coverage within 92.5-97.5%. 1000 data sets per cell.
# Results (2026-10-19):
#   nest g = 10  normal: 0.895  t: 0.928
#   nest g = 20  normal: 0.918  t: 0.942
#   nest g = 50  normal: 0.946  t: 0.949
#   reps g = 10  normal: 0.880  t: 0.913
#   reps g = 20  normal: 0.931  t: 0.940
#   reps g = 50  normal: 0.936  t: 0.941
# Decision: t(g - 1) quantiles for reps/nest. (ccc_test() now uses t, so the
# "normal" column below reproduces the t column; the normal results came
# from the version before this change.)
library(parallel)
cl <- makeCluster(max(1, detectCores() - 2))
clusterEvalQ(cl, suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE)))
sim_one <- function(seed, g, type) {
  set.seed(seed)
  sa <- 1; sab <- 0.25; se <- 0.5; d <- 0.5; sgam <- 0.5
  if (type == "reps") {
    k <- 3
    id <- rep(seq_len(g), each = k)
    a <- rnorm(g, 0, sqrt(sa))[id]
    x <- a + rnorm(g, 0, sqrt(sab))[id] + rnorm(g * k, 0, sqrt(se))
    y <- d + a + rnorm(g, 0, sqrt(sab))[id] + rnorm(g * k, 0, sqrt(se))
    y[sample(g * k, round(0.1 * g * k))] <- NA
    truth <- sa / (sa + sab + d^2 / 2 + se)
  } else {
    k <- 4
    id <- rep(seq_len(g), each = k)
    t <- rnorm(g, 0, sqrt(sa))[id] + rnorm(g * k, 0, sqrt(sgam))
    x <- t + rnorm(g * k, 0, sqrt(se)); y <- d + t + rnorm(g * k, 0, sqrt(se))
    v <- sa + sgam
    truth <- 2 * v / (2 * (v + se) + d^2)
  }
  dat <- data.frame(id = id, x = x, y = y)
  r <- tryCatch(ccc_test("x", "y", "id", data = dat, data_type = type), error = function(e) NULL)
  if (is.null(r)) return(c(NA, NA, NA))
  z <- atanh(unname(r$estimate)); s <- r$stderr[["SE Z"]]; ci_t <- tanh(z + c(-1,1)*qt(.975, g-1)*s)
  c(cover = r$conf.int[1] <= truth & truth <= r$conf.int[2], cover_t = ci_t[1] <= truth & truth <= ci_t[2], hi_miss_t = truth > ci_t[2])
}
clusterExport(cl, "sim_one")
for (type in c("nest", "reps")) for (g in c(10, 20, 50)) {
  out <- do.call(rbind, parLapply(cl, 1:1000, sim_one, g = g, type = type))
  cat(type, "g =", g, " normal:", round(mean(out[,1], na.rm=TRUE), 3), " t:", round(mean(out[,2], na.rm=TRUE), 3),
      " t miss-above:", round(mean(out[,3], na.rm=TRUE), 3), " fails:", sum(is.na(out[,1])), "\n")
}
stopCluster(cl)
