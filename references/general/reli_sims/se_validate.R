# Phase 2: influence / jackknife / normal SE vs true SD of z with missing data.
# Note: its ICC(1) rows are invalid (crossed item effects); see se_validate_oneway.R.
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

library(parallel)
cl <- makeCluster(12)
invisible(clusterEvalQ(cl, suppressMessages(devtools::load_all(".", quiet = TRUE))))
nsim <- 600
cells <- expand.grid(n = c(20, 40), miss = c(0, 0.15, 0.30), mech = c("MCAR", "MAR"), dist = c("normal", "non-normal"),
                     stringsAsFactors = FALSE)
cells <- cells[!(cells$miss == 0 & cells$mech == "MAR"), ]
one <- function(seed, n, miss, mech, dist) {
  set.seed(seed); k <- 3
  s <- if (dist == "normal") rnorm(n) else (rchisq(n, 3) - 3) / sqrt(6)
  e <- if (dist == "normal") rnorm(n * k, 0, .6) else rt(n * k, 5) * sqrt(.36 * 3 / 5)
  Y <- outer(s, rep(1, k)) + outer(rep(1, n), rnorm(k, 0, .3)) + matrix(e, n)
  if (miss > 0) {
    M <- matrix(FALSE, n, k)
    p <- if (mech == "MCAR") rep(miss * k / (k - 1), n) else plogis(qlogis(miss * k / (k - 1)) + 1.5 * as.vector(scale(Y[, 1])))
    M[, -1] <- runif(n * (k - 1)) < p
    Y[M] <- NA
  }
  prep <- .reli_prep_from_long(data.frame(values = as.vector(Y), items = rep(1:k, each = n), id = rep(1:n, k)))
  method <- if (prep$balanced) "anova" else "reml"
  out <- c()
  for (fm in c("C1", "A1", "1")) {
    model <- if (fm == "1") "oneway" else "twoway"
    f <- suppressWarnings(.reli_fit(prep, method, model))
    out <- c(out, setNames(c(.icc_z_fit(f, fm),
      vapply(c("influence", "jackknife", "normal"), function(m) suppressWarnings(.icc_se_z(prep, f, fm, m)), 1)),
      paste(fm, c("z", "influence", "jackknife", "normal"), sep = "_")))
  }
  out
}
clusterExport(cl, "one")
res <- lapply(seq_len(nrow(cells)), function(i) {
  c0 <- cells[i, ]
  m <- do.call(rbind, parLapply(cl, seq_len(nsim) + 9000 * i, function(s, n, miss, mech, dist) one(s, n, miss, mech, dist),
                                n = c0$n, miss = c0$miss, mech = c0$mech, dist = c0$dist))
  unlist(lapply(c("C1", "A1", "1"), function(fm) {
    sdz <- sd(m[, paste0(fm, "_z")], na.rm = TRUE)
    setNames(c(sdz, colMeans(m[, paste0(fm, c("_influence", "_jackknife", "_normal"))], na.rm = TRUE) / sdz),
             paste(fm, c("trueSD", "infl/true", "jack/true", "norm/true"), sep = ":"))
  }))
})
stopCluster(cl)
tab <- cbind(cells, round(do.call(rbind, res), 3))
for (fm in c("C1", "A1", "1")) { cat("\n==", fm, ": mean SE / true SD of z ==\n"); print(tab[, c("n", "miss", "mech", "dist", names(tab)[startsWith(names(tab), paste0(fm, ":"))])], row.names = FALSE) }
