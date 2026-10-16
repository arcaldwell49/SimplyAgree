# Phase 1: coverage of REML intervals with observed-data df, all ICC forms and
# SEM rows, MCAR/MAR missingness (Section 3 table).
# Run from the package root (it calls devtools::load_all(".")). Written against the
# engine as of 2026-10-15/16 on branch reli-update; internal function names may
# have changed since. Results are recorded in references/general/reli_redesign.md.

suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(20261015)
R <- 400; n <- 20; k <- 3
v_id <- 1; v_it <- 0.09; v_e <- 0.43
truth <- c("1" = v_id / (v_id + v_it + v_e), "k" = v_id / (v_id + (v_it + v_e) / k),
           C1 = v_id / (v_id + v_e), Ck = v_id / (v_id + v_e / k),
           A1 = v_id / (v_id + v_it + v_e), Ak = v_id / (v_id + (v_it + v_e) / k),
           sem_C = sqrt(v_e), sem_A = sqrt(v_it + v_e), sem_1 = sqrt(v_it + v_e))
gen <- function(oneway) {
  it <- if (oneway) matrix(rnorm(n * k, 0, sqrt(v_it)), n) else outer(rep(1, n), rnorm(k, 0, sqrt(v_it)))
  outer(rnorm(n, 0, sqrt(v_id)), rep(1, k)) + it + matrix(rnorm(n * k, 0, sqrt(v_e)), n)
}
make_missing <- function(Y, p, mech) {
  if (p == 0) return(Y)
  M <- matrix(FALSE, n, k)
  if (mech == "MCAR") M[, -1] <- runif(n * (k - 1)) < p * k / (k - 1)
  else { # MAR: later trials more likely missing for subjects with high trial-1 values
    pr <- plogis(qlogis(p * k / (k - 1)) + 1.5 * as.vector(scale(Y[, 1])))
    M[, -1] <- runif(n * (k - 1)) < pr
  }
  Y[M] <- NA; Y
}
cover <- function(ci, t) ci[1] <= t && t <= ci[2]
run <- function(p, mech) {
  hits <- replicate(R, {
    out <- c()
    for (ow in c(FALSE, TRUE)) {
      Y <- make_missing(gen(ow), p, mech)
      prep <- .reli_prep_from_long(data.frame(values = as.vector(Y), items = rep(1:k, each = n), id = rep(1:n, k)))
      if (ow) {
        f <- .reli_fit(prep, "reml", "oneway", keep_fit = FALSE)
        out <- c(out, `1` = cover(.icc_ci(f, "1"), truth[["1"]]), k = cover(.icc_ci(f, "k"), truth[["k"]]))
        s <- .sem_var(f, "oneway", "agreement"); out <- c(out, sem_1 = cover(sqrt(.var_ci(s$var, s$df)), truth[["sem_1"]]))
      } else {
        f <- .reli_fit(prep, "reml", "twoway", keep_fit = FALSE)
        for (fm in c("C1", "Ck", "A1", "Ak")) out[fm] <- cover(.icc_ci(f, fm), truth[[fm]])
        s <- .sem_var(f, "twoway", "consistency"); out["sem_C"] <- cover(sqrt(.var_ci(s$var, s$df)), truth[["sem_C"]])
        s <- .sem_var(f, "twoway", "agreement"); out["sem_A"] <- cover(sqrt(.var_ci(s$var, s$df)), truth[["sem_A"]])
      }
    }
    out
  })
  rowMeans(hits)
}
res <- rbind(`0%` = run(0, "MCAR"), `15% MCAR` = run(.15, "MCAR"), `30% MCAR` = run(.30, "MCAR"),
             `15% MAR` = run(.15, "MAR"), `30% MAR` = run(.30, "MAR"))
print(round(res[, c("1", "k", "C1", "Ck", "A1", "Ak", "sem_1", "sem_C", "sem_A")], 3))
cat("MC SE at 0.95 with R =", R, ":", round(sqrt(.95 * .05 / R), 3), "\n")
