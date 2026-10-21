# Check fast_vc.R's vc_ccc() against the package's lme4-based .ccc_vc() on
# 200 random data sets across the simulation grid, including jackknife and
# bootstrap (weighted) samples. Run from the package root.
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("references/general/ccc_sims/fast_vc.R")
source("references/general/ccc_sims/gen.R")
long <- function(dat) {
  d <- tidyr::drop_na(tidyr::pivot_longer(dat, !id, names_to = "method",
                                          values_to = "measure"))
  as.data.frame(d)
}
set.seed(20261019)
res <- t(replicate(200, {
  g <- sample(c(10, 20, 50), 1); k <- sample(c(2, 3, 6), 1)
  sim <- gen_data("reps", g, k, sample(c(.5, .8, .95), 1), runif(1) < .5,
                  runif(1) < .5, sample(c("normal", "nonnormal"), 1))
  dat <- sim$dat
  st <- vc_stats(dat$id, dat$x, dat$y)
  # one plain fit, one jackknife sample, one bootstrap sample
  w_boot <- tabulate(sample(g, g, TRUE), g)
  drop1 <- sample(g, 1)
  # bootstrap sample as data, with fresh ids for repeated subjects
  bid <- rep(seq_len(g), w_boot)
  bdat <- do.call(rbind, lapply(seq_along(bid), function(j) {
    s <- dat[dat$id == bid[j], ]; s$id <- j; s }))
  c(lmer = SimplyAgree:::.ccc_vc(long(dat)), fast = vc_ccc(st),
    lmer_j = SimplyAgree:::.ccc_vc(long(dat[dat$id != drop1, ])),
    fast_j = vc_ccc(st, replace(rep(1, g), drop1, 0)),
    lmer_b = SimplyAgree:::.ccc_vc(long(bdat)), fast_b = vc_ccc(st, w_boot))
}))
d <- abs(res[, c(1, 3, 5)] - res[, c(2, 4, 6)])
cat("max |lmer - fast|: full", signif(max(d[, 1]), 3), " jack",
    signif(max(d[, 2]), 3), " boot", signif(max(d[, 3]), 3), "\n")
cat("quantiles of all diffs:\n"); print(signif(quantile(d, c(.5, .9, .99, 1)), 3))
cat("n > 1e-6:", sum(d > 1e-6), " n > 1e-4:", sum(d > 1e-4), "of", length(d), "\n")
bench <- gen_data("reps", 20, 3, .8, TRUE, TRUE, "normal")$dat
stb <- vc_stats(bench$id, bench$x, bench$y)
cat("ms per fit: fast", 1000 * system.time(for (i in 1:200) vc_ccc(stb))[[3]] / 200,
    " lmer", 1000 * system.time(for (i in 1:20) SimplyAgree:::.ccc_vc(long(bench)))[[3]] / 20, "\n")
