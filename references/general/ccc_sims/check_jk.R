# Check that sim_funs.R's jackknife-t interval reproduces ccc_test() for
# both data types (so the simulated "current method" is the package's).
# Run from the package root.
suppressMessages(devtools::load_all(".", quiet = TRUE))
dir <- "references/general/ccc_sims"
B <- 9; Bs <- 0
source(file.path(dir, "sim_funs.R")); worker_init(dir)
grid <- expand.grid(g = c(10, 50), k = c(2, 6), rho = c(.5, .95),
                    diff = c(FALSE, TRUE), miss = c(FALSE, TRUE),
                    dist = c("normal", "nonnormal"), stringsAsFactors = FALSE)
for (type in c("reps", "nest")) {
  d <- sapply(seq_len(nrow(grid)), function(r) {
    cell <- as.list(grid[r, ])
    o <- one_data_set(r, cell, type, stud = FALSE)
    set.seed(r)
    dat <- gen_data(type, cell$g, cell$k, cell$rho, cell$diff, cell$miss,
                    cell$dist)$dat
    ct <- ccc_test("x", "y", "id", data = dat, data_type = type)
    max(abs(c(o["est"] - ct$estimate, o[c("jk_lo", "jk_hi")] - ct$conf.int)))
  })
  cat(type, ": max |sim - ccc_test| over", length(d), "data sets:",
      signif(max(d), 3), "\n")
}
