# bls_reg() standard errors: se_method = "delta" (default) against
# "bivregbls" as the reliability of x falls (see README.md, study 6).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_se.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# t ~ N(20, t_sd^2); x = t + e_x (SD 1), y = 1 + 1.1 t + e_y (SD 1.2):
# - "known": one measurement each, variances given;
# - "reps": 3 exchangeable replicates of each method;
# - "linked": 3 occasions, occasion effect (SD 2) shared by both methods.
# The reliability of a subject mean of x is t_sd^2 / (t_sd^2 + its error
# variance).
grid <- expand.grid(n = c(15, 40), t_sd = c(1.5, 3, 6),
                    design = c("known", "reps", "linked"),
                    stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim, conf) {
  set.seed(9700 + i)
  n <- g$n[i]; t_sd <- g$t_sd[i]; design <- g$design[i]
  sx <- 1; sy <- 1.2; a <- 1; b <- 1.1
  m <- if (design == "known") 1 else 3
  om <- if (design == "linked") 2 else 0
  gen <- function() {
    t <- rnorm(n, 20, t_sd)
    id <- rep(seq_len(n), each = m)
    occ <- rnorm(n * m, 0, om)
    data.frame(id = id, x = t[id] + occ + rnorm(n * m, 0, sx),
               y = a + b * (t[id] + occ) + rnorm(n * m, 0, sy))
  }
  fit <- function(dat, se) {
    switch(design,
           known = bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2,
                           equation_error = FALSE, se_method = se),
           reps = bls_reg(y ~ x, data = dat, id = "id",
                          equation_error = FALSE, se_method = se),
           linked = bls_reg(y ~ x, data = dat, id = "id",
                            replicates = "linked", equation_error = FALSE,
                            se_method = se))
  }
  cover <- function(f) {
    mt <- f$model_table
    cb <- f$cbls$model_table
    g1 <- 2 * (1 - b) / (1 + b)
    c(mt$lower.ci[2] <= b & b <= mt$upper.ci[2],
      mt$lower.ci[1] <= a & a <= mt$upper.ci[1],
      cb$lower.ci[2] <= g1 & g1 <= cb$upper.ci[2])
  }
  res <- matrix(NA, n_sim, 8,
                dimnames = list(NULL, c("slope_delta", "int_delta",
                                        "cbls_delta", "slope_bls",
                                        "int_bls", "cbls_bls", "slope",
                                        "se_ratio")))
  for (s in seq_len(n_sim)) {
    dat <- gen()
    fd <- tryCatch(suppressWarnings(fit(dat, "delta")),
                   error = function(e) NULL)
    fb <- tryCatch(suppressWarnings(fit(dat, "bivregbls")),
                   error = function(e) NULL)
    if (is.null(fd) || is.null(fb)) next
    res[s, ] <- c(cover(fd), cover(fb), unname(coef(fd)[2]),
                  sqrt(fd$vcov[2, 2] / fb$vcov[2, 2]))
  }
  ok <- !is.na(res[, 1])
  r <- res[ok, ]
  # the actual SD of the slope, against the mean SEs
  data.frame(g[i, ], n_ok = sum(ok),
             t(colMeans(r[, 1:6])),
             slope_sd = stats::sd(r[, "slope"]),
             se_ratio = mean(r[, "se_ratio"]))
}
clusterExport(cl, "one_cell")

out <- do.call(rbind, parLapplyLB(cl, seq_len(nrow(grid)), one_cell,
                                  g = grid, n_sim = N_SIM, conf = conf))
write.csv(out, file.path(out_dir, "summary_se.csv"), row.names = FALSE)
print(out, digits = 3)
