# bls_reg(replicates = "linked") (see README.md, study 5).
# Run from the package root:
#   Rscript references/general/cbls_sims/sim_cbls_linked.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# t ~ N(20, 5^2) per item; a ~ N(0, omega^2) per item and occasion, shared
# by both methods; c_x, c_y ~ N(0, tau^2) per item (equation error);
# x = t + a + c_x + e_x (SD 0.6), y = 1 + 1.1 (t + a + c_y) + e_y (SD 0.9),
# both methods on the same `reps` occasions
grid <- expand.grid(n = c(15, 40), reps = c(2, 4), omega = c(0.5, 2),
                    tau = c(0, 0.5))

one_cell <- function(i, g, n_sim, conf) {
  set.seed(9500 + i)
  n <- g$n[i]; m <- g$reps[i]; om <- g$omega[i]; tau <- g$tau[i]
  sx <- 0.6; sy <- 0.9; a <- 1; b <- 1.1
  gen_one <- function(n_items, n_occ) {
    t <- rnorm(n_items, 20, 5)
    cx <- rnorm(n_items, 0, tau)
    cy <- rnorm(n_items, 0, tau)
    id <- rep(seq_len(n_items), each = n_occ)
    occ <- rnorm(n_items * n_occ, 0, om)
    data.frame(id = id, repl = rep(seq_len(n_occ), n_items),
               x = t[id] + occ + cx[id] + rnorm(n_items * n_occ, 0, sx),
               y = a + b * (t[id] + occ + cy[id]) +
                 rnorm(n_items * n_occ, 0, sy))
  }
  res <- matrix(NA, n_sim, 8,
                dimnames = list(NULL, c("slope", "intercept", "pi", "pi_md",
                                        "slope_exch", "pi_exch", "rho",
                                        "pi_width_ratio")))
  for (s in seq_len(n_sim)) {
    dat <- gen_one(n, m)
    fl <- tryCatch(suppressWarnings(
      bls_reg(y ~ x, data = dat, id = "id", replicates = "linked")),
      error = function(e) NULL)
    fe <- tryCatch(suppressWarnings(bls_reg(y ~ x, data = dat, id = "id")),
                   error = function(e) NULL)
    if (is.null(fl) || is.null(fe)) next
    # a new item measured once by both methods on one occasion
    new <- gen_one(1, 1)
    inside <- function(p, v) p$lwr <= v & v <= p$upr
    pl <- predict(fl, new$x, interval = "prediction")
    pe <- predict(fe, new$x, interval = "prediction")
    res[s, ] <- c(
      fl$model_table$lower.ci[2] <= b & b <= fl$model_table$upper.ci[2],
      fl$model_table$lower.ci[1] <= a & a <= fl$model_table$upper.ci[1],
      inside(pl, new$y),
      inside(predict(fl, (new$x + new$y) / 2, type = "md",
                     interval = "prediction"), new$x - new$y),
      fe$model_table$lower.ci[2] <= b & b <= fe$model_table$upper.ci[2],
      inside(pe, new$y),
      fl$errors$rho,
      (pl$upr - pl$lwr) / (pe$upr - pe$lwr))
  }
  ok <- !is.na(res[, 1])
  data.frame(g[i, ], n_ok = sum(ok),
             t(colMeans(res[ok, ], na.rm = TRUE)))
}
clusterExport(cl, "one_cell")

summ <- do.call(rbind, parLapplyLB(cl, seq_len(nrow(grid)), one_cell,
                                   g = grid, n_sim = N_SIM, conf = conf))
write.csv(summ, file.path(out_dir, "summary_linked.csv"), row.names = FALSE)
print(summ, digits = 3)
