# bls_reg() with equation error (see README.md, study 3).
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_eq.R
library(parallel)

out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "1000"))
conf <- 0.95

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))

# t ~ N(20, 5^2); x = t + e_x (SD 0.6); y = 1 + 1.1 t + delta + e_y (SD 0.9),
# delta ~ N(0, sd_eq^2) per subject (equation error); replicates or known
# variances with one measurement each
grid <- expand.grid(n = c(20, 60), reps = c("known", "2", "3"),
                    sd_eq = c(0, 0.5, 1.5), stringsAsFactors = FALSE)

one_cell <- function(i, g, n_sim, conf) {
  set.seed(9100 + i)
  n <- g$n[i]; reps <- g$reps[i]; sd_eq <- g$sd_eq[i]
  sx <- 0.6; sy <- 0.9; a <- 1; b <- 1.1
  gen <- function() {
    t <- rnorm(n, 20, 5)
    dev <- rnorm(n, 0, sd_eq)
    m <- if (reps == "known") 1 else as.numeric(reps)
    id <- rep(seq_len(n), each = m)
    data.frame(id = id, x = t[id] + rnorm(n * m, 0, sx),
               y = a + b * t[id] + dev[id] + rnorm(n * m, 0, sy))
  }
  fit_one <- function(dat, eq) {
    if (reps == "known") {
      bls_reg(y ~ x, data = dat, var_x = sx^2, var_y = sy^2,
               equation_error = eq)
    } else {
      bls_reg(y ~ x, data = dat, id = "id", equation_error = eq)
    }
  }
  res <- matrix(NA, n_sim, 6,
                dimnames = list(NULL, c("slope", "pi_eq", "pi_noeq",
                                        "pi_md_eq", "pi_md_noeq",
                                        "disp_reject")))
  for (s in seq_len(n_sim)) {
    dat <- gen()
    f1 <- tryCatch(fit_one(dat, TRUE), error = function(e) NULL)
    if (is.null(f1)) next
    f0 <- fit_one(dat, FALSE)
    t0 <- rnorm(1, 20, 5)
    x0 <- t0 + rnorm(1, 0, sx)
    y0 <- a + b * t0 + rnorm(1, 0, sd_eq) + rnorm(1, 0, sy)
    inside <- function(p, v) p$lwr <= v & v <= p$upr
    res[s, ] <- c(
      f1$model_table$lower.ci[2] <= b & b <= f1$model_table$upper.ci[2],
      inside(predict(f1, x0, interval = "prediction"), y0),
      inside(predict(f0, x0, interval = "prediction"), y0),
      inside(predict(f1, (x0 + y0) / 2, type = "md", interval = "prediction"),
             x0 - y0),
      inside(predict(f0, (x0 + y0) / 2, type = "md", interval = "prediction"),
             x0 - y0),
      f1$dispersion$p.value < 1 - conf)
  }
  data.frame(n = n, reps = reps, sd_eq = sd_eq, n_sim = n_sim,
             t(colMeans(res, na.rm = TRUE)))
}

out <- do.call(rbind, parLapply(cl, seq_len(nrow(grid)), one_cell, g = grid,
                                n_sim = N_SIM, conf = conf))
rownames(out) <- NULL
write.csv(out, file.path(out_dir, "summary_eq.csv"), row.names = FALSE)
print(out, digits = 3)
