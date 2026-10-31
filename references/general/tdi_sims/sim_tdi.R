# Coverage of the tdi() bounds (see README.md).
# Run from the package root: Rscript references/general/tdi_sims/sim_tdi.R
library(parallel)

out_dir <- "references/general/tdi_sims"
methods <- c("gpq", "delta", "tolerance")
conf <- 0.95
N_S <- as.numeric(Sys.getenv("TDI_NS", "2000"))
N_R <- as.numeric(Sys.getenv("TDI_NR", "1000"))

cl <- makeCluster(max(1, detectCores() - 2))
on.exit(stopCluster(cl))
invisible(clusterEvalQ(cl, suppressMessages(pkgload::load_all(".", quiet = TRUE))))
clusterExport(cl, "conf")

# 1. Simple (independent) differences: the engine with the inputs that
#    agreement_limit(data_type = "simple") gives, without refitting.
grid_s <- expand.grid(n = c(10, 20, 50), mu = c(0, 0.5, 1.5),
                      p = c(0.8, 0.9, 0.95))
one_simple <- function(i, g, n_sim) {
  set.seed(1000 + i)
  n <- g$n[i]; mu <- g$mu[i]; p <- g$p[i]
  true_tdi <- tdi_exact(mu, 1, p)
  res <- matrix(NA, n_sim, 6,
                dimnames = list(NULL, c("tdi_gpq", "tdi_delta", "tdi_tol",
                                        "cp_gpq", "cp_delta", "cp_tol")))
  for (s in seq_len(n_sim)) {
    d <- rnorm(n, mu, 1)
    r <- data.frame(bias = mean(d), se = sd(d) / sqrt(n), df_bias = n - 1,
                    sd = sd(d), df_sd = n - 1, scale_sd = TRUE)
    info <- list(type = "df", nu = n - 1)
    gq <- .tdi_gpq(r, list(v = r$sd^2, df = n - 1, scale = TRUE), p, true_tdi, conf, B = 2000)
    dl <- .tdi_delta(r, p, true_tdi, conf)
    tl <- .tdi_tol(r, info, p, true_tdi, conf)
    res[s, ] <- c(gq$upper >= true_tdi, dl$upper >= true_tdi,
                  tl$upper >= true_tdi,
                  gq$cp_lower <= p, dl$cp_lower <= p, tl$cp_lower <= p)
  }
  data.frame(design = "simple", n = n, mu = mu, p = p, n_sim = n_sim,
             t(colMeans(res)))
}
if (Sys.getenv("TDI_SKIP_SIMPLE") != "true") {
simple <- do.call(rbind, parLapply(cl, seq_len(nrow(grid_s)), one_simple,
                                   g = grid_s, n_sim = N_S))
write.csv(simple, file.path(out_dir, "summary_simple.csv"), row.names = FALSE)
print(simple, digits = 3)
}

# 2. Replicated and nested data through the fitted objects.
#    reps: exchangeable replicates, true value constant within subject.
#    nest: linked occasions; the true value changes between occasions
#          (omega), which both methods share.
#    The SD of a single difference is sqrt(2 tau^2 + sx^2 + sy^2) = 1.
grid_r <- expand.grid(n = c(10, 20, 40), mu = c(0, 1),
                      design = c("reps", "nest", "tol_id", "mvc"),
                      stringsAsFactors = FALSE)
one_rep <- function(i, g, n_sim) {
  set.seed(5000 + i)
  n <- g$n[i]; mu <- g$mu[i]; design <- g$design[i]
  p <- 0.9; m <- 3
  tau <- 0.4; sx <- 0.5; sy <- sqrt(1 - 2 * tau^2 - sx^2)
  true_tdi <- tdi_exact(mu, 1, p)
  res <- matrix(NA, n_sim, 6,
                dimnames = list(NULL, c("tdi_gpq", "tdi_delta", "tdi_tol",
                                        "cp_gpq", "cp_delta", "cp_tol")))
  for (s in seq_len(n_sim)) {
    id <- rep(seq_len(n), each = m)
    truth <- rnorm(n, 10, 2)[id]
    if (design != "reps") truth <- truth + rnorm(n * m, 0, 0.7)
    x <- truth + rnorm(n, 0, tau)[id] + rnorm(n * m, 0, sx)
    y <- truth - mu + rnorm(n, 0, tau)[id] + rnorm(n * m, 0, sy)
    dat <- data.frame(id = id, repl = rep(seq_len(m), n), x = x, y = y)
    fit <- tryCatch(switch(design,
                  reps = agreement_limit(x = "x", y = "y", id = "id",
                                         data = dat, data_type = "reps"),
                  nest = agreement_limit(x = "x", y = "y", id = "id",
                                         data = dat, data_type = "nest"),
                  tol_id = tolerance_limit(x = "x", y = "y", id = "id",
                                           data = dat),
                  mvc = method_vc(mc_long(dat, methods = c("x", "y"), id = "id",
                                          replicate = "repl"),
                                  y = "y", method = "method", id = "id",
                                  replicate = "repl", replicates = "linked")),
                    error = function(e) NULL)
    if (is.null(fit)) next
    out <- lapply(c("gpq", "delta", "tolerance"), function(mt) {
      r <- tryCatch(suppressMessages(tdi(fit, p = p, delta = true_tdi,
                                         conf.level = conf, method = mt,
                                         B = 2000)),
                    error = function(e) NULL)
      if (is.null(r)) return(c(NA, NA))
      c(r$table$tdi_upper >= true_tdi, r$table$cp_lower <= p)
    })
    res[s, ] <- c(out[[1]][1], out[[2]][1], out[[3]][1],
                  out[[1]][2], out[[2]][2], out[[3]][2])
  }
  data.frame(design = design, n = n, mu = mu, p = p, n_sim = n_sim,
             n_failed = sum(!stats::complete.cases(res)),
             t(colMeans(res, na.rm = TRUE)))
}
reps <- do.call(rbind, parLapply(cl, seq_len(nrow(grid_r)), one_rep,
                                 g = grid_r, n_sim = N_R))
write.csv(reps, file.path(out_dir, "summary_reps.csv"), row.names = FALSE)
print(reps, digits = 3)
