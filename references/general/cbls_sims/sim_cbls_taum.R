# method_convert(estimator = "cbls", tau = "method"): method-specific
# equation error with three or more methods. Slope bias, slope CI coverage
# and prediction-limit coverage against tau = "common" (study 8 in
# README.md). Written 2026-11-04.
# Run from the package root: Rscript references/general/cbls_sims/sim_cbls_taum.R
# y_m = a_m + b_m (mu + c_m) + e_m, mu ~ N(20, 5^2), c_m ~ N(0, tau_m^2)
# (true-value scale), 3 exchangeable replicates; slopes 1, 1.1, 0.9, 1.2.
# Conversions B from A and C from B; prediction limits checked on 200 new
# items measured once by each method.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
out_dir <- "references/general/cbls_sims"
N_SIM <- as.numeric(Sys.getenv("CBLS_NSIM", "300"))
a <- c(0, 1, -2, 3); b <- c(1, 1.1, 0.9, 1.2); sig <- c(0.6, 0.9, 0.7, 0.8)
gen <- function(n, tau, M, R = 3) {
  mu <- rnorm(n, 20, 5)
  do.call(rbind, lapply(1:M, function(m) { cm <- rnorm(n, 0, tau[m])
    data.frame(id = rep(1:n, R), repl = rep(1:R, each = n), method = LETTERS[m],
               y = a[m] + b[m] * (mu + cm) + rnorm(n * R, 0, sig[m])) }))
}
newpair <- function(k, m, N = 200, tau) {   # single new measurement by each
  mu <- rnorm(N, 20, 5)
  list(from = a[m] + b[m] * (mu + rnorm(N, 0, tau[m])) + rnorm(N, 0, sig[m]),
       to = a[k] + b[k] * (mu + rnorm(N, 0, tau[k])) + rnorm(N, 0, sig[k]))
}
one <- function(n, tau, M) {
  d <- gen(n, tau, M)
  out <- c()
  for (tt in c("common", "method")) {
    f <- suppressWarnings(method_convert(d, y = "y", method = "method", id = "id", replicate = "repl",
                                         estimator = "cbls", tau = tt))
    cv <- f$conversion[f$conversion$to == "B" & f$conversion$from == "A", ]
    tb <- b[2] / b[1]
    np <- newpair(2, 1, tau = tau)
    p <- predict(f, newdata = np$from, from = "A", to = "B")
    cv2 <- f$conversion[f$conversion$to == "C" & f$conversion$from == "B", ]
    np2 <- newpair(3, 2, tau = tau)
    p2 <- predict(f, newdata = np2$from, from = "B", to = "C")
    out <- c(out, cv$beta - tb, cv$beta_lower < tb & cv$beta_upper > tb,
             mean(np$to > p$lower & np$to < p$upper),
             cv2$beta - b[3] / b[2], cv2$beta_lower < b[3] / b[2] & cv2$beta_upper > b[3] / b[2],
             mean(np2$to > p2$lower & np2$to < p2$upper))
  }
  out
}
set.seed(21)
cells <- expand.grid(n = c(30, 100), tauset = 1:4, M = c(3, 4))
taus <- list(c(0, 1.5, 0, 0), c(1.5, 0, 0, 0), c(1, 1, 1, 1), c(0, 0, 0, 0))
res <- list()
for (i in seq_len(nrow(cells))) {
  cl <- cells[i, ]; tau <- taus[[cl$tauset]]
  r <- t(replicate(N_SIM, tryCatch(one(cl$n, tau, cl$M), error = function(e) rep(NA, 12))))
  m <- colMeans(r, na.rm = TRUE)
  row <- data.frame(M = cl$M, n = cl$n, tau = paste(tau[1:cl$M], collapse = "/"),
    AB_bias_c = m[1], AB_ci_c = m[2], AB_pi_c = m[3], BC_bias_c = m[4], BC_ci_c = m[5], BC_pi_c = m[6],
    AB_bias_m = m[7], AB_ci_m = m[8], AB_pi_m = m[9], BC_bias_m = m[10], BC_ci_m = m[11], BC_pi_m = m[12],
    fail = sum(is.na(r[, 1])))
  print(row, digits = 3); res[[i]] <- row
}
out <- do.call(rbind, res)
write.csv(out, file.path(out_dir, "summary_taum.csv"), row.names = FALSE)
print(out, digits = 3)
