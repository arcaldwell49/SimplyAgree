args = commandArgs(TRUE)
design = args[1]; nsim = as.integer(args[2]); off = as.integer(args[3])
suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
beta = 0.95; zp = qnorm(0.975)

gen = switch(design,
  # persistent subject effect plus AR(1) serial correlation in the residuals
  riar1 = function() {
    ng = 15; k = 8; sb = 0.7; phi = 0.6
    d = data.frame(id = rep(1:ng, each = k), time = rep(1:k, ng)); d$y = 0
    d$x = 0.8 + rnorm(ng, 0, sb)[d$id] +
      as.vector(replicate(ng, arima.sim(list(ar = phi), k, sd = sqrt(1 - phi^2))))
    list(d = d, truth = data.frame(mu = 0.8, sig = sqrt(sb^2 + 1)),
         fits = list(gls_ar1 = list(id = "id", time = "time", cor_type = "ar1"),
                     gls_sym = list(id = "id"),
                     lme_ar1 = list(id = "id", time = "time", cor_type = "ar1", model = "lme"),
                     lme_sym = list(id = "id", model = "lme")))
  },
  # random intercept common to conditions, residual SD differs by condition
  rivar = function() {
    ng = 12; k = 4; sb = 1; sd_c = c(A = 1, B = 2); mu_c = c(A = 0.5, B = -0.5)
    d = data.frame(id = rep(1:ng, each = 2 * k),
                   condition = rep(rep(c("A", "B"), each = k), ng)); d$y = 0
    d$x = mu_c[d$condition] + rnorm(ng, 0, sb)[d$id] + rnorm(nrow(d), 0, sd_c[d$condition])
    list(d = d, truth = data.frame(mu = mu_c, sig = sqrt(sb^2 + sd_c^2)),
         fits = list(gls_cond = list(id = "id", condition = "condition"),
                     lme_cond = list(id = "id", condition = "condition", model = "lme")))
  })

set.seed(which(c("riar1", "rivar") == design) * 1000 + 7919 * off)
res = replicate(nsim, {
  g = gen()
  out = c()
  for (fn in names(g$fits)) {
    for (bt in c("joint", "iu")) {
      r = tryCatch(suppressWarnings(suppressMessages(do.call(tolerance_limit,
            c(list(data = g$d, x = "x", y = "y", bound_type = bt), g$fits[[fn]])))),
            error = function(e) NULL)
      if (is.null(r)) { out[paste(fn, bt, sep = "_")] = NA; next }
      lim = r$limits[order(as.character(r$limits$condition)), ]
      tr = g$truth
      if (bt == "joint") {
        cont = pnorm((lim$upper.TL - tr$mu)/tr$sig) - pnorm((lim$lower.TL - tr$mu)/tr$sig)
        out[paste(fn, "joint", sep = "_")] = mean(cont >= beta)
      } else {
        out[paste(fn, "iu_lo", sep = "_")] = mean(lim$lower.TL <= tr$mu - zp * tr$sig)
        out[paste(fn, "iu_hi", sep = "_")] = mean(lim$upper.TL >= tr$mu + zp * tr$sig)
      }
    }
  }
  out
})
saveRDS(res, sprintf("lm_%s_%d.rds", design, off))
cat(sprintf("%s (nsim=%d) fails: %d\n", design, nsim, sum(is.na(res))))
print(round(rowMeans(res, na.rm = TRUE), 3))
