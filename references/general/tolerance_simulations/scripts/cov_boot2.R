args = commandArgs(TRUE)
design = args[1]; nsim = as.integer(args[2]); B = as.integer(args[3]); off = as.integer(args[4])
suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
suppressMessages(library(nlme))
beta = 0.95; gam = 0.95; zp = qnorm((1 + beta)/2)

# each design returns list(data, fit args, truth data.frame(mu, sig) per limits row)
gen = switch(design,
  cs10x5 = function() {
    ng = 10; k = 5; sa = 1; se = 1
    d = data.frame(id = rep(1:ng, each = k)); d$y = 0
    d$x = 0.8 + rnorm(ng, 0, sa)[d$id] + rnorm(ng * k, 0, se)
    list(d = d, args = list(id = "id"), truth = data.frame(mu = 0.8, sig = sqrt(sa^2 + se^2)))
  },
  cs20x19a = function() {
    ng = 20; k = 19; sa = 0.5; se = 1
    d = data.frame(id = rep(1:ng, each = k)); d$y = 0
    d$x = 0.8 + rnorm(ng, 0, sa)[d$id] + rnorm(ng * k, 0, se)
    list(d = d, args = list(id = "id"), truth = data.frame(mu = 0.8, sig = sqrt(sa^2 + se^2)))
  },
  cs20x19b = function() {
    ng = 20; k = 19; sa = 1.5; se = 1
    d = data.frame(id = rep(1:ng, each = k)); d$y = 0
    d$x = 0.8 + rnorm(ng, 0, sa)[d$id] + rnorm(ng * k, 0, se)
    list(d = d, args = list(id = "id"), truth = data.frame(mu = 0.8, sig = sqrt(sa^2 + se^2)))
  },
  ar1 = function() {
    ng = 15; k = 8; phi = 0.6
    d = data.frame(id = rep(1:ng, each = k), time = rep(1:k, ng)); d$y = 0
    d$x = 0.8 + as.vector(replicate(ng, arima.sim(list(ar = phi), k, sd = sqrt(1 - phi^2))))
    list(d = d, args = list(id = "id", time = "time", cor_type = "ar1"),
         truth = data.frame(mu = 0.8, sig = 1))
  },
  csvar = function() {
    # gls-implied model: cov = rho * s_i * s_j within subject
    ng = 12; k = 4; rho = 0.4; sd_c = c(A = 1, B = 2); mu_c = c(A = 0.5, B = -0.5)
    cond = rep(c("A", "B"), each = k)
    sds = sd_c[cond]
    V = rho * outer(sds, sds); diag(V) = sds^2
    Lc = t(chol(V))
    d = data.frame(id = rep(1:ng, each = 2 * k), condition = rep(cond, ng)); d$y = 0
    d$x = as.vector(replicate(ng, mu_c[cond] + Lc %*% rnorm(2 * k)))
    list(d = d, args = list(id = "id", condition = "condition"),
         truth = data.frame(mu = mu_c, sig = sd_c))
  })

set.seed(which(c("cs10x5","cs20x19a","cs20x19b","ar1","csvar") == design) * 1000 + 7919 * off)
res = replicate(nsim, {
  g = gen()
  t0 = suppressMessages(do.call(tolerance_limit, c(list(data = g$d, x = "x", y = "y"), g$args)))
  m = t0$model; tf = nlme::getData(m); emm = t0$emmeans
  e = tol_grid(emm, m, NULL) %>% add_pred_limits(m, 1 - beta)
  info = sd_bound_info(m, e, tf)
  e$SD.df = sd_df_at(e$SD, info); e$SD.upper = sd_upper_at(e$SD, gam, info)
  ind = is.null(m$modelStruct$corStruct)
  bt = boot_delta_gls(m, tf, NULL, emm, info, B)
  out = c()
  for (bty in c("joint", "iu")) {
    a = tol_approx(e, info, beta, gam, bty, ind)
    b = suppressWarnings(tol_boot(e, bt, info, beta, gam, bty, ind))
    for (meth in c("approx", "boot")) {
      r = if (meth == "approx") a else b
      tr = g$truth
      if (bty == "joint") {
        cont = pnorm((r$upper.TL - tr$mu)/tr$sig) - pnorm((r$lower.TL - tr$mu)/tr$sig)
        out[paste(meth, "joint", sep = "_")] = mean(cont >= beta)   # averaged over rows
      } else {
        out[paste(meth, "iu_lo", sep = "_")] = mean(r$lower.TL <= tr$mu - zp * tr$sig)
        out[paste(meth, "iu_hi", sep = "_")] = mean(r$upper.TL >= tr$mu + zp * tr$sig)
      }
    }
  }
  out
})
cat(sprintf("%s (nsim=%d, B=%d)\n", design, nsim, B))
print(round(rowMeans(res), 3)); saveRDS(res, sprintf("cc_%s_%d.rds", design, off))
