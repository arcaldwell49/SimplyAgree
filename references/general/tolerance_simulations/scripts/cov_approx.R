args = commandArgs(TRUE)
ng = as.integer(args[1]); k = as.integer(args[2]); sa = as.numeric(args[3]); se = as.numeric(args[4])
nsim = as.integer(args[5])
suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
suppressMessages(library(nlme))
beta = 0.95; gam = 0.95; mu = 0.8; sig = sqrt(sa^2 + se^2); zp = qnorm((1 + beta)/2)
set.seed(100 + ng + k + 10*sa)
out = t(replicate(nsim, {
  d = data.frame(id = rep(1:ng, each = k))
  d$delta = mu + rnorm(ng, 0, sa)[d$id] + rnorm(ng * k, 0, se)
  g = gls(delta ~ 1, d, correlation = corCompSymm(form = ~1 | id))
  emm = gls_emm_delta(g, d, avg_vals = NULL)
  e = tol_grid(emm, g, NULL) %>% add_pred_limits(g, 1 - beta)
  e = tol_sd_upper(g, e, d, gam)
  j = tol_approx(e, beta, gam, "joint", FALSE)
  iu = tol_approx(e, beta, gam, "iu", FALSE)
  # old: df of the bias as the variance df
  e_old = e; e_old$SD.upper = e_old$SD * sqrt(e_old$df / qchisq(1 - gam, e_old$df))
  jo = tol_approx(e_old, beta, gam, "joint", FALSE)
  cont = function(L, U) pnorm((U - mu)/sig) - pnorm((L - mu)/sig)
  c(joint = cont(j$lower.TL, j$upper.TL) >= beta,
    joint_old = cont(jo$lower.TL, jo$upper.TL) >= beta,
    iu_lo = iu$lower.TL <= mu - zp*sig, iu_hi = iu$upper.TL >= mu + zp*sig,
    iu_both = (iu$lower.TL <= mu - zp*sig) & (iu$upper.TL >= mu + zp*sig),
    nu = e$SD.df, df_bias = e$df)
}))
cat(sprintf("%d x %d, sa=%.1f, se=%.1f (nsim=%d)\n", ng, k, sa, se, nsim))
print(round(colMeans(out), 3))
