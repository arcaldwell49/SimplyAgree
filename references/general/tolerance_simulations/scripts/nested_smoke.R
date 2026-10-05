suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
step = function(label, expr) {
  t0 = Sys.time(); cat(">>", label, "... "); flush.console()
  r = withCallingHandlers(expr, warning = function(w) {cat("[warn:", conditionMessage(w), "] "); invokeRestart("muffleWarning")})
  cat(sprintf("%.1fs\n", as.numeric(Sys.time() - t0, units = "secs"))); flush.console()
  invisible(r)
}
# Phase 1 regression values
data(reps)
l = step("lme reps", tolerance_limit(reps, "x", "y", id = "id", model = "lme"))
cat("reps SD.upper (was 4.153513):", l$limits$SD.upper, "\n")
data(temps); t2 = temps; t2$x = t2$trec_pre; t2$y = t2$teso_pre; t2$ts = as.numeric(t2$trial_num)
lc = step("lme condition", tolerance_limit(t2, "x", "y", id = "id", condition = "tod", model = "lme"))
cat("AM TL (was -0.3525269 0.6598602):", lc$limits$lower.TL[1], lc$limits$upper.TL[1], " PM (was -0.2027331 0.6587331):", lc$limits$lower.TL[2], lc$limits$upper.TL[2], "\n")
la = step("lme ar1", tolerance_limit(t2, "x", "y", id = "id", time = "ts", cor_type = "ar1", model = "lme"))
cat("ar1 SD.df (was 47.35625):", la$limits$SD.df, " TL (was -0.2540072 0.6330702):", la$limits$lower.TL, la$limits$upper.TL, "\n")
g = step("gls reps", tolerance_limit(reps, "x", "y", id = "id"))
cat("gls reps SD.upper (was 4.153516):", g$limits$SD.upper, "\n")

# nested design: golfer / club
set.seed(12); n = 15; s = 3; k = 5
d = expand.grid(shot = 1:k, club = paste0("c", 1:s), golfer = paste0("g", 1:n))
d$x = 100 + rnorm(nrow(d))
g_eff = rnorm(n, 0, 0.7); c_eff = rnorm(n * s, 0, 0.5)
d$y = d$x - (0.8 + g_eff[as.integer(d$golfer)] + c_eff[(as.integer(d$golfer) - 1) * s + as.integer(d$club)] + rnorm(nrow(d)))
d = d[sample(nrow(d)), ]
tn = step("nested", tolerance_limit(d, "x", "y", id = c("golfer", "club"), model = "lme"))
print(tn$limits[, c("bias", "SD", "SD.between", "SD.nested", "SD.within", "SD.df", "SD.upper", "lower.TL", "upper.TL")])
print(nlme::VarCorr(tn$model))
print(tn)
ta = step("nested ar1", tolerance_limit(d, "x", "y", id = c("golfer", "club"), time = "shot", cor_type = "ar1", model = "lme"))
print(ta$limits[, c("SD", "SD.between", "SD.nested", "SD.within", "lower.TL", "upper.TL")])
for (e in list(quote(tolerance_limit(d, "x", "y", id = c("golfer", "club"))),
               quote(tolerance_limit(d, "x", "y", id = c("golfer", "club", "shot"), model = "lme")),
               quote(tolerance_limit(d, "x", "y", id = c("golfer", "club"), model = "lme", correlation = nlme::corAR1(form = ~shot | id))))) {
  r = try(eval(e), silent = TRUE); cat("error:", conditionMessage(attr(r, "condition")), "\n")
}
u = update(tn$model, . ~ .); cat("update ok:", isTRUE(all.equal(logLik(u), logLik(tn$model))), "\n")
step("check", invisible(check(tn))); step("plot", invisible(plot(tn)))
set.seed(1); tb = step("boot_cal nested", tolerance_limit(d, "x", "y", id = c("golfer", "club"), model = "lme", tol_method = "boot_cal", replicates = 30))
print(tb$limits[, c("lower.TL", "upper.TL", "lower.TL.level", "upper.TL.level")])
# simulator covariance vs hand-built
m = ta$model; dat = nlme::getData(m)
st = gls_sim_setup(m, dat); sims = step("sim", replicate(20000, gls_sim_draw(st) - st$mu)); S = cov(t(sims))
g1 = as.character(dat$id[1]); r1 = which(as.character(dat$id) == g1)
vc = re_vars(m); phi = unname(coef(m$modelStruct$corStruct, unconstrained = FALSE))
cl = as.character(dat$id_2[r1]); tm = dat$time[r1]
V = vc[["id"]] + vc[["id_2"]] * outer(cl, cl, "==") + sigma(m)^2 * outer(cl, cl, "==") * phi^abs(outer(tm, tm, "-"))
cat("max abs diff sim vs hand-built:", max(abs(S[r1, r1] - V)), " (scale", max(V), ")\n")
