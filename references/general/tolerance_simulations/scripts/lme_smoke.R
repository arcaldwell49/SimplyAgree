suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
step = function(label, expr) {
  t0 = Sys.time(); cat(">>", label, "... "); flush.console()
  r = withCallingHandlers(expr, warning = function(w) {cat("[warn:", conditionMessage(w), "] "); invokeRestart("muffleWarning")})
  cat(sprintf("%.1fs\n", as.numeric(Sys.time() - t0, units = "secs"))); flush.console()
  invisible(r)
}
data(reps)
cols = c("bias","SEM","SD","SD.upper","lower.PL","upper.PL","lower.TL","upper.TL","SD.between")
g = step("gls reps", tolerance_limit(reps, "x", "y", id = "id"))
l = step("lme reps", tolerance_limit(reps, "x", "y", id = "id", model = "lme"))
print(rbind(gls = unlist(g$limits[, cols]), lme = unlist(l$limits[, cols])))
cat("df gls/lme:", g$limits$df, l$limits$df, "\n")
data(temps); t2 = temps; t2$x = t2$trec_pre; t2$y = t2$teso_pre; t2$ts = as.numeric(t2$trial_num)
cat("temps rows:", nrow(t2), " ids:", length(unique(t2$id)), "\n")
lc = step("lme condition", tolerance_limit(t2, "x", "y", id = "id", condition = "tod", model = "lme"))
print(lc$limits[, c("condition","bias","SD","SD.between","SD.within","SD.df","lower.TL","upper.TL")])
la = step("lme ar1", tolerance_limit(t2, "x", "y", id = "id", time = "ts", cor_type = "ar1", model = "lme"))
print(la$limits[, c("SD","SD.between","SD.within","SD.df","lower.TL","upper.TL")])
print(la)
for (e in list(quote(tolerance_limit(reps, "x", "y", model = "lme")),
               quote(tolerance_limit(reps, "x", "y", id = "id", model = "lme", cor_type = "none")),
               quote(tolerance_limit(reps, "x", "y", id = "id", model = "lme", correlation = nlme::corAR1())))) {
  r = try(eval(e), silent = TRUE); cat("error:", conditionMessage(attr(r, "condition")), "\n")
}
u = update(la$model, . ~ .); cat("update ok:", isTRUE(all.equal(logLik(u), logLik(la$model))), " getData rows:", nrow(nlme::getData(la$model)), "\n")
step("check", invisible(check(lc))); step("plot", invisible(plot(lc)))
set.seed(1); lb = step("boot_cal lme condition", tolerance_limit(t2, "x", "y", id = "id", condition = "tod", model = "lme", tol_method = "boot_cal", replicates = 30))
print(lb$limits[, c("condition","lower.TL","upper.TL","lower.TL.level","upper.TL.level")])
m = la$model; d = nlme::getData(m)
st = gls_sim_setup(m, d); sims = step("sim", replicate(20000, gls_sim_draw(st) - st$mu)); S = cov(t(sims))
i1 = which(as.character(d$id) == as.character(d$id[1]))
V = nlme::getVarCov(m, individuals = as.character(d$id[1]), type = "marginal")[[1]]
cat("max abs diff sim vs getVarCov:", max(abs(S[i1, i1] - unclass(V))), " (scale", max(abs(V)), ")\n")
