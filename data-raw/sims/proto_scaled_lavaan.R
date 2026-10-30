suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(lavaan)

# structural scaled model, 2 methods (A reference), R linked replicates
scaled_syntax = function(R) {
  yA = paste0("yA_", 1:R); yB = paste0("yB_", 1:R)
  c(paste("mu =~", paste(c(paste0("1*", yA), paste0("b*", yB)), collapse = " + ")),
    sprintf("a%d =~ 1*%s + b*%s", 1:R, yA, yB),
    sprintf("a%d ~~ om2*a%d", 1:R, 1:R),
    paste("cA =~", paste(paste0("1*", yA), collapse = " + ")),
    paste("cB =~", paste(paste0("b*", yB), collapse = " + ")),
    "cA ~~ t2*cA", "cB ~~ t2*cB", "mu ~~ mu", "mu ~ 1",
    sprintf("%s ~~ sA*%s", yA, yA), sprintf("%s ~~ sB*%s", yB, yB),
    sprintf("%s ~ 0*1", yA), sprintf("%s ~ a*1", yB),
    "sd_pred := sqrt(b^2*t2 + sB + b^2*(t2 + sA))")
}
to_wide = function(dat) {
  w = reshape(data.frame(id = dat$id, k = paste0("y", dat$method, "_", dat$repl), y = dat$y),
              idvar = "id", timevar = "k", direction = "wide")
  names(w) = sub("y.", "", names(w), fixed = TRUE); w
}
fit_lav = function(dat, R = 3) {
  lavaan(scaled_syntax(R), data = to_wide(dat), missing = "ml", meanstructure = TRUE)
}
sim = function(I, seed) {
  set.seed(seed)
  sim_methods(n_items = I, n_reps = 3, methods = c("A", "B"), alpha = c(0, 1),
              beta = c(1, 1.2), mu_mean = 10, mu_sd = 2, tau = 0.3, omega = 0.3,
              sigma = c(0.3, 0.4), replicates = "linked")
}

## 1. one data set: lavaan vs AltReg vs truth ----
dat = sim(30, 1)
f = fit_lav(dat)
pe = parameterEstimates(f)
mc = suppressMessages(method_convert(dat, y = "y", method = "method", id = "id",
        replicate = "repl", replicates = "linked", bias = "scaled", keep_model = FALSE))
cv = mc$conversion; cv = cv[cv$to == "B" & cv$from == "A", ]
tr = attr(dat, "truth")$conversion; tr = tr[tr$to == "B" & tr$from == "A", ]
cat("truth   : alpha", tr$alpha, " beta", tr$beta, " sd_pred", round(tr$sd_pred, 3), "\n")
cat("AltReg  : alpha", round(cv$alpha, 3), "(se", round(cv$alpha_se, 3), ") beta", round(cv$beta, 3),
    "(se", round(cv$beta_se, 3), ") sd_pred", round(cv$sd_pred, 3), "\n")
g = function(l) pe[pe$label == l, ][1, ]
cat("lavaan  : alpha", round(g("a")$est, 3), "(se", round(g("a")$se, 3), ") beta", round(g("b")$est, 3),
    "(se", round(g("b")$se, 3), ") sd_pred", round(g("sd_pred")$est, 3), "\n")
print(pe[pe$label %in% c("om2", "t2", "sA", "sB") & !duplicated(pe$label), c("label", "est", "se")])
cat("converged:", lavInspect(f, "converged"), "\n")

## 2. quick coverage of lavaan Wald CIs (95%) ----
cov_run = function(I, n = 300) {
  r = t(sapply(seq_len(n), function(s) {
    d = sim(I, 1e5 * I + s)
    f = tryCatch(suppressWarnings(fit_lav(d)), error = function(e) NULL)
    if (is.null(f) || !lavInspect(f, "converged")) return(rep(NA, 4))
    p = parameterEstimates(f)
    a = p[p$label == "a", ][1, ]; b = p[p$label == "b", ][1, ]
    tr = attr(d, "truth")$conversion; tr = tr[tr$to == "B" & tr$from == "A", ]
    c(a$ci.lower <= tr$alpha & tr$alpha <= a$ci.upper,
      b$ci.lower <= tr$beta & tr$beta <= b$ci.upper, b$est - tr$beta, 1)
  }))
  data.frame(I = I, cover_alpha = mean(r[, 1], na.rm = TRUE),
             cover_beta = mean(r[, 2], na.rm = TRUE),
             beta_bias = mean(r[, 3], na.rm = TRUE), n_ok = sum(!is.na(r[, 4])))
}
t0 = Sys.time()
print(do.call(rbind, lapply(c(15, 30, 60), cov_run)))
cat("coverage run took", format(Sys.time() - t0), "\n")
