# Oximetry data: scaled model by lavaan vs alternating regression ----
# Phase 1 of HANDOFF_method_convert_scaled_ci.md. Compares the lavaan (full
# ML, structural) fit with method_convert(bias = "scaled") (alternating
# regression, AltReg) and MethComp::AltReg() on the ox data (CO and pulse,
# linked replicates). MethComp is optional (archived on CRAN; see
# mess/methcomp_tour.R).
# Run from the package root:
#   Rscript data-raw/sims/lavaan_ox_compare.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
source(file.path("data-raw", "sims", "lavaan_scaled_helpers.R"))
options(width = 150)

ox = readRDS(file.path("tests", "testthat", "fixtures", "ox.rds"))
methods = levels(ox$meth)  # CO (reference), pulse
dat = data.frame(id = ox$item, method = ox$meth, repl = ox$repl, y = ox$y)
cat("items:", length(unique(dat$id)), " rows:", nrow(dat), "\n")

## fits ----
t0 = proc.time()[["elapsed"]]
f_ml = lav_scaled_fit(dat, methods, "linked", estimator = "ML")
cat("lavaan ML fit:", round(proc.time()[["elapsed"]] - t0, 2), "s; converged:",
    lavaan::lavInspect(f_ml, "converged"), "\n")
f_mlr = lav_scaled_fit(dat, methods, "linked", estimator = "MLR")
lav = lav_scaled_conversion(f_ml, methods)
lav_r = lav_scaled_conversion(f_mlr, methods)

alt = method_convert(ox, y = "y", method = "meth", id = "item",
                     replicate = "repl", replicates = "linked",
                     bias = "scaled")
ac = alt$conversion
ac = ac[match(paste(lav$to, lav$from), paste(ac$to, ac$from)), ]

has_mc = requireNamespace("MethComp", quietly = TRUE)
if (has_mc) {
  oxM = MethComp::Meth(ox, print = FALSE)
  mc = MethComp::AltReg(oxM, linked = TRUE, eps = 1e-6, maxiter = 500)
  mc_conv = function(what) {
    mapply(function(to, from) mc$Conv[to, from, what], lav$to, lav$from)
  }
}

## conversion: point estimates, SEs, CIs ----
out = data.frame(
  to = lav$to, from = lav$from,
  alpha_lav = lav$alpha, alpha_alt = ac$alpha,
  alpha_mc = if (has_mc) mc_conv("alpha") else NA,
  beta_lav = lav$beta, beta_alt = ac$beta,
  beta_mc = if (has_mc) mc_conv("beta") else NA,
  sd_lav = lav$sd_pred, sd_alt = ac$sd_pred,
  sd_mc = if (has_mc) mc_conv("sd.pred") else NA
)
rownames(out) = NULL
cat("\nPoint estimates (lav = lavaan ML, alt = method_convert AltReg,",
    "mc = MethComp::AltReg)\n")
print(out, digits = 5)

se = data.frame(
  to = lav$to, from = lav$from,
  alpha_se_lav = lav$alpha_se, alpha_se_mlr = lav_r$alpha_se,
  alpha_se_alt = ac$alpha_se,
  beta_se_lav = lav$beta_se, beta_se_mlr = lav_r$beta_se,
  beta_se_alt = ac$beta_se
)
cat("\nStandard errors (lavaan ML, lavaan MLR, AltReg delta)\n")
print(se, digits = 4)

ci = data.frame(
  to = lav$to, from = lav$from,
  beta_lav = sprintf("%.4f (%.4f, %.4f)", lav$beta, lav$beta_lower,
                     lav$beta_upper),
  beta_alt = sprintf("%.4f (%.4f, %.4f)", ac$beta, ac$beta_lower,
                     ac$beta_upper),
  alpha_lav = sprintf("%.3f (%.3f, %.3f)", lav$alpha, lav$alpha_lower,
                      lav$alpha_upper),
  alpha_alt = sprintf("%.3f (%.3f, %.3f)", ac$alpha, ac$alpha_lower,
                      ac$alpha_upper)
)
cat("\n95% CIs (lavaan Wald vs AltReg delta)\n")
print(ci)

## variance components (SDs on the item scale) ----
v = lav_scaled_variances(f_ml)
cat("\nlavaan variance estimates (item scale; s* are residual variances",
    "on each method's scale)\n")
print(round(v, 4))
cat("lavaan SDs: omega", round(sqrt(v[["om2"]]), 4),
    " tau", round(sqrt(v[["t2"]]), 4),
    " sigma", paste(round(sqrt(v[grepl("^s", names(v))]), 4),
                    collapse = ", "), "\n")
cat("\nmethod_convert (AltReg) variance components\n")
print(alt$varcomp)
if (has_mc) {
  cat("\nMethComp::AltReg variance components (each method's scale)\n")
  print(mc$VarComp)
}
