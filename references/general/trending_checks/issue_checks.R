# Checks for the BAtrending gap analysis ---------------------------------
# Run from any directory: Rscript issue_checks.R <path-to-SimplyAgree>
# Uses the SimplyAgree source tree read-only (pkgload::load_all).

args = commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[1], quiet = TRUE, export_all = FALSE)
suppressPackageStartupMessages(library(lme4))

# simulate ----------------------------------------------------------------
# true_ij = mu_i + w_ij; ref = true + e; alt = mu_i + b_w * w_ij + u_i + e
sim = function(n = 200, m = 6, sd_wt = 1, b_w = 1, sd_ref = 0.25,
               sd_alt = 0.25, sd_u = 0.25, seed = 1) {
  set.seed(seed)
  id = rep(seq_len(n), each = m)
  mu = rep(rnorm(n, 5, 1), each = m)
  w = rnorm(n * m, 0, sd_wt)
  u = rep(rnorm(n, 0, sd_u), each = m)
  data.frame(id = id, occ = rep(seq_len(m), n),
             ref = mu + w + rnorm(n * m, 0, sd_ref),
             alt = mu + b_w * w + u + rnorm(n * m, 0, sd_alt))
}

# within-subject slope diagnostics ----------------------------------------
diag_one = function(d, sd_ref, sd_alt) {
  d$D = d$alt - d$ref
  d$A = (d$alt + d$ref) / 2
  d$Ac = d$A - ave(d$A, d$id)
  tl = tolerance_limit(data = d, x = "alt", y = "ref", id = "id",
                       model = "lme")
  sdw = tl$limits$SD.within[1]
  # naive within slope of D on subject-centred A (pooled LMM slope too)
  b_naive = fixef(lmer(D ~ Ac + (1 | id), data = d))[2]
  b_pooled = fixef(lmer(D ~ A + (1 | id), data = d))[2]
  # Deming on subject-centred data with the TRUE error ratio (oracle)
  dc = data.frame(xc = d$ref - ave(d$ref, d$id),
                  yc = d$alt - ave(d$alt, d$id))
  dem_c = dem_reg(yc ~ xc, data = dc, error.ratio = sd_ref^2 / sd_alt^2)
  # existing id-based tools
  dem_id = dem_reg(alt ~ ref, data = d, id = "id")
  pb_id = suppressWarnings(pb_reg(alt ~ ref, data = d, id = "id"))
  c(SD.within = sdw,
    change_LoA = 1.96 * sqrt(2) * sdw,
    slope_D_on_Ac = unname(b_naive),
    slope_D_on_A_pooled = unname(b_pooled),
    deming_centred_oracle = unname(coef(dem_c)[2]),
    dem_reg_id_slope = unname(coef(dem_id)[2]),
    dem_reg_id_err_ratio = unname(dem_id$error.ratio %||% NA),
    pb_reg_id_slope = unname(coef(pb_id)[2]))
}
`%||%` = function(a, b) if (is.null(a)) b else a

# issue 1: scenarios ------------------------------------------------------
scen = expand.grid(sd_wt = c(0.3, 1.5), b_w = c(1, 0.5, 0), sd_alt = 0.25)
scen = rbind(scen, data.frame(sd_wt = c(0.3, 1.5), b_w = 1, sd_alt = 0.6))
res = t(sapply(seq_len(nrow(scen)), function(k) {
  s = scen[k, ]
  d = sim(sd_wt = s$sd_wt, b_w = s$b_w, sd_alt = s$sd_alt)
  diag_one(d, sd_ref = 0.25, sd_alt = s$sd_alt)
}))
out1 = cbind(scen, round(res, 3))
print(out1)
cat("\nExpected change LoA with b_w = 1, sd_alt = 0.25:",
    round(1.96 * sqrt(2) * sqrt(0.25^2 + 0.25^2), 3), "\n")

# issue 1: scaled model with linked replicates ----------------------------
# Does method_convert(bias = "scaled") pick up b_w? It assumes one slope for
# mu_i and the occasion effect a_ir, so b_between = 1, b_w = 0 is a misfit.
sc = function(b_w, sd_wt) {
  d = sim(n = 100, sd_wt = sd_wt, b_w = b_w)
  long = rbind(data.frame(id = d$id, repl = d$occ, method = "ref", y = d$ref),
               data.frame(id = d$id, repl = d$occ, method = "alt", y = d$alt))
  long$method = factor(long$method, levels = c("ref", "alt"))
  fit = suppressWarnings(method_convert(long, y = "y", method = "method",
                                        id = "id", replicate = "repl",
                                        bias = "scaled",
                                        replicates = "linked"))
  cv = fit$conversion
  r = cv[cv$to == "alt" & cv$from == "ref", ]
  c(b_w = b_w, sd_wt = sd_wt, beta_alt_ref = r$beta,
    lower = r$beta_lower, upper = r$beta_upper)
}
cat("\nScaled model (linked replicates), slope alt|ref:\n")
print(round(rbind(sc(1, 1.5), sc(0.5, 1.5), sc(0, 1.5), sc(0, 0.3)), 3))

# issue 2: mean(avg) vs mixed-model mean with unbalanced data -------------
set.seed(2)
n = 40
mu_i = rnorm(n, 5, 1.2)
m_i = pmax(1, round(2 + 3 * (mu_i - 3)))       # more readings in high-CO subjects
d2 = data.frame(id = rep(seq_len(n), m_i), mu = rep(mu_i, m_i))
d2$ref = d2$mu + rnorm(nrow(d2), 0, 0.5)
d2$alt = d2$mu + 0.3 + rnorm(nrow(d2), 0, 0.7)
d2$avg = (d2$alt + d2$ref) / 2
d2$diff = d2$alt - d2$ref
mean_raw = mean(d2$avg)
mean_lmm = fixef(lmer(avg ~ 1 + (1 | id), data = d2, REML = TRUE))[[1]]
vc = as.data.frame(VarCorr(lmer(diff ~ 1 + (1 | id), data = d2, REML = TRUE)))
sd_tot = sqrt(sum(vc$vcov))
cat("\nIssue 2: m_i range", range(m_i), "; N =", nrow(d2), "\n")
cat("mean(avg) =", round(mean_raw, 3), "; lmer distr.mean =",
    round(mean_lmm, 3), "\n")
cat("PE point (lmer mean) =", round(100 * 1.96 * sd_tot / mean_lmm, 2),
    "%; PE rescaled by mean(avg) =", round(100 * 1.96 * sd_tot / mean_raw, 2),
    "%\n")

# concordance rate check (paper Figure 4) ---------------------------------
# P(same sign | dRef = x) = pnorm(x / (sqrt(2) * sd_w)); change LoA 1.5, x = 1
sdw = 1.5 / 1.96 / sqrt(2)
cat("\nExpected concordance, change LoA 1.5, dRef 1:",
    round(pnorm(1 / (sqrt(2) * sdw)), 3), "\n")
