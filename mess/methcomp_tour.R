# MethComp tour: SimplyAgree's multi-method functions side by side with the
# original MethComp package (Carstensen et al., v1.30.0, archived on CRAN).
#
# Run section by section (Ctrl+Enter) rather than sourcing; the point is to
# look at the output and plots.
#
# SimplyAgree:  method_vc(), method_convert(), sim_methods(), mc_long()
# MethComp:     Meth(), BA.est(), DA.reg(), AltReg(), Meth.sim(), BA.plot()
#
# Notes on comparing the two:
# - MethComp's limits use a multiplier of 2, SimplyAgree uses qnorm(0.975).
#   `pred_2sd` below sets pred_level so SimplyAgree also uses exactly 2.
# - MethComp reports "To - From" conversions; SimplyAgree's bias is
#   first level minus second level (method1 - method2).
# - MCmcmc() (Bayesian scaled model) needs JAGS and is not covered here;
#   method_convert(bias = "scaled", ci_method = "boot") is the stand-in.

# Setup ----

# MethComp is archived on CRAN and depends on rjags. Installing rjags from CRAN
# is enough for MethComp to install and load (JAGS itself is only needed
# for MCmcmc()). One-time install:
# install.packages("rjags")
# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/MethComp/MethComp_1.30.0.tar.gz",
#   repos = NULL, type = "source", INSTALL_opts = "--no-test-load")

devtools::load_all()
library(ggplot2)

has_mc = requireNamespace("MethComp", quietly = TRUE)
if (!has_mc) message("MethComp not installed: MethComp sections are skipped.")

pred_2sd = .95

# The ox and fat data sets from MethComp, saved as test fixtures
ox = readRDS("tests/testthat/fixtures/ox.rds")    # oximetry, linked reps
fat = readRDS("tests/testthat/fixtures/fat.rds")  # fat thickness, exch. reps
if (has_mc) {
  oxM = MethComp::Meth(ox, print = FALSE)
  fatM = MethComp::Meth(fat, meth = "Obs", item = "Id", repl = "Rep",
                        y = "Sub", print = FALSE)
}

# 1. Data helpers: mc_long() and sim_methods() ----

## mc_long(): wide (one column per method) to long ----
data("reps")
head(reps)
reps_long = mc_long(reps, methods = c("x", "y"), id = "id")
head(reps_long)

if (has_mc) {
  # MethComp's equivalents: Meth() to make a long "Meth" object, to.wide()
  # back again
  head(MethComp::to.wide(oxM))
}

## sim_methods(): simulate from the (scaled) variance component model ----
sim3 = sim_methods(n_items = 40, n_reps = 3, methods = c("A", "B", "C"),
                   alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
                   sigma = c(0.3, 0.5, 0.4), replicates = "linked",
                   seed = 1)
head(sim3)
attr(sim3, "truth")  # true limits, RCs, conversions

# with non-unit slopes (scaled model), for method_convert() below
sim_sc = sim_methods(n_items = 60, n_reps = 2, methods = c("A", "B"),
                     alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                     mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)
attr(sim_sc, "truth")$conversion

if (has_mc) {
  # MethComp::Meth.sim() uses sigma.mi (tau), sigma.ir (omega) and
  # sigma.mir (sigma). Fit the same simulated data with both packages.
  set.seed(42)
  msim = MethComp::Meth.sim(Ni = 50, Nm = 3, Nr = 3,
                            alpha = c(0, 2, -1), beta = c(1, 1, 1),
                            sigma.mi = c(2, 2, 2), sigma.ir = 3,
                            sigma.mir = c(2, 4, 3))
  msim_df = as.data.frame(msim)[, c("meth", "item", "repl", "y")]
  print(MethComp::BA.est(msim, linked = TRUE))
  print(method_vc(msim_df, y = "y", method = "meth", id = "item",
                  replicate = "repl", replicates = "linked",
                  pred_level = pred_2sd))
}

# 2. method_vc(): variance components, bias, limits ----

## Oximetry, linked replicates: vs MethComp::BA.est() ----
fit_ox = method_vc(ox, y = "y", method = "meth", id = "item",
                   replicate = "repl", replicates = "linked",
                   pred_level = pred_2sd)
fit_ox
if (has_mc) {
  ba_ox = MethComp::BA.est(oxM, linked = TRUE)
  ba_ox
}

if (has_mc) {
  # numbers side by side (CO - pulse)
  vc = fit_ox$varcomp
  data.frame(
    quantity = c("bias", "sd(diff)", "LoA lower", "LoA upper",
                 "omega (IxR)", "tau (MxI)", "sigma CO", "sigma pulse",
                 "RC CO", "RC pulse"),
    SimplyAgree = c(fit_ox$bias$bias, fit_ox$limits$sd,
                    fit_ox$limits$lower_loa, fit_ox$limits$upper_loa,
                    vc$sd[vc$component == "omega"],
                    vc$sd[vc$component == "tau"],
                    vc$sd[vc$component == "sigma"],
                    fit_ox$repeatability$rc),
    MethComp = c(ba_ox$Conv["CO", "pulse", "alpha"],
                 ba_ox$Conv["CO", "pulse", "sd.pred"],
                 ba_ox$Conv["CO", "pulse", "LoA-lo"],
                 ba_ox$Conv["CO", "pulse", "LoA-up"],
                 ba_ox$VarComp["CO", "IxR"],
                 ba_ox$VarComp["CO", "MxI"],
                 ba_ox$VarComp[, "res"],
                 ba_ox$RepCoef[, "Coef."])
  )
}

## Oximetry on the percent-logit scale (Carstensen 2010, ch. 8) ----
fit_ox_lg = method_vc(ox, y = "y", method = "meth", id = "item",
                      replicate = "repl", replicates = "linked",
                      transform = "pctlogit", pred_level = pred_2sd)
fit_ox_lg
if (has_mc) {
  MethComp::BA.est(oxM, linked = TRUE, Transform = "pctlogit")
}

## Fat data, exchangeable replicates (Carstensen 2010, ch. 5) ----
fit_fat = method_vc(fat, y = "Sub", method = "Obs", id = "Id",
                    replicate = "Rep")
fit_fat
if (has_mc) {
  MethComp::BA.est(fatM, linked = FALSE)
}

## Three methods with replicates ----
fit3 = method_vc(sim3, y = "y", method = "method", id = "id",
                 replicate = "repl", replicates = "linked")
fit3
attr(sim3, "truth")$pairs  # compare to the truth

# method-specific tau (needs 3+ methods)
fit3_tau = method_vc(sim3, y = "y", method = "method", id = "id",
                     replicate = "repl", replicates = "linked",
                     tau = "method")
fit3_tau$varcomp

# include the item-by-replicate variance (methods used on different occasions)
method_vc(sim3, y = "y", method = "method", id = "id",
          replicate = "repl", replicates = "linked",
          include_omega = TRUE)$limits

if (has_mc && "sbp" %in% data(package = "MethComp")$results[, "Item"]) {
  # Systolic blood pressure: 2 observers (J, R) and a machine (S),
  # 3 linked replicates (Bland & Altman 1999)
  data("sbp", package = "MethComp")
  sbp_df = as.data.frame(sbp)[, c("meth", "item", "repl", "y")]
  # BA.est() defaults to a method-specific MxI variance with 3+ methods, so
  # tau = "method" is the matching fit. Here tau for R is ~0, so the
  # variance-component covariance is singular and CIs are not computed.
  fit_sbp = method_vc(sbp_df, y = "y", method = "meth", id = "item",
                      replicate = "repl", replicates = "linked",
                      tau = "method", pred_level = pred_2sd)
  print(fit_sbp)
  print(MethComp::BA.est(sbp, linked = TRUE))
  # common tau (SimplyAgree's default): different limits, but CIs available
  print(method_vc(sbp_df, y = "y", method = "meth", id = "item",
                  replicate = "repl", replicates = "linked",
                  pred_level = pred_2sd))
}

## Interval and bound options ----
# tolerance limits instead of limits of agreement
method_vc(ox, y = "y", method = "meth", id = "item", replicate = "repl",
          replicates = "linked", interval = "tolerance")$limits
# joint confidence bounds for the limits of agreement
method_vc(ox, y = "y", method = "meth", id = "item", replicate = "repl",
          replicates = "linked", bound_type = "joint")$limits

## Equivalence with agreement_limit() and tolerance_limit() ----
# two methods, one measurement per item: the paired-difference analysis
set.seed(1)
d1 = data.frame(x = rnorm(30, 10), y = rnorm(30, 10.3))
d1_long = mc_long(d1, methods = c("x", "y"))
agreement_limit(x = "x", y = "y", data = d1)$loa
method_vc(d1_long, y = "y", method = "method", id = "id")$limits
tolerance_limit(x = "x", y = "y", data = d1)$limits
method_vc(d1_long, y = "y", method = "method", id = "id",
          interval = "tolerance")$limits

## S3 methods ----
summary(fit_ox)
coef(fit_ox)
confint(fit_ox)
confint(fit_ox, parm = "sd", level = 0.90)
vcov(fit_ox)
# constant-bias conversion with prediction limits
predict(fit_ox, newdata = c(60, 75, 90), from = "CO", to = "pulse")

plot(fit_ox)                   # Bland-Altman with bias/limit bands
plot(fit3, pair = c("B", "C")) # choose the pair
plot(fit3, type = "matrix")    # all pairs
plot(fit_ox, type = "vc")      # variance components and RCs
check(fit_ox)                  # diagnostics

if (has_mc) {
  # MethComp's base-graphics versions
  MethComp::BA.plot(oxM, model = "linked")
  plot(MethComp::BA.est(oxM, linked = TRUE))
}

# 3. method_convert(): conversion equations ----

## Constant bias, from a method_vc() fit ----
conv_ox = method_convert(object = fit_ox)
conv_ox
if (has_mc) {
  ba_ox$Conv  # the same table from BA.est()
}

## Linear bias (differences regressed on averages) vs MethComp::DA.reg() ----
# one measurement per item: exactly the DA regression
ox1 = ox[ox$repl == 1, ]
conv_ox1 = method_convert(ox1, y = "y", method = "meth", id = "item")
conv_ox1
if (has_mc) {
  MethComp::DA.reg(MethComp::Meth(ox1, print = FALSE))
}

# with replicates: pairs replicates and uses cluster-robust SEs
conv_ox_lin = method_convert(ox, y = "y", method = "meth", id = "item",
                             replicate = "repl", replicates = "linked")
conv_ox_lin
if (has_mc) {
  MethComp::DA.reg(oxM)
}

# SD of the differences increasing with the level
method_convert(ox, y = "y", method = "meth", id = "item",
               replicate = "repl", replicates = "linked",
               variance = "linear")

## Scaled model (alternating regression) vs MethComp::AltReg() ----
conv_ox_sc = method_convert(ox, y = "y", method = "meth", id = "item",
                            replicate = "repl", replicates = "linked",
                            bias = "scaled")
conv_ox_sc
if (has_mc) {
  # tight eps so AltReg converges to the same point (default eps = 0.001)
  MethComp::AltReg(oxM, linked = TRUE, eps = 1e-6, maxiter = 500)
}

# recovers the slope of simulated data (true beta B|A = 1.2)
conv_sim_sc = method_convert(sim_sc, y = "y", method = "method", id = "id",
                             replicate = "repl", bias = "scaled",
                             tol = 1e-5)
conv_sim_sc
attr(sim_sc, "truth")$conversion

# parametric bootstrap CIs (slow; increase boot_reps in practice)
set.seed(1)
conv_boot = method_convert(sim_sc, y = "y", method = "method", id = "id",
                           replicate = "repl", bias = "scaled", tol = 1e-4,
                           ci_method = "boot", boot_reps = 100)
conv_boot             # print shows the bootstrap percentile CIs
confint(conv_boot)    # same percentile CIs; other levels via `level =`
confint(conv_boot, level = 0.80)
confint(conv_sim_sc)  # delta method, known to be too narrow

## Transformations ----
method_convert(ox, y = "y", method = "meth", id = "item",
               replicate = "repl", replicates = "linked",
               transform = "pctlogit")
if (has_mc) {
  MethComp::DA.reg(oxM, Transform = "pctlogit")
}

## S3 methods ----
summary(conv_ox_sc)
coef(conv_ox_sc)
confint(conv_ox_sc)
confint(conv_ox_sc, parm = "beta", level = 0.90)
predict(conv_ox_sc, newdata = c(60, 75, 90), from = "CO", to = "pulse")
predict(conv_ox_lin, newdata = c(60, 75, 90), from = "pulse", to = "CO")

plot(conv_ox_sc)                              # conversion plot
plot(conv_ox_lin, pair = c("pulse", "CO"))    # from pulse to CO
plot(conv_ox_sc, type = "ba")
check(conv_ox_lin)
check(conv_ox_sc)

if (has_mc) {
  plot(MethComp::AltReg(oxM, linked = TRUE, eps = 1e-6, maxiter = 500))
  MethComp::BA.plot(oxM, dif.type = "lin", sd.type = "lin")
}
