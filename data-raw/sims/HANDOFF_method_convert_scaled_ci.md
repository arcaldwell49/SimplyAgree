# Handoff: confidence intervals for `method_convert(bias = "scaled")`

Branch `MethComp`, last updated 2026-10-30.

**Status:** Phase 1 is done (`method_convert_lavaan_phase1_results.md`), and
the lavaan fit is implemented as `method_convert(estimator = "ml")` following
`method_convert_ml_plan.md`, with the docs listed at the end updated. Phase 2
has not been started.

## Problem

The scaled model is fit by alternating regression (AltReg). Its delta-method
CIs for the conversion intercepts and slopes cover only 53-73% of the time
(already done: `method_convert_coverage_summary.rds`), because they treat the
predicted true values as known.

## Direction: frequentist likelihood via lavaan; Bayes probably not needed

If the true values are treated as normally distributed (the structural
model), the scaled model is a confirmatory factor model, and lavaan can fit
it by full maximum likelihood:

- wide data, one row per item, one column per method x replicate;
- factors for the true value (`mu`), each replicate (`a_r`, linked only) and
  each method (`c_m`), all with loadings equal to the method slope `b_m`
  (the reference method fixed at 1);
- common `om2` and `t2`, one residual variance per method, method
  intercepts (the reference fixed at 0);
- `missing = "ml"` for unbalanced data.

The prototype is `data-raw/sims/proto_scaled_lavaan.R` (2 methods, linked
replicates, 3 replicates per item). Its Wald 95% CIs covered 92-95% for the
intercept and slope at I = 15/30/60 (300 data sets each, about 0.1 s per
fit), and its point estimates matched AltReg.

**Bayesian methods (MCMC) will probably not be used if the lavaan
frequentist intervals hold up.** They are a fallback only if Phase 1 fails.
In that case: blavaan (same syntax, precompiled Stan) first, then brms with
an item-level random effect and proper priors on the residual SDs. Not JAGS,
not custom samplers, and nothing that compiles a model at run time unless
there is no alternative.

## Phase 1 (do this first): lavaan coverage across designs

1. Generalize the prototype's syntax builder to M methods (one `b_m`, `a_m`
   and residual variance per non-reference method) and to exchangeable
   replicates (drop the `a_r` factors; residual variances equal within
   method, so the column order within an item doesn't matter).
2. Coverage simulation (95% Wald CIs for alpha and beta for every ordered
   pair, using `:=`-defined parameters for pairs without the reference
   method), using `sim_methods()` with the same true values as the existing
   sim (`alpha = c(0, 1, -0.5)`, `beta = c(1, 1.2, 0.9)`,
   `sigma = c(0.3, 0.4, 0.5)`, `tau = omega = 0.3`, 3 replicates). Cells:
   - I in {15, 30, 60} x M in {2, 3} x linked / exchangeable;
   - unbalanced data: `p_missing = 0.15`;
   - non-normal true values via `mu_values` (e.g. uniform, or a right-skewed
     gamma), each with `estimator = "ML"` and `"MLR"`.
   Also record bias of beta and of `sd_pred`, the convergence rate, and
   Heywood cases (negative or zero variance estimates, especially `om2`).
   500 data sets per cell. At about 0.1 s per fit, this is minutes, not
   hours, and it can run on any machine.
3. Compare with AltReg and MethComp on the oximetry data
   (`tests/testthat/fixtures/ox.rds`; see `mess/methcomp_tour.R`). Note any
   differences in the point estimates (lavaan won't reproduce AltReg's
   I/(I - 2) correction to the method-by-item SD).
4. Report the results and decide: if coverage is close to 95% across the
   cells, plan a `ci_method` (or a new fitting method) for `method_convert()`
   built on lavaan, with lavaan in Suggests.

## Phase 2 (only if approved separately): other simulation checks

Don't start these without explicit approval after Phase 1 is reviewed.

### Parametric bootstrap coverage (`ci_method = "boot"`)

The bootstrap has never had its coverage checked. A run was started on
2026-10-30 and stopped (too long for that machine); nothing was saved.

```bash
MC_PARTS=boot Rscript data-raw/sims/sim_method_convert_coverage.R > data-raw/sims/method_convert_coverage_boot_log.txt 2>&1
```

- Run from the package root. `MC_PARTS=boot` runs Part B only (I = 15 and
  30, 2 methods, linked replicates, 3 replicates per item) and writes
  `method_convert_coverage_boot.rds` and
  `method_convert_coverage_boot_summary.rds`, so the Part A files are left
  alone.
- Defaults: `MC_NBOOTSIM=200` data sets per cell, `MC_BOOT=199` bootstrap
  replicates, `MC_CORES = detectCores() - 2`.
- Cost: about 1.1 s per refit per core, so about 24 core-hours (about 2 h on
  12 cores). `MC_NBOOTSIM=100` halves it.
- Progress: `data-raw/sims/method_convert_progress.txt` (delete it before
  starting; don't commit it). Smoke test, about 30 s:
  `MC_PARTS=boot MC_NBOOTSIM=2 MC_BOOT=5 MC_CORES=2 Rscript ...`, then
  delete its output files.
- In the summary, look at `boot_alpha`, `boot_beta` (the target is 0.95),
  `delta_alpha`/`delta_beta` for comparison, `boot_sd`, `fail_rate`, and
  `n_boot_fail` in the raw results.

If lavaan works, this mainly decides whether the bootstrap stays as an option
or gets dropped or relabelled.

### Bayesian fallback

Only if Phase 1 fails and this is approved (see "Direction" above).

## Docs to update afterwards

Each of these currently says the intervals are too narrow and that the
bootstrap "has not yet been checked by simulation" or that MCMC is planned:

- `R/method_convert.R` (roxygen for `ci_method`; then `devtools::document()`)
- `R/mc_convert_internals.R`, the TODO comment above `mc_scaled_conversion()`
  (its plan to use MCMC is superseded by this note)
- `vignettes/method_conversion.Rmd`, the coverage section
- `NEWS.md`, the `bias = "scaled"` entry ("a Bayesian (MCMC) fit ... is
  planned")
