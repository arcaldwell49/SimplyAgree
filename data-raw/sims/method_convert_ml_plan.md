# Plan: maximum likelihood (lavaan) fit for `method_convert(bias = "scaled")`

Branch `MethComp`, 2026-10-30. Follows `method_convert_lavaan_phase1_results.md`
(Phase 1 of `HANDOFF_method_convert_scaled_ci.md`). Nothing below is
implemented yet. Phase 2 (bootstrap coverage, Bayesian work) is not part of
this plan.

## Decisions for review (recommendation first)

1. **API.** New argument `estimator = c("altreg", "ml")`, used only with
   `bias = "scaled"`. Alternative: `ci_method = "ml"`. A separate argument is
   better because the ML point estimates differ from AltReg's (oximetry
   slope 0.90 vs 0.94), and a "CI method" that changes the estimates would
   be misleading.
2. **Default.** Keep `"altreg"` as the default for now: lavaan is in
   Suggests, so the default can't depend on it, and AltReg matches MethComp.
   The docs and the AltReg print note point users to `estimator = "ml"` for
   intervals. (Revisit if lavaan ever moves to Imports.)
3. **Intervals.** Wald with t(I - 1) quantiles (I = number of items):
   alpha and beta on the natural scale; `sd_pred` on the log scale
   (`exp(log(sd) +/- q se/sd)`). Post hoc on the Phase 1 results, the log
   `sd_pred` intervals covered 92.4-93.2% (I = 15), 93.5-94.1% (I = 30) and
   94.2-94.9% (I = 60) by scenario (natural scale: 89.7-94.2%); alpha/beta
   92-97% in every cell (`summarise_method_convert_lavaan.R`). Variance
   component CIs were not checked, so `varcomp$lower/upper` stay `NA`.
4. **`sd_pred` small-sample correction.** Report the ML estimate as is
   (3-5% low at I = 15, about 1% at I = 60) and document it, rather than
   adding AltReg's I/(I - 2) factor to an ML fit; the intervals above
   already include this bias. Alternative: scale `t2` by I/(I - 2) to match
   AltReg/MethComp (not checked by simulation).
5. **`ci_method = "boot"` with `estimator = "ml"`.** Not supported (error).
   The bootstrap stays AltReg-only and unvalidated until Phase 2 decides
   its fate.
6. **MLR.** Not offered (2-8 pp worse coverage, frequent failures with
   missing data).

## Implementation

### New internal file `R/mc_scaled_ml.R`

Port of `data-raw/sims/lavaan_scaled_helpers.R`, in package style:

- `mc_lav_wide(df, linked)`: from the processed data (`mc_data_org()`
  output, possibly transformed); exchangeable replicates are renumbered
  within item and method.
- `mc_lav_syntax(cols, linked, start)`: as in the helper; starting values
  from the AltReg fit (`alt$a`, `alt$b`, `alt$tau_z`, `alt$omega_z`,
  `alt$sigma_z`, `var(alt$mu)`). Undo the I/(I - 2) factor on `tau_z`
  for the start.
- `mc_scaled_ml(df, alt, linked, conf_level)`: checks for lavaan with
  `requireNamespace("lavaan", quietly = TRUE)` (message telling the user to
  install it), fits with `missing = "ml"`, `meanstructure = TRUE`,
  `estimator = "ML"`; returns a list shaped like `mc_altreg()` output
  where the downstream code needs it (`a`, `b`, `lv`, `linked`,
  `tau_y`, `omega_y`, `sigma_y`, `mu` = factor scores from
  `lavaan::lavPredict()`), plus `fit` (lavaan object), `converged`,
  `iterations`, `heywood` (named logical: `t2`, `om2`, `s_<method>`),
  `logl`, and fit measures (`chisq`, `df`, `pvalue`, `rmsea`, `srmr`).
- `mc_scaled_ml_conversion(ml, conf_level)`: conversion table with the
  same columns as `mc_scaled_conversion()`: `alpha_se`, `beta_se` from the
  `:=` parameters, `df = I - 1`, t limits, plus `sd_lower`/`sd_upper`
  (log scale), and method-to-itself rows (`sqrt(2) sigma_m`).
- Diagnostics for `check()`: residuals `y - (a_m + b_m * eta_hat)` with
  `eta_hat` from the factor scores (mu + a_r + c_m), the same tibble shape
  as `mc_scaled_diag()`.

### `R/method_convert.R`

- Add `estimator = c("altreg", "ml")` (after `ci_method`); `match.arg()`;
  errors: `estimator = "ml"` with `bias = "linear"` or with `object`;
  `estimator = "ml"` with `ci_method = "boot"`.
- In `mc_convert_scaled()`, always run AltReg (cheap, and supplies the
  starts); if `estimator = "ml"`, fit by ML and build `conversion`,
  `varcomp` (`lower`/`upper` `NA`; not checked by simulation),
  `scaled$methods`, `scaled$mu`, `diagnostics`, `model` (the lavaan fit if
  `keep_model`). `scaled$trace` stays the AltReg trace (labelled as the
  starting fit) or `NULL`.
- `fit_info` gains `estimator`, `heywood`, `logl`, `fit_measures`;
  `iterations`/`converged` refer to the estimator used.
- Warnings: non-convergence; Heywood case ("A variance estimate is zero or
  negative (t2 ...); the intervals may be unreliable. This is common with
  fewer than about 20 items."); `var(mu) <= 0` is an error-level warning
  (the fit is unusable).
- `call2$estimator = estimator`.

### `R/methods.simple_mconv.R`

- `print()`: title "Scaled Model (maximum likelihood)" vs "(alternating
  regression)"; ML note: "CIs: Wald (ML), t with I - 1 df; about 92-97%
  coverage of 95% intervals in simulations with 15-60 items" and the
  Heywood note when present. Update the AltReg delta note to point to
  `estimator = "ml"`.
- `summary()`/its print: estimator line; for ML, log-likelihood, iterations,
  convergence, and model fit (chi-square, df, p, RMSEA) with a one-line
  reminder that a poor fit means the normal structural model is
  questionable.
- `confint()`: ML uses the stored SEs with `qt(, df)` as now (works
  unchanged); add `parm = "sd"` only if we decide to expose the `sd_pred`
  intervals there (optional).
- `check()`: for ML, replace the convergence-trace panel with the model fit
  annotation (or drop to two panels); the normality and spread panels use
  the ML residuals.
- `plot()`, `predict()`, `coef()`, `model_equation()`: no changes expected
  (they use `conversion`, `limits_par`, `scaled$methods`, `varcomp`);
  verify.

### DESCRIPTION, NAMESPACE

- `lavaan` in Suggests. No new imports (`lavaan::` calls only).

## Tests (`tests/testthat/test-method_convert.R`, `test-simple_mconv_methods.R`)

All ML tests: `skip_if_not_installed("lavaan")`. Oximetry fit takes about 1 s;
keep one ML fit on CRAN and put the rest under `skip_on_cran()`.

- Oximetry reference values (from `lavaan_ox_compare_log.txt`, tolerance
  1e-4): pulse|CO beta 0.89748, alpha 5.2836, sd_pred 5.8153, beta SE
  0.05123; CO|pulse beta 1.11423; log-likelihood -1144.113.
- Internal consistency: `beta[k|m] = 1 / beta[m|k]`,
  `alpha[k|m] = -alpha[m|k] * beta[k|m]`; `df = I - 1`; the CIs agree
  with `confint()`.
- Agreement with the Phase 1 helper on a seeded `sim_methods()` data set
  (M = 3, linked; the case that needs starting values): converged and
  `var(mu) > 0`.
- Exchangeable with `p_missing > 0` (unbalanced): fits; columns line up.
- Transform (`transform = "pctlogit"` on ox): fits, and `predict()`
  back-transforms.
- Errors: `estimator = "ml"` with `bias = "linear"`, with `object`, with
  `ci_method = "boot"`; missing lavaan (mock a small `mc_has_lavaan()`
  wrapper with `local_mocked_bindings()`).
- Heywood warning: find a seed with I = 15 that gives `t2 <= 0` (about 1-3%
  of data sets in Phase 1) and test the warning and `fit_info$heywood`.
- S3 methods on an ML fit: print (snapshot or `expect_output` on the
  estimator line), summary, check (returns a patchwork), confint, predict,
  plot, model_equation.
- Coverage itself is not re-tested; the simulation stays in `data-raw/sims/`.

## Docs

- `R/method_convert.R` roxygen: `estimator` param; scaled details describe
  both fits (structural ML vs functional AltReg; why the estimates can
  differ; the oximetry example); replace the coverage paragraph with the
  Phase 1 results (AltReg delta 53-73%; ML 89-96% z, 92-97% t(I - 1); MLR
  not offered; Heywood cases at small I); the MCmcmc sentence becomes "not
  needed". Example: `estimator = "ml"` inside
  `if (requireNamespace("lavaan", quietly = TRUE))`. Then
  `devtools::document()`.
- `R/mc_convert_internals.R`: replace the TODO above
  `mc_scaled_conversion()` (MCMC/brms plan superseded) with a pointer to
  the ML fit and the Phase 1 results.
- `vignettes/method_conversion.Rmd`: coverage section rewritten; add an
  `estimator = "ml"` chunk (eval only if lavaan is installed) next to the
  AltReg one, with the oximetry comparison and model fit.
- `NEWS.md`: the `bias = "scaled"` entry: ML fit via lavaan (Suggests)
  with validated intervals; remove "a Bayesian (MCMC) fit ... is planned".
- `.claude/CLAUDE.md`: add lavaan to Suggests list.
- `HANDOFF_method_convert_scaled_ci.md`: mark Phase 1 done, link the
  results and this plan.

## Order of work

1. `R/mc_scaled_ml.R` + `method_convert()` wiring + `fit_info`.
2. S3 methods.
3. Tests; `devtools::test(filter = "method_convert|mconv")`.
4. Docs, `devtools::document()`, vignette.
5. `devtools::check()` (expect only the known NOTEs); time the
   method_convert tests with `NOT_CRAN` unset.
