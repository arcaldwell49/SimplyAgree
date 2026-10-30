# Phase 1 results: lavaan (full ML) CIs for `method_convert(bias = "scaled")`

Branch `MethComp`, 2026-10-30. Plan and context:
`HANDOFF_method_convert_scaled_ci.md`. Phase 2 has not been started.

## Files

| File | What |
|------|------|
| `lavaan_scaled_helpers.R` | Syntax builder for M methods and linked or exchangeable replicates; wide data; moment-based starting values; conversion table; variance estimates |
| `sim_method_convert_lavaan.R` | Coverage simulation (48 cells x 500 data sets, each fit with ML and MLR; 17.9 min on 14 cores) |
| `method_convert_lavaan_coverage.rds` / `_summary.rds` / `_log.txt` | Raw results (one row per data set x estimator x ordered pair); summary by cell (`$cells`) and by ordered pair (`$pairs`) |
| `summarise_method_convert_lavaan.R`, `method_convert_lavaan_ztsummary_log.txt` | Post hoc z vs t(I - 1) coverage from the stored estimates and SEs, next to the AltReg delta-method coverage |
| `lavaan_ox_compare.R`, `lavaan_ox_compare_log.txt` | Oximetry: lavaan vs `method_convert()` (AltReg) vs `MethComp::AltReg()` |

## Builder

- One `b<m>`, `a<m>` and `s<m>` (residual variance) per method; method 1 is
  the reference (`b1 = 1`, `a1 = 0`); common `t2`, and common `om2` when the
  replicates are linked. Exchangeable replicates have no `a_r` factors and
  are renumbered within item and method, so that the observed values fill
  the first columns (intercepts, loadings and residual variances are equal
  within method, so the order doesn't matter).
- Every ordered pair `(to k, from m)` is a `:=` parameter: `bc_k_m`,
  `ac_k_m`, `sd_k_m`, so pairs without the reference get delta-method SEs
  from lavaan.
- **Starting values are needed.** With lavaan's default starts, a 3-method
  linked data set converged to a local optimum with `var(mu) < 0` and
  nonsense slopes (`b = -0.98`, `25.9`). Moment starts (slopes from SD
  ratios of item means, pooled within-item variances, `t2 = om2 =
  0.05 var(mu)`) fixed it. In a package version, AltReg's estimates (always
  computed) are the natural starts.

## Coverage (nominal 95%, 500 data sets per cell, all ordered pairs)

Cells: I in {15, 30, 60} x M in {2, 3} x linked/exchangeable x scenario
{normal; `p_missing = 0.15`; uniform true values; gamma(shape 2) true values,
skewness 1.41}. True values as in the AltReg sim.

### ML, Wald (z) vs t(I - 1) quantiles, range over the 16 cells at each I

| I | alpha, z | beta, z | alpha, t(I-1) | beta, t(I-1) |
|---|----------|---------|---------------|--------------|
| 15 | 0.894-0.932 (mean 0.914) | 0.896-0.929 (0.912) | 0.918-0.955 (0.937) | 0.920-0.952 (0.936) |
| 30 | 0.927-0.950 (0.935) | 0.924-0.943 (0.935) | 0.935-0.953 (0.945) | 0.935-0.952 (0.945) |
| 60 | 0.933-0.961 (0.946) | 0.932-0.963 (0.947) | 0.937-0.964 (0.951) | 0.936-0.966 (0.952) |

For comparison, AltReg delta method (normal, balanced cells): 0.53-0.73 at
every I, with no improvement as I grows.

- The worst ordered pair in any cell (ML, z) is 0.87 (uniform, linked, M = 3,
  I = 15); at I >= 30 every pair is >= 0.91.
- No visible penalty for non-normal true values or for missing data:
  gamma and uniform cells are within Monte Carlo error (+/- 1 pp) of the
  normal cells at each I.
- The small-I shortfall is SEs that are too small: mean SE / empirical SD of
  beta-hat is about 0.84-0.94 at I = 15, 0.92-1.02 at I = 30, and 0.93-1.04
  at I = 60. t(I - 1) quantiles roughly correct for it.

### MLR (robust sandwich SEs): worse, and fragile with missing data

- Coverage 2-8 pp *below* ML at every I (for example, 0.84-0.89 at I = 15),
  with no gain for the skewed true values.
- With `p_missing = 0.15`, MLR failed (error or no convergence) in 42%
  (M = 2, exchangeable), 33% (M = 2, linked), 96% (M = 3, exchangeable) and
  97% (M = 3, linked)
  of the fits at I = 15, and in 19-39% of some I = 30 cells. ML converged in
  every one of the 24,000 data sets.
- Don't offer MLR.

### Bias, Heywood cases, speed (ML)

- Relative bias of beta-hat: 0.05-0.35% (largest at I = 15).
- `sd_pred`: mean ratio to the truth of 0.945-0.967 at I = 15, about 0.97-0.98 at
  I = 30, and 0.98-0.99 at I = 60 (ML variances are not corrected for degrees of freedom).
  AltReg's `sd_pred` (with its I/(I - 2) correction) has ratio 1.00-1.02,
  so for `sd_pred` the AltReg point estimate is better at small I.
- Heywood cases (a variance estimate <= 1e-8) occur only at I = 15: 0.6-3.2%
  of data sets with balanced data, 1.6-6.8% with missing data, almost always
  `t2`, rarely `om2` or a residual variance. None at I >= 30. These fits were
  kept in the coverage figures.
- About 0.1-0.6 s per fit (mean 0.24 s, all cells).

## Oximetry (CO reference, pulse; 61 items, linked)

`method_convert()` matches `MethComp::AltReg(eps = 1e-6)` to every digit
shown. lavaan differs:

| pulse from CO | lavaan ML | AltReg |
|---------------|-----------|--------|
| beta (95% CI) | 0.8975 (0.7971, 0.9979) | 0.9393 (0.8961, 0.9825) |
| alpha (95% CI) | 5.28 (-2.40, 12.96) | 2.12 (-1.19, 5.43) |
| sd_pred | 5.815 | 6.002 |
| beta SE (ML / MLR / AltReg delta) | 0.0512 / 0.0513 / 0.0218 | |

- It is the global optimum: three different starting values give the same
  log-likelihood (-1144.113).
- The likelihood is flat in the slope: fixing `b` at AltReg's 0.9393 lowers
  the log-likelihood by only 0.32 (a likelihood-ratio statistic of 0.64), so
  AltReg's slope is well inside the lavaan CI.
- The structural model fits poorly: chi-square 59.8 on 19 df, RMSEA 0.19,
  SRMR 0.16. The true values are strongly left-skewed (CO item means have
  skewness -1.68, range 26-90 %), more extreme than the gamma cells (+1.41).
  The point estimates can therefore differ noticeably between the
  structural (lavaan) and functional (AltReg) fits on real data, even
  though the simulated cells showed no coverage penalty.
- The method SDs are close (lavaan item-scale omega 3.61, tau 2.92,
  sigma 1.90/4.14; AltReg omega 3.52/3.31, tau 2.98/2.80 on each method's
  scale, sigma 2.05/4.08).

## Recommendation

Coverage is close to 95% across all the cells (ML with z: >= 0.93 for
I >= 30; with t(I - 1): 0.92-0.97 in every cell, including I = 15, missing
data and non-normal true values), compared with 53-73% for AltReg. Proceed
with a lavaan-based option, lavaan in Suggests:

1. Add the fit as a new fitting method rather than only a new `ci_method`,
   because its point estimates are not AltReg's (oximetry beta 0.90 vs 0.94).
   For example, `method_convert(bias = "scaled", fit = c("altreg", "ml"))`, or
   make `ci_method = "ml"` report lavaan's own estimates and say so. Either
   way, the print output and docs should name the estimator.
2. ML only, no MLR. Use t(I - 1) quantiles for the Wald CIs (I = number of
   items).
3. Start lavaan from the AltReg estimates; check convergence and
   `var(mu) > 0`; report Heywood cases (zero/negative `t2`, `om2`, residual
   variances) in `fit_info` with a warning.
4. Point estimate of `sd_pred` is 3-5% low at I = 15; either report it as
   is (ML) or apply the same I/(I - 2) scaling as AltReg. Worth a decision
   before implementing. Its CI was not checked here (truth is available in
   the stored results if needed: `sd_hat`, `sd_se`, `sd_true`).
5. Report the model fit (chi-square/RMSEA, or at least a note) so that users
   can see when the normal structural model is a poor description, as with
   the oximetry data.

Not done (Phase 2, needs approval): the parametric bootstrap coverage, and
any Bayesian fallback, which these results suggest isn't needed.
