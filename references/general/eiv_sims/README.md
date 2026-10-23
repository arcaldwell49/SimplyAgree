# dem_reg() / pb_reg() joint tests and the Deming power functions: simulation

Status (2026-10-20): **runs complete; decided and implemented (see end).** Decisions follow
the reliability redesign (`../reli_redesign.md`): criteria are set before
running, and the user decides from the evidence.

## Questions

1. Does `joint_test()` (Sadler 2010 joint confidence region, F(2, n - 2)
   default) hold its nominal 5% type I error for `dem_reg()` and `pb_reg()`
   when the model's assumptions hold? Also: marginal CI coverage.
2. Do `deming_power_sim()` and `deming_sample_size()` give accurate power and
   N, judged against what a user then gets from `dem_reg()` + `joint_test()`?

Issues noticed before running (the study quantifies them):

- `deming_power_sim()` calls `.test_joint_enclosure()` without
  `df_residual`, so its joint test is chi-squared(2), while `joint_test()`
  defaults to F(2, n - 2). `check_fidelity.R` confirms it data set by data
  set (n = 12: power 0.595 chi-squared vs 0.515 F).
- `.compute_pair_weights_from_ratio()` (pb_reg, error.ratio != 1) grew a
  vector in a double loop (5.5 s for 199 bootstrap refits at n = 100).
  Vectorised before the study, same output (`expect_identical` test against
  the loop in `tests/testthat/test-pb_reg.R`).
- `pb_reg(se_method = "dufey")` is documented at 7-11% joint rejection.
- `deming_sample_size()` takes the first N whose noisy estimate (500 sims)
  crosses the target, which should bias N low.
- Aside, not studied: `.get_simple_eiv_data()` is defined in both
  `R/dem_reg.R` and `R/methods.simple_eiv.R`; collation order decides which
  wins.

## What Sadler (2010) actually does

The package's `joint_test()` is a Wald ellipse: Mahalanobis distance with the
delete-1 jackknife vcov, against 2 F(2, n - 2). Sadler's region (his Eq. 11,
after Draper & Smith) uses no vcov. It is the residual sum-of-squares region
S(a, b) <= S-hat (1 + 2F / (N - 2)), with S(a, b) = sum W_i(b) (Y - a - bX)^2
and Deming weights W_i(b) = 1 / (var_Y,i + b^2 var_X,i) recomputed at each
trial slope. In test form: F = ((S(a0, b0) - S-hat) / 2) / (S-hat / (N - 2)).
His Table 1 shows 95% enclosure at N = 3. It needs only the error-variance
ratio (scaling cancels). He fits iteratively reweighted Deming; `dem_reg()`
uses fixed data-based weights.

## Joint tests compared (Deming)

1. **F / chi-sq**: `joint_test()` (jackknife Wald ellipse), both references.
2. **SS**: Sadler's sum-of-squares F test (`ss_p()` in `sim_funs.R`).
   `check_fidelity.R` checks that the `dem_reg()` estimate minimises S(a, b)
   (plain and weighted); pilot type I at lambda = 4: 0.054 (n = 3), 0.052
   (n = 10).
3. **BB**: generalised Bradley-Blackwood. Regress d = y - b0 - b1 x on
   s = x + lambda b1 y by OLS and F-test both coefficients = 0. With
   constant normal errors and known lambda the error parts of d and s are
   independent and d has mean 0 under H0, so the test is exactly
   F(2, n - 2) (checked: 0.0498 at n = 10, lambda = 4). At lambda = 1 it is
   the classic Bradley-Blackwood test. With data-based weights or estimated
   lambda (reps) SS and BB are approximate.

These were added after the first runs started (user request, 2026-10-20), so
`dem`, `reps` and `truth` were rerun; criteria unchanged.

## Part A: type I error (H0 true: intercept 0, slope 1)

`gen.R`: x_true ~ Uniform(range), normal errors, error ratio
lambda = var(e_x) / var(e_y); the fit is given the true lambda. err =
SD(e_y) / SD(x_true). Ranges: narrow 50-100 (2:1, high intercept-slope
correlation), wide 5-100 (20:1).

| Block | Grid | Sims/cell |
|---|---|---|
| dem | n 10/20/50/100 x range x lambda 1/4 x err .05/.20 x variance const (unweighted) / constant CV (`weighted = TRUE`): 64 cells | 5000 (MC SE 0.31 pt) |
| reps | k = 2 replicates per subject, `dem_reg(id = )` estimates lambda; n x range x lambda 1/4, err .20: 16 cells | 5000 |
| pb | scissors: n 20/50/100 x range x err x lambda 1/4 (24 cells), analytic + bootstrap + delete-d jackknife (B = 999, same data) + Dufey (lambda = 1); symmetric/invariant at n = 50, err .20, lambda 1, bootstrap (4 cells) | 1000 (MC SE 0.69 pt) |

**Pass criteria (set before running):** joint rejection 2.5-7.5% (Bradley
liberal), target 4-6%; marginal CI coverage 92.5-97.5% for intercept and
slope; failures < 1%. Also reported: bias, empirical SD vs mean SE,
empirical intercept-slope correlation vs the vcov-implied one.

## Part B: power functions (Deming only; there is no PB power function)

Scenarios (`power_scenarios` in `gen.R`, `deming_power_sim()` argument
names): P1 constant variance, x 10-100, slope 1.05; P2 narrow x 50-100,
lambda 0.5, slope 1.05; P3 power-variance, `weighted = TRUE`, x 1-100, slope
1.05; P4 intercept 3; P5 H0 true. The error ratio is set as
`deming_power_sim()` sets it (variance functions at the mean observed x).
`gen_power()` reproduces `.generate_deming_data()` draw for draw
(`check_fidelity.R`), so differences come from the testing, not the data.

1. **truth**: 10,000 data sets per scenario x n (n 10-300), `dem_reg()` +
   `joint_test()` (F and chi-squared) + marginal CIs; smoothed with a
   binomial GLM on a natural spline in log n.
2. **powersim**: 20 runs of `deming_power_sim(n_sims = 1000)` per scenario
   at n = 20, 50, 100, 200; pass if the mean is within 3 MC SE of the F
   truth.
3. **samplesize**: 50 runs of `deming_sample_size()` at defaults (target
   0.90, `n_sims = 500`, step 5) for P1-P4; pass if the median true power at
   the returned N is within 0.90 +/- 0.025 and < 20% of runs have true power
   below 0.85.

## How to run

From the package root (PowerShell: `& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe"`).
Workers: `detectCores() - 2`. Raw RDS go to `EIV_SIMS_OUT` (default
`references/working/eiv_out`, untracked).

```sh
Rscript references/general/eiv_sims/check_fidelity.R
Rscript references/general/eiv_sims/sim_type1.R dem
Rscript references/general/eiv_sims/sim_type1.R reps
Rscript references/general/eiv_sims/sim_type1.R pb
Rscript references/general/eiv_sims/sim_power.R truth
Rscript references/general/eiv_sims/sim_power.R powersim
Rscript references/general/eiv_sims/sim_power.R samplesize
Rscript references/general/eiv_sims/summarise.R   # summary_*.csv + tables
```

## Files

| File | Role |
|---|---|
| `gen.R` | data generators, power scenarios |
| `sim_funs.R` | one data set -> statistics (package functions only) |
| `sim_type1.R` | Part A driver |
| `sim_power.R` | Part B driver |
| `check_fidelity.R` | generator and test checks; reproduces `deming_power_sim()` |
| `summarise.R` | criteria tables, `summary_type1.csv`, `summary_powersim.csv`, `summary_samplesize.csv` |

## Results

Run 2026-10-20 on the 16-core Windows machine (14 workers): dem 1 min, reps
2 min, pb 49 min, truth 4 min, powersim 2 min, samplesize 13 min. Fidelity
checks pass. Per-cell tables: `summary_type1.csv`, `summary_truth.csv`,
`summary_powersim.csv`, `summary_samplesize.csv`; raw RDS in
`references/working/eiv_out/` (untracked).

### Deming joint tests (64 cells, 5000 sims each; reps 16 cells)

Mean type I by n (range over cells in brackets where it matters):

| n | jackknife Wald, F | Wald, chi-sq | Sadler SS | Bradley-Blackwood | either marginal CI |
|---|---|---|---|---|---|
| 10 | .085 (.078-.097) | .144 | .050 | .049 | .070 |
| 20 | .074 | .100 | .052 | .051 | .074 |
| 50 | .061 | .070 | .051 | .050 | .073 |
| 100 | .056 | .060 | .051 | .050 | .070 |

- `joint_test()` (jackknife Wald) fails 2.5-7.5% in 21 of 64 cells: all
  n = 10 cells, and n = 20-50 with constant-CV (weighted) errors, up to .097.
  The intercept-slope correlation from the jackknife vcov is right (e.g.
  -.984 vs empirical -.983); the inflation comes from the variability of the
  jackknife variance at small n. At n = 10 with weights it can be wild (mean
  jackknife SE of the slope 2.9-4.8 vs empirical SD 0.2).
- **Sadler SS and BB pass every cell** (SS .043-.059, BB .044-.058), weighted
  and estimated-lambda (reps) cells included. reps behaves like dem (Wald F
  .085 at n = 10; SS/BB .052).
- Marginal CIs pass coverage everywhere (intercept .926-.954, slope
  .942-.967). "Either CI rejects" is ~.07: two 5% tests, uncorrected.

### Passing-Bablok joint tests (scissors, 1000 sims; 28 cells)

lambda = 1:

| n | bootstrap | jackknife (delete-d) | Dufey | analytic CI cov (int / slope) |
|---|---|---|---|---|
| 20 | .043 | .046 | .072 | .929 / .959 |
| 50 | .054 | .052 | .098 | .925 / .957 |
| 100 | .052 | .052 | .092 | .929 / .956 |

symmetric and invariant (n = 50, bootstrap): .051 and .054. Slope bias at
lambda = 1 is negligible (|bias| <= .003).

- Bootstrap and delete-d jackknife pass at lambda = 1.
- **Dufey fails 9 of 12 cells (.076-.114)**, confirming the documented 7-11%.
- The analytic intercept CI sits on the coverage boundary (.925-.929).
- **error.ratio != 1 is broken.** At lambda = 4 the slope is biased by
  -.07 (err .05) and -.43 (err .20), not shrinking with n. Rejection is
  .14-.15 at n = 20 / err .05, and .81-1.00 elsewhere. The bias is larger than
  naive OLS attenuation would give (about -.14 at err .20), so the ad hoc
  pair weights `1 / (1 + lambda (dy/dx)^2)` make the estimate worse, not
  better. Bootstrap and jackknife SEs match the empirical SD; the problem is
  the estimate.

### Power functions

Truth (10,000 sims per point), true N for 90% power:

| Scenario | Wald F | Wald chi-sq | Sadler SS | BB | either CI |
|---|---|---|---|---|---|
| P1 const, wide | 30 | 28 | 29 | 29 | 112 |
| P2 narrow, lambda .5 | <= 10 | <= 10 | <= 10 | <= 10 | 119 |
| P3 power var, weighted | 44 | 42 | 40 | 40 | 210 |
| P4 intercept 3 | 29 | 27 | 29 | 29 | 117 |

(P2 is at 95% power already at n = 10; the spline's N below 10 is an
extrapolation.)

- At correct size, Sadler SS is **at least as powerful** as the jackknife
  Wald test. The Wald test's small-n "power" partly reflects its inflated
  size (P5 null at n = 10: .084), and in the weighted scenario P3 Wald has
  less power than SS (n = 10: .29 vs .40). SS and BB are practically
  identical in power (differences <= .001).
- `deming_power_sim()` matches the **chi-squared** truth (|z| <= 1.9) and
  misses the F truth (z up to 13 at n = 20). Under H0 (P5) it reports .093 at
  n = 20 (the test users run gives .065; SS .052). Its either-CI power is
  accurate (|z| <= 2.7).
- `deming_sample_size()`: joint N medians 30 / 20 / 45 / 30 vs true (Wald F)
  30 / <= 10 / 44 / 29. P2 returns 20 because `initial_n = 20` is the grid
  floor (conservative). P4: 20% of runs have true power below .85 (q10
  .849), at the boundary of the criterion, the effect of first crossing a
  noisy curve with its chi-squared test. The CI arm passes all four. Step = 5
  rounding offsets part of the downward bias.

### Recommendations (for the user's decision)

1. `joint_test()` for Deming: default to Sadler's sum-of-squares F test
   (`method = "sadler"`), keep the jackknife Wald ellipse as
   `method = "wald"`. `plot_joint()` draws the exact SS region (closed form in
   a for each b). BB adds nothing over SS in size or power, so it is not
   added.
2. `deming_power_sim()` calls `joint_test()` itself (so the power function
   always matches the default test); `deming_sample_size()` inherits it.
   Consider honouring `use_joint` (currently ignored) and smoothing the
   crossing.
3. `pb_reg()`: drop or hard-error `error.ratio != 1` (and the `id`-estimated
   ratio, which feeds the same weights) until a principled method exists.
   Joint tests for PB: bootstrap or delete-d jackknife only; mark Dufey as
   not for joint tests (or remove it from `joint_test()`).
4. Docs: n >= 20-30 caveat for the Wald option; PB analytic intercept CI
   slightly liberal.

Decision (user, 2026-10-20): implement 1-4. Done:

- `joint_test()` / `plot_joint()` gain `method` (`NULL` = `"sadler"` for
  Deming, `"wald"` for PB). Sadler test and exact region in
  `R/methods.simple_eiv.R` (`.sadler_setup()`, `.sadler_S()`,
  `.sadler_test()`, `.sadler_region()`); `test_method = "asymptotic"` only
  with `"wald"`. Dufey fits and PB fits with `error.ratio != 1` (older
  objects) are refused.
- `deming_power_sim()` calls `joint_test()` (new `joint_method`, default
  `"sadler"`); `deming_sample_size(use_joint = NULL)` keeps the old search,
  `TRUE`/`FALSE` stop at that target.
- `pb_reg()` errors on `error.ratio != 1`; with `id` it averages replicates
  without an error ratio. The pair-weight code is removed.
- BB was not added (no gain over Sadler in size or power).
- Docs, Deming vignette, NEWS updated. The harness now computes the Wald
  p-values itself (`fit_stats()`), so a rerun reproduces the study under the
  new defaults; `check_fidelity.R` check 4 now confirms `deming_power_sim()`
  matches the Sadler test.

Status: **decided and implemented** (2026-10-20).
