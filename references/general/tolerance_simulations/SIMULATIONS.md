# Simulation record: `tolerance_limit()` revisions (branch `update-tolerance`)

Simulations and checks run while revising `tolerance_limit()` in response to the JSS review (`tolerance_limit_issues.md`). They were run between 2026-09-23 and 2026-09-24 with R 4.5.3, nlme and emmeans 2.0.4, on Windows. Each entry gives:
- the question the study answered
- the design
- the script
- the code state it ran against
- the results
- the decision it led to

Several studies ran against intermediate code that was never committed, or that was later replaced. They are kept because they are the reason for design decisions, not because they can be rerun as is.

## Layout

- `scripts/` holds the scripts exactly as they were run. They call `devtools::load_all("C:/GitHub/SimplyAgree")`, and most use internal functions whose names or signatures changed later (see "Reproducing" below).
- `results/NN_*/` (NN = study number below) holds the raw console output (`*.txt`, `output.txt`) and, where saved, the per-replicate results (`*.rds`: a matrix of rows = metrics, columns = simulated data sets; the mean of each row is the coverage).

## Code states

| Label | Commit | Contents |
|---|---|---|
| CRAN 0.3.0 | installed package | Original `sim_gls()`, `"approx"`/`"perc"` |
| Batch 3 (uncommitted) | between `dc14f44` and `e973451` | New block simulator (`gls_sim_setup()`), point-estimate bootstrap |
| Batch 4 intermediate (uncommitted) | none | Various `tol_approx()`/`tol_perc()` versions, described per study |
| `e973451` | "Calibrated bootstrap added" | Component MOVER for compound symmetry (CS), `boot_cal` calibration, `bound_type`; whole-data cluster sizes with `condition` |
| `251fe88` | "Simulations confirmed" | Per-condition cluster sizes/df with `condition`; `boot_cal` marked experimental |
| Phase 1 (uncommitted when run) | after `251fe88` | `model = "lme"` option; share-based (`k_b`) SD bound |
| Phase 2 (uncommitted when run) | after Phase 1 | Nested random intercepts (two `id` columns); J-piece share-based SD bound (`shares`, `df`) |

## Common definitions

- **Target:** content β = `pred_level` = 0.95 and confidence γ = `tol_level` = 0.95, unless stated.
- **Joint coverage:** the proportion of simulated data sets in which the tolerance interval covers at least β of the true distribution of differences, N(μ, σ²) with σ² the marginal variance. With several limits rows (conditions), it is averaged over rows.
- **IU coverage:** the proportion in which `lower.TL` ≤ μ − z σ (`iu_lo`) or `upper.TL` ≥ μ + z σ (`iu_hi`), with z = qnorm(0.975). `iu_both` means both hold; about 0.90 is expected for IU bounds.
- **Monte Carlo SE:** about 0.013 with 300 data sets, 0.007 with 1,000, and 0.0063 with 1,200.
- **Design shorthand:** "n × k" means n subjects with k measurements each. σ_a is the SD of the subject effect, σ_e the residual SD, and μ = 0.8 unless stated.

---

## 01. Covariance of the original bootstrap simulator (`sim_gls`)

- **Question:** does the original `sim_gls()` draw residuals with the fitted marginal covariance?
- **Script:** `scripts/simcheck_old.R`. **Code:** CRAN 0.3.0 (`SimplyAgree:::sim_gls`, `psim = 2`).
- **Design:** 30 subjects, 2 rows each, with ids *not contiguous* (`id = rep(1:30, 2)`). CS plus `varIdent` by condition (SD 1 vs 3). 4,000 draws, compared with `getVarCov()`.
- **Result (console only):**

  | | Fitted | Simulated |
  |---|---|---|
  | Var row 1 | 3.75 | 4.39 |
  | Cov(row 1, row 31), same subject | 2.07 | 0.03 |
  | Var row 31 | 8.82 | 10.8 |
  | Cov(row 1, row 2), different subjects | 0 | 1.32 |

- **Conclusion:** two bugs.
  - The correlation blocks were placed as if each subject's rows were contiguous.
  - The Cholesky factor was used in the wrong orientation: `R %*% z` has covariance RR′, not V.
  - The variances were also inflated by drawing the coefficients (`psim = 2`, review Issue 2).
- **Decision:** replaced by `gls_sim_setup()`/`gls_sim_draw()`: per-group blocks matched by group name, `t(chol(S))`, and simulation from the point estimates.

## 02. New simulator and bootstrap SE (Issue 2) and speed (Issue 5)

- **Script:** `scripts/batch3.R`. **Code:** Batch 3.
- **Results (console only):**
  - **Covariance, same design as 01, 20,000 draws:**

    | | Fitted | New simulator |
    |---|---|---|
    | Var row 1 | 3.75 | 3.75 |
    | Cov(row 1, row 31) | 2.07 | 2.12 |
    | Var row 31 | 8.82 | 8.84 |
    | Cov(row 1, row 2) | 0 | 0.02 |

  - **AR(1) with shuffled rows (10 × 6):** the simulated covariance showed the expected φ^|Δt| decay.
  - **Issue 2 check (20 × 19, σ_a = 0.5, σ_e = 1, 400 refits):** SD of the bootstrapped bias ÷ model SE = 0.956. The review observed 1.29 with the old simulator and expected about 1.41.
  - **Speed (366 rows, CS, 199 replicates):** 1.8 s. The review measured 10.5 s.
- **Decision:** kept. The refit bias and SEM come from the coefficients and `vcov()` rather than emmeans.

## 03. Analytic (Howe) limits: effective df for σ̂² (Issue 6), first attempt

- **Question:** does replacing the df of the bias with a delta-method effective df for σ̂² (from `apVar`) give nominal coverage for clustered data?
- **Script:** `scripts/cov_approx.R`, run in its **first form**, which was later edited in place:
  - it called `e$SD.df = tol_sd_df(g, e)`
  - the "old" comparison used `e_old$SD.df = e_old$df`
- **Code:** Batch 4 intermediate. `tol_approx()` used `sqrt(nu / qchisq(1 - gamma, nu))` with ν = `SD.df`.
- **Design:** CS; 20 × 19 with σ_a = 0.5, 20 × 19 with σ_a = 1.5, and 10 × 5 with σ_a = 1 (σ_e = 1); 1,000 data sets each.
- **Results:** `results/03_analytic_effective_df/output.txt`

  | Design | joint | joint (old: df of bias) | IU lo / hi | ν |
  |---|---|---|---|---|
  | 20×19, σ_a = .5 | 0.921 | 1.000 | 0.927 / 0.941 | 224 |
  | 20×19, σ_a = 1.5 | 0.911 | 0.989 | 0.918 / 0.931 | 41 |
  | 10×5, σ_a = 1 | 0.912 | 1.000 | 0.915 / 0.922 | 25 |

- **Conclusion:** the old df was very conservative, but a single Satterthwaite df is liberal, because it misses the skewness of the between-subject component (only n − 1 df).
- **Decision:** use a component-wise MOVER bound for σ under CS (study 04).

## 04. Analytic limits: component MOVER bound for σ under CS (Issue 6)

- **Script:** `scripts/cov_approx.R` (the surviving, edited version, which calls `tol_sd_upper()`). **Code:** Batch 4, later committed in `e973451`.
- **Design:** as in 03; 1,000 data sets each.
- **Results:** `results/04_analytic_mover/output.txt`

  | Design | joint | IU lo / hi | IU both |
  |---|---|---|---|
  | 20×19, σ_a = .5 | 0.957 | 0.940 / 0.958 | 0.901 |
  | 20×19, σ_a = 1.5 | 0.946 | 0.939 / 0.946 | 0.896 |
  | 10×5, σ_a = 1 | 0.949 | 0.951 / 0.950 | 0.912 |

- **Decision:** kept. The effective df remains only for non-CS structures.

## 05. Bootstrap limits, first β-content version

- **Question:** do the first `"perc"` versions reach nominal coverage? These were: joint as a k-multiplier calibrated so that b\* ± k s\* has content β; IU as plain percentiles of b\* ∓ z s\*.
- **Script:** `scripts/cov_perc.R`. **Code:** Batch 4 intermediate (`tol_perc()`, `content_k()`; both since removed).
- **Design:** 10×5 (σ_a = 1) and 20×19 (σ_a = .5); 300 data sets × 199 replicates.
- **Results:** `results/05_perc_uncalibrated/output.txt`

  | Design | joint | IU lo / hi |
  |---|---|---|
  | 10×5 | 0.883 | 0.870 / 0.870 |
  | 20×19 | 0.917 | 0.923 / 0.923 |

- **Conclusion:** liberal.

## 06. Bootstrap limits: independent vs clustered, percentile vs studentized IU

- **Question:** is 05 a bug or a plug-in limitation? Is a studentized IU bound better?
- **Script:** `scripts/cov_perc2.R` (adds the studentized IU, Q\* = (θ̂\* − θ̂)/s\*). **Code:** Batch 4 intermediate.
- **Design:** 10×5 independent (σ_a = 0, no correlation), 10×5 CS, and 20×19 CS; 300 × 199.
- **Results:** `results/06_perc_studentized/output.txt`

  | Design | joint | IU percentile lo / hi | IU studentized lo / hi |
  |---|---|---|---|
  | Independent 10×5 | 0.957 | 0.927 / 0.917 | 0.950 / 0.953 |
  | CS 10×5 | 0.883 | 0.870 / 0.870 | 0.890 / 0.910 |
  | CS 20×19 | 0.917 | 0.923 / 0.923 | 0.937 / 0.927 |

- **Conclusion:** the implementation is correct (exact for independent data), and studentizing fixes the IU percentile bias. With clusters, the bootstrap is liberal because the estimated correlation is treated as known.
- **Decision:** led to the bootstrap calibration of the analytic limits (07).

## 07. `boot_cal` (bootstrap calibration of the analytic limits): five designs

- **Script:** `scripts/cov_boot.R`. **Code:** `e973451`. It uses `tol_approx()`, `tol_boot()`, `boot_delta_gls()`, `sd_bound_info()` and `sd_upper_at()` with the ρ-based `"cs"` info.
- **Designs:** 300 data sets × 199 replicates each.
  - `cs10x5`: σ_a = σ_e = 1.
  - `cs20x19a`: σ_a = 0.5, σ_e = 1.
  - `cs20x19b`: σ_a = 1.5, σ_e = 1.
  - `ar1`: 15 × 8, AR(1) with φ = 0.6, σ = 1, no subject effect; fitted with `cor_type = "ar1"`.
  - `csvar`: 12 subjects × 4 per condition, generated from the gls-implied model (ρ = 0.4, SD 1 vs 2, means 0.5 vs −0.5); fitted with `condition`.
- **Results:** `results/07_bootcal_five_designs/cb_*.txt`

  | Design | analytic joint | boot_cal joint | analytic IU lo / hi | boot_cal IU lo / hi |
  |---|---|---|---|---|
  | cs10x5 | 0.957 | 0.927 | 0.967 / 0.953 | 0.947 / 0.940 |
  | cs20x19a | 0.967 | 0.950 | 0.937 / 0.943 | 0.920 / 0.937 |
  | cs20x19b | 0.943 | 0.953 | 0.927 / 0.937 | 0.940 / 0.943 |
  | ar1 | 0.920 | 0.937 | 0.923 / 0.903 | 0.937 / 0.920 |
  | csvar | 0.918 | 0.965 | 0.923 / 0.942 | 0.950 / 0.955 |

- **Note:** the `ar1` analytic result here turned out to be noise; see 08.

## 08. Confirmatory runs: CS 10×5 and AR(1)

- **Script:** `scripts/cov_boot2.R` (`cov_boot.R` plus a seed-offset argument). **Code:** `e973451`. These designs have no `condition`, so the `251fe88` change does not affect them, and the Phase 1 `k_b` refactor is numerically identical for CS.
- **Design:** as in 07; 4 × 250 = 1,000 data sets each, 199 replicates.
- **Results:** `results/08_confirm_cs10x5_ar1/` (`cc_*_{1..4}.rds` and `.txt`), pooled:

  | Design | analytic joint | boot_cal joint | analytic IU lo / hi | boot_cal IU lo / hi |
  |---|---|---|---|---|
  | cs10x5 | 0.954 | 0.932 | 0.950 / 0.948 | 0.944 / 0.943 |
  | ar1 | 0.950 | 0.960 | 0.951 / 0.952 | 0.958 / 0.960 |

- **Conclusion:** the analytic limits are nominal for AR(1) (the 300-run result in 07 was noise). `boot_cal` joint limits are about 2 points liberal with 10 subjects.

## 09. CS with `condition`: whole-data cluster sizes

- **Script:** `scripts/cov_boot2.R` (design `csvar`). **Code:** `e973451`.
- **Design:** as in 07; 6 × 200 = 1,200 data sets. (The `.rds` files were written to the repository root by mistake at the time and moved here.)
- **Results:** `results/09_csvar_wholedata_df/`, pooled:

  | analytic joint | boot_cal joint | analytic IU lo / hi | boot_cal IU lo / hi |
  |---|---|---|---|
  | 0.931 | 0.958 | 0.935 / 0.941 | 0.952 / 0.955 |

- **Conclusion:** the analytic limits are liberal even though the data follow the fitted model. The cause: each condition's variance components were bounded with whole-data cluster sizes (mh = 8, when subject means within a condition use 4 measurements).
- **Decision:** per-condition `mh`/`df_b`/`df_w` (10).

## 10. CS with `condition`: per-condition cluster sizes

- **Script:** `scripts/cov_boot2.R` (design `csvar`). **Code:** `251fe88`.
- **Design:** as in 09; 1,200 data sets.
- **Results:** `results/10_csvar_percondition_df/` (`cc_csvar_*.rds`, `pc_*.txt`), pooled:

  | analytic joint | boot_cal joint | analytic IU lo / hi | boot_cal IU lo / hi |
  |---|---|---|---|
  | 0.965 | 0.930 | 0.958 / 0.963 | 0.934 / 0.943 |

- **Conclusion:** the analytic limits are now slightly conservative. `boot_cal` got worse, because its calibration against the fitted model (estimated correlation treated as known) overcorrects.
- **Decision:** analytic limits recommended; `boot_cal` marked experimental. An idea for improving `boot_cal` (drawing the variance parameters from `apVar` for each replicate) is recorded as a TODO above `tol_boot()`.

## 11. `model = "lme"` (random intercept) validation

- **Script:** `scripts/cov_lme.R`. It uses only the exported `tolerance_limit()`, so it runs on the current code. **Code:** Phase 1.
- **Designs:** 4 × 250 = 1,000 data sets each; analytic limits only.
  - `riar1`: 15 × 8, subject effect σ_b = 0.7 plus AR(1) residuals (φ = 0.6, marginal SD 1). Each data set fitted four ways: `gls` AR1, `gls` CS, `lme` AR1, `lme` random intercept only.
  - `rivar`: 12 subjects × 4 per condition, common σ_b = 1, residual SD 1 vs 2, means 0.5 vs −0.5. Fitted with `gls` + `condition` and `lme` + `condition`.
- **Results:** `results/11_lme/` (`lm_*.rds` and `.txt`), pooled:

  | Design | Fit | joint | IU lo / hi |
  |---|---|---|---|
  | riar1 | gls, AR1 | 0.893 | 0.895 / 0.916 |
  | riar1 | gls, CS | 0.948 | 0.936 / 0.956 |
  | riar1 | lme, AR1 | 0.949 | 0.939 / 0.957 |
  | riar1 | lme, RI only | 0.948 | 0.937 / 0.956 |
  | rivar | gls + condition | 0.957 | 0.953 / 0.949 |
  | rivar | lme + condition | 0.971 | 0.969 / 0.965 |

- **Conclusion:** confirms review Issue 9 for the tolerance limits (`gls` AR1 is liberal with a persistent subject effect) and that `lme` AR1 fixes it. With `condition`, `lme` is slightly conservative, because the between piece uses per-condition cluster sizes while σ_b is estimated from all of a subject's measurements.

## 12. Nested random intercepts (`id = c(outer, inner)`, review Issue 11)

- **Script:** `scripts/cov_nested.R`. It uses only the exported `tolerance_limit()`, so it runs on the current code. **Code:** Phase 2 (nested `lme`, three-piece MOVER bound; uncommitted when run). Smoke checks are in `scripts/nested_smoke.R`.
- **Truth:** subject random intercept σ_1 = 0.7, setting-within-subject random intercept σ_2 = 0.5, residual σ_e = 1, μ = 0.8.
- **Designs:** 4 × 250 = 1,000 data sets each; analytic limits only.
  - `balanced`: 15 subjects × 3 settings × 5 measurements.
  - `unbalanced`: 15 subjects, with 2–4 settings per subject and 3–7 measurements per setting.
- **Fits** for each data set:
  - `lme_nested`: `id = c("subject", "setting")`, `model = "lme"`.
  - `lme_subject`: `id = "subject"`, `model = "lme"`.
  - `lme_combined`: `id` = the subject:setting identifier (the review's workaround).
  - `gls_subject`: `id = "subject"`, CS.
- **Results:** `results/12_nested/` (`nest_*.rds` and `.txt`), pooled:

  | Fit | Balanced: joint / IU lo / IU hi | Unbalanced: joint / IU lo / IU hi |
  |---|---|---|
  | lme_nested | 0.961 / 0.965 / 0.957 | 0.963 / 0.948 / 0.954 |
  | lme_subject | 0.958 / 0.965 / 0.953 | 0.957 / 0.940 / 0.960 |
  | lme_combined | 0.905 / 0.898 / 0.894 | 0.883 / 0.855 / 0.890 |
  | gls_subject | 0.958 / 0.965 / 0.953 | 0.957 / 0.940 / 0.960 |

- **Conclusion:**
  - Confirms review Issue 11: using the inner level as `id` is liberal (0.86–0.90).
  - Nested random intercepts are close to nominal, slightly conservative.
  - Grouping by the outer level only is also close to nominal for these marginal limits, because the inner-level variance is absorbed into the residual. The nested model additionally reports the inner-level component (`SD.nested`).

## Other checks (console only)

These were not coverage simulations. Their outputs appear in the session log only; where a script exists it's in `scripts/`.

- **Review reproductions (Issue 1, variance functions):**
  - condition B SEP went from 1.40 to 5.15 (empirical SD 5.09)
  - `varPower(~avg)` SEP at avg ≈ 203 went from 0.87 to 9.18, matching `sigma * avg^delta` exactly
- **Howe regression check:** independent data, n = 30 (seed 5). The analytic joint limits are −6.1724, 4.5375, identical to Howe's formula. The noncentral-t IU bound matches `qt(0.95, n − 1, ncp = z·√n)/√n` exactly. Now unit tests.
- **Golfer-sized example (20 subjects, 366 rows, CS; `scripts/final4.R`, `scripts/smoke4.R`, `scripts/smoke5.R`):**
  - analytic joint −1.70, 3.22
  - `boot_cal` joint −1.70, 3.22 (calibrated level 0.95)
  - analytic IU −1.77, 3.29
  - `agreement_limit(data_type = "nest")` IU −1.72, 3.24
- **emmeans with `gls` + variance function (`scripts/emm.R`):** emmeans passes the call's `weights` to `model.frame` and errors for intercept-only models ("replacement has 200 rows, data has 2"). Dropping `weights` from a local copy of the call gives identical results where emmeans already worked.
- **`apVar` structure (`scripts/apvar.R`):** a plain `gls` has `apVar = NULL`. With CS, 1/(2·var(lSigma)) = 348 against N − p = 379. `coef<-` on a `varIdent` updates the weights.
- **`lme` vs `gls` (Phase 1; `scripts/lme_smoke.R`):**
  - `lme(~1|id)` and `gls(corCompSymm)` have identical log-likelihoods, and give identical limits on `reps` (bias, SEM, SD, SD.upper, TL and SD.between to about 1e-6)
  - emmeans' Satterthwaite (and appx-Satterthwaite) df for `lme` errored for intercept-only models and *hung* for `lme` + `varIdent` on the `temps` data, so containment df are used for `lme`
  - the `lme` simulator matches `getVarCov(type = "marginal")` (max absolute difference 0.0005 on a scale of 0.03, 20,000 draws)
  - `predict_varFunc()` returned `NA` for some groups with tibble data; fixed

## Reproducing

- `cov_lme.R` and `cov_nested.R` run against the current code as is. Usage: `Rscript cov_lme.R <design> <nsim> <seed offset>`, with results written to the working directory.
- `cov_boot2.R` (usage: `Rscript cov_boot2.R <design> <nsim> <B> <seed offset>`) needs these renames for the Phase 1 code:
  - `boot_delta_gls()` is now `boot_delta()`
  - the SD-bound info type `"cs"` (with `rho`/`mh`) is now `"comp"` (with a `shares` matrix and a `df` matrix, one column per variance piece)
  - `gls_sim_setup()` also handles `lme`
- `cov_approx.R`, `cov_perc.R` and `cov_perc2.R` use functions that no longer exist (`tol_sd_upper()`, `tol_perc()`, `content_k()`), so they document historical code states only.
- Seeds are set inside each script from the design and the seed offset. Designs were run as parallel processes, 4 or 6 offsets per design; the offsets used are in the result file names.
