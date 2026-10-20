# ccc_test() intervals for replicate and nested data: simulation plan

Status (2026-10-19): **plan fixed, scripts written and checked, main runs not
started.** The work is moving to another machine; the next step there is
"How to run" below. Decisions follow the reliability redesign
(`../reli_redesign.md`): criteria are set before running, and the user
decides from the evidence.

## Question

`ccc_test(data_type = "reps" | "nest")` uses a delete-one-subject jackknife
SE of Fisher z with t(g - 1) quantiles (`.ccc_clustered()` in
`R/ccc_test.R`). The one-scenario check in `coverage.R` gave 94-95%
coverage at g >= 20 but 91-93% at g = 10, with the CCC biased down about
0.03 and most misses above the interval. Should `ccc_test()` get an opt-in
subject (cluster) bootstrap, and which interval?

## Intervals compared (95%, same data sets, same resamples)

1. **jk**: the current method, jackknife SE of z, t(g - 1) quantiles.
2. **perc**: subject bootstrap, B = 999, percentile.
3. **bca**: the same resamples, BCa as `.bca_params()` in `R/reli_engine.R`
   (bias correction from the resamples, acceleration from the jackknife
   values on the CCC scale).
4. **stud**: studentized bootstrap, t* = (z* - z) / SE*, where SE* is a
   delete-one jackknife within each resample (first 499 resamples). This was
   added at the user's request; the original brief excluded it as too slow
   with lme4, so the reps CCC uses the fast REML below.

A resample draws g subjects with replacement, and a subject drawn twice
counts as two subjects (fresh ids), as in `.reli_boot()`. The methods are
fixed effects, so subject resampling should carry the method-difference
uncertainty (no hybrid bootstrap). The method-difference = 0 cells (where
`s2_method` is truncated at 0) are reported separately.

## Grid (216 cells per data type, `gen.R`)

| Factor | Levels |
|---|---|
| subjects g | 10, 20, 50 |
| measurements per subject k | 2, 3, 6 (reps: replicates per method; nest: pairs) |
| true CCC | .50, .80, .95 |
| method difference | none; d^2/2 = 25% of the non-subject variance |
| missing | balanced; 15% of measurements dropped (nest: one half of 15% of pairs) |
| distribution | normal; chi-square(3) subject effects with t(5) within-subject terms and errors (standardized) |

Total variance is 1. reps: truth is the Carrasco-Jover CCC
s_a / (s_a + s_ab + d^2/2 + s_e), which `.ccc_vc()` estimates. nest: the true
value varies within subject (subject : within = 2 : 1), truth is
2v / (2(v + s_e) + d^2), Lin's CCC over pairs.

Simulations per cell: jk/perc/bca 1000 (MC SE 0.69 points at 95%);
stud 500 (MC SE 0.97). stud runs on all nest cells and, for reps, all
g = 10 and 20 cells plus 12 cells at g = 50 (k = 3, missing data, all CCC x
difference x distribution).

## Pass criteria (set before running)

- coverage of the 95% interval within **92.5-97.5%** (Bradley's liberal
  criterion);
- each tail's miss rate within **1.25-3.75%**; report which side misses
  fall on;
- failures (no interval) **< 1%** of data sets;
- also reported: median width, bias of the estimate, MC SE.

Decision rule: an opt-in bootstrap is worth adding only if it passes where
jk fails (mainly g = 10) and does not fail where jk passes. If no interval
passes at g = 10, document the small-g limitation instead.

## Checks done

- `verify_fast_vc.R`: the C++ REML in `fast_vc.R` / `vc_dev.cpp` matches
  `.ccc_vc()` (lme4) on 200 random data sets (full, jackknife and bootstrap
  samples): median |difference| 1e-7, max 5e-5 (lme4's optimizer
  tolerance). 0.2 ms per fit vs 22 ms for lme4. Optimization is BOBYQA on
  theta with lower bound 0 from (1, 1), as lme4; nlminb stopped early or
  stuck at theta_2 = 0, so do not swap it in.
- `check_jk.R`: the simulated jk interval reproduces `ccc_test()` on 64
  data sets per type (nest exact; reps within 1.6e-4).
- Smoke test of `sim_ci.R` -> `summarise.R` end to end on tiny runs.

## How to run

Needs R with Rtools (for `Rcpp::sourceCpp()`), Rcpp, minqa, lme4, tidyr,
devtools (checks only). Run from the package root; results (one RDS per
cell) go to `CCC_SIMS_OUT`. The scripts use `detectCores() - 2` workers.

```sh
export CCC_SIMS_OUT=/path/to/ccc_out
Rscript references/general/ccc_sims/verify_fast_vc.R    # re-check on the new machine
Rscript references/general/ccc_sims/check_jk.R
Rscript references/general/ccc_sims/sim_ci.R nest main  # ~15 min
Rscript references/general/ccc_sims/sim_ci.R nest stud
Rscript references/general/ccc_sims/sim_ci.R reps main  # ~2.6 h
Rscript references/general/ccc_sims/sim_ci.R reps stud  # ~9.6 h
Rscript references/general/ccc_sims/summarise.R         # writes summary.csv, prints tables
```

Times are from a 12-worker pilot (0.8 ms per fit per worker under load):
reps per data set ~0.8 s without stud; with stud ~3.5 s (g = 10), ~6 s
(g = 20), ~12 s (g = 50). reps total ~12 h; keep the machine awake. To
roughly halve the stud time, set `Bs <- 299` in `sim_ci.R` (more MC noise in
the t* quantiles, no bias). Seeds are fixed per cell and data set, so a
stage can be re-run alone; reps main skips seeds 1-500 of the stud cells
because the stud stage also returns jk/perc/bca for them.

## Files

| File | Role |
|---|---|
| `coverage.R` | earlier one-scenario check (normal vs t quantiles) |
| `gen.R` | data generators and true CCC |
| `fast_vc.R`, `vc_dev.cpp` | fast REML variance-components CCC (sim only) |
| `sim_funs.R` | one data set -> all four intervals |
| `sim_ci.R` | grid and parallel driver |
| `summarise.R` | criteria table, `summary.csv` |
| `verify_fast_vc.R`, `check_jk.R` | fidelity checks |

## After the runs

1. Report to the user, for each type: share of cells passing by g and
   method, coverage and tail misses by each factor, the method-difference =
   0 cells, the failing cells, widths and failure rates.
2. Record the tables and the user's decision here.
3. Only if the user approves an interval: add `boot = FALSE`,
   `replicates`, `boot_ci` to `ccc_test()` mirroring `icc_test()`
   (`R/icc_test.R`), reusing `.reli_boot()`, `.bca_params()`,
   `.boot_interval()` and `.boot_pvalue()` from `R/reli_engine.R`
   (`stud` there needs a per-resample `se_z`: the inner jackknife, g lmer
   fits per resample). Tests: cheap `replicates` on CRAN, property checks
   `skip_on_cran()`, long simulations behind `SIMPLYAGREE_SLOW_TESTS=true`;
   roxygen docs; NEWS.

## Results

Not yet run.
