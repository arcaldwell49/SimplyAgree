# ccc_test() intervals for replicate and nested data: simulation plan

Status (2026-10-20): **runs complete; results below, decision pending.** Decisions follow the reliability redesign
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

Run 2026-10-19/20 on a 16-core Windows machine (14 workers): nest main 1
min, nest stud 2.4 min, reps main 1.9 h, reps stud 6.9 h. Fidelity checks
re-run first (fast REML vs lme4 max 5e-5; jk vs `ccc_test()` max 1.6e-4
reps, exact nest). Per-cell results in `summary.csv`; raw RDS in
`references/working/ccc_out/` (not tracked). Below, "cov pass" = share of
cells with coverage in 92.5-97.5% and failures < 1%; "full pass" also needs
both tails in 1.25-3.75%. Note stud has 500 sims per cell (MC SE ~1 point),
so cells near 92.5% flip on noise.

Mean coverage (cov pass / full pass) by g:

| type | g | jk | perc | bca | stud |
|---|---|---|---|---|---|
| nest | 10 | .917 (44% / 1%) | .834 (0 / 0) | .849 (0 / 0) | .923 (44% / 18%) |
| nest | 20 | .925 (63% / 3%) | .879 (0 / 0) | .885 (1% / 0) | .928 (68% / 17%) |
| nest | 50 | .935 (74% / 31%) | .913 (38% / 1%) | .912 (36% / 24%) | .935 (75% / 47%) |
| reps | 10 | .904 (31% / 3%) | .801 (0 / 0) | .824 (0 / 0) | .941 (85% / 44%) |
| reps | 20 | .918 (50% / 15%) | .860 (0 / 0) | .874 (0 / 0) | .935 (67% / 32%) |
| reps | 50 | .930 (56% / 35%) | .902 (19% / 1%) | .907 (25% / 11%) | .940 (92% / 50%)* |

\* reps stud at g = 50 covers only the 12 k = 3, missing-data cells.

Coverage by distribution (normal / non-normal):

| type | g | jk | stud |
|---|---|---|---|
| nest | 10 | .931 / .903 | .933 / .913 |
| nest | 20 | .937 / .913 | .934 / .923 |
| nest | 50 | .946 / .925 | .943 / .928 |
| reps | 10 | .928 / .881 | .945 / .937 |
| reps | 20 | .941 / .895 | .943 / .926 |
| reps | 50 | .946 / .914 | .948 / .931 |

Findings:

- **Percentile and BCa fail almost everywhere** and are worse than jk in
  every slice (reps g = 10 percentile .80, 19% of misses above). The
  subject bootstrap distribution of the CCC sits low (it adds the estimator's
  downward bias a second time) and is too narrow; BCa misses on both sides.
  Ruled out.
- **jk**: fine with normal data at g >= 20 (.94); g = 10 normal is borderline
  (.93 nest, .93 reps). With non-normal data it fails at every g (reps .88-.91,
  nest .90-.93). Misses are lopsided: 4-10% above vs ~1.4% below, so it
  almost never meets the tail criterion. The estimate is biased low (mean
  bias -.040 reps / -.033 nest at g = 10; -.008 / -.006 at g = 50).
- **stud**: for **reps** it fixes most of what jk gets wrong: +5.7 points at
  g = 10 non-normal, +3.1 at g = 20, and cov pass in 85% of g = 10 cells vs
  31% for jk. Tails are more balanced. Median width is within 2% of jk (it
  moves the interval rather than widening it). For **nest** the gain is
  small (+1 point non-normal, about 0 normal) and non-normal coverage stays
  below 92.5% at g <= 20.
- The cells where jk passes and stud fails (24 of 288 shared cells) have
  stud coverage .910-.924, within ~1.5 MC SE of the bound.
- Worst stud cells are all non-normal, mostly k = 6 (.87-.89, misses above):
  more replicates do not help when subject effects are skewed and g is small.
- Failures: none for jk/perc/stud; BCa up to 2.5% in one reps cell (z0
  infinite).
- The method-difference factor makes no material difference for any
  interval (reps jk .918 vs .917; stud .939 vs .938), so the s2_method = 0
  truncation is not a problem for coverage.

Against the decision rule: stud passes where jk fails mainly for reps (g =
10 and 20, especially non-normal) and does not clearly fail where jk passes.
For nest no interval is a clear improvement.

Cost if added to the package: stud needs an inner jackknife per resample
(g fits x Bs resamples). With lme4 at ~20 ms per fit that is ~100 s at
g = 10 and ~200 s at g = 20 with Bs = 499. The package has no compiled code;
the pure-R REML in `fast_vc.R` (`.vc_dev`, no Rcpp) would avoid lme4 for the
reps CCC.

Decision (user, 2026-10-20): add an opt-in subject bootstrap to
`ccc_test()` for `data_type = "reps"` only, following `icc_test()`
(`boot = FALSE`, `boot_ci = "stud"` default with `"perc"`/`"bca"` for
comparison, `replicates = 999`), documented as a good idea for replicate
data with few subjects. Nested data keeps the jackknife-t with the
limitation documented. Implemented in `R/ccc_boot.R`: a pure-R port of
`fast_vc.R` (`.ccc_vc_fit()`, BOBYQA via minqa, rhoend 1e-7, inner
jackknife warm-started at the replicate's theta; ~3.5-5.5 ms per fit, so
~20 ms per studentized replicate at g = 10). Next: a bias-correction study
for the small-g downward bias (see `../ccc_bias/` once started).
