# ccc_test() small-sample bias correction: simulation plan

Status (2026-10-20): **runs complete; results below, decision pending.**
Follows `../ccc_sims/` (same grid, generators and seeds, so every data set
is one of the interval study's) and the convention of
`../reli_redesign.md`: criteria are set before running and the user
decides from the evidence.

## Question

With few subjects the replicate (variance-components) and nested (Lin) CCC
are biased downward: mean bias -0.040 (reps) and -0.033 (nest) at g = 10,
-0.020 / -0.017 at g = 20, -0.008 / -0.006 at g = 50 (`../ccc_sims/`).
The jackknife-t interval misses mostly above (4-10% vs ~1.4% below).
Should `ccc_test()` report a bias-corrected estimate and/or centre the
interval on it?

## Candidates

Estimates (from the delete-one-subject jackknife `ccc_test()` already
computes, so no extra fits):

1. **raw**: the current estimate.
2. **jk_r**: jackknife bias correction on the CCC scale,
   g * r - (g - 1) * mean(r_loo), clamped to [-1, 1].
3. **jk_z**: the same on Fisher's z scale, tanh(g * z - (g - 1) *
   mean(z_loo)) (stays in (-1, 1); the scale the interval uses).
4. **nest only, n1**: Lin's estimator with n - 1 variances and covariance
   (the ICC-like form; Nickerson 1997). Changes only the mean-difference
   term, so expected to matter little; included because it is free.

Intervals (95%):

- **jk**: the current one, z +/- t(g - 1) * SE_jk.
- **jk_bc**: the same width centred on the jk_z estimate,
  z_bc +/- t(g - 1) * SE_jk.
- **stud**: the studentized bootstrap from `../ccc_sims/` (same data sets),
  for reference.

## Grid and size

As `../ccc_sims/`: 216 cells per type (g 10/20/50, k 2/3/6, CCC
.50/.80/.95, method difference none/present, balanced/15% missing,
normal/non-normal), 1000 data sets per cell, same seeds. The reps CCC
uses the C++ REML in `../ccc_sims/fast_vc.R`. Expected run time: a few
minutes for each type.

## Criteria (set before running)

Estimates, per cell:
- an estimator is an improvement if it cuts mean |bias| by at least half
  at g = 10 and g = 20, averaged over cells, **and** its RMSE is no more
  than 10% above raw's in any g = 10 cell (a bias correction that adds
  more variance than it removes is not worth it);
- report bias, RMSE and median absolute error by g, distribution and true
  CCC; and how often jk_r is clamped at 1.

Intervals: the `../ccc_sims/` criteria (coverage 92.5-97.5%, each tail
1.25-3.75%, failures < 1%). jk_bc is worth adopting only if it passes in
more cells than jk at g = 10 and 20, does not fail where jk passes at
g = 50, and narrows the tail imbalance.

Decision options for the user:
- report a bias-corrected estimate (in addition to, or instead of, raw);
- centre the default interval on it (jk_bc), possibly only for reps;
- or neither, and document the bias.

## How to run

From the package root, with `CCC_BIAS_OUT` set (default `tempdir()`):

```sh
Rscript references/general/ccc_bias/sim_bias.R nest
Rscript references/general/ccc_bias/sim_bias.R reps
Rscript references/general/ccc_bias/summarise_bias.R   # writes summary_bias.csv
```

## Results

Run 2026-10-20 (14 workers): nest 18 s, reps 6.6 min. Per-cell results in
`summary_bias.csv`; raw RDS in `references/working/ccc_bias_out/`.

Mean bias and RMSE by g (averaged over cells):

| type | g | bias raw | bias jk_r | bias jk_z | RMSE raw | RMSE jk_r | RMSE jk_z | jk_r clamped |
|---|---|---|---|---|---|---|---|---|
| nest | 10 | -.033 | -.001 | -.017 | .105 | .106 | .105 | 1.8% |
| nest | 20 | -.017 | -.000 | -.008 | .071 | .072 | .072 | 0.3% |
| nest | 50 | -.006 | .000 | -.003 | .044 | .045 | .045 | 0.0% |
| reps | 10 | -.040 | -.002 | -.025 | .134 | .141 | .138 | 5.3% |
| reps | 20 | -.020 | -.000 | -.012 | .088 | .092 | .090 | 1.3% |
| reps | 50 | -.008 | -.000 | -.005 | .054 | .055 | .054 | 0.1% |

Interval coverage (and misses above / below) by g:

| type | g | jk | jk_bc |
|---|---|---|---|
| nest | 10 | .917 (7.0 / 1.4) | .919 (5.9 / 2.2) |
| nest | 20 | .925 (6.2 / 1.4) | .926 (5.3 / 2.0) |
| nest | 50 | .935 (4.8 / 1.6) | .936 (4.3 / 2.2) |
| reps | 10 | .904 (8.1 / 1.5) | .903 (7.6 / 2.1) |
| reps | 20 | .918 (6.6 / 1.6) | .917 (6.3 / 2.1) |
| reps | 50 | .930 (5.3 / 1.7) | .929 (5.1 / 2.1) |

Findings:

- **jk_r removes almost all of the bias** (82-88% at g = 10 and 20) but adds
  variance: RMSE is about equal with normal data (slightly lower at
  g >= 20) and higher with non-normal data (reps g = 10: .165 vs .153);
  the worst g = 10 cell has RMSE 28% (reps) and 11% (nest) above raw. It
  exceeds 1 and is clamped in 5% of reps data sets at g = 10 (9% with
  non-normal data). **Fails the RMSE criterion.**
- **jk_z** removes only 37-50% of the bias (the z-scale correction
  shrinks after tanh) and also raises the worst-cell RMSE (21% reps, 17%
  nest). **Fails both criteria.**
- **n1** (nested, n - 1 form) changes the bias by < 0.001: not worth it.
- **jk_bc interval**: coverage unchanged (within 0.2 points of jk at
  every g); it moves about 1 point of misses from above to below, so the
  tails are better balanced (full pass at reps g = 10: 11% of cells vs 3%),
  but coverage-passing cells are no more frequent (reps g = 10: 29% vs
  31%). **Fails the interval criterion.** The under-coverage with
  non-normal data comes from the SE and skewness, not the centre; the
  studentized bootstrap (which adjusts both) is what fixes it for reps.

Against the decision options: no candidate meets the pre-set criteria.
Recommendation: keep the raw estimate and the jk interval as defaults,
keep documenting the downward bias, and point to `boot = TRUE` for
replicate data with few subjects.

Follow-up checks (same data sets): the jk_r estimate is closer to the
truth than raw in only 43-50% of data sets (reps g = 10 normal: 43%), and
it falls outside the jk interval in 10.6% of reps g = 10 non-normal data
sets (1.8% normal), almost all from clamping at 1.

Decision (user, 2026-10-20): keep the raw estimate, interval and test.
Keep the jackknife bias as extra output in the saved object
(`$jackknife`: `bias`, `estimate_bc` = jk_r clamped to [-1, 1],
`clamped`; reps/nest only, `NULL` for simple data, not printed).
`summary()` mentions it when |bias| > 0.01 or > 10% of the interval
width, says it is a guide that removes average bias but is not more
accurate for one study (useful for pooling), notes truncation, and for
reps without the bootstrap suggests `boot = TRUE`.
