# method_vc(methods = "random"): coverage of the limits for random raters

Status (2026-11-01; `tdi()` columns, the MOVER rater SD interval and cells 17-18 added 2026-11-02): **run**. Limits: criterion met after switching the bounds to the MOVER over the components. `tdi()` GPQ bound: criterion met. Rater SD interval: lower side of the criterion met after switching to a MOVER interval (0.94 to 0.98), slightly conservative in six cells (see Results).

## Question

With `methods = "random"`, `method_vc()` reports one row of limits of
agreement for two randomly chosen raters, ±z sqrt(V) with
V = 2 (sigma_rater^2 + tau^2 + mean sigma_m^2), and MOVER bounds in which
the rater variance has a chi-square distribution on M - 1 df (M raters)
and the other components their delta-method df (first run: combined by
Satterthwaite; now: by the MOVER over the components, see Results).

1. Do the one-sided ("iu") 95% bounds of the limits cover the true limits
   at about the nominal rate, and how does that depend on the number of
   raters and the share of the rater variance?
2. Does the interval for the rater SD cover (first: chi-square on M - 1 df;
   now: MOVER, see Results)?
3. How often is the rater SD estimated at zero?

Criteria (set before running):

- One-sided coverage of each bound at least 0.93 in every cell with
  M >= 5; report M = 3 (2 df) separately, where the approximation is
  expected to be rough.
- Rater SD interval coverage between 0.93 and 0.97 with M >= 5.
- If the bounds are clearly liberal with few raters, consider a
  parametric bootstrap (the planned option) before release.

## Design

`sim_method_vc_random.R`. Data from `sim_methods(replicates = "linked")`
(or exchangeable) with new rater effects alpha_m ~ N(0, rater^2) drawn for
every data set, tau = 0.3, omega = 0.3 (linked), sigma = 0.4 for every
method (fitted with `sigma = "common"`), 2 replicates.

| cells | rater SD | raters M | items | replicates | sigma |
|---|---|---|---|---|---|
| 1-12 | 0.2, 0.5 | 3, 5, 10 | 15, 30 | linked | common |
| 13-14 | 0.2, 0.5 | 5 | 30 | exchangeable | common |
| 15-16 | 0.2, 0.5 | 5 | 30 | linked | 0.3 to 0.6 by method, `sigma = "method"` |
| 17-18 | 0.1, 0.05 | 5 | 30 | linked | common (added for the rater SD interval) |

1000 data sets per cell. The true limits are ±1.96 sqrt(V) with V as above
(the mean of the method-specific variances for cells 15-16).
The GPQ `tdi()` columns, the MOVER rater SD interval and cells 17-18 were
added on 2026-11-02; the reruns reproduced every other column of cells 1-16
exactly (same seeds).

## How to run

From the package root on branch `MethComp`:

```
Rscript references/general/method_vc_random_sims/sim_method_vc_random.R
```

Environment variables: `MVR_NSIM`, `MVR_CORES` (default: all cores but 2),
`MVR_CELLS`. Finished cells are saved to `raw/` (not committed) and skipped
on a restart; `summary.csv` is rebuilt at the end.

## Results

Run on 2026-11-01 (R 4.4.1, Windows, 14 workers; under a minute per cell).
"Bound coverage" is the proportion of data sets whose one-sided 95% upper
bound for the upper limit is at least the true upper limit, among the fits
with bounds; the lower bound is its mirror image (the bias is 0), so its
coverage is the same. The first run used one chi-square on the
Satterthwaite df of V (column "Satterthwaite"); the bounds now use the
MOVER over the components, each with its own df (rater variance M - 1).
Both columns come from the same data sets. Likewise for the two-sided 95%
rater SD interval: "rater SD CI (MOVER)" is the current interval,
"(chi-square)" the first one, on M - 1 df (not run for cells 17-18).

| cell | rater SD | raters | items | replicates | sigma | fits with bounds | bound coverage (MOVER) | (Satterthwaite) | rater SD CI (MOVER) | (chi-square) | rater SD = 0 | sd_true | mean sd |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 0.2 | 3 | 15 | linked | common | 913 | 0.997 | 0.958 | 0.974 | 0.793 | 0.117 | 0.762 | 0.760 |
| 2 | 0.5 | 3 | 15 | linked | common | 965 | 0.973 | 0.804 | 0.975 | 0.928 | 0.020 | 1.000 | 0.986 |
| 3 | 0.2 | 5 | 15 | linked | common | 946 | 0.998 | 0.933 | 0.974 | 0.858 | 0.024 | 0.762 | 0.759 |
| 4 | 0.5 | 5 | 15 | linked | common | 996 | 0.953 | 0.827 | 0.956 | 0.935 | 0.000 | 1.000 | 0.991 |
| 5 | 0.2 | 10 | 15 | linked | common | 992 | 0.992 | 0.936 | 0.953 | 0.851 | 0.002 | 0.762 | 0.761 |
| 6 | 0.5 | 10 | 15 | linked | common | 1000 | 0.943 | 0.869 | 0.945 | 0.930 | 0.000 | 1.000 | 0.995 |
| 7 | 0.2 | 3 | 30 | linked | common | 896 | 0.997 | 0.915 | 0.973 | 0.911 | 0.029 | 0.762 | 0.763 |
| 8 | 0.5 | 3 | 30 | linked | common | 977 | 0.939 | 0.740 | 0.962 | 0.931 | 0.007 | 1.000 | 0.977 |
| 9 | 0.2 | 5 | 30 | linked | common | 990 | 0.992 | 0.917 | 0.966 | 0.904 | 0.012 | 0.762 | 0.762 |
| 10 | 0.5 | 5 | 30 | linked | common | 1000 | 0.956 | 0.850 | 0.957 | 0.947 | 0.001 | 1.000 | 0.986 |
| 11 | 0.2 | 10 | 30 | linked | common | 999 | 0.978 | 0.901 | 0.956 | 0.912 | 0.000 | 0.762 | 0.759 |
| 12 | 0.5 | 10 | 30 | linked | common | 1000 | 0.948 | 0.855 | 0.954 | 0.949 | 0.000 | 1.000 | 0.985 |
| 13 | 0.2 | 5 | 30 | exchangeable | common | 989 | 0.980 | 0.889 | 0.974 | 0.896 | 0.011 | 0.762 | 0.758 |
| 14 | 0.5 | 5 | 30 | exchangeable | common | 1000 | 0.942 | 0.816 | 0.949 | 0.940 | 0.000 | 1.000 | 0.986 |
| 15 | 0.2 | 5 | 30 | linked | by method | 988 | 0.996 | 0.920 | 0.970 | 0.889 | 0.016 | 0.829 | 0.827 |
| 16 | 0.5 | 5 | 30 | linked | by method | 999 | 0.941 | 0.809 | 0.941 | 0.932 | 0.002 | 1.052 | 1.036 |
| 17 | 0.1 | 5 | 30 | linked | common | 932 | 0.998 | | 0.971 | | 0.094 | 0.721 | 0.722 |
| 18 | 0.05 | 5 | 30 | linked | common | 826 | 0.996 | | 0.955 | | 0.231 | 0.711 | 0.711 |

1. **Bounds of the limits: criterion met (after the change).** With the
   Satterthwaite chi-square the bounds were liberal whenever the rater
   variance was large (0.74 to 0.87 with rater SD 0.5, even with 10
   raters): one chi-square on Satterthwaite df understates the upper tail
   when one component with few df dominates. The MOVER over the components
   gives 0.94 to 0.97 with rater SD 0.5 and 0.98 to 1.00 (conservative)
   with rater SD 0.2, in every cell including 3 raters, exchangeable
   replicates and method-specific residual SDs. `method_vc()` now uses it
   for random raters (`mc_limits(sd_df = "components")`); fixed methods
   keep the Satterthwaite chi-square.
2. **Rater SD interval: lower side of the criterion met after the change;
   slightly conservative.** The first interval, a chi-square on M - 1 df,
   covered 0.93 to 0.95 with rater SD 0.5 but only 0.85 to 0.91 with rater
   SD 0.2 and 5 or 10 raters (0.79 with 3 raters and 15 items): the M - 1
   df ignore the noise from the item-level components, which matters when
   the rater variance is small. A prototype (same data sets for cells 1-12,
   plus rater SD 0.1 and 0.05) compared Satterthwaite df for the rater
   variance (0.81 to 0.98, but 0.85 and 0.58 for rater SD 0.1 and 0.05)
   with a MOVER for v = S2 - c, where S2 = v + c is the variance of the
   method means on M - 1 df and c = (mean tau^2 + mean sigma^2 / r) / n
   its part from the items, on its delta-method df (as for a between-group
   variance in a one-way ANOVA). `method_vc()` now uses the MOVER
   (`mc_vc_rater_ci()`): 0.94 to 0.98 in every cell, including 3 raters,
   rater SD 0.05, exchangeable replicates and method-specific residual SDs.
   Six cells (1, 2, 3, 7, 13, 17) are above the 0.97 of the criterion
   (0.971 to 0.975), all with 3 raters or a rater SD of at most 0.2; the
   interval is then wide anyway. The rater variance keeps M - 1 df in the
   bounds of the limits and `tdi()`, so items 1 and the `tdi()` section are
   unchanged.
3. **Fits without bounds.** When the rater SD is estimated at zero the
   approximate covariance of the variance parameters is often not available
   and no bounds are reported (the existing behaviour for boundary fits): up
   to 10% of data sets with 3 raters and rater SD 0.2, 0.4% to 5% with 5
   raters and rater SD 0.2 or 0.5, 7% and 17% with rater SD 0.1 and 0.05,
   and at most 1% with 10 raters. The coverage above is among the fits with
   bounds (the column "rater SD = 0" is among those too).
4. The SD of the differences is nearly unbiased (mean 0.977 to 0.995 of
   the true value, slightly low with 3 raters).

### `tdi()` GPQ bound (added 2026-11-02)

After `tdi()` was changed to draw each variance component on its own df
for random raters, the same data sets were rerun (the columns above
reproduced exactly) with the default GPQ bound (`B = 2000`) for p = 0.95
and p = 0.8; the true TDI is qnorm((1 + p) / 2) times the true SD, since the
bias is 0. Coverage among the fits with bounds (`cover_tdi95`,
`cover_tdi80` in `summary.csv`):

| cell | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| GPQ | 0.998 | 0.979 | 0.998 | 0.961 | 0.993 | 0.946 | 0.997 | 0.950 | 0.994 | 0.959 | 0.980 | 0.948 | 0.983 | 0.943 | 0.999 | 0.950 | 0.998 | 0.998 |

**Criterion (at least 0.93) met in every cell**: 0.94 to 0.98 with rater
SD 0.5 and 0.98 to 1.00 with rater SD 0.2 or less, slightly above the MOVER bounds
of the limits. The two values of p give identical coverage: with bias 0 the
TDI and its GPQ draws are the SD times a constant, and the draws are the
same for a given seed. `method = "tolerance"` equals the MOVER bound of the
limits at p = 0.95 (tested), so its coverage is the column above.
