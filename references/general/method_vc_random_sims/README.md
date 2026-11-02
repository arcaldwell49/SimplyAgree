# method_vc(methods = "random"): coverage of the limits for random raters

Status (2026-11-01): **run**. Limits: criterion met after switching the bounds to the MOVER over the components. Rater SD interval: criterion not met for a small rater SD (see Results).

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
2. Does the chi-square interval for the rater SD (M - 1 df) cover?
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

1000 data sets per cell. The true limits are ±1.96 sqrt(V) with V as above
(the mean of the method-specific variances for cells 15-16).

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
Both columns come from the same data sets.

| cell | rater SD | raters | items | replicates | sigma | fits with bounds | bound coverage (MOVER) | (Satterthwaite) | rater SD CI | rater SD = 0 | sd_true | mean sd |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 0.2 | 3 | 15 | linked | common | 913 | 0.997 | 0.958 | 0.793 | 0.117 | 0.762 | 0.760 |
| 2 | 0.5 | 3 | 15 | linked | common | 965 | 0.973 | 0.804 | 0.928 | 0.020 | 1.000 | 0.986 |
| 3 | 0.2 | 5 | 15 | linked | common | 946 | 0.998 | 0.933 | 0.858 | 0.024 | 0.762 | 0.759 |
| 4 | 0.5 | 5 | 15 | linked | common | 996 | 0.953 | 0.827 | 0.935 | 0.000 | 1.000 | 0.991 |
| 5 | 0.2 | 10 | 15 | linked | common | 992 | 0.992 | 0.936 | 0.851 | 0.002 | 0.762 | 0.761 |
| 6 | 0.5 | 10 | 15 | linked | common | 1000 | 0.943 | 0.869 | 0.930 | 0.000 | 1.000 | 0.995 |
| 7 | 0.2 | 3 | 30 | linked | common | 896 | 0.997 | 0.915 | 0.911 | 0.029 | 0.762 | 0.763 |
| 8 | 0.5 | 3 | 30 | linked | common | 977 | 0.939 | 0.740 | 0.931 | 0.007 | 1.000 | 0.977 |
| 9 | 0.2 | 5 | 30 | linked | common | 990 | 0.992 | 0.917 | 0.904 | 0.012 | 0.762 | 0.762 |
| 10 | 0.5 | 5 | 30 | linked | common | 1000 | 0.956 | 0.850 | 0.947 | 0.001 | 1.000 | 0.986 |
| 11 | 0.2 | 10 | 30 | linked | common | 999 | 0.978 | 0.901 | 0.912 | 0.000 | 0.762 | 0.759 |
| 12 | 0.5 | 10 | 30 | linked | common | 1000 | 0.948 | 0.855 | 0.949 | 0.000 | 1.000 | 0.985 |
| 13 | 0.2 | 5 | 30 | exchangeable | common | 989 | 0.980 | 0.889 | 0.896 | 0.011 | 0.762 | 0.758 |
| 14 | 0.5 | 5 | 30 | exchangeable | common | 1000 | 0.942 | 0.816 | 0.940 | 0.000 | 1.000 | 0.986 |
| 15 | 0.2 | 5 | 30 | linked | by method | 988 | 0.996 | 0.920 | 0.889 | 0.016 | 0.829 | 0.827 |
| 16 | 0.5 | 5 | 30 | linked | by method | 999 | 0.941 | 0.809 | 0.932 | 0.002 | 1.052 | 1.036 |

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
2. **Rater SD interval: criterion not met for a small rater SD.** With
   rater SD 0.5 the chi-square interval on M - 1 df covers 0.93 to 0.95;
   with rater SD 0.2 (smaller than tau and sigma) it covers 0.85 to 0.91
   with 5 or 10 raters and 0.79 with 3 raters and 15 items. The M - 1 df
   ignore the noise from the item-level components, which matters when the
   rater variance is small. This affects the reported rater CI only, not
   the limits. Possible fix: Satterthwaite df for the rater variance from
   the method mean square (as for the between-rater variance in an ANOVA).
3. **Fits without bounds.** When the rater SD is estimated at zero the
   approximate covariance of the variance parameters is not available and
   no bounds are reported (the existing behaviour for boundary fits): up to
   10% of data sets with 3 raters and a small rater SD, 0.4% to 5% with 5
   raters, and at most 1% with 10. The coverage above is among the fits
   with bounds.
4. The SD of the differences is nearly unbiased (mean 0.977 to 0.995 of
   the true value, slightly low with 3 raters).
