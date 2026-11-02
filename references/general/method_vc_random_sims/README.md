# method_vc(methods = "random"): coverage of the limits for random raters

Status (2026-11-01; updated 2026-11-02): **run**. Bounds of the limits:
criterion met (MOVER over the components) in every cell with M >= 5; with 3
raters one cell is at 0.923. `tdi()` GPQ bound: criterion met with M >= 5.
Rater SD interval (MOVER): 0.94 to 0.98, lower side of the criterion met,
slightly conservative in six cells. Fits with a variance component at zero
now get bounds from a refit without it: 998 to 1000 data sets per cell have
bounds (was 826 to 1000).

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
3. How often is the rater SD estimated at zero, and what happens then?

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

1000 data sets per cell, the same seeds in every run. The true limits are
±1.96 sqrt(V) with V as above (the mean of the method-specific variances
for cells 15-16); the true TDI is qnorm((1 + p) / 2) sqrt(V), since the
bias is 0.

## How to run

From the package root on branch `MethComp`:

```
Rscript references/general/method_vc_random_sims/sim_method_vc_random.R
```

Environment variables: `MVR_NSIM`, `MVR_CORES` (default: all cores but 2),
`MVR_CELLS`. Finished cells are saved to `raw/` (not committed) and skipped
on a restart; `summary.csv` is rebuilt at the end.

## Results

Runs on 2026-11-01 and 2026-11-02 (R 4.4.1, Windows, 14 workers; one to
two minutes per cell). The table is the current version of `method_vc()`
(`summary.csv`); columns in parentheses are earlier versions on the same
data sets:

- "before refit": before fits with a variance component at zero were refit
  without it (those fits had no bounds and were left out);
- "Satterthwaite": bounds from one chi-square on the Satterthwaite df of V
  (first run, before refit);
- "chi-square": rater SD interval from a chi-square on M - 1 df (before
  refit; not run for cells 17-18).

"Bound coverage" is the proportion of data sets whose one-sided 95% upper
bound for the upper limit is at least the true upper limit, among the fits
with bounds; the lower bound is its mirror image (the bias is 0), so its
coverage is the same. "TDI" is the default GPQ bound of `tdi()` (`B =
2000`, p = 0.95; p = 0.8 gives identical coverage, see 5). The rater SD
interval is two-sided 95%. "rater SD = 0" is the share of fits whose rater
SD is below 1e-3 times sigma.

| cell | rater SD | raters | items | replicates | sigma | fits with bounds (before refit) | bound coverage (before refit) | (Satterthwaite) | rater SD CI (before refit) | (chi-square) | TDI (before refit) | rater SD = 0 | sd_true | mean sd |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 0.2 | 3 | 15 | linked | common | 1000 (913) | 0.993 (0.997) | (0.958) | 0.976 (0.974) | (0.793) | 0.993 (0.998) | 0.194 | 0.762 | 0.756 |
| 2 | 0.5 | 3 | 15 | linked | common | 999 (965) | 0.969 (0.973) | (0.804) | 0.975 (0.975) | (0.928) | 0.974 (0.979) | 0.037 | 1.000 | 0.982 |
| 3 | 0.2 | 5 | 15 | linked | common | 1000 (946) | 0.997 (0.998) | (0.933) | 0.975 (0.974) | (0.858) | 0.998 (0.998) | 0.076 | 0.762 | 0.757 |
| 4 | 0.5 | 5 | 15 | linked | common | 1000 (996) | 0.951 (0.953) | (0.827) | 0.954 (0.956) | (0.935) | 0.959 (0.961) | 0.001 | 1.000 | 0.990 |
| 5 | 0.2 | 10 | 15 | linked | common | 1000 (992) | 0.991 (0.992) | (0.936) | 0.945 (0.953) | (0.851) | 0.992 (0.993) | 0.010 | 0.762 | 0.761 |
| 6 | 0.5 | 10 | 15 | linked | common | 1000 (1000) | 0.943 (0.943) | (0.869) | 0.945 (0.945) | (0.930) | 0.946 (0.946) | 0.000 | 1.000 | 0.995 |
| 7 | 0.2 | 3 | 30 | linked | common | 998 (896) | 0.994 (0.997) | (0.915) | 0.976 (0.973) | (0.911) | 0.995 (0.997) | 0.128 | 0.762 | 0.757 |
| 8 | 0.5 | 3 | 30 | linked | common | 998 (977) | 0.923 (0.939) | (0.740) | 0.945 (0.962) | (0.931) | 0.934 (0.950) | 0.029 | 1.000 | 0.972 |
| 9 | 0.2 | 5 | 30 | linked | common | 1000 (990) | 0.991 (0.992) | (0.917) | 0.962 (0.966) | (0.904) | 0.993 (0.994) | 0.024 | 0.762 | 0.761 |
| 10 | 0.5 | 5 | 30 | linked | common | 1000 (1000) | 0.956 (0.956) | (0.850) | 0.957 (0.957) | (0.947) | 0.959 (0.959) | 0.001 | 1.000 | 0.986 |
| 11 | 0.2 | 10 | 30 | linked | common | 1000 (999) | 0.978 (0.978) | (0.901) | 0.955 (0.956) | (0.912) | 0.980 (0.980) | 0.001 | 0.762 | 0.759 |
| 12 | 0.5 | 10 | 30 | linked | common | 1000 (1000) | 0.948 (0.948) | (0.855) | 0.954 (0.954) | (0.949) | 0.948 (0.948) | 0.000 | 1.000 | 0.985 |
| 13 | 0.2 | 5 | 30 | exchangeable | common | 1000 (989) | 0.978 (0.980) | (0.889) | 0.967 (0.974) | (0.896) | 0.982 (0.983) | 0.025 | 0.762 | 0.757 |
| 14 | 0.5 | 5 | 30 | exchangeable | common | 1000 (1000) | 0.942 (0.942) | (0.816) | 0.949 (0.949) | (0.940) | 0.943 (0.943) | 0.000 | 1.000 | 0.986 |
| 15 | 0.2 | 5 | 30 | linked | by method | 1000 (988) | 0.996 (0.996) | (0.920) | 0.970 (0.970) | (0.889) | 0.999 (0.999) | 0.034 | 0.829 | 0.826 |
| 16 | 0.5 | 5 | 30 | linked | by method | 1000 (999) | 0.940 (0.941) | (0.809) | 0.940 (0.941) | (0.932) | 0.949 (0.950) | 0.004 | 1.052 | 1.036 |
| 17 | 0.1 | 5 | 30 | linked | common | 1000 (932) | 0.998 (0.998) | | 0.973 (0.971) | | 0.999 (0.998) | 0.170 | 0.721 | 0.721 |
| 18 | 0.05 | 5 | 30 | linked | common | 999 (826) | 0.995 (0.996) | | 0.963 (0.955) | | 0.996 (0.998) | 0.393 | 0.711 | 0.710 |

1. **Bounds of the limits: criterion met with M >= 5.** With the
   Satterthwaite chi-square the bounds were liberal whenever the rater
   variance was large (0.74 to 0.87 with rater SD 0.5, even with 10
   raters): one chi-square on Satterthwaite df understates the upper tail
   when one component with few df dominates. The MOVER over the components
   gives 0.94 to 0.96 with rater SD 0.5 and 0.98 to 1.00 (conservative)
   with rater SD 0.2 or less, with 5 or 10 raters, exchangeable replicates
   and method-specific residual SDs. `method_vc()` uses it for random
   raters (`mc_limits(sd_df = "components")`); fixed methods keep the
   Satterthwaite chi-square. With 3 raters: 0.97 to 0.99, except cell 8
   (rater SD 0.5, 30 items) at 0.923 (see 3).
2. **Rater SD interval: lower side of the criterion met; slightly
   conservative.** The first interval, a chi-square on M - 1 df, covered
   0.93 to 0.95 with rater SD 0.5 but only 0.85 to 0.91 with rater SD 0.2
   and 5 or 10 raters (0.79 with 3 raters and 15 items): the M - 1 df
   ignore the noise from the item-level components, which matters when the
   rater variance is small. A prototype (same data sets for cells 1-12,
   plus rater SD 0.1 and 0.05) compared Satterthwaite df for the rater
   variance (0.81 to 0.98, but 0.85 and 0.58 for rater SD 0.1 and 0.05)
   with a MOVER for v = S2 - c, where S2 = v + c is the variance of the
   method means on M - 1 df and c = (mean tau^2 + mean sigma^2 / r) / n
   its part from the items, on its delta-method df (as for a between-group
   variance in a one-way ANOVA). `method_vc()` uses the MOVER
   (`mc_vc_rater_ci()`): 0.94 to 0.98 in every cell, including 3 raters,
   rater SD 0.05, exchangeable replicates and method-specific residual SDs.
   Five cells (1, 2, 3, 7, 17) are above the 0.97 of the criterion (0.973
   to 0.976), all with 3 raters or a rater SD of at most 0.2; the interval
   is then wide anyway. The rater variance keeps
   M - 1 df in the bounds of the limits and `tdi()`.
3. **Variance components at zero: refit.** Before the refit, fits with a
   component at the boundary had no bounds: nlme's approximate covariance
   of the variance parameters was not positive definite (or, when it
   existed, gave Wald intervals with log-SD standard errors of 100 or
   more). That left out up to 10% of data sets with 3 raters and rater SD
   0.2, 7% and 17% with rater SD 0.1 and 0.05. The zero component was
   usually the rater, but with 3 raters also omega or tau (6 of 31 such
   fits in cell 1, where dropping the rater instead gave a REML
   log-likelihood up to 5 lower). `method_vc()` now refits without every
   random component whose SD is below 1e-3 times sigma
   (`mc_vc_refit_boundary()`; nlme stops at 1e-5 to 5e-4 times sigma at the
   boundary, so a cutoff of 1e-4 still missed 1 to 21 data sets per cell)
   and keeps the refit if its likelihood is not lower; a dropped rater
   gets the MOVER interval from 0, a dropped tau or omega 0 to NA, and the
   rater adds nothing to the bounds of the limits. Now 998 to 1000 data
   sets per cell have bounds, and the coverage of the other cells barely
   changes. Among the rescued fits (counted in an intermediate run with a
   1e-4 cutoff), coverage is high when the rater SD is small (0.96 to 1.00
   in cells 1, 3, 7, 17 and 18) but low when a large rater SD is estimated
   at zero with 3 raters: 25 fits in cell 2 covered 0.84, and 18 in cell 8
   covered 0.17, which takes cell 8 from 0.939 to 0.923. Using the MOVER
   upper bound for the rater piece instead of 0 was tried on the same fits: 0.92 in cell 2 and 1.00 in cell 7, but still
   0.17 in cell 8 and no change with 5 raters, so it was not adopted. With
   3 raters (2 df) the three rater effects can be close by chance however
   large the rater SD; `method_vc()` warns with fewer than five raters.
4. The SD of the differences is nearly unbiased (mean 0.97 to 1.00 of the
   true value, slightly low with 3 raters).
5. **`tdi()` GPQ bound: criterion met with M >= 5** (0.94 to 1.00; 0.93
   to 0.99 with 3 raters), slightly above the MOVER bounds of the limits.
   `tdi()` draws each variance component on its own df for random raters
   (changed 2026-11-02). p = 0.95 and p = 0.8 (`cover_tdi95`,
   `cover_tdi80`) give identical coverage: with bias 0 the TDI and its GPQ
   draws are the SD times a constant, and the draws are the same for a
   given seed. `method = "tolerance"` equals the MOVER bound of the limits
   at p = 0.95 (tested), so its coverage is the bound coverage above.
