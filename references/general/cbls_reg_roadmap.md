# `cbls_reg()`: roadmap

Status (2026-10-31): **planned, not started.** Nothing is implemented. This
note records the goal, sources, API sketch, how the function fits into the
existing errors-in-variables (EIV) family, and how to validate it.

## Why

The MethComp vignettes (`vignette("method_comparison_vc")`,
`vignette("method_conversion")`) note two criticisms by Francq and
colleagues that apply to the Bland-Altman tradition and to MethComp:

1. When the differences change with the level, horizontal limits are not
   valid, and the ordinary regression of the differences D on the averages A
   (`method_convert(bias = "linear")`, MethComp's `DA.reg()`) is biased
   whenever the two methods have different measurement errors, because the
   errors of D and A are correlated (Francq & Govaerts 2016).
2. Limits of agreement are approximate; prediction and tolerance intervals
   are exact (Francq et al. 2020). SimplyAgree already covers this for
   constant bias (`tolerance_limit()`, `method_vc(interval = "tolerance")`).

What no SimplyAgree function does yet is what BivRegBLS does for two
methods: errors-in-variables regression with **known or replicate-estimated
error variances that may differ between methods and change with the level**,
and with **confidence, prediction, and tolerance intervals around a
non-constant bias**.

## Sources

- Francq BG, Govaerts BB (2014). Measurement methods comparison with
  errors-in-variables regressions. From horizontal to vertical OLS
  regression, review and new perspectives. *Chemometrics and Intelligent
  Laboratory Systems* 134:123-139. doi:10.1016/j.chemolab.2014.03.006
  (BLS, Deming, CIs; unbiased estimate of the error ratio from replicates).
- Francq BG, Govaerts BB (2014). Hyperbolic confidence bands of
  errors-in-variables regression lines applied to method comparison studies.
  *Journal de la Société Française de Statistique* 155(1):23-45
  (BLS under heteroscedastic errors; confidence bands; joint region).
- Francq BG, Govaerts B (2016). How to regress and predict in a Bland-Altman
  plot? *Statistics in Medicine* 35(14):2328-2358. doi:10.1002/sim.6872
  (CBLS: correlated errors in the (M, D) plot; predictive and tolerance
  intervals with or without replicates). **Full text needed** before
  implementing: the formulas are not in the abstract.
- Francq BG, Berger M, Boachie C (2020). To tolerate or to agree.
  *Statistics in Medicine* 39(28):4334-4349. doi:10.1002/sim.8709
  (tutorial; §3.6 recommends CBLS/BLS when the BA plot shows a pattern).
- BivRegBLS (CRAN; Francq & Berger): `BLS()`, `CBLS()`, `MD.horiz.lines()`,
  `XY.plot()`, `MD.plot()`, plus their confidence/prediction/tolerance
  options (`conf.level`, `pred.level`, `TI.conf.level`).

## Scope

- **BLS** in the (X, Y) plot: Y = β0 + β1 X with errors in both, error
  variances `var_x`, `var_y` known (from validation studies) or estimated
  from replicates; homoscedastic (λ constant) and heteroscedastic (variance
  as a function of the level) versions.
- **CBLS** in the (M, D) plot: the same line expressed as D = γ0 + γ1 M,
  accounting for the correlated errors of M and D. BLS and CBLS give
  equivalent lines (Francq 2016); offer both views through one fit.
- Intervals: confidence (for the line), prediction (a new difference or a
  new Y at given X), and beta-content tolerance intervals; a joint
  confidence region (ellipse) for (intercept, slope).
- Two methods only (as BivRegBLS). More methods: open question below.

## API sketch (SimplyAgree style)

```r
cbls_reg(formula, data, id = NULL,
         var_x = NULL, var_y = NULL, error.ratio = NULL,
         space = c("xy", "md"),
         conf.level = 0.95, pred_level = 0.95, tol_level = 0.95,
         keep_data = TRUE, ...)
```

- `formula = y ~ x` as in `dem_reg()`; `id` identifies replicates (long
  format), from which `var_x` and `var_y` are estimated when not given.
- **Do not use `replicates`** as an argument name: `pb_reg()` uses it for
  the number of bootstrap resamples.
- `space` only chooses the default reporting/plot scale; the fit is the same.
- Returns class `simple_eiv` with `method = "Bivariate least squares (BLS)"`
  and the usual elements (`coefficients`, `model_table`, `vcov`,
  `df.residual`, `call`, `terms`, `model`, `error.ratio`, `conf.level`),
  plus `var_x`, `var_y`, and the interval settings.

## Integration with the existing EIV family

- `R/methods.simple_eiv.R`: methods branch on
  `grepl("Passing-Bablok", x$method)`. Every branch (print, summary, plot,
  check, fitted, residuals, predict) needs a BLS case; consider switching
  to an explicit `x$fit_type` field first.
- `plot.simple_eiv()`: documents `interval = "prediction"` but `match.arg()`
  rejects it. Add `"prediction"` and `"tolerance"`, and a Bland-Altman
  (`type = "md"`) view, which BLS/CBLS make natural.
- `predict.simple_eiv()`: add `interval = "prediction"` / `"tolerance"`.
- `joint_test()`: `.joint_method()` picks Sadler or Wald; BLS gets a Wald
  ellipse from its exact vcov (and possibly an F-based region as in
  Francq & Govaerts 2014, JSFS).
- `check()`: Deming-style residual panels (normality; spread against the
  level, to choose the heteroscedastic version).
- `dem_reg()` already estimates `error.ratio` from replicates
  (`R/dem_reg.R`, pooled within-subject variances) but cannot take separate
  known variances. BLS with constant λ should reproduce Deming exactly: use
  this as a test anchor.
- Later: `method_convert(bias = "linear", estimator = "cbls")`, so the
  linear-bias model of Part 2 can drop the correlated-errors bias.

## Validation plan

- Live comparisons with BivRegBLS (Suggests, `skip_if_not_installed`) on
  its example data sets: estimates, SEs, CIs, PIs, TIs, and the ellipse,
  `tolerance = 1e-6` where the formulas are identical.
- BLS with constant λ against `dem_reg(error.ratio = )`.
- Coverage simulation in `references/general/cbls_sims/` (model on
  `eiv_sims/`): homoscedastic and heteroscedastic errors, unequal error
  variances, with and without replicates, n = 15 to 100. Criteria set
  before running.

## Open questions

- How the user chooses the variance function under heteroscedasticity
  (BivRegBLS takes per-point variances; a parametric `varPower`-style
  option may be friendlier).
- Whether to support three or more methods (pairwise only, or a joint
  model like `method_vc()`).
- Whether CBLS should replace the default of
  `method_convert(bias = "linear")`.
