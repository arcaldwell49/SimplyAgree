# `bls_reg()`: roadmap

Status (2026-10-31): **first version implemented** (`R/bls_reg.R`,
`R/methods.simple_bls.R`, `tests/testthat/test-bls_reg.R`); see "Done and
left" at the end. The rest of this note is the original plan. This
note records the goal, sources, API sketch, how the function fits into the
existing errors-in-variables (EIV) family, and how to validate it.

Renamed from `cbls_reg()` to `bls_reg()` on 2026-11-01, before release: the
main fit is the BLS line in the (x, y) plot, with CBLS (Bland-Altman plot)
as its companion (`$cbls`, `type = "md"`), as `dem_reg()` and `pb_reg()`
are named for their fits. The simulation folder keeps the name
`cbls_sims/`, and `method_convert(estimator = "cbls")` keeps its name
because it uses the CBLS fit.

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
bls_reg(formula, data, id = NULL,
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

## Done and left (2026-10-31)

Done, following BivRegBLS 1.1.1's source (read for the formulas; our own
implementation):

- `bls_reg()` returns `c("simple_bls", "simple_eiv")`: BLS in (X, Y) and
  CBLS in (M, D), with D = x - y (the `agreement_limit()` convention;
  BivRegBLS uses y - x). One engine (`.bls_engine()`) for both, iterated to
  convergence with per-subject error variances `var / n_i` (BivRegBLS takes
  one step from Deming and assumes balanced replicates; identical when
  balanced).
- Variances: `var_x`/`var_y` (+ `df_var_x`/`df_var_y`), pooled
  within-subject variances from replicates, or `error.ratio` only (no PIs).
- `predict()`: CI and PI (single or `reps_x`/`reps_y` means; BivRegBLS's
  "GI"), Satterthwaite df, in both plots. CBLS PIs include the D-M error
  covariance (BivRegBLS omits it).
- `joint_test()`: Wald, F(2, n - 2), no Hotelling factor (BivRegBLS uses
  2(n - 1)/(n - 2) instead of 2). Sadler disabled for BLS.
- Tests reproduce BivRegBLS to 1e-8 (estimates, SEs, CIs, PIs, GIs, CBLS),
  Deming identity, minimiser of the BLS objective with unequal replicates,
  CBLS line = transformed BLS line.

Done since (2026-10-31, second round):

- Coverage simulations (`references/general/cbls_sims/`): constant
  variances (known, balanced/unbalanced replicates) and proportional error
  (known functions, `var_model = "proportional"`/`"power"`); 93-97%
  coverage, joint test size 4-6.5%.
- Simultaneous band: `predict(interval = "band")`, Working-Hotelling
  `sqrt(2 F(2, n - 2))`, dual to `joint_test()` (BivRegBLS adds
  `sqrt((n - 1)/(n - 2))`).
- Variance functions: `var_x`/`var_y` as functions of the level;
  `var_model = "constant"/"proportional"/"power"` from replicates; variances
  evaluated at the estimated true values, refit until stable.

Done in the third round (2026-10-31):

- Equation error: done (common tau, `equation_error = TRUE`; dispersion
  test in `summary()`); see `cbls_sims/README.md` study 3.
- Tolerance limits around the line: done, experimental (own construction;
  Francq & Govaerts 2016 give only horizontal tolerance intervals); see
  `cbls_sims/README.md` study 4.
- CBLS in `method_convert(bias = "linear", estimator = "cbls")`: done
  (exchangeable replicates, constant variance).

Still open: linked replicates for `estimator = "cbls"` (the item means of
the two methods share occasion effects, so their errors are correlated;
the CBLS engine supports a covariance, which would need the occasion
variance from `method_vc()`); variance functions in `method_convert()`;
method-specific equation error with three or more methods.

## Gaps against BivRegBLS and MethComp (2026-11-01)

Compared with the exports of BivRegBLS 1.1.1 and MethComp 1.30.2. Most of
both packages is covered (`bls_reg()`, `dem_reg()`, `pb_reg()`,
`method_vc()`, `method_convert()`, `tdi()`, `ccc_test()`, `sim_methods()`,
`joint_test()`). In priority order:

1. **Error-ratio sensitivity** (BivRegBLS `FullCIs.XY`, `GraphFullCIs.XY`,
   `GraphFullCIs.MD`). With no replicates and an unknown error ratio, show
   the slope and intercept estimates and CIs over the whole range of ratios
   (0 to Inf: OLS of y on x through to OLS of x on y). Candidate:
   `plot(..., type = "lambda")` or a helper for `dem_reg()`/`bls_reg()`
   objects. Validate against BivRegBLS on its data sets.
2. **Per-row error variances in `bls_reg()`** (BivRegBLS `var.x.col`,
   `nxi.col`, `df.var.x.col`). Let `var_x`/`var_y` also be a column name or
   a vector of length n (with per-row replicate counts and df), e.g.
   variances from a separate validation study. Currently `var_x`/`var_y`
   take a number or a function of the level only.
3. **Log scale in `bls_reg()`** (BivRegBLS `antilog.pred`). A `transform`
   argument (`"none"`, `"log"`), as in `method_vc()`/`method_convert()`,
   that fits on the log scale and back-transforms predictions and
   intervals for `predict()` and `plot()`.
4. **Replicate-permutation sensitivity for `method_vc()`** (MethComp
   `perm.repl`). Randomly permute replicate labels within item and method
   and refit, to show whether the linked/exchangeable choice changes the
   limits. Cheap to add (a few refits).
5. **Random raters in `method_vc()`/`method_convert()`** (MethComp
   `random.raters` in `BA.est` and `DA.reg`). Treat the methods as a
   random sample of raters, so the method effect is a variance component
   and the limits apply to a new rater.

Lower priority, probably not worth it:

- MethComp `MCmcmc`: Bayesian (JAGS) fit of the full model. The frequentist
  routes cover the estimates; would add a JAGS dependency.
- MethComp `corMxI`: correlated method-by-item effects. Niche.
- MethComp `choose.trans`/`check.trans`: arbitrary user transform pairs
  (e.g. logit); check whether the `transform` argument of `method_vc()`
  and `method_convert()` should accept a function pair.
- BivRegBLS `OLSh`, `raw.plot`, `desc.stat`; MethComp `Meth` class,
  `make.repl`, `sample.Meth`, `bothlines`. Cosmetic or covered by
  `check()` and `mc_long()`.

### Status (2026-11-01, later)

- Item 1 done: `error_ratio_profile()` (R/error_ratio_profile.R), reproducing
  `FullCIs.XY()` (estimates, BLS and exact Creasy CIs, intercept CIs).
  The joint test is the Wald F without BivRegBLS's Hotelling factor, as in
  `joint_test()`. Not done: the per-ratio ellipses and hyperbolic bands of
  `FullCIs.XY()`/`GraphFullCIs.*`, and an MD-plot version (`FullCIs.MD`).
- Item 2 done: `var_x`/`var_y` (and `df_var_x`/`df_var_y`) can name columns
  of per-subject values; reproduces `BLS.ht()` estimates and CIs. A new
  measurement gets the mean variance (Satterthwaite df). Not done: per-row
  variance formulas for new X values (`BLS.ht(var.x.formula = )`); a
  function of the level already covers that.
- Item 3 done: `bls_reg(transform = "log")` with back-transformed
  `predict()`/`plot()`. Log scale only (no general transform pairs).
- Item 4 done: `linked_reps_test()` (R/linked_reps_test.R), a one-sided
  permutation test of omega with the permuted limits. About 0.7 s per refit
  for 30 items x 3 methods x 3 replicates.
- Item 5 open: random raters. Needs a design decision (see below).

Random raters: design agreed 2026-11-01 (nlme, `methods = c("fixed",
"random")`, a `sigma` argument for common or method-specific residual SDs,
agreement limits with MOVER bounds first); see "Next steps" in `TODO.md`.
The original questions:
- Model: methods as a random effect, `y ~ id + (1 | method) + (1 | method:id)
  [+ (1 | id:repl)]` with a common residual SD (exchangeable raters). Crossed
  effects with linked replicates need lme4 rather than nlme.
- Limits: one row for "two randomly chosen raters", bias 0 and
  SD sqrt(2 (sigma_alpha^2 + tau^2 + sigma^2)) (plus 2 omega^2 with
  `include_omega`), instead of one row per pair.
- Confidence bounds: sigma_alpha^2 has only (M - 1) df, so the bounds are
  wide with few raters. MOVER with approximate df per component, or a
  parametric bootstrap (slow).
- Downstream: print/summary/plot/check/tdi for `simple_mvc`, and whether
  `method_convert()` gets it (MethComp's `DA.reg(random.raters = )`).
