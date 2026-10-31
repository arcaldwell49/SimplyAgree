# Changelog

## SimplyAgree 0.5.0 (development version)

New functions for comparing two or more methods, with or without
replicate measurements, following Carstensen (2010, *Comparing Clinical
Measurement Methods*) and the MethComp package. All are marked
experimental.

- Added
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md),
  which fits the variance component model for method comparison
  (method-by-item, item-by-replicate, and method-specific residual
  variances) to two or more methods at once, with linked or exchangeable
  replicates.
  - Reports the bias for every pair of methods, the variance components
    with confidence intervals, and limits of agreement
    (`interval = "agreement"`, MOVER bounds as in
    [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md))
    or prediction and tolerance limits (`interval = "tolerance"`, as in
    [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)),
    with `bound_type = "iu"` or `"joint"` (which, as in
    [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
    affects only the tolerance limits).
  - Reports repeatability coefficients for each method and, with linked
    replicates, reproducibility coefficients.
  - Supports transformations (`"log"`, `"logit"`, `"pctlogit"`,
    `"sqrt"`, `"asinsqrt"`, or a custom pair of functions).
    `print(scale = "original", at = )` gives the bias, limits,
    confidence bounds, and repeatability coefficients in original units
    at chosen levels (default: the median of the item averages).
  - With one measurement per item by two methods it reproduces
    `agreement_limit(data_type = "simple")` and
    [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md);
    on the oximetry data of Carstensen (2010) it reproduces MethComp’s
    `BA.est()`.
  - With one measurement per item and three or more methods, the items
    are random effects (with fixed item effects the likelihood is
    unbounded) and the method-specific residual SDs absorb the
    method-by-item variation.
  - In simulations (15 to 60 items, 2 or 3 replicates, 2 or 3 methods;
    95% targets), coverage of the bias CIs was 0.94 to 0.96 and of the
    confidence bounds of the limits 0.91 to 0.96, lowest (slightly
    liberal) with 15 items.
  - Methods: [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`coef()`](https://rdrr.io/r/stats/coef.html),
    [`confint()`](https://rdrr.io/r/stats/confint.html),
    [`vcov()`](https://rdrr.io/r/stats/vcov.html),
    [`predict()`](https://rdrr.io/r/stats/predict.html),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
    (`type = "ba"`, `"matrix"`, `"vc"`), and
    [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).
- Added
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)
  for conversion equations between methods, with prediction limits:
  - from a
    [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
    fit (constant bias, `object =`);
  - `bias = "linear"`: regression of the differences on the averages for
    each pair, with constant or linear SD of the differences, and
    cluster-robust standard errors with replicates;
  - `bias = "scaled"`: the scaled model, fit by alternating regression
    (`estimator = "altreg"`, the default), which reproduces MethComp’s
    `AltReg()` on the oximetry data, or by maximum likelihood of the
    structural model (`estimator = "ml"`, with the lavaan package, now
    in Suggests).
    - Maximum likelihood gives Wald intervals for the conversion
      intercepts, slopes, and prediction SDs, with t quantiles on
      (items - 1) df. In simulations with 15 to 60 items, 2 or 3
      methods, linked or exchangeable replicates, missing measurements,
      and uniform or skewed true values, 95% intervals covered 92% to
      97% (prediction SDs 92% to 95%). The summary reports the model’s
      chi-square test of fit, RMSEA, and SRMR; zero or negative variance
      estimates give a warning.
    - The alternating regression gives delta-method (default) or
      parametric bootstrap (`ci_method = "boot"`) intervals. The
      delta-method intervals treat the predicted true values as known
      and are much too narrow (53% to 73% coverage of 95% intervals in
      the same simulations); the bootstrap has not been checked by
      simulation.
  - Methods: [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`coef()`](https://rdrr.io/r/stats/coef.html),
    [`confint()`](https://rdrr.io/r/stats/confint.html),
    [`predict()`](https://rdrr.io/r/stats/predict.html)
    (back-transformed to the original scale),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
    (`type = "conversion"`, `"ba"`), and
    [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).
  - With `variance = "linear"`, a warning is given when the fitted SD of
    the differences is not positive over part of the observed range of
    the averages.
- The Bland-Altman plots and
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  of
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  and
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)
  follow the style of
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  and
  [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md):
  - With a constant bias, the bias and limits are drawn as points with
    their confidence intervals to the left of the data; with a bias that
    changes with the level, as coloured lines.
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html) takes the
    `geom` and `delta` (maximal allowable difference) arguments of
    [`plot.loa()`](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md).
  - Each
    [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
    panel has a subtitle saying what to look for, and the tests are
    given in its caption: Shapiro-Wilk for normality, Breusch-Pagan for
    homogeneity of variance, Pitman-Morgan for proportional bias, and
    (linear conversions) a RESET test of linearity. Tests for pairs of
    methods use item means, so replicates are not counted more than
    once.
- Added
  [`sim_methods()`](https://aaroncaldwell.us/SimplyAgree/reference/sim_methods.md)
  to simulate method comparison data from the scaled variance component
  model, with the true limits, repeatability coefficients, and
  conversions in the `truth` attribute.
- Added
  [`mc_long()`](https://aaroncaldwell.us/SimplyAgree/reference/mc_long.md)
  to convert data with one column per method to the long format.
- New vignettes: “Comparing Several Methods with Replicate Measurements”
  and “Converting Between Methods”.

### Proportional bias diagnostics in `check()`

A slope of the differences on the means only shows that the two methods
have unequal variances, since cov(x - y, x + y) = var(x) - var(y). This
can come from unequal measurement error as well as from proportional
bias (Bland & Altman, 1999; Hopkins, 2004). The proportional bias panel
of
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
for
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
and
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
now says what it tests and, where possible, corrects for the error
variances:

- The test is labelled as the Pitman-Morgan test of equal variances.
  When it is significant with one measurement per method,
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  gives a message that it cannot separate proportional bias from unequal
  measurement error.
- With replicates (`agreement_limit(data_type = "reps")`,
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  with replicates), the test uses subject means and is corrected for the
  measurement error variances estimated from the replicates (jackknife
  over subjects). Only their difference is used, so linked replicates do
  not bias it. The error-adjusted slope is drawn as a dashed line. In
  simulations with unequal error and no proportional bias, it rejected
  at about 5% to 6.5%.
- New `error.ratio` argument to
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  for single-measurement designs: with a known error variance ratio, the
  Deming slope (as in
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md))
  is tested against 1.
- New `bias_line` argument to
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  chooses the dashed line drawn with the corrected test: `"adjusted"`
  (default), the error-adjusted slope, which should be flat without
  proportional bias and gives its size (`beta`); or `"expected"`, the
  slope expected from unequal measurement error alone, which the
  observed line should follow. The help pages (“Choosing `bias_line`” in
  [`?check.loa`](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md))
  and the agreement vignette describe the pros and cons of each. The
  panel now has a legend for the two lines.
- [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  for
  [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)
  now reports the Pitman-Morgan test (on subject means with `id`, by
  condition with `condition`).
- The results are attached to the output of
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  as the attribute `"prop_bias"` (a data frame with the test, the
  observed and error-adjusted slopes, the implied scale `beta` of one
  method relative to the other, and the within-subject variances), so
  they can be reported.
- The “Proportional Bias” section of the agreement vignette now explains
  how to read the plot and the test, and what to do with the result for
  each design.

Results that change:

- [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  for `agreement_limit(data_type = "reps")` previously tested the slope
  with a row-level mixed model, which pairs exchangeable replicates
  arbitrarily; with unequal measurement error and no proportional bias
  it rejected about 40% of the time at the 5% level.
- With `log_tf = TRUE`, the test now uses the mean on the log scale (it
  used the mean on the original scale), so it tests equal variances on
  the analysis scale. The `prop_bias` models are unchanged.
- The proportional bias panel shows the centred differences, not the
  residuals of the fitted model, so it no longer looks flat when
  `prop_bias = TRUE`.

Bug fixes:

- [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  for `agreement_limit(prop_bias = TRUE)` reported p = NA for
  proportional bias.
- [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  for `tolerance_limit(keep_model = FALSE)` failed; it now draws the
  proportional bias panel and notes that the other panels need the
  model.

### Model equations

- Added
  [`model_equation()`](https://aaroncaldwell.us/SimplyAgree/reference/model_equation.md)
  (experimental), which writes the statistical model behind a result as
  LaTeX: the mean model, the distributions of the random effects and
  residuals, and any variance function or correlation structure.
  - Methods for
    [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
    [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
    [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
    [`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md),
    [`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md),
    [`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
    [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
    [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
    and
    [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)
    results, using the notation of the vignettes, and for `lm`, `lmer`,
    `gls` and `lme` models.
  - Symbols by default; `use_coefs = TRUE` substitutes the estimates
    (`coef_digits` sets the decimals).
  - Prints the LaTeX code at the console and renders as display math in
    R Markdown and Quarto. No new dependencies.

## SimplyAgree 0.4.0

### Results that change

Several fixes change results from earlier versions:

- **ICC intervals**
  ([`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
  [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md))
  are now two-sided; a “95%” interval was previously 90%. Old results at
  level `c` equal new results at `1 - 2 * (1 - c)`.
- **CCC for replicate and nested data**
  ([`ccc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/ccc_test.md),
  [`agree_reps()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_reps.md),
  [`agree_nest()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_nest.md)):
  new variance-components estimate for replicate data, and subject-level
  (jackknife) standard errors; the old intervals were too narrow.
- **Tolerance limits**
  ([`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md))
  with variance functions, clustered data or the bootstrap; prediction
  limits are unchanged unless a variance function is used.
- **Deming joint tests**
  ([`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md),
  [`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md),
  and the joint power of
  [`deming_power_sim()`](https://aaroncaldwell.us/SimplyAgree/reference/deming_power.md)/[`deming_sample_size()`](https://aaroncaldwell.us/SimplyAgree/reference/deming_sample_size.md))
  now use Sadler’s sum-of-squares F test. Version 0.3.0 used a
  chi-squared Wald test from the jackknife variance-covariance matrix,
  which rejected a true identity line up to 14% of the time at n = 10
  (nominal 5%); `method = "wald", test_method = "asymptotic"` reproduces
  it.
- **[`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md)**:
  `error.ratio != 1` is now an error; bootstrap standard errors were 4-5
  times too small before; tie-corrected slope intervals.
- [`find_n()`](https://aaroncaldwell.us/SimplyAgree/reference/powerCurve-methods.md)
  can return a sample size one larger;
  `agreement_limit(data_type = "reps")` and `"nest"` change with
  unbalanced or missing data.

### Reliability

- New
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
  (one ICC as an `htest`, two-sided F intervals, REML for missing data,
  optional subject bootstrap),
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
  (SEM, SEE, SEP and CVs), and
  [`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md)
  (minimal detectable change).
- New
  [`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md)
  (experimental; **its name may change**) for subjects measured more
  than once by each rater or device: exchangeable repeats within
  subject-by-rater cells, or shared occasions (`occasion`, e.g. every
  rater on day 1 and day 2). Intra-rater, inter-rater and overall
  coefficients and SEMs from the generalizability-theory model, in
  consistency or agreement forms, with D-study projections, exact or
  MLS/Satterthwaite intervals, REML for unbalanced data, and the subject
  bootstrap.
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
  and
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
  point to it when the data have repeats, and
  [`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md)
  accepts its SEMs.
- New
  [`power_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/power_icc.md)
  and
  [`precision_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/precision_icc.md)
  for ICC power and interval-width sample sizes, including missing data.
- New [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  methods for these results, with a new section in the power vignette.
- [`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md)
  and
  [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md)
  are superseded and now use the same engine. Beyond the two-sided
  intervals: observed-data degrees of freedom with missing data, subject
  bootstrap, delta-method intervals for SEE/SEP/CV, ANOVA variance
  components, and stricter data checks.
- The jamovi reliability analysis uses the new functions.

### Categorical ratings

- New
  [`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md)
  (Gwet’s AC1/AC2, Fleiss’ kappa, Krippendorff’s alpha, percent
  agreement; matches irrCAC) and
  [`ord_kappa_test()`](https://aaroncaldwell.us/SimplyAgree/reference/ord_kappa_test.md)
  (model-based kappa for ordinal ratings from many raters, with group
  comparisons;
  [\#63](https://github.com/arcaldwell49/SimplyAgree/issues/63)), with
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- New vignette, “Agreement for Categorical and Ordinal Ratings”.
- [`agree_coef()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_coef.md)
  is superseded by
  [`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md);
  it gains `categories` and `weights`, and no longer fails or mislabels
  in edge cases.

### Agreement

- New
  [`ccc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/ccc_test.md):
  concordance correlation coefficient as an `htest`, with an opt-in
  studentized subject bootstrap for replicate data.
- [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  gains `bound_type` (`"iu"` default, unchanged; `"joint"`), and prints
  within-subject SDs for replicate data.
- [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md):
  - limits now use the residual SD for each row when there is a variance
    function (they were too narrow);
  - new `bound_type`, `model = "lme"` (including nested `id`), and
    `conf_level`;
  - corrected analytic limits for clustered data, and a fixed, faster
    bootstrap (`tol_method = "boot_cal"`, formerly `"perc"`);
  - its returned `model` now works with
    [`update()`](https://rdrr.io/r/stats/update.html) and `emmeans()`.
- [`loa_lme()`](https://aaroncaldwell.us/SimplyAgree/reference/loa_lme.md)
  is deprecated in favour of `tolerance_limit(model = "lme")`.
- Fixed plots failing after saving and reloading (e.g. in jamovi;
  [\#76](https://github.com/arcaldwell49/SimplyAgree/issues/76)), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)/[`print()`](https://rdrr.io/r/base/print.html)
  failing when settings were expressions
  ([\#50](https://github.com/arcaldwell49/SimplyAgree/issues/50)).

### Agreement power and sample size

- New
  [`power_agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_limit.md)
  (Lu et al. 2016; reproduces PASS);
  [`blandPowerCurve()`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md)
  is superseded by it.
- [`power_agreement_exact()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_exact.md),
  [`agree_expected_half()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_expected_half.md),
  [`agree_assurance()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_assurance.md)
  and
  [`blandPowerCurve()`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md)
  gain `m` and `icc` for clustered data.
- Fixed several sample-size searches and the tolerance factor for very
  small `n`.

### Method comparison regression

- [`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  and
  [`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  gain `method`. For Deming fits the default is Sadler’s (2010)
  sum-of-squares F test and its exact region. The jackknife Wald ellipse
  (`method = "wald"`) rejected a true identity line about 8.5% of the
  time at n = 10 and 7.4% at n = 20 with the F reference
  (`test_method = "F"`, its default), and 14% and 10% with the
  chi-squared reference that 0.3.0 used (nominal 5%); Sadler’s test held
  4.3-5.9% (`references/general/eiv_sims`). Passing-Bablok fits use the
  Wald test; Dufey (2020) fits are refused (8-11%).
- [`deming_power_sim()`](https://aaroncaldwell.us/SimplyAgree/reference/deming_power.md)
  now uses
  [`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  (new `joint_method`), so its power matches the test users run; it
  previously overstated joint power. `deming_sample_size(use_joint = )`
  now takes effect.
- [`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md):
  - pairs bootstrap replaces a wild bootstrap that gave standard errors
    4-5 times too small;
  - new `se_method` (`"bootstrap"`, `"jackknife"`, `"dufey"`);
  - `error.ratio != 1` is now an error (it biased the slope in
    simulations; use
    [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md));
  - `id` averages replicates and now works with missing values;
  - fixed tie-corrected slope intervals and `reject_h0` on tied data.
- [`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  takes `ideal_intercept` before `ideal_slope`, like
  [`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md),
  and both default to the model’s confidence level.
- The documentation of `dem_reg(error.ratio)` now correctly states
  var(x)/var(y), as computed (thanks to Hugo Lachuer).

## SimplyAgree 0.3.0

CRAN release: 2026-01-21

- Add enhanced support for Deming regression.
  - Updated `dem_reg` function to include options for confidence
    intervals and plotting.
- Added more power and sample size determination functions for limits of
  agreement.
- Added Passing-Bablok regression function (`pb_reg`) for method
  comparison studies.
- Updated methods for `simple_eiv` objects to be more extensive and act
  like other statistical models

## SimplyAgree 0.2.2

- Small fixes to errors related to plotting and df for lmer based
  results.

## SimplyAgree 0.2.1

CRAN release: 2025-02-24

- Add sympercent options for log transformed results in
  `tolerance_limit` and `agreement_limit`
- Add argument to `agreement_limit` to allow of asymptotic confidence
  intervals to avoid errors related `emmeans` calculations
  - Only a problem for “big” data with \> 1000 observations
- Small edit to `reli_stats` documentation to remove references to BCa
  confidence intervals

## SimplyAgree 0.2.0

CRAN release: 2024-03-21

- Add universal tolerance limits function: `tolerance_limit`
- Add universal agreement limits function: `agreement_limit`

## SimplyAgree 0.1.5

- Add option to drop “CCC” calculation from `agree_reps` and
  `agree_nest` functions.

## SimplyAgree 0.1.4

- Major error in SEP/SEE calculations. SD total calculation adjusted.
  See
  [`vignette("reliability_analysis",package ="SimplyAgree")`](https://aaroncaldwell.us/SimplyAgree/articles/reliability_analysis.md)
  for details.

## SimplyAgree 0.1.3

- Added `chisq` type of confidence intervals for “Other” CIs for
  `reli_stats` and `reli_aov`

## SimplyAgree 0.1.2

CRAN release: 2022-12-14

- Fix to `agree_np` plot. (colors now align with order of LoA elements).
- Added `reli_aov` function for sums of squares approach.

## SimplyAgree 0.1.1

- Minor cosmetic updates for jamovi submission.
- Minor cosmetic changes to plots (transparent backgrounds)

## SimplyAgree 0.1.0

CRAN release: 2022-08-24

- Updated CV calculations for `reli_stats` with the cv_calc argument.
  - Options now included CV for the model residuals, mean-squared error
    (MSE), or for the standard error of measurement (SEM).
  - Parametric bootstrap CI for all “other” statistics (CV, SEM, SEE,
    and SEP)
- Updated plots to include the `delta` argument on plot if specified,
  and estimate of LoA.
- Line-of-identity plot now includes trend line from Deming regression
  rather than OLS.
- Deming regression function (`dem_reg`) has been added to the package.
- Agreement coefficients (`agree_coef`), otherwise known as
- Added agreement coefficient function (`agree_coef`).
- Added `loa_lme` function
  - `loa_mixed` now deprecated
  - `loa_lme` allows for heterogenous variance by condition
  - New function is faster and relies on parametric bootstrap
- Assumptions checks are added for all “simple-agree” class results
  - Checks include: normality, heteroscedasticity, and proportional bias
- Assumptions checks are added for all “simple-reli” class results
  - Checks include: normality and heteroscedasticity
- All jamovi functions updated with new UI layout
- Big thanks go to Greg Atkinson and Ivan Jukic for the many suggestions
  leading to this update.

## SimplyAgree 0.0.3

CRAN release: 2022-03-14

- Fixed error in `agree_nest` and `agree_reps`; now properly handles
  missing values
- Remove dependencies on sjstats and cccrm packages

## SimplyAgree 0.0.2

CRAN release: 2021-05-18

- Fixes typos in jamovi/jmv functions
- Adds more descriptive errors to jamovi output
- Remove dontrun from examples in documentation
- Add more details and references to the package’s functions
