# NEWS

# SimplyAgree 0.5.0 (development version)

New functions for comparing two or more methods, with or without replicate measurements, following Carstensen (2010, *Comparing Clinical Measurement Methods*) and the MethComp package. All are marked experimental.

- Added `method_vc()`, which fits the variance component model for method comparison (method-by-item, item-by-replicate, and method-specific residual variances) to two or more methods at once, with linked or exchangeable replicates.
  - Reports the bias for every pair of methods, the variance components with confidence intervals, and limits of agreement (`interval = "agreement"`, MOVER bounds as in `agreement_limit()`) or prediction and tolerance limits (`interval = "tolerance"`, as in `tolerance_limit()`), with `bound_type = "iu"` or `"joint"` (which, as in `tolerance_limit()`, affects only the tolerance limits).
  - Reports repeatability coefficients for each method and, with linked replicates, reproducibility coefficients.
  - Supports transformations (`"log"`, `"logit"`, `"pctlogit"`, `"sqrt"`, `"asinsqrt"`, or a custom pair of functions). `print(scale = "original", at = )` gives the bias, limits, confidence bounds, and repeatability coefficients in original units at chosen levels (default: the median of the item averages).
  - With one measurement per item by two methods it reproduces `agreement_limit(data_type = "simple")` and `tolerance_limit()`; on the oximetry data of Carstensen (2010) it reproduces MethComp's `BA.est()`.
  - With one measurement per item and three or more methods, the items are random effects (with fixed item effects the likelihood is unbounded) and the method-specific residual SDs absorb the method-by-item variation.
  - In simulations (15 to 60 items, 2 or 3 replicates, 2 or 3 methods; 95% targets), coverage of the bias CIs was 0.94 to 0.96 and of the confidence bounds of the limits 0.91 to 0.96, lowest (slightly liberal) with 15 items.
  - Methods: `print()`, `summary()`, `coef()`, `confint()`, `vcov()`, `predict()`, `plot()` (`type = "ba"`, `"matrix"`, `"vc"`), and `check()`.
- Added `method_convert()` for conversion equations between methods, with prediction limits:
  - from a `method_vc()` fit (constant bias, `object = `);
  - `bias = "linear"`: regression of the differences on the averages for each pair, with constant or linear SD of the differences, and cluster-robust standard errors with replicates;
  - `bias = "scaled"`: the scaled model fit by alternating regression, which reproduces MethComp's `AltReg()` on the oximetry data, with delta-method (default) or parametric bootstrap (`ci_method = "boot"`) confidence intervals for the conversion parameters. The delta-method intervals treat the predicted true values as known and are much too narrow (53% to 73% coverage of 95% intervals in simulations with 15 to 60 items); the bootstrap may improve them but has not yet been checked by simulation. Intervals with the stated coverage likely need a Bayesian (MCMC) fit of the scaled model, which is planned for the future.
  - Methods: `print()`, `summary()`, `coef()`, `confint()`, `predict()` (back-transformed to the original scale), `plot()` (`type = "conversion"`, `"ba"`), and `check()`.
- Added `sim_methods()` to simulate method comparison data from the scaled variance component model, with the true limits, repeatability coefficients, and conversions in the `truth` attribute.
- Added `mc_long()` to convert data with one column per method to the long format.
- New vignettes: "Comparing Several Methods with Replicate Measurements" and "Converting Between Methods".

# SimplyAgree 0.4.0

## Results that change

Several fixes change results from earlier versions:

- **ICC intervals** (`reli_stats()`, `reli_aov()`) are now two-sided; a "95%" interval was previously 90%. Old results at level `c` equal new results at `1 - 2 * (1 - c)`.
- **CCC for replicate and nested data** (`ccc_test()`, `agree_reps()`, `agree_nest()`): new variance-components estimate for replicate data, and subject-level (jackknife) standard errors; the old intervals were too narrow.
- **Tolerance limits** (`tolerance_limit()`) with variance functions, clustered data or the bootstrap; prediction limits are unchanged unless a variance function is used.
- **Deming joint tests** (`joint_test()`, `plot_joint()`, and the joint power of `deming_power_sim()`/`deming_sample_size()`) now use Sadler's sum-of-squares F test. Version 0.3.0 used a chi-squared Wald test from the jackknife variance-covariance matrix, which rejected a true identity line up to 14% of the time at n = 10 (nominal 5%); `method = "wald", test_method = "asymptotic"` reproduces it.
- **`pb_reg()`**: `error.ratio != 1` is now an error; bootstrap standard errors were 4-5 times too small before; tie-corrected slope intervals.
- `find_n()` can return a sample size one larger; `agreement_limit(data_type = "reps")` and `"nest"` change with unbalanced or missing data.

## Reliability

- New `icc_test()` (one ICC as an `htest`, two-sided F intervals, REML for missing data, optional subject bootstrap), `reli_err_test()` (SEM, SEE, SEP and CVs), and `mdc_test()` (minimal detectable change).
- New `reli_facet_test()` (experimental; **its name may change**) for subjects measured more than once by each rater or device: exchangeable repeats within subject-by-rater cells, or shared occasions (`occasion`, e.g. every rater on day 1 and day 2). Intra-rater, inter-rater and overall coefficients and SEMs from the generalizability-theory model, in consistency or agreement forms, with D-study projections, exact or MLS/Satterthwaite intervals, REML for unbalanced data, and the subject bootstrap. `icc_test()` and `reli_err_test()` point to it when the data have repeats, and `mdc_test()` accepts its SEMs.
- New `power_icc()` and `precision_icc()` for ICC power and interval-width sample sizes, including missing data.
- New `summary()`, `plot()` and `check()` methods for these results, with a new section in the power vignette.
- `reli_stats()` and `reli_aov()` are superseded and now use the same engine. Beyond the two-sided intervals: observed-data degrees of freedom with missing data, subject bootstrap, delta-method intervals for SEE/SEP/CV, ANOVA variance components, and stricter data checks.
- The jamovi reliability analysis uses the new functions.

## Categorical ratings

- New `cac_test()` (Gwet's AC1/AC2, Fleiss' kappa, Krippendorff's alpha, percent agreement; matches irrCAC) and `ord_kappa_test()` (model-based kappa for ordinal ratings from many raters, with group comparisons; #63), with `summary()` and `plot()` methods.
- New vignette, "Agreement for Categorical and Ordinal Ratings".
- `agree_coef()` is superseded by `cac_test()`; it gains `categories` and `weights`, and no longer fails or mislabels in edge cases.

## Agreement

- New `ccc_test()`: concordance correlation coefficient as an `htest`, with an opt-in studentized subject bootstrap for replicate data.
- `agreement_limit()` gains `bound_type` (`"iu"` default, unchanged; `"joint"`), and prints within-subject SDs for replicate data.
- `tolerance_limit()`:
  - limits now use the residual SD for each row when there is a variance function (they were too narrow);
  - new `bound_type`, `model = "lme"` (including nested `id`), and `conf_level`;
  - corrected analytic limits for clustered data, and a fixed, faster bootstrap (`tol_method = "boot_cal"`, formerly `"perc"`);
  - its returned `model` now works with `update()` and `emmeans()`.
- `loa_lme()` is deprecated in favour of `tolerance_limit(model = "lme")`.
- Fixed plots failing after saving and reloading (e.g. in jamovi; #76), and `plot()`/`print()` failing when settings were expressions (#50).

## Agreement power and sample size

- New `power_agreement_limit()` (Lu et al. 2016; reproduces PASS); `blandPowerCurve()` is superseded by it.
- `power_agreement_exact()`, `agree_expected_half()`, `agree_assurance()` and `blandPowerCurve()` gain `m` and `icc` for clustered data.
- Fixed several sample-size searches and the tolerance factor for very small `n`.

## Method comparison regression

- `joint_test()` and `plot_joint()` gain `method`. For Deming fits the default is Sadler's (2010) sum-of-squares F test and its exact region. The jackknife Wald ellipse (`method = "wald"`) rejected a true identity line about 8.5% of the time at n = 10 and 7.4% at n = 20 with the F reference (`test_method = "F"`, its default), and 14% and 10% with the chi-squared reference that 0.3.0 used (nominal 5%); Sadler's test held 4.3-5.9% (`references/general/eiv_sims`). Passing-Bablok fits use the Wald test; Dufey (2020) fits are refused (8-11%).
- `deming_power_sim()` now uses `joint_test()` (new `joint_method`), so its power matches the test users run; it previously overstated joint power. `deming_sample_size(use_joint = )` now takes effect.
- `pb_reg()`:
  - pairs bootstrap replaces a wild bootstrap that gave standard errors 4-5 times too small;
  - new `se_method` (`"bootstrap"`, `"jackknife"`, `"dufey"`);
  - `error.ratio != 1` is now an error (it biased the slope in simulations; use `dem_reg()`);
  - `id` averages replicates and now works with missing values;
  - fixed tie-corrected slope intervals and `reject_h0` on tied data.
- `plot_joint()` takes `ideal_intercept` before `ideal_slope`, like `joint_test()`, and both default to the model's confidence level.
- The documentation of `dem_reg(error.ratio)` now correctly states var(x)/var(y), as computed (thanks to Hugo Lachuer).


# SimplyAgree 0.3.0

- Add enhanced support for Deming regression.
  - Updated `dem_reg` function to include options for confidence intervals and plotting.
- Added more power and sample size determination functions for limits of agreement.
- Added Passing-Bablok regression function (`pb_reg`) for method comparison studies.
- Updated methods for `simple_eiv` objects to be more extensive and act like other statistical models

# SimplyAgree 0.2.2

- Small fixes to errors related to plotting and df for lmer based results.

# SimplyAgree 0.2.1

- Add sympercent options for log transformed results in `tolerance_limit` and `agreement_limit`
- Add argument to `agreement_limit` to allow of asymptotic confidence intervals to avoid errors related `emmeans` calculations
  - Only a problem for "big" data with > 1000 observations
- Small edit to `reli_stats` documentation to remove references to BCa confidence intervals

# SimplyAgree 0.2.0

- Add universal tolerance limits function: `tolerance_limit`
- Add universal agreement limits function: `agreement_limit`

# SimplyAgree 0.1.5

- Add option to drop "CCC" calculation from `agree_reps` and `agree_nest` functions.

# SimplyAgree 0.1.4

- Major error in SEP/SEE calculations. SD total calculation adjusted. See `vignette("reliability_analysis",package ="SimplyAgree")` for details.

# SimplyAgree 0.1.3

- Added `chisq` type of confidence intervals for "Other" CIs for `reli_stats` and `reli_aov`

# SimplyAgree 0.1.2
- Fix to `agree_np` plot. (colors now align with order of LoA elements).
- Added `reli_aov` function for sums of squares approach.

# SimplyAgree 0.1.1

- Minor cosmetic updates for jamovi submission.
- Minor cosmetic changes to plots (transparent backgrounds)

# SimplyAgree 0.1.0

- Updated CV calculations for `reli_stats` with the cv_calc argument.
  - Options now included CV for the model residuals, mean-squared error (MSE), or for the standard error of measurement (SEM).
  - Parametric bootstrap CI for all "other" statistics (CV, SEM, SEE, and SEP)
- Updated plots to include the `delta` argument on plot if specified, and estimate of LoA.
- Line-of-identity plot now includes trend line from Deming regression rather than OLS.
- Deming regression function (`dem_reg`) has been added to the package.
- Agreement coefficients (`agree_coef`), otherwise known as 
- Added agreement coefficient function (`agree_coef`).
- Added `loa_lme` function
  - `loa_mixed` now deprecated
  - `loa_lme` allows for heterogenous variance by condition
  - New function is faster and relies on parametric bootstrap
- Assumptions checks are added for all "simple-agree" class results
  - Checks include: normality, heteroscedasticity, and proportional bias
- Assumptions checks are added for all "simple-reli" class results
  - Checks include: normality and heteroscedasticity
- All jamovi functions updated with new UI layout
- Big thanks go to Greg Atkinson and Ivan Jukic for the many suggestions leading to this update.


# SimplyAgree 0.0.3
- Fixed error in `agree_nest` and `agree_reps`; now properly handles missing values
- Remove dependencies on sjstats and cccrm packages

# SimplyAgree 0.0.2
- Fixes typos in jamovi/jmv functions
- Adds more descriptive errors to jamovi output
- Remove dontrun from examples in documentation
- Add more details and references to the package's functions
