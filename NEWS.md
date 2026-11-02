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
  - Random raters (`methods = "random"`, as MethComp's `random.raters`): the methods are a random sample of raters, observers or devices, with a rater SD in the variance components and one row of limits for two randomly chosen raters (bias 0, SD sqrt(2 (rater^2 + tau^2 + sigma^2)), with means of method-specific variances). The rater variance has M - 1 df, and the bounds of the limits combine the components by the MOVER, each with its own df; the rater SD interval is a MOVER for the variance of the method means (M - 1 df) minus the part due to the items; at least three methods are needed, with a warning below five. In simulations (3 to 10 raters, 15 or 30 items) the one-sided 95% bounds covered 0.94 to 0.96 with 5 or 10 raters when the rater variance dominated and 0.98 to 1.00 otherwise (one chi-square on Satterthwaite df gave 0.74 to 0.87 in the first case); the rater SD interval covered 0.94 to 0.98, including rater SDs of 0.05 (a chi-square on M - 1 df alone covered 0.79 to 0.91 for a small rater SD), and with 3 raters a rater SD estimated at zero (up to 19% of fits) can hide a large one (bound coverage 0.92 in one cell). With `interval = "tolerance"` the prediction limits are ±t SD and the tolerance limits are z times the upper bound for the SD; since the bias is known to be 0, these are the same for both `bound_type`s and equal the upper bounds of the limits of agreement. `df_sd` (and the t df) are the Satterthwaite df of the components, with the rater variance on M - 1 df. In the same simulations the prediction limits had an expected content of 0.946 to 0.951 with 5 or 10 raters (0.940 to 0.953 with 3). Fitted with nlme (a rater block crossed with the items); with common SDs it matches the crossed `lme4::lmer()` fit. On MethComp's `sbp` data, MethComp's `BA.est(random.raters = TRUE)` stops at a rater variance of zero, with a lower REML likelihood than this fit.
  - `tdi()` for random raters draws each variance component on its own df, as in these bounds (`method = "tolerance"` then reproduces the upper bound of the limits); `method = "delta"`, which needs one df for the SD, is an error for random raters. In the same simulations the default GPQ bound covered the true TDI 0.94 to 1.00 with 5 or 10 raters (0.93 to 1.00 with 3).
  - A random component (rater, tau or omega) estimated at zero (SD below 0.001 times the residual SD) no longer leaves the fit without confidence intervals and bounds, or with meaningless Wald intervals: the model is refit without it (same REML likelihood) for the covariance of the other components, and it is reported as 0 with interval 0 to `NA` (the rater SD: 0 to its upper limit). This applies to fixed methods too. In the random-raters simulations, 998 to 1000 of 1000 data sets per cell now have bounds (before: as few as 826).
  - New `sigma` argument: a "common" residual SD for all methods, or method-specific SDs ("method"). The default is "method" for fixed methods, as before, and "common" for random raters.
- Added `method_convert()` for conversion equations between methods, with prediction limits:
  - from a `method_vc()` fit (constant bias, `object = `);
  - `bias = "linear"`: regression of the differences on the averages for each pair, with constant or linear SD of the differences, and cluster-robust standard errors with replicates;
  - `bias = "scaled"`: the scaled model, fit by alternating regression (`estimator = "altreg"`, the default), which reproduces MethComp's `AltReg()` on the oximetry data, or by maximum likelihood of the structural model (`estimator = "ml"`, with the lavaan package, now in Suggests).
    - Maximum likelihood gives Wald intervals for the conversion intercepts, slopes, and prediction SDs, with t quantiles on (items - 1) df. In simulations with 15 to 60 items, 2 or 3 methods, linked or exchangeable replicates, missing measurements, and uniform or skewed true values, 95% intervals covered 92% to 97% (prediction SDs 92% to 95%). The summary reports the model's chi-square test of fit, RMSEA, and SRMR; zero or negative variance estimates give a warning.
    - The alternating regression gives delta-method (default) or parametric bootstrap (`ci_method = "boot"`) intervals. The delta-method intervals treat the predicted true values as known and are much too narrow (53% to 73% coverage of 95% intervals in the same simulations); the bootstrap has not been checked by simulation.
  - Methods: `print()`, `summary()`, `coef()`, `confint()`, `predict()` (back-transformed to the original scale), `plot()` (`type = "conversion"`, `"ba"`), and `check()`.
  - With `variance = "linear"`, a warning is given when the fitted SD of the differences is not positive over part of the observed range of the averages.
- The Bland-Altman plots and `check()` of `method_vc()` and `method_convert()` follow the style of `agreement_limit()` and `tolerance_limit()`:
  - With a constant bias, the bias and limits are drawn as points with their confidence intervals to the left of the data; with a bias that changes with the level, as coloured lines. `plot()` takes the `geom` and `delta` (maximal allowable difference) arguments of `plot.loa()`.
  - Each `check()` panel has a subtitle saying what to look for, and the tests are given in its caption: Shapiro-Wilk for normality, Breusch-Pagan for homogeneity of variance, Pitman-Morgan for proportional bias, and (linear conversions) a RESET test of linearity. Tests for pairs of methods use item means, so replicates are not counted more than once.
- Added `linked_reps_test()` (experimental): a permutation test of linked replicates for `method_vc()` fits, following MethComp's `perm.repl()`. The replicate labels are permuted within each item and method and the model refitted; the observed `omega` is compared with those of the permuted fits, and the limits of the permuted fits show how much the results depend on the linking. In simulations (two or three methods, 15 or 30 items, 2 or 3 replicates) it held its level (rejection rates at the attainable level with 49 permutations) and, for an occasion SD equal to the residual SD, had about 80% power with 30 items and 2 replicates or 15 items and 3 replicates, but only about 50% with 15 items and 2 replicates; smaller occasion effects are often missed with two methods. The limits of the permuted fits were up to 4% narrower than those of an exchangeable fit.
- Added `sim_methods()` to simulate method comparison data from the scaled variance component model, with the true limits, repeatability coefficients, and conversions in the `truth` attribute.
- Added `mc_long()` to convert data with one column per method to the long format.
- New vignettes: "The MethComp Approach, Part 1: Agreement from Variance Components" and "Part 2: Converting Between Methods", which credit Carstensen's approach, compare it with `agreement_limit()`/`tolerance_limit()`, and note the criticisms of Francq and colleagues.

## Bivariate least squares regression

- Added `bls_reg()` (experimental): bivariate least squares (BLS) regression of one method on the other and correlated BLS (CBLS) in the Bland-Altman plot (Francq & Govaerts, 2014, 2016), as in the BivRegBLS package. The measurement error variances are given (`var_x`, `var_y`, with optional df) or estimated from replicates (`id`), may differ between the methods, and the error variance of each subject's mean depends on its number of replicates (BLS is then fitted iteratively; with constant variances it is Deming regression). With only `error.ratio`, the estimates and confidence intervals are available but not the prediction intervals.
  - Exact standard errors and t-based confidence intervals; `predict()` gives confidence intervals, a simultaneous (Working-Hotelling) confidence band for the whole line (`interval = "band"`, consistent with `joint_test()`), and prediction intervals (for single measurements or means of `reps_x`/`reps_y` replicates, Satterthwaite df) in either plot (`type = "xy"` or `"md"`).
  - Error variances that change with the level: `var_x`/`var_y` can be functions of the level (e.g. a constant CV), or, with replicates, `var_model = "proportional"` or `"power"` estimates the variance function of each method. The variances are evaluated at the estimated true values, and the fit is repeated until stable.
  - Tolerance limits around the line (experimental): `predict(interval = "tolerance")` and `plot(interval = "tolerance")`, with `pred_level` (proportion of new measurements), `tol_level` (confidence), and `bound_type = "joint"` or `"iu"`, as in `tolerance_limit()`. Francq and Govaerts (2016) give tolerance intervals only for a constant bias; these are SimplyAgree's own extension. In simulations their confidence was 0.93 to 0.97 with known error variances, or with replicates and equation error; conservative (up to 0.99) with replicates and no equation error; and 0.875 with 10 subjects, known variances, and equation error.
  - Equation error (`equation_error = TRUE`, default): `summary()` reports the dispersion (1 when the measurement errors explain all the scatter about the line) with a test. When it is above 1, item-specific deviations of the true values from the line (method-by-item or matrix effects) are estimated as a common variance for both methods, as in `method_vc(tau = "common")`, included in the fit and added to the prediction intervals. In simulations with equation error, the prediction intervals then covered 94% to 96% (76% to 81% without it, as in BivRegBLS). The slope is unbiased when the deviations are shared equally; with two methods, how they divide between the methods cannot be estimated. `equation_error = FALSE` reproduces BivRegBLS, as does the default when the dispersion is at most 1.
  - In simulations (`references/general/cbls_sims/`), 95% intervals for the coefficients, the line, and new measurements covered 93% to 97% of the time, and the joint test held its size (4% to 6.5%), with known or replicate-estimated, constant or proportional error variances. Ignoring proportional error (`var_model = "constant"`) gave prediction intervals with 100% coverage at low levels and 81% at high levels.
  - Methods: `print()`, `summary()`, `plot()` (`type = "xy"`, `"md"`, with interval bands), `check()`, `fitted()`, `residuals()`; as a `simple_eiv` object also `coef()`, `vcov()`, `confint()`, `joint_test()` (Wald, `F(2, n - 2)`; Sadler's region does not apply) and `plot_joint()`.
  - The estimates, standard errors, and confidence and prediction intervals reproduce BivRegBLS (tests; BivRegBLS is now in Suggests). Differences: the joint test does not use BivRegBLS's Hotelling factor, and the CBLS prediction intervals include the covariance between the errors of a new difference and a new mean, which BivRegBLS omits. The differences are `x - y`, as in `agreement_limit()` (BivRegBLS uses `y - x`).
  - New section in `vignette("Deming")`; the MethComp vignettes point to it.
- `bls_reg()` accepts each subject's own error variance: `var_x`/`var_y` can name columns of the data, with `df_var_x`/`df_var_y` naming columns of their df (as BivRegBLS's `BLS.ht()`, which the tests reproduce). A new measurement gets the mean variance, with Satterthwaite df.
- `bls_reg(transform = "log")` fits the line to the logarithms (constant error variances on the log scale, i.e. error SDs proportional to the level). `predict()` and `plot()` back-transform to the original scale: `y` against `x`, and the ratio `x / y` against the geometric mean in the Bland-Altman plot (`back_transform = FALSE` keeps the log scale), as BivRegBLS's `antilog.pred()`.
- Added `error_ratio_profile()` (experimental) for `dem_reg()` and `bls_reg()` fits: refits the line for error ratios from 0 (least squares of y on x) to `Inf` (x on y), with BLS and exact (Creasy, 1956) intervals for the slope, intervals for the intercept, and the joint test of identity at each ratio, and the full confidence intervals over all ratios (Francq & Govaerts, 2014). For use when the error ratio is unknown. Reproduces BivRegBLS's `FullCIs.XY()`. Methods: `print()`, `plot()`.
- `method_convert(bias = "linear", estimator = "cbls")` fits each pair's regression of the differences on the averages by CBLS (via `bls_reg()`) instead of least squares, with the measurement errors estimated from exchangeable replicates; the least squares slope is biased when the methods' measurement errors differ. On the vignette data (true slope 1.2), CBLS gives 1.201, like the scaled model.
- The MethComp vignette (Part 1) now describes Francq and Govaerts' (2016) criticisms of Carstensen's approach accurately (they address his regression in the Bland-Altman plot and his treatment of replicate pairs directly), and notes the reverse point: BLS/CBLS as proposed ignore the method-by-item variation that Carstensen's model includes.

## Total deviation index

- Added `tdi()` (experimental; issue #82) for the results of `agreement_limit()`, `tolerance_limit()`, and `method_vc()`: the total deviation index (the value that a proportion `p` of the absolute differences do not exceed; Lin, 2000), computed exactly, with Lin's approximation for reference, and a one-sided upper confidence bound. With `delta`, also the coverage probability of `delta` with a lower bound, and the test of `H0: TDI >= delta`, which agrees with both bounds.
  - Bounds by generalized pivotal quantities (`method = "gpq"`, default), the delta method on the log scale (`"delta"`), or a one-sided tolerance bound for the quantile of the differences that defines the TDI (`"tolerance"`; Escaramis et al., 2010). The point estimates match TDIagree; its tolerance bound counts measurements rather than differences and is narrower, and the tests check that the same formula reproduces it with TDIagree's inputs. TDIagree and MethComp are now in Suggests.
  - `p` and the confidence level default to those of the object. Rows for `prop_bias`, conditions, and pairs of methods are all reported; the printed result is the row with the largest bound. On the log scale the TDI is reported as a ratio.
  - For replicated and nested data, the generalized pivotal quantities draw each component of the variance separately; `agreement_limit()` results now keep these components (`var_comp`). In simulations (10 to 50 subjects; bias of 0 to 1.5 SDs; independent, replicated, and nested data), 95% bounds covered the true TDI 94% to 99% of the time with `"gpq"`, 91% to 97% with `"delta"`, and 93% to 99% with `"tolerance"` (`references/general/tdi_sims/`).
  - When the degrees of freedom of the SD are unavailable (e.g., `method_vc()` with a variance component estimated at zero), the TDI is estimated without a bound, with a warning.
  - Methods: `print()`, `summary()` (`style`), and `plot()`.
- `agreement_limit()` output gains a `df_sd` column: the degrees of freedom of the SD of a single difference (Satterthwaite for `data_type = "reps"` and `"nest"`).

## Proportional bias diagnostics in `check()`

A slope of the differences on the means only shows that the two methods have unequal variances, since cov(x - y, x + y) = var(x) - var(y). This can come from unequal measurement error as well as from proportional bias (Bland & Altman, 1999; Hopkins, 2004). The proportional bias panel of `check()` for `agreement_limit()`, `tolerance_limit()`, and `method_vc()` now says what it tests and, where possible, corrects for the error variances:

- The test is labelled as the Pitman-Morgan test of equal variances. When it is significant with one measurement per method, `check()` gives a message that it cannot separate proportional bias from unequal measurement error.
- With replicates (`agreement_limit(data_type = "reps")`, `method_vc()` with replicates), the test uses subject means and is corrected for the measurement error variances estimated from the replicates (jackknife over subjects). Only their difference is used, so linked replicates do not bias it. The error-adjusted slope is drawn as a dashed line. In simulations with unequal error and no proportional bias, it rejected at about 5% to 6.5%.
- New `error.ratio` argument to `check()` for single-measurement designs: with a known error variance ratio, the Deming slope (as in `dem_reg()`) is tested against 1.
- New `bias_line` argument to `check()` chooses the dashed line drawn with the corrected test: `"adjusted"` (default), the error-adjusted slope, which should be flat without proportional bias and gives its size (`beta`); or `"expected"`, the slope expected from unequal measurement error alone, which the observed line should follow. The help pages ("Choosing `bias_line`" in `?check.loa`) and the agreement vignette describe the pros and cons of each. The panel now has a legend for the two lines.
- `check()` for `tolerance_limit()` now reports the Pitman-Morgan test (on subject means with `id`, by condition with `condition`).
- The results are attached to the output of `check()` as the attribute `"prop_bias"` (a data frame with the test, the observed and error-adjusted slopes, the implied scale `beta` of one method relative to the other, and the within-subject variances), so they can be reported.
- The "Proportional Bias" section of the agreement vignette now explains how to read the plot and the test, and what to do with the result for each design.

Results that change:

- `check()` for `agreement_limit(data_type = "reps")` previously tested the slope with a row-level mixed model, which pairs exchangeable replicates arbitrarily; with unequal measurement error and no proportional bias it rejected about 40% of the time at the 5% level.
- With `log_tf = TRUE`, the test now uses the mean on the log scale (it used the mean on the original scale), so it tests equal variances on the analysis scale. The `prop_bias` models are unchanged.
- The proportional bias panel shows the centred differences, not the residuals of the fitted model, so it no longer looks flat when `prop_bias = TRUE`.

Bug fixes:

- `check()` for `agreement_limit(prop_bias = TRUE)` reported p = NA for proportional bias.
- `check()` for `tolerance_limit(keep_model = FALSE)` failed; it now draws the proportional bias panel and notes that the other panels need the model.

## Model equations

- Added `model_equation()` (experimental), which writes the statistical model behind a result as LaTeX: the mean model, the distributions of the random effects and residuals, and any variance function or correlation structure.
  - Methods for `tolerance_limit()`, `icc_test()`, `reli_err_test()`, `mdc_test()`, `reli_facet_test()`, `reli_stats()`, `reli_aov()`, `method_vc()` and `method_convert()` results, using the notation of the vignettes, and for `lm`, `lmer`, `gls` and `lme` models.
  - Symbols by default; `use_coefs = TRUE` substitutes the estimates (`coef_digits` sets the decimals).
  - Prints the LaTeX code at the console and renders as display math in R Markdown and Quarto. No new dependencies.

## Non-parametric limits of agreement

- `agree_np()` and its `plot()` could run without end on data with many exact ties (for example, the data of GitHub issue #50), depending on the random number state. The bootstrap standard errors of the quantile regressions came from quantreg's `summary(se = "boot")`, whose simplex fits can cycle on such resamples. The bootstrap now uses the same resampling of (x, y) pairs (200 resamples) but fits each resample with the Frisch-Newton interior point method (`quantreg::rq.fit.fnb()`), which always ends. The estimates are unchanged; the standard errors agree with quantreg's bootstrap up to Monte Carlo error.
- Results that change: with `prop_bias = FALSE` and `TOST = TRUE` (the default), the confidence bounds of the limits of agreement were computed at `conf.level` (e.g. 95%) although they were reported, and printed, as `1 - 2 * (1 - conf.level)` (e.g. 90%). They are now computed at the reported level, so they are narrower than before. The bias bounds, the hypothesis test, `TOST = FALSE` and `prop_bias = TRUE` are unchanged.
- The `plot()` of `agree_np()` now shows the printed limits and bounds. It used to refit the quantile regressions with the mean as a covariate whatever `prop_bias` was (so with `prop_bias = FALSE` it drew sloped limits instead of the constant ones), drew the bands of the limits at `conf.level` even with `TOST = TRUE` (where the printed bounds, and the plot caption, use `1 - 2 * (1 - conf.level)`), and drew a new bootstrap. The plot now uses the fitted model and the bootstrap covariance stored in the result (new element `qr_vcov`).

# SimplyAgree 0.4.1 (development version)

## Bug fixes

- **Nested limits of agreement** (`agreement_limit(data_type = "nest")`): the confidence bounds of the limits were too narrow. The bias variance and the variance piece with n - 1 degrees of freedom used the between-subject variance alone, rather than the variance of the subject mean differences (between + within / m_h; Zou, 2011), so the within-subject part of it got no uncertainty. In simulations (10 to 40 subjects, 2 to 5 pairs each), the default MOVER bounds covered 85% to 94% of the time instead of 95%, worst when the within-subject variance dominates; after the fix, 94% to 97%. The bounds now reproduce Zou's published example (and `agree_nest()`) closely. The limits themselves and the bias are unchanged. (`agree_nest()` with `prop_bias = TRUE`, superseded, has the same substitution and is unchanged.)

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
