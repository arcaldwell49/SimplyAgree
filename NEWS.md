# NEWS

# SimplyAgree 0.4.0

## Reliability

- New `icc_test()`: one intraclass correlation coefficient (ICC), chosen with `model`, `type` and `unit` (McGraw & Wong forms, with the Shrout & Fleiss names), returned as an `htest`. Two-sided F-based intervals, optional F test against `null.value` (default `NULL`: estimate and interval only, since testing against an ICC of 0 is rarely informative), `method = "reml"` for missing data (observed-data degrees of freedom, with a warning), and an optional subject bootstrap (`boot = TRUE`) whose interval type and standard error were chosen from simulations.
- New `reli_err_test()`: the standard error of measurement (SEM), of the estimate (SEE) or of prediction (SEP), with intervals and an optional test against an acceptable error. `scale = "log"` (sympercents) and `scale = "mean"` give coefficients of variation.
- New `mdc_test()`: the minimal detectable change (MDC, also SDD/SRD or repeatability coefficient), 1.96 × √2 × SEM by default, with its interval (the SEM interval, scaled) and an optional test against a change that matters. It works from the data or from an existing `reli_err_test()` SEM, and offers one-sided MDCs (`sided`), scores averaged over several trials (`k_avg`), and percentage increases and decreases on the log scale. The documentation explains that the MDC is an individual threshold (not for group effects or counting "responders") and why no "smallest worthwhile change" is computed from the reliability data.
- New `power_icc()` and `precision_icc()`: power and sample size for the ICC F test, and sample size for an ICC confidence interval of a given width with a given assurance (exact F-based calculations for the one-way and consistency ICCs, single or average measures (`unit`), plus the Walter et al. and Zou approximations). With `missing`, the expected proportion of missing measurements, they give an approximate calculation for a REML analysis of incomplete data, checked by simulation up to 30% missing. The documentation explains how to use them as a guide for agreement ICCs (an optimistic lower bound on the sample size), missing data and non-normal data.
- New `summary()`, `plot()` and `check()` methods for these results. `summary(style = "estimation")` describes the interval as a compatibility interval and converts any p-value to an S-value; `style = "hypothesis"` gives the usual test decision. `plot()` shows the subjects across items with the item means and 95% CIs, or, with `type`, the subjects ordered by their mean (`"subjects"`), deviations from the subject mean by item (`"items"`), or each pair of items against the line of identity (`"pairs"`). `check()` adds an additivity panel with Mandel's test for non-additivity (items that differ in scale; built from the fitted additive model as in Milliken & Graybill, so it also works with missing measurements) and the within-subject SD against the subject mean, to show whether the error grows with the mean.
- `reli_stats()` and `reli_aov()` are superseded by these functions. They still work and keep their output structure, but now use the same engine, which fixes several problems. **Results change:**
  - **The ICC confidence intervals are now two-sided.** They previously used `conf.level` in each tail (`qf(1 - alpha)`), so a "95%" interval was really a 90% interval (each limit was a one-sided 95% bound). The intervals now match Shrout & Fleiss (1979), McGraw & Wong (1996), psych, irr, SPSS and Stata. To reproduce the old numbers at `conf.level = c`, use `conf.level = 1 - 2 * (1 - c)` (e.g. old 0.95 = new 0.90). For a one-sided lower bound on an ICC, use `icc_test(alternative = "greater")`.
  - `reli_stats()` with missing data now uses degrees of freedom based on the observed data (and warns that the intervals are approximate), instead of complete-data degrees of freedom.
  - `reli_aov()` now drops every subject not measured on every item (including subjects with a missing row in long data, which were previously analysed with the wrong degrees of freedom), with a message.
  - The variance components of `reli_aov()` now use the ANOVA estimates (the item variance was previously MSW - MSE).
  - With `other_ci = TRUE` and `type = "chisq"`, the intervals for the SEE, SEP, ICC-based SEM and CV now use the delta method (log scale); the chi-square interval with the error degrees of freedom and Vangel's CV interval did not apply to these statistics. The SEM from the error mean square keeps its exact chi-square interval.
  - The bootstrap intervals (`type = "perc"`, `"norm"` or `"basic"`) now resample subjects in both functions (`reli_stats()` previously used a parametric bootstrap).
  - `cv_calc = "residuals"` (a downward-biased version of `"MSE"`) is replaced by `"MSE"` with a warning.
  - `print()` no longer fails when `conf.level` or `other_ci` is passed as a variable.
  - Long data with more than one measurement per subject and item is now an error (aggregate first).
- The jamovi reliability analysis now uses `icc_test()` and `reli_err_test()`: two-sided ICC intervals, intervals for the SEM, SEE and SEP (the SEE and SEP now use the SD matched to the ICC's denominator), the CV in sympercents, and REML when measurements are missing.
- The power and sample size vignette has a new section on planning reliability studies with `power_icc()` and `precision_icc()` (tests and interval widths, trading subjects for trials, average measures, missing measurements, and using the calculations as a guide for agreement ICCs).

## Categorical ratings

- New `cac_test()`: one chance-corrected agreement coefficient for categorical ratings (Gwet's AC1/AC2, Fleiss' kappa, Krippendorff's alpha, or percent agreement), returned as an `htest` with an optional t test against `null.value`. It takes the same formula, long and wide interfaces as `icc_test()`, accepts numeric, character or factor ratings, and offers all of Gwet's weighting schemes (`"quadratic"`, `"linear"`, `"ordinal"`, `"radical"`, `"ratio"`, `"circular"`, `"bipolar"`) or a custom weight matrix. Results match the irrCAC package.
- New `ord_kappa_test()`: the model-based kappa of Nelson & Edwards (2015) and Nelson, Mitani & Edwards (2017) for ordinal ratings from many raters (#63). It is computed from a cumulative probit mixed model with crossed subject and rater random effects (fitted with `ordinal::clmm()`; `ordinal` is now in Suggests), is not affected by category prevalence, and returns the rater effects (conditional modes). The interval is formed on the logit of the subject variance share, so it stays within [0, 1]. With `group`, it compares two groups of raters (e.g. experienced and inexperienced) or subjects (e.g. younger and older patients): the group is a fixed effect with its own rater (or subject) variance, and the result gives kappa_m in each group, an interval for their difference, the kappa between a rater from each group, and a likelihood ratio test of equal variances. The model is the identified form of the random-slope model of Nelson et al. (2017), whose intercept-slope covariance is not identified when the group is constant within raters or subjects.
- New `summary()` and `plot()` methods for both: category use by rater, ratings by subject, and (for `ord_kappa_test()`) rater effects.
- New vignette, "Agreement for Categorical and Ordinal Ratings", covering how to choose a coefficient (including the kappa paradox), weights for ordinal scales, the rating scale, the model-based kappa, rater effects, and group comparisons. The inter-rater section of the reliability vignette now points to it.
- `agree_coef()` is superseded by `cac_test()`. It keeps its output but now uses the same engine, which fixes several problems:
  - It no longer fails when all ratings are in one category (the chance-corrected coefficients are returned as `NA`, with a warning).
  - New `categories` argument for scale points that were never used. Gwet's AC1/AC2 and the weights depend on the full set of categories, so they were wrong when a category was unused.
  - New `weights` argument for the other weighting schemes (`weighted = TRUE` still means quadratic weights).
  - Long data with more than one rating per subject and rater is now an error; the extra ratings were silently dropped.
  - Lower confidence limits are clipped to the possible range of the coefficient (they were only clipped at 1).
  - The row name "Kririppendorff's Alpha" is now "Krippendorff's Alpha".

## Agreement power and sample size

- `power_agreement_exact()`, `agree_expected_half()`, `agree_assurance()` and `blandPowerCurve()` gain `m` (paired measurements per subject) and `icc` (the ICC of the paired *differences*) for clustered data; `n` is then the number of subjects, and the output gains `m`, `icc` and `n.obs`. The exact methods integrate over the exact distribution of the variance estimate under a balanced compound symmetry model (checked against simulation); `blandPowerCurve()` uses a Satterthwaite approximation. With `m = 1` (the default) the results are unchanged.
- The power and sample size vignette now uses these arguments. The design effect it previously recommended, 1 + (m - 1) × ICC applied to the whole sample size, is correct for the bias but overstates the penalty for the SD, so it was conservative; the vignette also now makes clear that the ICC is that of the differences.
- `find_n()` now returns the smallest sample size at which the target power is reached. It previously returned the sample size with the *closest* power, which could fall just below the target (**results can change by one**), and it failed when several sample sizes tied. If the target is not reached, `N` is `NA` with a warning.
- Fixed the sample size searches:
  - `power_agreement_exact()` could not return fewer than 6 pairs, and `agree_expected_half()` and `agree_assurance()` fewer than 5.
  - `power_agreement_exact()` silently returned the upper search limit (10000) when power could not be reached. It now stops when the alternative proportion `p1_star` does not exceed `p0_star`, and warns when the limit is reached.
- `agree_assurance()` capped `omega` at 10 × `sigma` when solving for it, so with small `n` or high assurance it could return an `omega` with assurance below the target. `omega` now has a closed form.
- `power_agreement_exact()` used absolute tolerances when solving for `delta` or `sigma`, which failed for data on a small scale; they are now relative.
- The tolerance factor in `agree_expected_half()` and `agree_assurance()` hung with `n = 2` and was inaccurate for very small `n` (e.g., 24.96 instead of 30.48 for `n = 3` with 99% confidence and coverage, which gave 98.5% rather than 99% confidence in simulation). The integration grid is now accurate for any `n`, the search has an iteration limit and expands its bracket, and the three functions share one implementation. Results for `n >= 5` are unchanged to about 4 significant digits, and the published examples are unchanged.
- `agree_expected_half()` and `agree_assurance()` validate `n`, and return a `note`. Their documented return names now match the object.
- The documentation of `power_agreement_exact()` no longer says the power "roughly" matches tolerance limits. It is the power of Shieh's test; deciding agreement by whether `tolerance_limit()` limits fall within the bounds is less powerful (about 0.66 in simulation at a planned 0.80).

## Other changes

- Fixed error in the jamovi module (and any other use where the analysis is saved and reloaded) where plots failed with "object 'y' not found" (#76).
  - The data needed by the `plot` and `check` methods are now stored in the returned object itself, rather than as a formula that pointed back at the environment the analysis was run in.
- Fixed `plot()` and `print()` failing for `agree_test()`, `agree_reps()`, `agree_nest()`, and `agree_np()` when `delta` (or `conf.level`, `agree.level`, `TOST`, `prop_bias`) was supplied as an expression (e.g., `delta = log(1.10)`) or from inside another function (#50). The stored call now holds the evaluated values of these settings rather than the code as typed; the data arguments are still not copied into the object.
- Fixed `tolerance_limit()` prediction and tolerance limits when the model has a variance function (from `condition` or a user-supplied `weights`). The limits previously used `sigma(model)`, which is only the residual SD at the reference level of the variance function, so they were too narrow for other conditions or covariate values. The residual SD is now evaluated for each row of the limits (funnily caught when reviewing another manuscript!).
  - `limits` gains an `SD` column with the residual SD used for each row.
  - If the variance depends on `avg` (e.g., `weights = nlme::varPower(form = ~avg)`), the limits are reported at the minimum, median, and maximum of `avg`, even when `prop_bias = FALSE`.
- The `model` returned by `tolerance_limit()` is now self-contained: its call stores the data, correlation structure, and variance function, and calls `nlme::gls`. `update()`, `nlme::getData()`, and `emmeans()` now work on it outside of the function (previously they failed with errors like "object 'var1' not found").
- Renamed the `tol_method` options of `tolerance_limit()`: `"approx"` is now `"analytic"` (the default) and `"perc"` is now `"boot_cal"`. The old names still work but are deprecated.
- Fixed the parametric bootstrap used by `tolerance_limit(tol_method = "boot_cal")` (previously `"perc"`). **Results for the bootstrap will change.**
  - Replicates are now simulated from the point estimates of the fitted model. Previously the coefficients were also drawn from their sampling distribution before adding residual noise, which roughly doubled the sampling variance of the bootstrapped bias and made the tolerance limits too wide.
  - The simulated residuals now have the fitted covariance. Previously the within-subject correlation blocks were placed assuming each subject's rows were contiguous (so data not sorted by `id` got correlation between the wrong rows), and the Cholesky factor was applied in the wrong orientation.
  - The bootstrap is about 5 times faster: the covariance factorization is computed once, the bias and its standard error are computed directly from each refit instead of calling `emmeans`, and results are no longer accumulated with `rbind()`.
  - `MASS` and `Matrix` are no longer imported.
- The tolerance limits from `tolerance_limit()` now have a clearly defined target, set by the new `bound_type` argument, and both `tol_method` options estimate the same thing. **Tolerance limits (`lower.TL`/`upper.TL`) will change**; prediction limits do not.
  - `bound_type = "joint"` (default): a beta-content, gamma-confidence tolerance interval (with confidence `tol_level`, at least `pred_level` of differences lie within the limits). The analytic limits use Howe's approximation (unchanged for independent data). Previously the bootstrap took quantiles of the bootstrapped prediction limits, which targeted a different quantity.
  - `bound_type = "iu"`: a one-sided `tol_level` bound on each of the lower and upper (1 -/+ `pred_level`)/2 percentiles, for an intersection-union test against a maximal allowable difference. The analytic bounds use the exact noncentral t for independent data and the MOVER otherwise.
  - For clustered data, the analytic limits previously used the degrees of freedom of the bias for the residual variance, which was very conservative. The upper confidence bound for the residual SD now combines the between- and within-subject variance components with the MOVER under compound symmetry, and uses an effective degrees of freedom for other correlation structures.
  - `"boot_cal"` is a bootstrap calibration of the analytic limits: the analytic limits are computed in each replicate over a range of nominal levels, and the level at which the replicates reach the target confidence is used (returned as `lower.TL.level`/`upper.TL.level`). Calibrating the raw bootstrap limits instead was liberal with few subjects, because the estimated correlation is treated as known.
  - With `condition` and compound symmetry, the variance components of each condition are bounded using that condition's cluster sizes and degrees of freedom (using those of the whole data set was liberal, about 0.93 coverage for a 95% target).
  - In simulations (95% target, 10 to 20 subjects), the analytic limits had coverage of about 0.95 to 0.965 with compound symmetry (with or without `condition`) and AR(1) correlation. `"boot_cal"` is marked experimental: treating the estimated correlation as known, its coverage ranged from about 0.93 to 0.96 with 10 to 15 subjects, so the analytic limits are recommended.
  - `limits` gains `SD.df` and `SD.upper` columns, and the printed output states what the tolerance limits claim.
- Added a `model` argument to `tolerance_limit()`. `model = "lme"` fits a linear mixed model (`nlme::lme`) with a random intercept for each `id` (which is then required), instead of the default marginal `gls` model.
  - With compound symmetry and no variance function it gives the same fit and limits as `"gls"`.
  - With `cor_type = "ar1"` or `"car1"`, the serial correlation is added to the residuals on top of the random intercept, so a persistent subject effect is kept. In simulations with a random subject effect and AR(1) residuals, the joint tolerance limits had coverage of about 0.95 with `"lme"` versus about 0.89 with `"gls"` (95% target).
  - With `condition` or `weights`, the variance function applies to the residuals only, so the between-subject variance is common to all conditions.
  - The degrees of freedom for the bias are the containment degrees of freedom, because emmeans' Satterthwaite approximation can fail for `lme` models.
  - The printed output names the model that was fit.
  - With `model = "lme"`, `id` can name two nested levels, outer level first (e.g., `id = c("golfer", "club")`), for nested random intercepts. The tolerance limits combine the three variance components (outer, inner, and residual) with the MOVER, and `limits` gains an `SD.nested` column. In simulations, using the inner level alone as `id` gave limits that were too narrow (about 0.88 to 0.91 coverage for a 95% target), while nested random intercepts had about 0.96.
- `print()` for `tolerance_limit()` results now reports the SD of the differences (coefficient of variation when log-transformed), as `agreement_limit()` does. If the SD differs between rows (e.g., by `condition`), it is shown as an `SD` column instead. Clustered models also print the between- and within-subject SDs.
- `print()` for `agreement_limit(data_type = "reps")` results now reports the within-subject SDs of x and y. They were computed (as variances) but never printed.
- Fixed the pooled within-subject variances in `agreement_limit(data_type = "reps")` for unbalanced data. A subject with only one measurement of a method (or none) was still counted in that method's degrees of freedom, which biased the variance (and so the SD of the differences and the LoA) low. A subject with a single measurement of one method also had its replicates of the other method dropped. Each method is now pooled over its own replicates (the one-way ANOVA residual mean square), and the MOVER and Bland-Altman confidence bounds use those degrees of freedom. Results for balanced data are unchanged.
- Fixed `predict_varFunc()` (used internally by `tolerance_limit()`) returning `NA` residual SDs for some groups when the data were a tibble.
- `tolerance_limit()` now reports the between- and within-subject components of the residual SD (`SD.between`, `SD.within`) under compound symmetry.
- Documented the model assumptions of `tolerance_limit()`: autoregressive correlation (`"ar1"`, `"car1"`) has no persistent subject effect and can understate the uncertainty in the bias; with compound symmetry, variance functions also scale the between-subject covariance; only one level of clustering is supported; and a proportional bias slope can arise from unequal measurement error variances alone (also noted for `agreement_limit()`).
- Fixed `agreement_limit(data_type = "nest")` counting rows with a missing measurement, and dropping subjects with a single measurement, when computing the number of measurements, the number of subjects, and the harmonic mean cluster size used for the confidence bounds of the limits of agreement. These now match the data used to fit the mixed model. Results change only when the data have missing measurements or subjects with a single measurement.
- Corrected the formulas for the limits of agreement in the "Agreement & Tolerance Limits" vignette to match the implementation (notably for `data_type = "reps"`, which uses separate within-subject variances for `x` and `y`).
- Added a `bound_type` argument to `agreement_limit()`. The default, `"iu"`, keeps the existing results: each confidence bound of the limits of agreement is a one-sided 1 - `alpha` bound (the outer end of a 1 - 2 * `alpha` interval), for an intersection-union test against a maximal allowable difference. `"joint"` uses 1 - `alpha`/2 per side (the outer ends of 1 - `alpha` intervals, the Bland-Altman convention), giving joint confidence for both limits. The printed output and plot caption now say which is shown.
- Added a `conf_level` argument to `tolerance_limit()` for the confidence interval of the bias (`lower.CL`/`upper.CL`), which was previously fixed at 95%. The level is shown in the printed output.
- Corrected the documentation of the `error.ratio` argument in `dem_reg`. The documentation previously described it as the ratio of error variances y/x, but `dem_reg` has always computed (and, with replicate data via `id`, estimated) the error ratio as var(x)/var(y), consistent with `pb_reg`. The documentation, the Deming vignette (including the coefficient-of-variation formula for the error ratio), and the jamovi module now consistently state var(x)/var(y). The computation is unchanged, so results are unaffected for users who supplied var(x)/var(y) or who estimated the ratio from replicates. Anyone who supplied a y/x ratio following the old documentation should invert it (use `1/error.ratio`) and refit. Thanks to Hugo Lachuer for reporting this.
- Fixed `plot(..., smooth_method = "gam")` raising an error instead of an informative message when the suggested `ggeffects` package is not installed (only `mgcv` was being checked). The Deming vignette now also builds when the suggested `deming` package is unavailable.
- Added `ccc_test()` for the concordance correlation coefficient (CCC), returning an `htest` object (class `ccc_htest`) with a Fisher-z confidence interval and, when `null.value` is given, a z test. As in `icc_test()`, no test is done by default, a one-sided alternative gives a one-sided interval, and `summary()` (hypothesis or estimation style, with Lin's precision and accuracy components for simple data), `plot()` (identity or Bland-Altman plot) and `check()` (normality and heteroskedasticity of the differences, linearity with a RESET test, and bivariate normality for simple data) methods are provided.
- Fixed the concordance correlation coefficient for replicate and nested data (`ccc_test()`, and the CCC of `agree_reps()` and `agree_nest()`). **Results change:**
  - Replicate data (`data_type = "reps"`, `agree_reps()`): with a missing value, the old U-statistic code paired the remaining measurements of the two methods out of order (and recycled values), so the CCC was wrong. The CCC is now estimated from variance components (Carrasco & Jover, 2003), which uses every measurement and needs no pairing of replicates; it matches `cccrm::ccc_vc(int = TRUE)`.
  - Nested data (`data_type = "nest"`, `agree_nest()`): the estimate (Lin's CCC over all complete pairs) is unchanged.
  - For both, the old standard error treated every pair as an independent subject and was too small, so the intervals were too narrow. The standard error is now a delete-one-subject jackknife, and the interval and test use a t distribution with (number of subjects - 1) degrees of freedom (the statistic is reported as `t` with `df`).
  - The CCC for simple data is unchanged; the documentation now notes that Lin's estimator uses n (not n - 1) in the variances, as in DescTools and epiR.
- Fixed the Passing-Bablok (`pb_reg`) bootstrap. The previous wild bootstrap perturbed the observed values rather than the fitted values, so resampled points never crossed the regression line; standard errors were roughly 4-5 times too small and joint tests rejected a true null hypothesis 80-95% of the time in simulations. `pb_reg` now uses a nonparametric pairs (case) bootstrap by default.
- Added a `se_method` argument to `pb_reg`: `"bootstrap"` (pairs bootstrap, default), `"jackknife"` (delete-d jackknife with d = n/2; Shao & Wu, 1989), and `"dufey"` (experimental; analytic variance-covariance matrix for the scissors estimator; Dufey, 2020). Standard errors in the model table are now taken from the variance-covariance matrix when one is available.
- Fixed the tie correction in the analytic `pb_reg` slope confidence interval. The correction for tied x values was subtracted after taking the square root of the variance of Kendall's S rather than inside it, which made intervals too narrow when x contained ties (simulated coverage fell to 72% at n = 80 with rounded data; now ~95%). The same tie-corrected variance is now also used for `ci_slopes`.
- Fixed `pb_reg` reporting `reject_h0 = TRUE` when a confidence interval collapsed onto the null value (common with coarsely rounded data), due to floating-point error in `tan(pi/4)`.
- Fixed `joint_test` and `plot_joint` to use the finite-sample F(2, n-2) reference distribution (Sadler, 2010) instead of the asymptotic chi-squared(2) approximation. The previous chi-squared approach was anti-conservative for small samples. A `test_method` argument allows selecting `"F"` (default) or `"asymptotic"` (old behavior).

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
