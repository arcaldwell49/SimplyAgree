# NEWS

# SimplyAgree 0.3.1

- Fixed error in the jamovi module (and any other use where the analysis is saved and reloaded) where plots failed with "object 'y' not found" (#76).
  - The data needed by the `plot` and `check` methods are now stored in the returned object itself, rather than as a formula that pointed back at the environment the analysis was run in.
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
  - In simulations (95% target), the analytic limits were close to nominal for independent data, compound symmetry, and AR(1) correlation, but somewhat liberal (about 0.93) with a variance function (`condition` or `weights`) combined with compound symmetry; `"boot_cal"` corrected this (about 0.95 to 0.96). With only 10 subjects, `"boot_cal"` joint limits were about 0.93, so the analytic limits remain the default.
  - `limits` gains `SD.df` and `SD.upper` columns, and the printed output states what the tolerance limits claim.
- `tolerance_limit()` now reports the between- and within-subject components of the residual SD (`SD.between`, `SD.within`) under compound symmetry.
- Documented the model assumptions of `tolerance_limit()`: autoregressive correlation (`"ar1"`, `"car1"`) has no persistent subject effect and can understate the uncertainty in the bias; with compound symmetry, variance functions also scale the between-subject covariance; only one level of clustering is supported; and a proportional bias slope can arise from unequal measurement error variances alone (also noted for `agreement_limit()`).
- Added a `bound_type` argument to `agreement_limit()`. The default, `"iu"`, keeps the existing results: each confidence bound of the limits of agreement is a one-sided 1 - `alpha` bound (the outer end of a 1 - 2 * `alpha` interval), for an intersection-union test against a maximal allowable difference. `"joint"` uses 1 - `alpha`/2 per side (the outer ends of 1 - `alpha` intervals, the Bland-Altman convention), giving joint confidence for both limits. The printed output and plot caption now say which is shown.
- Added a `conf_level` argument to `tolerance_limit()` for the confidence interval of the bias (`lower.CL`/`upper.CL`), which was previously fixed at 95%. The level is shown in the printed output.

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
