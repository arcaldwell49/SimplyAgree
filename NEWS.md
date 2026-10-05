# NEWS

# SimplyAgree 0.3.1

- Corrected the documentation of the `error.ratio` argument in `dem_reg`. The documentation previously described it as the ratio of error variances y/x, but `dem_reg` has always computed (and, with replicate data via `id`, estimated) the error ratio as var(x)/var(y), consistent with `pb_reg`. The documentation, the Deming vignette (including the coefficient-of-variation formula for the error ratio), and the jamovi module now consistently state var(x)/var(y). The computation is unchanged, so results are unaffected for users who supplied var(x)/var(y) or who estimated the ratio from replicates. Anyone who supplied a y/x ratio following the old documentation should invert it (use `1/error.ratio`) and refit. Thanks to Hugo Lachuer for reporting this.
- Added `ccc_test` function for concordance correlation coefficient hypothesis testing, returning an `htest` object.
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
