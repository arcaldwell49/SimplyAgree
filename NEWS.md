# NEWS

# SimplyAgree 0.3.1

- Fixed error in the jamovi module (and any other use where the analysis is saved and reloaded) where plots failed with "object 'y' not found" (#76).
  - The data needed by the `plot` and `check` methods are now stored in the returned object itself, rather than as a formula that pointed back at the environment the analysis was run in.
- Fixed `tolerance_limit()` prediction and tolerance limits when the model has a variance function (from `condition` or a user-supplied `weights`). The limits previously used `sigma(model)`, which is only the residual SD at the reference level of the variance function, so they were too narrow for other conditions or covariate values. The residual SD is now evaluated for each row of the limits (funnily caught when reviewing another manuscript!).
  - `limits` gains an `SD` column with the residual SD used for each row.
  - If the variance depends on `avg` (e.g., `weights = nlme::varPower(form = ~avg)`), the limits are reported at the minimum, median, and maximum of `avg`, even when `prop_bias = FALSE`.
- The `model` returned by `tolerance_limit()` is now self-contained: its call stores the data, correlation structure, and variance function, and calls `nlme::gls`. `update()`, `nlme::getData()`, and `emmeans()` now work on it outside of the function (previously they failed with errors like "object 'var1' not found").
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
