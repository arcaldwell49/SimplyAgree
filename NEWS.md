# NEWS

## v0.1.0

- Updated CV calculations for `reli_stats` with the cv_calc argument
  - Options now included CV for the model residuals, mean-squared error (MSE), or for the standard error of measurement (SEM)
- Updated plots to include the `delta` argument on plot if specified, and estimate of LoA
- Line-of-identity plot now includes trend line from Deming regression rather than OLS
- Added agreement coefficient function (`agree_coef`).
- Assumptions checks are added for all "simple-agree" class results
  - Checks include: normality, heteroscedasticity, and proportional bias


## v0.0.3
- Fixed error in `agree_nest` and `agree_reps`; now properly handles missing values
- Remove dependencies on sjstats and cccrm packages

## v0.0.2
- Fixes typos in jamovi/jmv functions
- Adds more descriptive errors to jamovi output
- Remove dontrun from examples in documentation
- Add more details and references to the package's functions
