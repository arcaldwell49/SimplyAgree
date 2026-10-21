# Passing-Bablok Regression for Method Comparison

**\[experimental\]**

A robust, nonparametric method for fitting a straight line to
two-dimensional data where both variables (X and Y) are measured with
error. Particularly useful for method comparison studies.

## Usage

``` r
pb_reg(
  formula,
  data,
  id = NULL,
  method = c("scissors", "symmetric", "invariant"),
  conf.level = 0.95,
  weights = NULL,
  error.ratio = 1,
  replicates = 0,
  se_method = c("bootstrap", "jackknife", "dufey"),
  model = TRUE,
  keep_data = TRUE,
  ...
)
```

## Arguments

- formula:

  A formula of the form `y ~ x` specifying the model.

- data:

  Data frame with all data.

- id:

  Column with subject identifier (optional). If provided, replicate
  measurements are averaged within each subject before fitting.

- method:

  Method for Passing-Bablok estimation. Options are:

  - "scissors": Scissors estimator (1988) - most robust, scale invariant
    (default)

  - "symmetric": Original Passing-Bablok (1983) - symmetric around
    45-degree line

  - "invariant": Scale-invariant method (1984) - adaptive reference line

- conf.level:

  The confidence level required. Default is 95%.

- weights:

  An optional vector of case weights to be used in the fitting process.
  Should be NULL or a numeric vector.

- error.ratio:

  Must be 1 (the default). Values other than 1 are no longer supported
  and give an error; see "Measurement Error" in Details. Use
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md)
  when the error variances of x and y differ.

- replicates:

  Number of resamples for confidence intervals and the
  variance-covariance matrix. For `se_method = "bootstrap"` this is the
  number of bootstrap resamples; for `se_method = "jackknife"` it is the
  number of random delete-d subsets. If 0 (default), analytical
  confidence intervals are used and no variance-covariance matrix is
  returned (unless `se_method = "dufey"`). Resampling is recommended for
  weighted data and 'invariant' or 'scissors' methods.

- se_method:

  Method used to estimate the variance-covariance matrix and confidence
  intervals of the coefficients. Options are:

  - "bootstrap": Nonparametric pairs (case) bootstrap with percentile
    confidence intervals (default). Used when `replicates > 0`.

  - "jackknife": Delete-d jackknife with d = floor(n/2), using
    `replicates` random subsets (Shao & Wu, 1989). Used when
    `replicates > 0`.

  - "dufey": **\[experimental\]** Analytic sandwich estimator of Dufey
    (2020). Does not require resampling (`replicates` is ignored). Only
    available for the "scissors" method without case weights. Not used
    by
    [`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
    or
    [`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
    (see Details).

- model:

  Logical. If TRUE (default), the model frame is stored in the returned
  object. This is needed for methods like
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html), and
  [`predict()`](https://rdrr.io/r/stats/predict.html) to work without
  supplying `data`. If FALSE, the model frame is not stored (saves
  memory for large datasets), but these methods will require a `data`
  argument.

- keep_data:

  Logical indicator (TRUE/FALSE). If TRUE, intermediate calculations are
  returned; default is FALSE.

- ...:

  Additional arguments (currently unused).

## Value

The function returns a simple_eiv object with the following components:

- `coefficients`: Named vector of coefficients (intercept and slope).

- `residuals`: Residuals from the fitted model.

- `fitted.values`: Predicted Y values.

- `model_table`: Data frame presenting the full results from the
  Passing-Bablok regression.

- `vcov`: Variance-covariance matrix for slope and intercept (if
  resampling or `se_method = "dufey"` is used; otherwise NULL).

- `df.residual`: Residual degrees of freedom.

- `call`: The matched call.

- `terms`: The terms object used.

- `model`: The model frame.

- `x_vals`: Original x values used in fitting.

- `y_vals`: Original y values used in fitting.

- `weights`: Case weights (if provided).

- `error.ratio`: Always 1 (kept for compatibility).

- `conf.level`: Confidence level used.

- `method`: Character string describing the method.

- `method_num`: Numeric method identifier (1, 2, or 3).

- `kendall_test`: Results of Kendall's tau correlation test.

- `cusum_test`: Results of CUSUM linearity test.

- `n_slopes`: Number of slopes used in estimation.

- `boot`: Resampling results (if replicates \> 0 and `se_method` is
  "bootstrap" or "jackknife").

- `se_method`: Method actually used for standard errors ("analytic",
  "bootstrap", "jackknife", or "dufey").

## Details

Passing-Bablok regression is a robust nonparametric method that
estimates the slope as the shifted median of all possible slopes between
pairs of points. The intercept is then calculated as the median of y -
slope\*x. This method is particularly useful when:

- Both X and Y are measured with error

- You want a robust method not sensitive to outliers

- The relationship is assumed to be linear

- X and Y are highly positively correlated

### Methods

Three Passing-Bablok methods are available:

**"scissors"** (default): The scissors estimator (1988), most robust and
scale-invariant. Uses the median of absolute values of angles.

**"symmetric"**: The original method (1983), symmetric about the y = x
line. Uses the line y = -x as the reference for partitioning points.

**"invariant"**: Scale-invariant method (1984). First finds the median
angle of slopes below the horizontal, then uses this as the reference
line.

### Measurement Error

Passing-Bablok regression implicitly assumes that the ratio of the error
variances of y and x equals the squared slope (equal error variances
when the slope is 1). Earlier versions accepted `error.ratio != 1` (or
estimated it from replicates via `id`) and weighted the pairwise slopes
by it. In simulations this biased the slope (by -0.07 to -0.43 with an
error ratio of 4), the bias did not shrink with sample size, and joint
tests then rejected a true identity line 81-100% of the time with n \>=
50. `error.ratio != 1` is therefore an error. When the error variances
differ, use
[`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md)
with the appropriate `error.ratio` (or `id`).

With `id`, replicate measurements are averaged within each subject and
the subject means are analysed.

### Weighting

Case weights can be provided via the `weights` argument. Case weights
allow you to down-weight or up-weight specific observations in the
analysis.

### Standard Errors and Variance-Covariance Matrix

The analytical (Passing & Bablok, 1983) confidence intervals do not
provide a covariance between the intercept and slope, so a
variance-covariance matrix (needed by
[`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
and
[`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md))
is only returned when one of the following is used:

- **Pairs bootstrap** (`se_method = "bootstrap"`, `replicates > 0`):
  whole (x, y) observations are resampled with replacement and the model
  is refit. The variance-covariance matrix is the covariance of the
  bootstrap estimates and the confidence intervals are percentile
  intervals.

- **Delete-d jackknife** (`se_method = "jackknife"`, `replicates > 0`):
  the model is refit on `replicates` random subsets that each leave out
  d = floor(n/2) observations. The delete-1 jackknife is inconsistent
  for median-type estimators such as Passing-Bablok; deleting d
  observations with \\\sqrt{n}/d \to 0\\ restores consistency (Shao &
  Wu, 1989). Confidence intervals are Wald-type intervals using a t(n-2)
  quantile.

- **Dufey (2020)** (`se_method = "dufey"`): an analytic,
  distribution-free estimator for the equivariant ("scissors")
  Passing-Bablok estimator, based on the U-statistic variance of
  Kendall's tau and a sandwich estimator for the intercept. The slope
  interval is formed from order statistics of the pairwise slopes and
  the intercept interval is Wald-type. **This option is experimental.**
  In simulations its marginal intervals had close to nominal coverage
  for n \>= 20, but a joint test of intercept and slope built from its
  variance-covariance matrix rejected a true null hypothesis 8-11% of
  the time at a nominal 5%.
  [`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  and
  [`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
  therefore refuse Dufey fits; use the pairs bootstrap or delete-d
  jackknife, which held 4.3-5.4% in the same simulations.

Resampling is particularly useful for:

- Weighted regression (case weights)

- Methods 'invariant' and 'scissors' (where analytical CI validity is
  uncertain)

- Joint tests of intercept and slope

The method automatically:

- Tests for high positive correlation using Kendall's tau

- Tests for linearity using a CUSUM test

- Computes confidence intervals (analytical or bootstrap)

## References

Passing, H., & Bablok, W. (1983). A New Biometrical Procedure for
Testing the Equality of Measurements from Two Different Analytical
Methods. Application of linear regression procedures for method
comparison studies in Clinical Chemistry, Part I. Cclm, 21(11), 709-720.
doi: 10.1515/cclm.1983.21.11.709

Passing, H., & Bablok, W. (1984). Comparison of Several Regression
Procedures for Method Comparison Studies and Determination of Sample
Sizes Application of linear regression procedures for method comparison
studies in Clinical Chemistry, Part II. Clinical Chemistry and
Laboratory Medicine, 22(6). doi: 10.1515/cclm.1984.22.6.431

Bablok, W., Passing, H., Bender, R., & Schneider, B. (1988). A General
Regression Procedure for Method Transformation. Application of Linear
Regression Procedures for Method Comparison Studies in Clinical
Chemistry, Part III. Clinical Chemistry and Laboratory Medicine, 26(11).
doi: 10.1515/cclm.1988.26.11.783

Dufey, F. (2020). Derivation of Passing-Bablok regression from Kendall's
tau. The International Journal of Biostatistics, 16(2), 20190157. doi:
10.1515/ijb-2019-0157

Sen, P. K. (1968). Estimates of the regression coefficient based on
Kendall's tau. Journal of the American Statistical Association, 63(324),
1379-1389. doi: 10.1080/01621459.1968.10480934

Shao, J., & Wu, C. F. J. (1989). A general theory for jackknife variance
estimation. The Annals of Statistics, 17(3), 1176-1197. doi:
10.1214/aos/1176347263

## Examples

``` r
# Peak flow: mini vs Wright meter (Bland & Altman, 1986)
data("ba1986")

# Analytic confidence intervals (scissors method, default)
fit <- pb_reg(mini1 ~ wright1, data = ba1986)
#> Warning: Bootstrap confidence intervals are recommended for 'invariant' and 'scissors' methods. Consider setting replicates > 0.
summary(fit)
#> Passing-Bablok (scissors) with 95% C.I. 
#> 
#> Call:
#> pb_reg(formula = mini1 ~ wright1, data = ba1986)
#> 
#> Coefficients:
#>        term    coef      se  lower.ci upper.ci df null_value reject_h0
#> 1 Intercept -22.502 59.8115 -153.2372   81.219 15          0     FALSE
#> 2   wright1   1.058  0.1289    0.8412    1.347 15          1     FALSE
#> 
#> 15 degrees of freedom
#> 
plot(fit)


# Symmetric and scale-invariant methods
pb_reg(mini1 ~ wright1, data = ba1986, method = "symmetric")
#> Passing-Bablok (symmetric) with 95% C.I. 
#> 
#> Call:
#> pb_reg(formula = mini1 ~ wright1, data = ba1986, method = "symmetric")
#> 
#> Coefficients:
#> (Intercept)     wright1 
#>     -24.306       1.065 
#> 
#> Kendall's Tau (H0: tau <= 0):
#>   tau:       0.7647
#>   p-value:   0.0000
#> 
#> CUSUM Linearity Test:
#>   Test stat: 0.6667
#>   p-value:   0.6601
#> 
pb_reg(mini1 ~ wright1, data = ba1986, method = "invariant")
#> Warning: Bootstrap confidence intervals are recommended for 'invariant' and 'scissors' methods. Consider setting replicates > 0.
#> Passing-Bablok (scale-invariant) with 95% C.I. 
#> 
#> Call:
#> pb_reg(formula = mini1 ~ wright1, data = ba1986, method = "invariant")
#> 
#> Coefficients:
#> (Intercept)     wright1 
#>     -13.748       1.034 
#> 
#> Kendall's Tau (H0: tau <= 0):
#>   tau:       0.7647
#>   p-value:   0.0000
#> 
#> CUSUM Linearity Test:
#>   Test stat: 1.0000
#>   p-value:   0.2827
#> 

# \donttest{
# Bootstrap (or delete-d jackknife) variance-covariance matrix, needed for
# the joint test of intercept and slope; use more replicates in practice
fit_boot <- pb_reg(mini1 ~ wright1, data = ba1986, replicates = 199)
joint_test(fit_boot)
#> 
#>  Joint Confidence Region Test [Wald, F-based] (H0: intercept = 0, slope
#>  = 1)
#> 
#> data:  mini1 ~ wright1
#> X-squared = 0.37141, df1 = 2, df2 = 15, p-value = 0.8324
#> alternative hypothesis: true intercept and slope are not equal to the null values
#> null values:
#> intercept     slope 
#>         0         1 
#> sample estimates:
#>  intercept      slope 
#> -22.502332   1.058061 
#> 
plot_joint(fit_boot)


fit_jack <- pb_reg(mini1 ~ wright1, data = ba1986,
                   se_method = "jackknife", replicates = 199)

# Case weights
w <- rep(1, nrow(ba1986))
w[1:3] <- 2
pb_reg(mini1 ~ wright1, data = ba1986, weights = w, replicates = 199)
#> Passing-Bablok (scissors) with 95% C.I. 
#> 
#> Call:
#> pb_reg(formula = mini1 ~ wright1, data = ba1986, weights = w, 
#>     replicates = 199)
#> 
#> Coefficients:
#> (Intercept)     wright1 
#>      -7.562       1.037 
#> 
#> Kendall's Tau (H0: tau <= 0):
#>   tau:       0.7647
#>   p-value:   0.0000
#> 
#> CUSUM Linearity Test:
#>   Test stat: 0.8572
#>   p-value:   0.3517
#> 
# }
```
