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

  Column with subject identifier (optional). If provided, measurement
  error ratio is calculated from replicate measurements.

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

  Ratio of measurement error variances (var(x)/var(y)). Default is 1.
  This argument is ignored if subject identifiers are provided via `id`.

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
    available for the "scissors" method without case weights and with
    `error.ratio = 1`. See Details before using it for joint tests.

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

- `error.ratio`: Error ratio used in fitting.

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

### Measurement Error Handling

If the data are measured in replicates, then the measurement error ratio
can be directly derived from the data. This can be accomplished by
indicating the subject identifier with the `id` argument. When
replicates are not available in the data, then the ratio of error
variances (var(x)/var(y)) can be provided with the `error.ratio`
argument (default = 1, indicating equal measurement errors).

The error ratio affects how pairwise slopes are weighted in the robust
median calculation. When error.ratio = 1, all pairs receive equal
weight. When error.ratio != 1, pairs are weighted to account for
heterogeneous measurement precision.

### Weighting

Case weights can be provided via the `weights` argument. These are
distinct from measurement error weighting (controlled by `error.ratio`).
Case weights allow you to down-weight or up-weight specific observations
in the analysis.

### Standard Errors and Variance-Covariance Matrix

The analytical (Passing & Bablok, 1983) confidence intervals do not
provide a covariance between the intercept and slope, so a
variance-covariance matrix (needed by
[`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md)
and
[`plot_joint()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_eiv-methods.md))
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
  In simulations its marginal standard errors were accurate for n \>=
  20, but joint tests of intercept and slope
  ([`joint_test()`](https://aaroncaldwell.us/SimplyAgree/reference/joint_test.md))
  rejected a true null hypothesis about 7-11% of the time at a nominal
  5%, suggesting the intercept-slope covariance is not yet reliable.
  Prefer the pairs bootstrap or delete-d jackknife for joint tests.

Resampling is particularly useful for:

- Weighted regression (case weights or error.ratio != 1)

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
if (FALSE) { # \dontrun{
# Basic Passing-Bablok regression (scissors method, default)
model <- pb_reg(method2 ~ method1, data = mydata)

# With known error ratio
model_er <- pb_reg(method2 ~ method1, data = mydata, error.ratio = 2)

# With replicate measurements
model_rep <- pb_reg(method2 ~ method1, data = mydata, id = "subject_id")

# With bootstrap confidence intervals
model_boot <- pb_reg(method2 ~ method1, data = mydata,
                     error.ratio = 1.5, replicates = 1000)

# Delete-d jackknife or analytic (Dufey 2020) variance-covariance matrix
model_jack <- pb_reg(method2 ~ method1, data = mydata,
                     se_method = "jackknife", replicates = 1000)
model_dufey <- pb_reg(method2 ~ method1, data = mydata, se_method = "dufey")
joint_test(model_dufey)

# Symmetric method
model_sym <- pb_reg(method2 ~ method1, data = mydata, method = "symmetric")

# Scale-invariant method
model_inv <- pb_reg(method2 ~ method1, data = mydata, method = "invariant")

# With case weights
model_wt <- pb_reg(method2 ~ method1, data = mydata,
                   weights = mydata$case_weights)

# View results
print(model)
summary(model)
plot(model)
} # }
```
