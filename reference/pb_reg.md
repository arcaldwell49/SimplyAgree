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

  Number of bootstrap iterations for confidence intervals. If 0
  (default), analytical confidence intervals are used. Bootstrap is
  recommended for weighted data and 'invariant' or 'scissors' methods.

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
  bootstrap used).

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

- `boot`: Bootstrap results (if replicates \> 0).

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
weight. When error.ratio ≠ 1, pairs are weighted to account for
heterogeneous measurement precision.

### Weighting

Case weights can be provided via the `weights` argument. These are
distinct from measurement error weighting (controlled by `error.ratio`).
Case weights allow you to down-weight or up-weight specific observations
in the analysis.

### Bootstrap

Wild bootstrap resampling is used when `replicates > 0`. This is
particularly useful for:

- Weighted regression (case weights or error.ratio ≠ 1)

- Methods 'invariant' and 'scissors' (where analytical CI validity is
  uncertain)

- Small sample sizes

The method automatically:

- Tests for high positive correlation using Kendall's tau

- Tests for linearity using a CUSUM test

- Computes confidence intervals (analytical or bootstrap)

## References

Passing, H. and Bablok, W. (1983). A new biometrical procedure for
testing the equality of measurements from two different analytical
methods. Journal of Clinical Chemistry and Clinical Biochemistry, 21,
709-720.

Passing, H. and Bablok, W. (1984). Comparison of several regression
procedures for method comparison studies and determination of sample
sizes. Journal of Clinical Chemistry and Clinical Biochemistry, 22,
431-445.

Bablok, W., Passing, H., Bender, R. and Schneider, B. (1988). A general
regression procedure for method transformation. Journal of Clinical
Chemistry and Clinical Biochemistry, 26, 783-790.

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
