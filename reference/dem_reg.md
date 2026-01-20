# Deming Regression

**\[stable\]**

A function for fitting a straight line to two-dimensional data (i.e., X
and Y) that are measured with error.

## Usage

``` r
dem_reg(
  formula = NULL,
  data,
  id = NULL,
  x = NULL,
  y = NULL,
  conf.level = 0.95,
  weighted = FALSE,
  weights = NULL,
  error.ratio = 1,
  model = TRUE,
  keep_data = FALSE,
  ...
)
```

## Arguments

- formula:

  A formula of the form `y ~ x` specifying the model. If provided, takes
  precedence over `x` and `y` arguments.

- data:

  Data frame with all data.

- id:

  Column with subject identifier (optional).

- x:

  Name of column with first measurement (deprecated in favor of formula
  interface).

- y:

  Name of other column with the other measurement to compare to the
  first (deprecated in favor of formula interface).

- conf.level:

  The confidence level required. Default is 95%.

- weighted:

  Logical indicator (TRUE/FALSE) for whether to use weighted Deming
  regression. Default is FALSE.

- weights:

  an optional vector of weights to be used in the fitting process.
  Should be NULL or a numeric vector.

- error.ratio:

  Ratio of the two error variances. Default is 1. This argument is
  ignored if subject identifiers are provided.

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

  Logical indicator (TRUE/FALSE). If TRUE, the jacknife samples are
  returned; default is FALSE.

- ...:

  Additional arguments (currently unused).

## Value

The function returns a simple_eiv (eiv meaning "error in variables")
object with the following components:

- `coefficients`: Named vector of coefficients (intercept and slope).

- `residuals`: Optimized residuals from the fitted model.

- `fitted.values`: Estimated true Y values (Y-hat).

- `model_table`: Data frame presenting the full results from the Deming
  regression analysis.

- `vcov`: Variance-covariance matrix for slope and intercept.

- `df.residual`: Residual degrees of freedom.

- `call`: The matched call.

- `terms`: The terms object used.

- `xlevels`: (Only for models with factors) levels of factors.

- `model`: The model frame.

- `x_vals`: Original x values used in fitting.

- `y_vals`: Original y values used in fitting.

- `x_hat`: Estimated true X values.

- `y_hat`: Estimated true Y values.

- `error.ratio`: Error ratio used in fitting.

- `weighted`: Whether weighted regression was used.

- `weights`: Weights used in fitting.

- `conf.level`: Confidence level used.

- `resamples`: List containing resamples from jacknife procedure (if
  keep_data = TRUE).

## Details

This function provides a Deming regression analysis wherein the sum of
distances in both x and y direction is minimized. Deming regression,
also known as error-in-variable regression, is useful in situations
where both X & Y are measured with error. The use of Deming regression
is beneficial when comparing to methods for measuring the same
continuous variable.

Currently, the `dem_reg` function covers simple Deming regression and
weighted Deming regression. Weighted Deming regression can be used by
setting the weighted argument to TRUE. The weights can be provided by
the user or can be calculated within function.

If the data are measured in replicates, then the measurement error can
be directly derived from the data. This can be accomplished by
indicating the subject identifier with the id argument. When the
replicates are not available in the data, then the ratio of error
variances (y/x) can be provided with the error.ratio argument.

## Interface Change

The `x` and `y` arguments are deprecated. Please use the `formula`
interface instead:

- Old: `dem_reg(x = "x_var", y = "y_var", data = df)`

- New: `dem_reg(y_var ~ x_var, data = df)`

## References

Linnet, K. (1990) Estimation of the linear relationship between the
measurements of two methods with proportional errors. Statistics in
Medicine, 9, 1463-1473.

Linnet, K. (1993). Evaluation of regression procedures for methods
comparison studies. Clinical chemistry, 39, 424-432.

Sadler, W.A. (2010). Joint parameter confidence regions improve the
power of parametric regression in method-comparison studies.
Accreditation and Quality Assurance, 15, 547-554.

## Examples

``` r
if (FALSE) { # \dontrun{
# New formula interface (recommended)
model <- dem_reg(y ~ x, data = mydata)

# Old interface (still works with deprecation warning)
model <- dem_reg(x = "x", y = "y", data = mydata)
} # }
```
