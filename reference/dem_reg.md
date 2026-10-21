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

  Ratio of the measurement error variance of x to that of y, i.e.,
  var(x)/var(y), where var() denotes the error (not total) variance of
  each method. Default is 1 (equal error variances). Values greater than
  1 indicate that x is measured with more error than y. This argument is
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
variances, var(x)/var(y), can be provided with the error.ratio argument.
When subject identifiers are provided, the error ratio is instead
estimated from the replicates as the within-subject variance of x
divided by the within-subject variance of y.

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
# Peak flow: mini vs Wright meter (Bland & Altman, 1986)
data("ba1986")
fit <- dem_reg(mini1 ~ wright1, data = ba1986)
summary(fit)
#> Deming Regression with 95% C.I. 
#> 
#> Call:
#> dem_reg(formula = mini1 ~ wright1, data = ba1986, weighted = FALSE, 
#>     conf.level = 0.95)
#> 
#> Coefficients:
#>              coef      bias     se df  lower.ci upper.ci       t p.value
#> Intercept 15.2316 -18.78805 69.480 15 -132.8606  163.324  0.2192  0.8294
#> Slope      0.9709   0.03764  0.139 15    0.6746    1.267 -0.2095  0.8369
#> 
#> 15 degrees of freedom
#> Error variance ratio (lambda): 1.0000
plot(fit)


# Joint test of intercept = 0 and slope = 1
joint_test(fit)
#> 
#>  Joint Confidence Region Test [Sadler sum-of-squares, F-based] (H0:
#>  intercept = 0, slope = 1)
#> 
#> data:  mini1 ~ wright1
#> F = 0.078441, df1 = 2, df2 = 15, p-value = 0.9249
#> alternative hypothesis: true intercept and slope are not equal to the null values
#> null values:
#> intercept     slope 
#>         0         1 
#> sample estimates:
#>  intercept      slope 
#> 15.2315555  0.9708808 
#> 

# Known error ratio, var(x) / var(y)
dem_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
#> Deming Regression with 95% C.I. 
#> 
#> Call:
#> dem_reg(formula = mini1 ~ wright1, data = ba1986, error.ratio = 2, 
#>     weighted = FALSE, conf.level = 0.95)
#> 
#> Coefficients:
#> (Intercept)     wright1 
#>      6.3264      0.9907 
#> 

# Replicate measurements: the error ratio is estimated from the replicates
data("reps")
dem_reg(y ~ x, data = reps, id = "id")
#> Deming Regression with 95% C.I. 
#> 
#> Call:
#> dem_reg(formula = y ~ x + id, data = reps, id = "id", weighted = FALSE, 
#>     conf.level = 0.95)
#> 
#> Coefficients:
#> (Intercept)           x 
#>      2.8298      0.3929 
#> 

# Weighted Deming regression (error proportional to the level)
dem_reg(mini1 ~ wright1, data = ba1986, weighted = TRUE)
#> Weighted Deming Regression with 95% C.I. 
#> 
#> Call:
#> dem_reg(formula = mini1 ~ wright1, data = ba1986, weighted = TRUE, 
#>     conf.level = 0.95)
#> 
#> Coefficients:
#> (Intercept)     wright1 
#>     66.1190      0.8528 
#> 
```
