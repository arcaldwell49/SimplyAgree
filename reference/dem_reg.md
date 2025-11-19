# Deming Regression

**\[stable\]**

A function for fitting a straight line to two-dimensional data (i.e., X
and Y) that are measured with error.

## Usage

``` r
dem_reg(
  x,
  y,
  id = NULL,
  data,
  conf.level = 0.95,
  weighted = FALSE,
  weights = NULL,
  error.ratio = 1,
  keep_data = FALSE,
  compute_joint = TRUE
)
```

## Arguments

- x:

  Name of column with first measurement.

- y:

  Name of other column with the other measurement to compare to the
  first.

- id:

  Column with subject identifier.

- data:

  Data frame with all data.

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

- keep_data:

  Logical indicator (TRUE/FALSE). If TRUE, the jacknife samples are
  returned; default is FALSE. Users may wish to set to FALSE if data is
  especially large.

- compute_joint:

  Logical indicator (TRUE/FALSE). If TRUE, joint confidence region is
  computed. Default is TRUE.

## Value

The function returns a simple_eiv (eiv meaning "error in variables")
object.

- `call`: The matched call.

- `model`: Data frame presenting the results from the Deming regression
  analysis.

- `resamples`: List containing resamples from jacknife procedure.

- `vcov`: Variance-covariance matrix for slope and intercept.

- `joint_region`: Joint confidence region ellipse coordinates (if
  compute_joint = TRUE).

- `joint_test`: Test of whether ideal point is enclosed by joint region.

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

When `compute_joint = TRUE`, the function computes the joint (slope,
intercept) confidence region based on the chi-square distribution with 2
degrees of freedom. This elliptical region accounts for the correlation
between slope and intercept estimates and can provide improved power for
detecting deviations from (null) hypothesized values (e.g., slope = 1,
intercept = 0).

## References

Linnet, K. (1990) Estimation of the linear relationship between the
measurements of two methods with proportional errors. Statistics in
Medicine, 9, 1463-1473.

Linnet, K. (1993). Evaluation of regression procedures for methods
comparison studies. Clinical chemistry, 39, 424-432.

Sadler, W.A. (2010). Joint parameter confidence regions improve the
power of parametric regression in method-comparison studies.
Accreditation and Quality Assurance, 15, 547-554.
