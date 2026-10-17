# Joint Confidence Region Test for Method Agreement

Tests whether the estimated intercept and slope jointly fall within a
confidence region around specified ideal values (typically intercept=0
and slope=1 for method comparison studies).

## Usage

``` r
joint_test(object, ...)

# S3 method for class 'simple_eiv'
joint_test(
  object,
  ideal_intercept = 0,
  ideal_slope = 1,
  conf.level = 0.95,
  test_method = c("F", "asymptotic"),
  ...
)
```

## Arguments

- object:

  A `simple_eiv` object from
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md)
  or
  [`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md).

- ...:

  Additional arguments (currently unused).

- ideal_intercept:

  The hypothesized intercept value (default: 0).

- ideal_slope:

  The hypothesized slope value (default: 1).

- conf.level:

  Confidence level for the test (default: 0.95).

- test_method:

  Reference distribution for the test. `"F"` (default) uses the
  finite-sample F(2, n-2) distribution per Sadler (2010). `"asymptotic"`
  uses the chi-squared(2) distribution, which is the large-sample
  approximation. The two converge for large n.

## Value

An object of class `htest` containing:

- statistic:

  The Mahalanobis distance.

- parameter:

  Degrees of freedom. For the F-based test, `df1` and `df2`; for the
  asymptotic test, `df` (always 2).

- p.value:

  The p-value for the test.

- conf.int:

  The confidence level used.

- estimate:

  Named vector of estimated intercept and slope.

- null.value:

  Named vector of hypothesized intercept and slope.

- alternative:

  Description of the alternative hypothesis.

- method:

  Description of the test.

- data.name:

  Name of the input object.

## Details

The test computes the Mahalanobis distance between the estimated
coefficients and the hypothesized values using the variance-covariance
matrix of the estimates. By default, the finite-sample F(2, n-2)
reference distribution is used (Sadler, 2010), which is more
conservative than the asymptotic chi-squared(2) approximation for small
samples.

For Deming regression, the variance-covariance matrix is computed via
jackknife. For Passing-Bablok regression, bootstrap resampling must have
been performed (i.e., `boot_ci = TRUE` in the original call).

## References

Sadler, W.A. (2010). Joint parameter confidence regions improve the
power of parametric regression in method-comparison studies.
*Accreditation and Quality Assurance*, 15, 547-554.
