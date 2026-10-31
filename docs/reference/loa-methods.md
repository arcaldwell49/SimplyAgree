# Methods for loa objects

Methods defined for objects returned from the agreement_limit function.

## Usage

``` r
# S3 method for class 'loa'
print(x, digits = 4, ...)

# S3 method for class 'loa'
plot(
  x,
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  delta = NULL,
  ...
)

# S3 method for class 'loa'
check(x, error.ratio = NULL, bias_line = c("adjusted", "expected"), ...)
```

## Arguments

- x:

  object of class `loa` as returned from a agreement_limit function.

- digits:

  The number of digits to print.

- ...:

  further arguments passed through, see description of return value for
  details.
  [`agreement_limit`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md).

- geom:

  String naming the type of geometry to display the data points. Default
  is "geom_point". Other options include: "geom_bin2d",
  "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".

- delta:

  The maximal allowable difference.

- error.ratio:

  For `check`, an optional known ratio of the measurement error
  variances, var(error of x) / var(error of y), as in
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md),
  on the analysis scale (the log scale with `log_tf = TRUE`). Used only
  with `data_type = "simple"`; see "Proportional bias diagnostic".

- bias_line:

  For `check`, the second (dashed) line drawn on the proportional bias
  panel when the test is corrected for measurement error (replicates or
  `error.ratio`): "adjusted" (default), the error-adjusted slope, or
  "expected", the slope expected from unequal measurement error alone.
  The test is the same either way. See "Choosing `bias_line`".

## Value

- `print`:

  Prints short summary of the Limits of Agreement.

- `plot`:

  Returns a plot of the limits of agreement.

- `check`:

  Returns plots testing the assumptions of a Bland-Altman analysis.
  P-values for the normality, heteroskedasticity, and proportional bias
  tests are provided as captions to the plot. The proportional bias
  results are attached as the attribute `"prop_bias"`, a data frame with
  the test (`test`, `corrected`, `n`, `statistic`, `df`, `p.value`, and
  `p.uncorrected`, the uncorrected Pitman-Morgan p-value), the observed
  slope of the differences on the means (`slope_obs`), the slope
  expected from unequal measurement error alone (`slope_null`) and the
  error-adjusted slope (`slope_adj`), the implied scale of y relative to
  x on the true values, `beta` = (2 - `slope_adj`)/(2 + `slope_adj`),
  the within-subject variances (`sw2_x`, `sw2_y`), and `error.ratio`.
  Columns that do not apply are `NA`.

## Proportional bias diagnostic

The proportional bias panel of
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
plots the differences (centred) against the means, on the analysis scale
(the log scale for log-transformed data), whether or not the fit used
`prop_bias`. Because cov(x - y, x + y) = var(x) - var(y), a slope of the
differences on the means only shows that the two methods have unequal
total variance; the test of the slope is the Pitman-Morgan test of equal
variances. Unequal variances arise from a true proportional bias (y =
a + b x with b not 1), but also from unequal measurement error or from a
prior calibration of one method on the other (Bland & Altman, 1999;
Hopkins, 2004). With one measurement per method these cannot be told
apart, and
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
says so when the test is significant.

What is tested depends on the design:

- **One measurement per method** (`data_type = "simple"`): the
  Pitman-Morgan test (t on n - 2 df). With `error.ratio`, the Deming
  slope of y on x for that error ratio is tested against 1 instead (the
  test reported by
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md)).
  The error variances implied by the Deming fit give the slope expected
  from unequal error.

- **Replicates** (`data_type = "reps"`): the test uses subject means and
  is corrected for unequal measurement error. The covariance of the
  subject-mean differences and sums is compared with its expected value
  without proportional bias, \\s^2\_{wx}\overline{1/m_x} -
  s^2\_{wy}\overline{1/m_y}\\, from the pooled within-subject variances,
  rather than with zero. The standard error is a jackknife over subjects
  (re-estimating the within-subject variances) with n - 1 df. Only the
  difference of the error variances enters, so an occasion effect shared
  by both methods (linked replicates) cancels. Complete pairs of
  measurements are used.

- **Nested data** (`data_type = "nest"`): the true value changes within
  a subject, so the error variances cannot be estimated; the slope is
  tested with a random-intercept model (likelihood ratio test) and is
  subject to the same limitation as one measurement per method.

## Choosing `bias_line`

When the test is corrected for measurement error (replicates or
`error.ratio`), the panel shows the observed least-squares slope (solid,
with its confidence band, which is not corrected) and a second, dashed
line chosen by `bias_line`. The test and its p-value are the same for
both choices; only the picture differs.

- **"adjusted"** (default): the error-adjusted slope, the slope of the
  differences on the means that would be seen without measurement error,
  2(1 - beta)/(1 + beta) for y = a + beta x on the true values. Without
  proportional bias it is horizontal.

  - Pros: one simple rule (is the dashed line flat?); its slope gives
    `beta`, the size of the proportional bias, which is reported in the
    caption; it applies in the same way to replicates and `error.ratio`.

  - Cons: the test is of the covariance, not of this slope, so how far
    the line is from flat is not a direct picture of the test statistic;
    when the error variances are similar it lies on top of the observed
    line and is hard to see.

- **"expected"**: the slope expected from unequal measurement error
  alone, with no proportional bias. Without proportional bias the
  observed (solid) line should follow it.

  - Pros: the gap between the two lines is what is tested, so the
    picture matches the p-value; it shows directly how much of the
    observed slope unequal error can explain.

  - Cons: "the solid line should follow the dashed one" is harder to
    judge than "the dashed line should be flat"; the size of the
    proportional bias (`beta`) is not shown (it is in the `"prop_bias"`
    attribute).

Neither line shows its own uncertainty; use the p-value (and, for the
size, `beta`) rather than judging the lines by eye, particularly with
few subjects.

## References

Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
comparison studies. Statistical Methods in Medical Research, 8(2),
135-160.

Hopkins, W. G. (2004). Bias in Bland-Altman but not regression validity
analyses. Sportscience, 8, 42-46.
