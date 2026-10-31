# Methods for tolerance_delta objects

Methods defined for objects returned from the tolerance_delta
function(s).

## Usage

``` r
# S3 method for class 'tolerance_delta'
print(x, digits = 4, ...)

# S3 method for class 'tolerance_delta'
plot(
  x,
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  delta = NULL,
  ...
)

# S3 method for class 'tolerance_delta'
check(x, error.ratio = NULL, bias_line = c("adjusted", "expected"), ...)
```

## Arguments

- x:

  object of class `tolerance_delta` as returned from a agreement_limit
  function.

- digits:

  The number of digits to print.

- ...:

  further arguments passed through, see description of return value for
  details.
  [`tolerance_limit`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md).

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
  on the analysis scale. Used only without `id` and `condition`.

- bias_line:

  For `check` with `error.ratio`, the dashed line on the proportional
  bias panel: "adjusted" (default; the error-adjusted slope) or
  "expected" (the slope expected from unequal measurement error alone).
  See "Choosing `bias_line`" in
  [loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md)
  for the pros and cons.

## Value

- `print`:

  Prints short summary of the tolerance limits, with the SD of the
  differences (coefficient of variation when log-transformed). If the SD
  differs between rows of the limits (e.g., by condition), it is shown
  as a column instead. For clustered models, the between- and
  within-subject SDs are also printed.

- `plot`:

  Returns a plot of the tolerance limits.

- `check`:

  Returns plots testing the assumptions of the model. P-values for the
  normality, heteroskedasticity, and proportional bias tests are
  provided as captions to the plot. With `keep_model = FALSE`, only the
  proportional bias panel is drawn. The proportional bias results are
  attached as the attribute `"prop_bias"` (one row per condition, named
  in `group`); see
  [loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md)
  for the columns.

## Proportional bias diagnostic

The proportional bias panel of
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
plots the differences (centred) against the means on the analysis scale
(the log scale with `log_tf`), with the Pitman-Morgan test of equal
variances of the two methods (rows, or subject means if `id` was given;
within each condition if `condition` was given). As explained under
"Proportional bias diagnostic" in
[loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md),
a slope can come from unequal measurement error rather than proportional
bias. Whether the measurements of an `id` are replicates of one true
value or repeated measures of a changing one is not known here, so there
is no correction for the error variances; use
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
with `data_type = "reps"` or
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
for that. Without `id` and `condition`, a known error variance ratio can
be given with `error.ratio`, as in
[`check.loa()`](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md).
