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
check(x, ...)
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

## Value

- `print`:

  Prints short summary of the tolerance limits.

- `plot`:

  Returns a plot of the tolerance limits.

- `check`:

  Returns plots testing the assumptions of the model. P-values for the
  normality and heteroskedasticity tests are provided as captions to the
  plot.
