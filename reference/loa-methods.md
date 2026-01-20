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
check(x, ...)
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

## Value

- `print`:

  Prints short summary of the Limits of Agreement.

- `plot`:

  Returns a plot of the limits of agreement.

- `check`:

  Returns plots testing the assumptions of a Bland-Altman analysis.
  P-values for the normality and heteroskedascity tests are provided as
  captions to the plot.
