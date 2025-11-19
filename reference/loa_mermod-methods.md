# Methods for loa_mermod objects

Methods defined for objects returned from the loa_lme.

## Usage

``` r
# S3 method for class 'loa_mermod'
print(x, ...)

# S3 method for class 'loa_mermod'
plot(
  x,
  x_label = "Average of Both Methods",
  y_label = "Difference Between Methods",
  geom = "geom_point",
  smooth_method = NULL,
  smooth_se = TRUE,
  ...
)

# S3 method for class 'loa_mermod'
check(x)
```

## Arguments

- x:

  object of class `loa_mermod`.

- ...:

  further arguments passed through, see description of return value for
  details.
  [`loa_mixed`](https://aaroncaldwell.us/SimplyAgree/reference/loa_mixed.md).

- x_label:

  Label for x-axis.

- y_label:

  Label for y-axis.

- geom:

  String naming the type of geometry to display the data points. Default
  is "geom_point". Other options include: "geom_bin2d",
  "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".

- smooth_method:

  Smoothing method (function) to use, accepts either NULL or a character
  vector, e.g. "lm", "glm", "gam", "loess" or a function. Default is
  NULL, which will not include a trend line.

- smooth_se:

  Display confidence interval around smooth?

## Value

- `print`:

  Prints short summary of the Limits of Agreement

- `plot`:

  Returns a plot of the limits of agreement
