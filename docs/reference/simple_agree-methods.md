# Methods for simple_agree objects

Methods defined for objects returned from the agree functions.

## Usage

``` r
# S3 method for class 'simple_agree'
print(x, ...)

# S3 method for class 'simple_agree'
plot(
  x,
  type = 1,
  x_name = "x",
  y_name = "y",
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  smooth_method = NULL,
  smooth_se = TRUE,
  ...
)

check(x, ...)

# S3 method for class 'simple_agree'
check(x, ...)
```

## Arguments

- x:

  object of class `simple_agree` as returned from a function starting
  with 'agree'

- ...:

  further arguments passed through, see description of return value for
  details.
  [`agree_test`](https://aaroncaldwell.us/SimplyAgree/reference/agree_test.md).

- type:

  Type of plot to output. Default (1) is Bland-Altman plot while type=2
  will produce a line-of-identity plot.

- x_name:

  Name/label for x values (first measurement)

- y_name:

  Name/label for y values (second measurement)

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

  Returns a plot of the limits of agreement (type = 1) or concordance
  plot (type = 2)

- `check`:

  Returns 2 plots, p_norm and p_het, testing the assumptions of a
  Bland-Altman analysis. P-values for the normality and
  heteroskedasticity tests are provided as captions to the plot.
