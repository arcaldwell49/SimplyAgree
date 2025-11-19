# Methods for simple_eiv objects

Methods defined for objects returned from the error-in-variables models
(e.g., dem_reg).

## Usage

``` r
# S3 method for class 'simple_eiv'
print(x, ...)

# S3 method for class 'simple_eiv'
plot(x, x_name = "x", y_name = "y", show_joint = TRUE, ...)

# S3 method for class 'simple_eiv'
check(x)

plot_joint(object, ...)

# S3 method for class 'simple_eiv'
plot_joint(
  object,
  ideal_slope = 1,
  ideal_intercept = 0,
  show_intervals = TRUE,
  ...
)

# S3 method for class 'simple_eiv'
vcov(object, ...)

# S3 method for class 'simple_eiv'
coef(object, ...)
```

## Arguments

- x:

  object of class `simple_eiv` from the dem_reg function.

- ...:

  further arguments passed through, see description of return value. for
  details.
  [`agree_test`](https://aaroncaldwell.us/SimplyAgree/reference/agree_test.md).

- x_name:

  Name/label for x values (first measurement)

- y_name:

  Name/label for y values (second measurement)

- show_joint:

  Logical. If TRUE and joint region computed, shows joint region status
  in subtitle.

- object:

  object of class `simple_eiv` for plot_joint method

- ideal_slope:

  The hypothesized slope value to test against (default = 1)

- ideal_intercept:

  The hypothesized intercept value to test against (default = 0)

- show_intervals:

  Logical. If TRUE, shows individual confidence intervals as well.

## Value

- `print`:

  Prints short summary of the error-in-variables (e.g., Deming)
  regression model.

- `plot`:

  Returns a plot of the deming regression line, the line-of-identity,
  and the raw data.

- `check`:

  Returns plots of the optimized residuals.

- `plot_joint`:

  Returns a plot of the joint confidence region in parameter space.
