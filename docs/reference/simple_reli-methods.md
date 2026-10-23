# Methods for simple_reli objects

Methods defined for objects returned from
[`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md)
and
[`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md).

## Usage

``` r
# S3 method for class 'simple_reli'
print(x, ...)

# S3 method for class 'simple_reli'
plot(x, ...)

# S3 method for class 'simple_reli'
check(x, ...)
```

## Arguments

- x:

  object of class `simple_reli` as returned from
  [`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md)
  or
  [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md).

- ...:

  further arguments passed through, see description of return value for
  details.

## Value

- `print`:

  Prints a short summary of the reliability statistics.

- `plot`:

  Returns a plot of the data points used in the reliability analysis.

- `check`:

  Returns diagnostic plots (normality and heteroskedasticity) of the
  model residuals.
