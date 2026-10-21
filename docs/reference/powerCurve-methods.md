# Methods for powerCurve objects

Methods defined for objects returned from the powerCurve function.

## Usage

``` r
find_n(x, power = 0.8)

# S3 method for class 'powerCurve'
plot(x, ...)
```

## Arguments

- x:

  object of class `powerCurve`

- power:

  Level of power (value between 0 and 1) for find_n to find the sample
  size.

- ...:

  further arguments passed through, see description of return value for
  details.
  [`blandPowerCurve`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md).

## Value

- `plot`:

  Returns a plot of the limits of agreement (type = 1) or concordance
  plot (type = 2)

- `find_n`:

  Find the smallest sample size at which the desired power is achieved,
  for each combination of settings. If the power is not reached within
  the sample sizes evaluated, `N` is `NA` (with a warning) and the range
  of sample sizes must be expanded.
