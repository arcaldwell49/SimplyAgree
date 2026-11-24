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

  A data frame with, for each combination of settings, the smallest
  sample size evaluated at which the desired power is achieved (`N`) and
  the power at that sample size. `N` is `NA`, with a warning, when the
  power is not reached at the largest sample size evaluated (`power` is
  then the highest power evaluated), or when the valid sample size
  immediately below the smallest evaluated size also reaches the target
  (`power` is then also `NA`). With gaps between the sample sizes, `N`
  can be larger than the exact minimum. Use
  [`power_agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_limit.md)
  (with `sigma` in place of `SD`) to find the exact sample size
  directly.

## See also

[`blandPowerCurve()`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md),
[`power_agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_limit.md)
