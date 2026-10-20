# Power Curve for Bland-Altman Limits of Agreement

**\[superseded\]**

This function calculates the power for the Bland-Altman method under
varying parameter settings and for a range of sample sizes. It is
superseded by
[`power_agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_limit.md),
which returns a `power.htest` object and can solve for the sample size,
power, `delta` or the SD directly. This function remains available for
drawing power curves.

## Usage

``` r
blandPowerCurve(
  samplesizes = seq(10, 100, 1),
  mu = 0,
  SD,
  delta,
  conf.level = 0.95,
  agree.level = 0.95,
  m = 1,
  icc = 0
)
```

## Arguments

- samplesizes:

  vector of samples sizes at which to estimate power: the number of
  pairs, or the number of subjects when `m > 1`.

- mu:

  mean of differences

- SD:

  standard deviation of differences (the *total* SD when `m > 1`)

- delta:

  The threshold below which methods agree/can be considered equivalent,
  can be in any units. Equivalence Bound for Agreement. More than one
  delta can be provided.

- conf.level:

  the confidence level(s) required. Default is 95%. More than one
  confidence level can be provided.

- agree.level:

  the agreement level(s) required. Default is 95%. The proportion of
  data that should lie between the thresholds, for 95% limits of
  agreement this should be 0.95. More than one confidence level can be
  provided.

- m:

  number of paired measurements per subject (default 1, independent
  pairs).

- icc:

  intraclass correlation of the paired *differences*, in \[0, 1).
  Ignored when `m = 1`.

## Value

A dataframe is returned containing the power analysis results. The
results can then be plotted with the plot.powerCurve function.

## Details

Power follows the approximate method of Lu et al. (2016). With clustered
data (`m > 1`), the standard error of the limits uses the effective
sample size n \\ m / (1 + (m - 1) \\ icc) for the mean difference and a
Satterthwaite df for the SD, under a balanced compound symmetry model
(see
[`agree_expected_half()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_expected_half.md)
for the model). Like the original method, this is an approximation;
`m = 1` gives the original results.

## References

Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between
Two Methods of Measurement by Bland-Altman Method. The international
journal of biostatistics, 12(2),
[doi:10.1515/ijb-2015-0039](https://doi.org/10.1515/ijb-2015-0039)

## Examples

``` r
# \donttest{
powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
mu = 0,
SD = 3.3,
delta = 8,
conf.level = .95,
agree.level = .95)
# Plot the power curve
plot(powerCurve, type = 1)

# Find at what N power of .8 is achieved
find_n(powerCurve, power = .8)
#> # A tibble: 1 × 7
#>   delta conf.level agree.level     m   icc power     N
#>   <dbl>      <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     8       0.95        0.95     1     0 0.800   145
# }
# If the desired power is not found then
## Sample size range must be expanded
```
