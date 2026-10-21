# Power Calculation for Bland-Altman Limits of Agreement

**\[stable\]**

Computes the sample size, power, maximal allowable difference, or
standard deviation for a method comparison study analyzed with the
Bland-Altman limits of agreement, using the method of Lu et al. (2016).
Agreement is concluded when the confidence intervals of both limits of
agreement fall within \[-delta, delta\].

## Usage

``` r
power_agreement_limit(
  n = NULL,
  delta = NULL,
  mu = 0,
  sigma = NULL,
  power = NULL,
  conf.level = 0.95,
  agree.level = 0.95,
  m = 1,
  icc = 0
)
```

## Arguments

- n:

  Number of pairs (sample size), or the number of subjects when `m > 1`.

- delta:

  Maximal allowable difference: the methods agree if the confidence
  limits of the limits of agreement lie within \[-delta, delta\].

- mu:

  Mean of the paired differences (default 0).

- sigma:

  Standard deviation of the paired differences. With clustered data
  (`m > 1`), the *total* SD of the differences.

- power:

  Target power.

- conf.level:

  Confidence level of the confidence intervals of the limits of
  agreement (default 0.95).

- agree.level:

  Agreement level of the limits of agreement: the proportion of
  differences the limits should contain (default 0.95).

- m:

  number of paired measurements per subject (default 1, independent
  pairs). See the "Clustered data" section.

- icc:

  intraclass correlation of the paired *differences* (between-subject
  share of the variance of the differences), in \[0, 1). Ignored when
  `m = 1`. This is not the test-retest ICC of the measurements
  themselves.

## Value

An object of class `"power.htest"`, a list with components:

- n:

  Sample size (pairs, or subjects when `m > 1`)

- delta:

  Maximal allowable difference

- mu:

  Mean of differences

- sigma:

  Standard deviation of differences

- conf.level:

  Confidence level of the CIs of the limits of agreement

- agree.level:

  Agreement level of the limits of agreement

- power:

  Power of the procedure

- m, icc:

  The clustering inputs

- n.obs:

  Total number of pairs, `n * m`

- method:

  Description of the method

- note:

  Additional notes

## Details

Specify three of `n`, `delta`, `sigma`, and `power`; the fourth is
calculated.

The limits of agreement are mu +/- z \\ sigma, with z the
`(1 + agree.level) / 2` quantile of the standard normal, and their
confidence intervals are those of Bland and Altman (1999), with standard
error sigma \\ sqrt(1/n + z^2 / (2 (n - 1))). This is the interval from
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
with `loa_calc = "blandaltman"`. The power is Lu et al. (2016), eq. 5:

power = 1 - T(t, n - 1, tau1) - T(t, n - 1, tau2),

where T is the CDF of the non-central t distribution, t is the
`1 - (1 - conf.level) / 2` quantile of the central t distribution with
n - 1 df, tau1 = (delta - mu - z \\ sigma) / SE, and tau2 = (delta +
mu - z \\ sigma) / SE. Negative values are set to 0. These are the
values reported by PASS (Chapter 427), which notes that the sample sizes
in Table 1 of Lu et al. (2016), based on their approximate eq. 6, are
sometimes slightly too large.

A sample size can only be found when the true limits of agreement lie
within the bounds, i.e. \|mu\| + z \\ sigma \< delta; otherwise power
goes to 0 as n increases and an error is returned.

With clustered data (`m > 1`), the standard error of the limits uses the
effective sample size n \\ m / (1 + (m - 1) \\ icc) for the mean
difference and a Satterthwaite df for the SD, under a balanced compound
symmetry model (see
[`agree_expected_half()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_expected_half.md)
for the model). This is an approximation; `m = 1` gives the original
results.

For a power curve over a range of sample sizes, see
[`blandPowerCurve()`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md).

## References

Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between
Two Methods of Measurement by Bland-Altman Method. The International
Journal of Biostatistics, 12(2).
[doi:10.1515/ijb-2015-0039](https://doi.org/10.1515/ijb-2015-0039)

Bland, J. M., and Altman, D. G. (1999). Measuring agreement in method
comparison studies. Statistical Methods in Medical Research, 8(2),
135-160.
[doi:10.1177/096228029900800204](https://doi.org/10.1177/096228029900800204)

NCSS (PASS Sample Size Software). Chapter 427: Bland-Altman Method for
Assessing Agreement in Method Comparison Studies.

## See also

[`blandPowerCurve()`](https://aaroncaldwell.us/SimplyAgree/reference/blandPowerCurve.md),
[`power_agreement_exact()`](https://aaroncaldwell.us/SimplyAgree/reference/power_agreement_exact.md),
[`agree_expected_half()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_expected_half.md),
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)

## Examples

``` r
# Sample size for 80% power (PASS Example 1: n = 60)
power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.80)
#> 
#>      Power for Bland-Altman Limits of Agreement (Lu et al. 2016) 
#> 
#>               n = 60
#>           delta = 7
#>              mu = 0.5
#>           sigma = 2.5
#>      conf.level = 0.95
#>     agree.level = 0.95
#>           power = 0.8059154
#>               m = 1
#>             icc = 0
#>           n.obs = 60
#> 
#> NOTE: H0: CIs of the 95% limits of agreement not within [-delta, delta]
#>      H1: CIs of the limits of agreement within [-delta, delta]
#>      n is the number of pairs. Two measurements per unit; one for each method.
#> 

# Power at a given sample size (PASS Example 2)
power_agreement_limit(n = 201, delta = 2.5, mu = 0.2, sigma = 1)
#> 
#>      Power for Bland-Altman Limits of Agreement (Lu et al. 2016) 
#> 
#>               n = 201
#>           delta = 2.5
#>              mu = 0.2
#>           sigma = 1
#>      conf.level = 0.95
#>     agree.level = 0.95
#>           power = 0.800283
#>               m = 1
#>             icc = 0
#>           n.obs = 201
#> 
#> NOTE: H0: CIs of the 95% limits of agreement not within [-delta, delta]
#>      H1: CIs of the limits of agreement within [-delta, delta]
#>      n is the number of pairs. Two measurements per unit; one for each method.
#> 

# Largest SD for which 90% power is reached with 100 pairs
power_agreement_limit(n = 100, delta = 7, mu = 0.5, power = 0.90)
#> 
#>      Power for Bland-Altman Limits of Agreement (Lu et al. 2016) 
#> 
#>               n = 100
#>           delta = 7
#>              mu = 0.5
#>           sigma = 2.577867
#>      conf.level = 0.95
#>     agree.level = 0.95
#>           power = 0.9
#>               m = 1
#>             icc = 0
#>           n.obs = 100
#> 
#> NOTE: H0: CIs of the 95% limits of agreement not within [-delta, delta]
#>      H1: CIs of the limits of agreement within [-delta, delta]
#>      n is the number of pairs. Two measurements per unit; one for each method.
#> 

# Clustered data, 3 pairs per subject, ICC of the differences 0.15
power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.80,
                      m = 3, icc = 0.15)
#> 
#>      Power for Bland-Altman Limits of Agreement (Lu et al. 2016) 
#> 
#>               n = 23
#>           delta = 7
#>              mu = 0.5
#>           sigma = 2.5
#>      conf.level = 0.95
#>     agree.level = 0.95
#>           power = 0.8119145
#>               m = 3
#>             icc = 0.15
#>           n.obs = 69
#> 
#> NOTE: H0: CIs of the 95% limits of agreement not within [-delta, delta]
#>      H1: CIs of the limits of agreement within [-delta, delta]
#>      n is the number of subjects, each with m = 3 paired measurements (n.obs pairs in total).
#>      Assumes balanced compound symmetry with ICC of the differences = 0.15.
#> 
```
