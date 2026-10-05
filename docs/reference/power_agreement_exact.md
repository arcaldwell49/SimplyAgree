# Power Calculation for Exact Agreement/Tolerance Test

**\[maturing\]**

Computes sample size, power, or other parameters for the exact method of
assessing agreement between two measurement methods, as described in
Shieh (2019). This method tests whether the central portion of paired
differences falls within specified bounds. This *roughly* equates to the
power for tolerance limits.

## Usage

``` r
power_agreement_exact(
  n = NULL,
  delta = NULL,
  mu = 0,
  sigma = NULL,
  p0_star = 0.95,
  power = NULL,
  alpha = 0.05,
  max_iter = 1000
)
```

## Arguments

- n:

  Number of subject pairs (sample size)

- delta:

  Maximum allowable difference bound (half-width of tolerance interval)

- mu:

  Mean of paired differences

- sigma:

  Standard deviation of paired differences

- p0_star:

  The coverage proportion (content) of the tolerance interval. Central
  proportion under null hypothesis (default = 0.95)

- power:

  Target power (probability of rejecting false null)

- alpha:

  Significance level (Type I error rate, default = 0.05, Confidence
  level = 1-alpha)

- max_iter:

  Maximum iterations for gamma computation (default = 1000)

## Value

An object of class "power.htest", a list with components:

- n:

  Sample size

- delta:

  c

- mu:

  Mean of differences

- sigma:

  Standard deviation of differences

- p0_star:

  Central proportion (null hypothesis)

- p1_star:

  Central proportion (alternative hypothesis)

- alpha:

  Significance level

- power:

  Power of the test

- critical_value:

  Critical value for test statistic

- method:

  Description of the method

- note:

  Additional notes

## Details

This function implements the exact agreement test procedure of Shieh
(2019) for method comparison studies. The test evaluates whether the
central proportion of the distribution of paired differences lies within
the interval \[-delta, delta\].

The null hypothesis is: H0: theta\_(1-p) \<= -delta or delta \<= theta_p
The alternative is: H1: -delta \< theta\_(1-p) and theta_p \< delta

where p = (1 + p0_star)/2, and theta_p represents the 100p-th percentile
of the paired differences.

Specify three of: n, delta, power, and sigma. The fourth will be
calculated. If mu is not specified, it defaults to 0.

Tolerance Interval Interpretation:

The parameter `p0_star` represents the tolerance coverage proportion,
i.e., the proportion of the population that must fall within the
specified bounds \[-delta, delta\] under the null hypothesis. This is
conceptually related to tolerance intervals, but formulated as a
hypothesis test rather than an estimation problem.

Note: This differs from Bland-Altman's "95% limits of agreement," which
are confidence intervals for the 2.5th and 97.5th percentiles, *not*
tolerance intervals.

## References

Shieh, G. (2019). Assessing Agreement Between Two Methods of
Quantitative Measurements: Exact Test Procedure and Sample Size
Calculation. Statistics in Biopharmaceutical Research, 12(3), 352-359.
https://doi.org/10.1080/19466315.2019.1677495

## Examples

``` r
# Example 1: Find required sample size
power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                      p0_star = 0.95, power = 0.80, alpha = 0.05)
#> Maximum iterations reached in gamma computation
#> 
#>      Power for Exact Method for Assessing Agreement Between Two Methods 
#> 
#>               n = 34
#>           delta = 7
#>              mu = 0.5
#>           sigma = 2.5
#>         p0_star = 0.95
#>         p1_star = 0.9939889
#>           alpha = 0.05
#>           power = 0.8018321
#>  critical_value = 13.57044
#> 
#> NOTE: H0: Central 95% of differences not within [-delta, delta]
#>      H1: Central 99.4% of differences within [-delta, delta] 
#> n is number pairs. Two measurements per unit; one for each method.
#> 

# Example 2: Calculate power for given sample size
power_agreement_exact(n = 15, delta = 0.1, mu = 0.011,
                      sigma = 0.044, p0_star = 0.80, alpha = 0.05)
#> 
#>      Power for Exact Method for Assessing Agreement Between Two Methods 
#> 
#>               n = 15
#>           delta = 0.1
#>              mu = 0.011
#>           sigma = 0.044
#>         p0_star = 0.8
#>         p1_star = 0.9726269
#>           alpha = 0.05
#>           power = 0.8315332
#>  critical_value = 6.391135
#> 
#> NOTE: H0: Central 80% of differences not within [-delta, delta]
#>      H1: Central 97.3% of differences within [-delta, delta] 
#> n is number pairs. Two measurements per unit; one for each method.
#> 

# Example 3: Find required delta for given power and sample size
power_agreement_exact(n = 50, mu = 0, sigma = 2.5,
                      p0_star = 0.95, power = 0.90, alpha = 0.05)
#> 
#>      Power for Exact Method for Assessing Agreement Between Two Methods 
#> 
#>               n = 50
#>           delta = 6.65619
#>              mu = 0
#>           sigma = 2.5
#>         p0_star = 0.95
#>         p1_star = 0.9922432
#>           alpha = 0.05
#>           power = 0.9000104
#>  critical_value = 15.89319
#> 
#> NOTE: H0: Central 95% of differences not within [-delta, delta]
#>      H1: Central 99.2% of differences within [-delta, delta] 
#> n is number pairs. Two measurements per unit; one for each method.
#> 
```
