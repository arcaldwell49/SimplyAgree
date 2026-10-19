# Power Calculation for Exact Agreement/Tolerance Test

**\[maturing\]**

Computes sample size, power, or other parameters for the exact method of
assessing agreement between two measurement methods, as described in
Shieh (2019). This method tests whether the central portion of paired
differences falls within specified bounds.

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
  max_iter = 1000,
  m = 1,
  icc = 0
)
```

## Arguments

- n:

  Number of subject pairs (sample size), or the number of subjects when
  `m > 1`

- delta:

  Maximum allowable difference bound (half-width of tolerance interval)

- mu:

  Mean of paired differences

- sigma:

  Standard deviation of paired differences. With clustered data
  (`m > 1`), the *total* SD of the differences.

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

- m:

  number of paired measurements per subject (default 1, independent
  pairs). See the "Clustered data" section.

- icc:

  intraclass correlation of the paired *differences* (between-subject
  share of the variance of the differences), in \[0, 1). Ignored when
  `m = 1`. This is not the test-retest ICC of the measurements
  themselves.

## Value

An object of class "power.htest", a list with components:

- n:

  Sample size (pairs, or subjects when `m > 1`)

- delta:

  Maximum allowable difference bound

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

- m, icc:

  The clustering inputs

- n.obs:

  Total number of pairs, `n * m`

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
calculated. If mu is not specified, it defaults to 0. A sample size can
only be found when the proportion of differences within \[-delta,
delta\] under the alternative (`p1_star`) exceeds `p0_star`; otherwise
power cannot exceed alpha and an error is returned.

Tolerance Interval Interpretation:

The parameter `p0_star` represents the tolerance coverage proportion,
i.e., the proportion of the population that must fall within the
specified bounds \[-delta, delta\] under the null hypothesis. This is
conceptually related to tolerance intervals, but formulated as a
hypothesis test rather than an estimation problem.

The power is that of Shieh's test (reject H0 when xbar -/+ gamma \\ SE
lies within \[-delta, delta\]; for independent pairs this test is
reported by
[`agree_test()`](https://aaroncaldwell.us/SimplyAgree/reference/agree_test.md)).
It is **not** the probability that tolerance limits from
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)
fall within \[-delta, delta\]; that decision rule is less powerful. In
simulations at a planned power of 0.80, it was about 0.66 for
independent pairs and about 0.57 to 0.61 for clustered data (m = 3, icc
= 0.1 to 0.6). Allow a larger sample if the analysis will be based on
tolerance limits.

Note: This differs from Bland-Altman's "95% limits of agreement," which
are confidence intervals for the 2.5th and 97.5th percentiles, *not*
tolerance intervals.

## Clustered test statistic

With `m > 1`, the test is Shieh's test with the standard error of the
mean difference taken as S \\ sqrt(DEFF / (n \\ m)), i.e. with the
design effect treated as known. No function in this package currently
carries out that clustered test, so treat the clustered power as a
planning approximation.

## Clustered data

When each of `n` subjects contributes `m` paired measurements (repeated
measures, or several pairs per subject), the pairs are not independent
and treating `n * m` pairs as independent overstates precision. Setting
`m` and `icc` accounts for this under a balanced one-way random-effects
(compound symmetry) model for the differences:

- The mean difference has variance sigma^2 \\ DEFF / (n \\ m), with
  design effect DEFF = 1 + (m - 1) \\ icc, so the effective sample size
  for the mean is n \\ m / DEFF.

- The total variance is estimated as S^2 = MSB / m + (m - 1) / m \\ MSW,
  whose distribution is a weighted sum of two independent chi-square
  variables with n - 1 and n(m - 1) degrees of freedom. This
  distribution is integrated numerically in place of the single
  chi-square of the independent case. It is more skewed than a
  Satterthwaite approximation suggests, which matters when icc is large.

Under these assumptions the calculation is exact (it agrees with
simulation to Monte Carlo error), and `m = 1` gives exactly the
published method. The simple design effect 1 + (m - 1) \\ icc applied to
the whole sample size overstates the penalty, because the SD has a much
smaller design effect, 1 + (m - 1) \\ icc^2. Unbalanced designs, other
correlation structures, or several levels of nesting are not covered;
use simulation for those.

`icc` is the ICC of the *differences*, which can be estimated from pilot
data with an intercept-only mixed model on the differences (e.g.
`lme4::lmer(diff ~ 1 + (1 | id))`), and `sigma` is then the square root
of the sum of the two variance components.

## References

Shieh, G. (2019). Assessing Agreement Between Two Methods of
Quantitative Measurements: Exact Test Procedure and Sample Size
Calculation. Statistics in Biopharmaceutical Research, 12(3), 352-359.
[doi:10.1080/19466315.2019.1677495](https://doi.org/10.1080/19466315.2019.1677495)

Bland, J.M. and Altman, D.G. (2007). Agreement between methods of
measurement with multiple observations per individual. *Journal of
Biopharmaceutical Statistics*, **17**(4), 571-582.
[doi:10.1080/10543400701329422](https://doi.org/10.1080/10543400701329422)

## Examples

``` r
# Example 1: Find required sample size
power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                      p0_star = 0.95, power = 0.80, alpha = 0.05)
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
#>           power = 0.8018401
#>  critical_value = 13.57038
#>               m = 1
#>             icc = 0
#>           n.obs = 34
#> 
#> NOTE: H0: Central 95% of differences not within [-delta, delta]
#>      H1: Central 99.4% of differences within [-delta, delta] 
#>      n is the number of pairs. Two measurements per unit; one for each method.
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
#>           power = 0.8315343
#>  critical_value = 6.391127
#>               m = 1
#>             icc = 0
#>           n.obs = 15
#> 
#> NOTE: H0: Central 80% of differences not within [-delta, delta]
#>      H1: Central 97.3% of differences within [-delta, delta] 
#>      n is the number of pairs. Two measurements per unit; one for each method.
#> 

# Example 3: Find required delta for given power and sample size
power_agreement_exact(n = 50, mu = 0, sigma = 2.5,
                      p0_star = 0.95, power = 0.90, alpha = 0.05)
#> 
#>      Power for Exact Method for Assessing Agreement Between Two Methods 
#> 
#>               n = 50
#>           delta = 6.656157
#>              mu = 0
#>           sigma = 2.5
#>         p0_star = 0.95
#>         p1_star = 0.9922429
#>           alpha = 0.05
#>           power = 0.9
#>  critical_value = 15.8932
#>               m = 1
#>             icc = 0
#>           n.obs = 50
#> 
#> NOTE: H0: Central 95% of differences not within [-delta, delta]
#>      H1: Central 99.2% of differences within [-delta, delta] 
#>      n is the number of pairs. Two measurements per unit; one for each method.
#> 

# Example 4: Clustered data, 3 pairs per subject, ICC of the differences 0.15
power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                      p0_star = 0.95, power = 0.80, m = 3, icc = 0.15)
#> 
#>      Power for Exact Method for Assessing Agreement Between Two Methods 
#> 
#>               n = 12
#>           delta = 7
#>              mu = 0.5
#>           sigma = 2.5
#>         p0_star = 0.95
#>         p1_star = 0.9939889
#>           alpha = 0.05
#>           power = 0.8047327
#>  critical_value = 12.17416
#>               m = 3
#>             icc = 0.15
#>           n.obs = 36
#> 
#> NOTE: H0: Central 95% of differences not within [-delta, delta]
#>      H1: Central 99.4% of differences within [-delta, delta] 
#>      n is the number of subjects, each with m = 3 paired measurements (n.obs pairs in total).
#>      Assumes balanced compound symmetry with ICC of the differences = 0.15.
#> 
```
