# Power and Sample Size for an Intraclass Correlation Test

**\[maturing\]**

Computes the power of the F test of an intraclass correlation
coefficient (ICC) used by
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
or solves for the number of subjects, the number of measurements per
subject, the ICC under the alternative, or the significance level that
gives a target power. Exactly one of `n`, `k`, `rho1`, `power` and
`sig.level` must be `NULL`; that one is computed from the others, as in
[`stats::power.t.test()`](https://rdrr.io/r/stats/power.t.test.html).

## Usage

``` r
power_icc(
  n = NULL,
  k = 2,
  rho0 = 0,
  rho1 = NULL,
  sig.level = 0.05,
  power = NULL,
  model = c("oneway", "twoway"),
  alternative = c("two.sided", "greater", "less"),
  method = c("exact", "approximate")
)
```

## Arguments

- n:

  Number of subjects.

- k:

  Number of measurements (items: trials, raters or occasions) per
  subject. When solved for, the smallest whole number of at least 2 that
  reaches the target is returned.

- rho0:

  The ICC under the null hypothesis (default 0).

- rho1:

  The ICC under the alternative hypothesis.

- sig.level:

  Significance level (Type I error rate; default 0.05).

- power:

  Power of the test (1 minus the Type II error rate).

- model:

  `"oneway"` (default) or `"twoway"`. `"twoway"` means the consistency
  ICC, ICC(C,1) of
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md);
  the absolute agreement ICC is not supported (see Details).

- alternative:

  `"two.sided"` (default), `"greater"`, or `"less"`, as in
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md).
  A design planned with the defaults matches the analysis run with the
  defaults.

- method:

  `"exact"` (default) computes power from the exact distribution of the
  F statistic. `"approximate"` uses the closed form of Walter, Eliasziw
  and Donner (1998), available only for `model = "oneway"`.

## Value

An object of class `"power.htest"`, printed by
[`stats::print.power.htest()`](https://rdrr.io/r/stats/print.power.htest.html),
with elements `n`, `k`, `rho0`, `rho1`, `sig.level`, `power`,
`alternative`, `method` and `note`.

## Details

**Exact power.** For the one-way and the two-way consistency ICC with
complete data, the F ratio of the subject mean square to the error mean
square is a scaled central F variable whatever the true ICC \\\rho\\:
\$\$F \sim \frac{1 + (k-1)\rho}{1 - \rho} F\_{n-1, \nu_2},\$\$ with
\\\nu_2 = n(k-1)\\ (one-way) or \\(n-1)(k-1)\\ (two-way). The test of
\\H_0: \rho = \rho_0\\ (McGraw & Wong, 1996) divides the F ratio by the
same factor at \\\rho_0\\, so its power is a single F probability
(Donner & Eliasziw, 1987). Non-integer `n` is allowed, as in
[`stats::power.t.test()`](https://rdrr.io/r/stats/power.t.test.html);
round up to the next whole number of subjects.

**Approximate power** uses the closed form of Walter, Eliasziw and
Donner (1998), \$\$n = 1 + \frac{2k (z\_{1-\alpha} +
z\_{1-\beta})^2}{(k-1)(\log C_0)^2}, \quad C_0 = \frac{1 +
k\theta_0}{1 + k\theta_1}, \quad \theta = \frac{\rho}{1 - \rho},\$\$
with \\\alpha/2\\ in place of \\\alpha\\ for a two-sided test. Walter et
al. (and the PASS software) write n for the number of measurements per
subject and k for the number of subjects; here n is always the number of
subjects and k the number of measurements per subject. The exact method
is preferred; the approximation is provided to reproduce published
tables.

**Two-sided tests.** When solving for `rho1` with
`alternative = "two.sided"`, the solution above `rho0` is returned; use
`alternative = "less"` (with the significance level halved) for the one
below.

**Agreement ICC.** The absolute agreement ICC (ICC(A,1)) is not
supported: its test has no exact F distribution, and because its
denominator degrees of freedom depend on the sample, its power "cannot
be determined exactly" (McGraw & Wong, 1996, footnote 3). Power for that
ICC would need simulation.

## References

Donner, A., & Eliasziw, M. (1987). Sample size requirements for
reliability studies. Statistics in Medicine, 6(4), 441-448.
[doi:10.1002/sim.4780060404](https://doi.org/10.1002/sim.4780060404)

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
[doi:10.1037/1082-989X.1.1.30](https://doi.org/10.1037/1082-989X.1.1.30)

Walter, S. D., Eliasziw, M., & Donner, A. (1998). Sample size and
optimal designs for reliability studies. Statistics in Medicine, 17(1),
101-110.
[doi:10.1002/(SICI)1097-0258(19980115)17:1\<101::AID-SIM727\>3.0.CO;2-E](https://doi.org/10.1002/%28SICI%291097-0258%2819980115%2917%3A1%3C101%3A%3AAID-SIM727%3E3.0.CO%3B2-E)

## See also

[`precision_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/precision_icc.md)
for the sample size for a confidence interval of a given width;
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
for the analysis.

## Examples

``` r
# Subjects needed to show an ICC above 0.6 when it is 0.8, with 2 trials
power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.8,
          alternative = "greater")
#> 
#>      Power calculation for the one-way ICC, ICC(1) (exact F distribution) 
#> 
#>               n = 38.90078
#>               k = 2
#>            rho0 = 0.6
#>            rho1 = 0.8
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = greater
#> 
#> NOTE: n is the number of subjects; round up to a whole number.
#>       n = number of subjects, k = measurements per subject
#> 

# Power with 30 subjects and 3 trials
power_icc(n = 30, k = 3, rho0 = 0.5, rho1 = 0.75, alternative = "greater",
          model = "twoway")
#> 
#>      Power calculation for the two-way consistency ICC, ICC(C,1) (exact F distribution) 
#> 
#>               n = 30
#>               k = 3
#>            rho0 = 0.5
#>            rho1 = 0.75
#>       sig.level = 0.05
#>           power = 0.8835267
#>     alternative = greater
#> 
#> NOTE: n = number of subjects, k = measurements per subject
#> 

# Trials needed with 20 subjects
power_icc(n = 20, k = NULL, rho0 = 0.5, rho1 = 0.8, power = 0.9,
          alternative = "greater")
#> 
#>      Power calculation for the one-way ICC, ICC(1) (exact F distribution) 
#> 
#>               n = 20
#>               k = 3
#>            rho0 = 0.5
#>            rho1 = 0.8
#>       sig.level = 0.05
#>           power = 0.9
#>     alternative = greater
#> 
#> NOTE: n = number of subjects, k = measurements per subject
#> 
```
