# Sample Size for the Precision of an Intraclass Correlation

**\[maturing\]**

Plans a reliability study so that the confidence interval for an
intraclass correlation coefficient (ICC) from
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
is narrow enough. Computes the assurance (the probability that the
interval is no wider than `width`), or solves for the number of
subjects, the number of measurements per subject, or the width. Exactly
one of `n`, `k`, `width` and `assurance` must be `NULL`.

## Usage

``` r
precision_icc(
  n = NULL,
  k = 2,
  rho = NULL,
  width = NULL,
  conf.level = 0.95,
  assurance = 0.5,
  model = c("oneway", "twoway"),
  unit = c("single", "average"),
  missing = 0,
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

- rho:

  The anticipated ICC.

- width:

  The full width of the confidence interval (upper minus lower limit).
  Zou (2012) uses the half-width \\\omega\\: `width = 2 * omega`.

- conf.level:

  Confidence level of the interval (default 0.95).

- assurance:

  The probability that the realised interval is no wider than `width`.
  The default, 0.5, plans for the median width.

- model:

  `"oneway"` (default) or `"twoway"`. `"twoway"` means the consistency
  ICC, ICC(C,1) of
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md);
  the absolute agreement ICC is not supported (see Details).

- unit:

  `"single"` (default) for the reliability of a single measurement, or
  `"average"` for the reliability of the mean of the `k` measurements
  (ICC(k) or ICC(C,k)), as in
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md).
  With `"average"`, the ICC values are on the average-measure scale and
  `k` cannot be solved for. See Details.

- missing:

  The expected proportion of measurements (subject-by-item cells) that
  will be missing, analysed with `icc_test(method = "reml")`. The
  default, 0, gives the exact complete-data calculation; values above 0
  give an approximation (see Details). For subjects who will be lost
  entirely, reduce `n` instead.

- method:

  `"exact"` (default) computes the assurance from the exact distribution
  of the interval width. `"approximate"` uses the closed form of Zou
  (2012), available only for `model = "oneway"`.

## Value

An object of class `"power.htest"`, printed by
[`stats::print.power.htest()`](https://rdrr.io/r/stats/print.power.htest.html),
with elements `n`, `k`, `rho`, `width`, `conf.level`, `assurance`,
`method` and `note`.

## Details

**Exact assurance.** The two-sided F-based interval for the one-way or
two-way consistency ICC (Searle's interval; Shrout & Fleiss, 1979;
McGraw & Wong, 1996) is a function of the F ratio only, and the F ratio
is a scaled central F variable for any true ICC (see
[`power_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/power_icc.md)).
The assurance is therefore the probability, under that distribution, of
the F values whose interval is no wider than `width`. The width is
largest at moderate F and shrinks towards both ends, so this set has two
parts; the lower part (estimates near \\-1/(k-1)\\) is negligible in
realistic designs. Mondal et al. (2024) recommend this interval for the
one-way ICC.

**Approximate sample size** (Zou, 2012) for a Wald interval with
Fisher's variance, half-width \\\omega\\ and assurance \\1 - \gamma\\:
\$\$n = 1 + \left\[\frac{A z\_{1-\alpha/2} + \sqrt{(A
z\_{1-\alpha/2})^2 + 4 \omega A \|B\| z\_{1-\gamma}
z\_{1-\alpha/2}}}{\omega\sqrt{2k(k-1)}}\right\]^2,\$\$ with \\A =
(1-\rho)(1 + (k-1)\rho)\\ and \\B = k - 2 + 2\rho(1-k)\\. It requires
`assurance` of at least 0.5. Mondal et al. (2024) note that Zou used
\\n - 1\\ in deriving it and that another expression in his paper (the
variance of the log-F statistic, not used here) is incorrect; the exact
method avoids both issues. For a one-sided lower limit, the assurance
that the lower limit exceeds a value \\\rho_0\\ is the power of the
one-sided test: use `power_icc(alternative = "greater")`.

**Average-measure ICCs** (`unit = "average"`): the interval for ICC(k)
or ICC(C,k) has limits \\1 - a/F\\ and \\1 - 1/(bF)\\ (with a and b the
F quantiles), so its width, \\(a - 1/b)/F\\, decreases with F and the
assurance is a single F probability. `width` is on the average-measure
scale. The approximate method is for single measures only.

**Missing measurements** (`missing > 0`): the interval width uses the
observed-data quantities described in
[`power_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/power_icc.md),
an approximation whose median widths were within 2.2% of simulated REML
intervals with up to 30% of measurements missing.

The absolute agreement ICC is not supported exactly; see "Planning other
designs" in
[`power_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/power_icc.md)
for using these calculations as a guide for it, for missing data, and
for non-normal data.

## References

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
[doi:10.1037/1082-989X.1.1.30](https://doi.org/10.1037/1082-989X.1.1.30)

Mondal, D., Vanbelle, S., Cassese, A., & Candel, M. J. J. M. (2024).
Review of sample size determination methods for the intraclass
correlation coefficient in the one-way analysis of variance model.
Statistical Methods in Medical Research, 33(3), 532-553.
[doi:10.1177/09622802231224657](https://doi.org/10.1177/09622802231224657)

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
[doi:10.1037/0033-2909.86.2.420](https://doi.org/10.1037/0033-2909.86.2.420)

Zou, G. Y. (2012). Sample size formulas for estimating intraclass
correlation coefficients with precision and assurance. Statistics in
Medicine, 31(29), 3972-3981.
[doi:10.1002/sim.5466](https://doi.org/10.1002/sim.5466)

## See also

[`power_icc()`](https://aaroncaldwell.us/SimplyAgree/reference/power_icc.md);
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
for the analysis.

## Examples

``` r
# Subjects for a median 95% CI width of 0.2 around an ICC of 0.8, 3 trials
precision_icc(rho = 0.8, k = 3, width = 0.2)
#> 
#>      Precision (confidence interval width) calculation for the one-way ICC, ICC(1) (exact F distribution) 
#> 
#>               n = 36.39072
#>               k = 3
#>             rho = 0.8
#>           width = 0.2
#>      conf.level = 0.95
#>       assurance = 0.5
#> 
#> NOTE: n is the number of subjects; round up to a whole number.
#>       n = number of subjects, k = measurements per subject; width = upper minus lower limit
#> 

# ... with 90% assurance
precision_icc(rho = 0.8, k = 3, width = 0.2, assurance = 0.9)
#> 
#>      Precision (confidence interval width) calculation for the one-way ICC, ICC(1) (exact F distribution) 
#> 
#>               n = 54.50067
#>               k = 3
#>             rho = 0.8
#>           width = 0.2
#>      conf.level = 0.95
#>       assurance = 0.9
#> 
#> NOTE: n is the number of subjects; round up to a whole number.
#>       n = number of subjects, k = measurements per subject; width = upper minus lower limit
#> 

# Zou (2012), Table 1: half-width 0.1, 80% assurance
precision_icc(rho = 0.6, k = 2, width = 0.2, assurance = 0.8,
              method = "approximate")
#> 
#>      Precision (confidence interval width) calculation for the one-way ICC, ICC(1) (approximate closed form) 
#> 
#>               n = 182.8004
#>               k = 2
#>             rho = 0.6
#>           width = 0.2
#>      conf.level = 0.95
#>       assurance = 0.8
#> 
#> NOTE: n is the number of subjects; round up to a whole number.
#>       n = number of subjects, k = measurements per subject; width = upper minus lower limit
#> 
```
