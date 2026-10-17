# Reliability Statistics

**\[superseded\]**

`reli_stats()` and `reli_aov()` return the intraclass correlation
coefficients (ICCs), the coefficient of variation (CV), and the standard
errors of measurement (SEM), estimate (SEE) and prediction (SEP)
described by Weir (2005), all at once.

These functions are superseded: they still work, but new code should use
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
for an ICC (with a test against a meaningful null value and optional
bootstrap inference) and
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
for the SEM, SEE or SEP, including the CV via `scale = "log"`
(sympercents) or `scale = "mean"`. See Details for how the two sets of
functions differ.

## Usage

``` r
reli_stats(
  measure = NULL,
  item = NULL,
  id = NULL,
  data,
  wide = FALSE,
  col.names = NULL,
  se_type = c("MSE", "ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k", "ICC3k"),
  cv_calc = c("MSE", "residuals", "SEM"),
  conf.level = 0.95,
  other_ci = FALSE,
  type = c("chisq", "perc", "norm", "basic"),
  replicates = 1999
)

reli_aov(
  measure = NULL,
  item = NULL,
  id = NULL,
  data,
  wide = FALSE,
  col.names = NULL,
  se_type = c("MSE", "ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k", "ICC3k"),
  cv_calc = c("MSE", "residuals", "SEM"),
  conf.level = 0.95,
  other_ci = FALSE,
  type = c("chisq", "perc", "norm", "basic"),
  replicates = 1999
)
```

## Arguments

- measure:

  Name of column containing the measurement of interest.

- item:

  Name of column containing the items. If this is a test-retest
  reliability study then this would indicate the time point (e.g.,
  time1, time2, time3, etc.).

- id:

  Column with subject identifier.

- data:

  Data frame with all data.

- wide:

  Logical value (TRUE or FALSE) indicating if data is in a "wide"
  format. Default is FALSE.

- col.names:

  If wide is equal to TRUE then col.names is a list of the column names
  containing the measurements for reliability analysis.

- se_type:

  Type of standard error calculation. The default, `"MSE"`, uses the
  error mean square for the SEM, and ICC3 (ICC(C,1)) for the SEE and
  SEP. Otherwise, the SD of all the measurements and the named ICC are
  used for all three (see Details).

- cv_calc:

  Coefficient of variation (CV) calculation: `"MSE"` (default), the
  square root of the error mean square divided by the grand mean, or
  `"SEM"`, the SEM (see `se_type`) divided by the grand mean.
  `"residuals"` is no longer supported and is replaced by `"MSE"` with a
  warning.

- conf.level:

  The confidence level required. Default is 95%. The ICC intervals are
  two-sided (see Details).

- other_ci:

  Logical value (TRUE or FALSE) indicating whether to calculate
  confidence intervals for the CV, SEM, SEP, and SEE.

- type:

  The type of confidence intervals for the CV, SEM, SEP, and SEE:
  `"chisq"` (default; analytic intervals, see Details), or a bootstrap
  interval, `"perc"` (percentile), `"norm"` (normal, with bias
  correction) or `"basic"`.

- replicates:

  The number of bootstrap replicates. Default is 1999.

## Value

Returns single list with the results of the reliability analysis.

- `icc`: Table of ICC results

- `lmer`: The fitted model: a linear mixed model from lme4
  (`reli_stats()`), or a linear model of subjects and items
  (`reli_aov()`)

- `anova`: Analysis of Variance table (mean squares reconstructed from
  the variance components for `reli_stats()`)

- `var_comp`: Table of Variance Components

- `n.id`: Number of subjects/participants

- `n.item`: Number of items/time points

- `cv`: Coefficient of Variation (a one-row data frame)

- `SEM`: Standard Error of Measurement (a one-row data frame)

- `SEE`: Standard Error of the Estimate (a one-row data frame)

- `SEP`: Standard Error of Prediction (a one-row data frame)

- `call`: the matched call, with the settings used

## Details

**Estimation.** Both functions use the engine of
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md):
`reli_stats()` fits a linear mixed model by REML (crossed random effects
for subjects and items) and allows missing measurements; `reli_aov()`
uses the analysis of variance and keeps only subjects measured on every
item (incomplete subjects are dropped with a message). With complete
data the two give the same results (unless a variance component is
estimated at zero). With missing data, `reli_stats()` uses degrees of
freedom based on the observed data and warns that its intervals are
approximate.

**ICCs.** The six forms of Shrout and Fleiss (1979) are returned, with
the F-based intervals of McGraw and Wong (1996). The intervals are
two-sided `conf.level` intervals, as in those papers (and in the psych,
irr, SPSS and Stata implementations). Before version 0.3.1 the intervals
used `conf.level` in each tail, so each limit was a one-sided
`conf.level` bound and the pair covered only `1 - 2 * (1 - conf.level)`;
the old output at `conf.level = c` is reproduced by
`conf.level = 1 - 2 * (1 - c)` (e.g. old 0.95 = new 0.90). For a
one-sided lower bound on a single ICC, use
`icc_test(alternative = "greater")`.

**SEM, SEE and SEP.** With `se_type = "MSE"`, SEM = \\\sqrt{MS_E}\\
(Weir, 2005, preferred), and SEE = \\SD\sqrt{ICC(1 - ICC)}\\ and SEP =
\\SD\sqrt{1 - ICC^2}\\ with ICC3. With another `se_type`, the named ICC
is used for all three, and SEM = \\SD\sqrt{1 - ICC}\\. Here SD is the SD
of all the measurements, \\\sqrt{SS\_{total}/(N - 1)}\\, as in Weir
(2005).
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
instead uses the SD matched to the ICC's denominator, so its SEE and SEP
differ slightly. A negative ICC is treated as zero.

**CV.** The CV is the SEM (or \\\sqrt{MS_E}\\) divided by the grand
mean, as a proportion (printed as a percentage).

**Intervals for the CV, SEM, SEE and SEP** (`other_ci = TRUE`). With
`type = "chisq"`, the SEM from \\MS_E\\ has the exact \\\chi^2\\
interval; every other statistic has a Wald interval on the log scale
with a delta-method standard error from the normal-theory variances of
the mean squares (and of the grand mean, for the CV). Before version
0.3.1 these used \\\chi^2\\ intervals with the error degrees of freedom
(and Vangel's interval for the CV), which do not apply to these
statistics. The bootstrap types resample subjects (as
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
does, including its hybrid step for the item mean square); before
version 0.3.1 `reli_stats()` used a parametric bootstrap.

## References

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. Journal of Strength and
Conditioning Research, 19(1), 231-240.
[doi:10.1519/15184.1](https://doi.org/10.1519/15184.1)

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
[doi:10.1037/0033-2909.86.2.420](https://doi.org/10.1037/0033-2909.86.2.420)

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
[doi:10.1037/1082-989X.1.1.30](https://doi.org/10.1037/1082-989X.1.1.30)

## See also

[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)

## Examples

``` r
data('reps')
reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
#> Only 2 items in data. It is recommended to use reli_aov instead of reli_stats.
#> Warning: 5.0% of subject-by-item cells are missing. Intervals use observed-data degrees of freedom and are approximate; consider icc_test(method = "reml", boot = TRUE).
#> 
#> Coefficient of Variation (%):  15.8
#> Standard Error of Measurement (SEM):  0.863
#> Standard Error of the Estimate (SEE):  0.639
#> Standard Error of Prediction (SEP):  1.08
#> 
#> Intraclass Correlation Coefficients with  95 % C.I.
#>            Model         Measures  Type    ICC Lower CI Upper CI
#> 1 one-way random        Agreement  ICC1 0.4956  0.05599   0.7682
#> 2 two-way random        Agreement  ICC2 0.5097  0.09605   0.7726
#> 3  two-way fixed      Consistency  ICC3 0.5384  0.10441   0.7925
#> 4 one-way random   Avg. Agreement ICC1k 0.6627  0.10603   0.8689
#> 5 two-way random   Avg. Agreement ICC2k 0.6753  0.17526   0.8717
#> 6  two-way fixed Avg. Consistency ICC3k 0.7000  0.18908   0.8842
#> 

# The recommended replacements (reps has missing measurements, so REML)
icc_test(data = reps, col.names = c("x", "y"), method = "reml")
#> Warning: 5.0% of subject-by-item cells are missing. Intervals and p-values use observed-data degrees of freedom and are approximate; they may be liberal as missingness increases. Consider boot = TRUE.
#> 
#>  Two-way, consistency, single-measure ICC [Shrout & Fleiss ICC(3,1)]
#>  (REML)
#> 
#> data:  x, y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.1044136 0.7924971
#> sample estimates:
#>  ICC(C,1) 
#> 0.5384325 
#> 
reli_err_test(data = reps, col.names = c("x", "y"), method = "reml")
#> Warning: 5.0% of subject-by-item cells are missing. Intervals and p-values use observed-data degrees of freedom and are approximate; they may be liberal as missingness increases. Consider boot = TRUE.
#> 
#>  Standard error of measurement (two-way, consistency) (REML)
#> 
#> data:  x, y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.6478589 1.2943096
#> sample estimates:
#>       SEM 
#> 0.8633657 
#> 
```
