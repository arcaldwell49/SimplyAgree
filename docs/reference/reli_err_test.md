# Measurement Error Test

**\[maturing\]**

Estimates one measurement error statistic, the standard error of
measurement (SEM), the standard error of estimate (SEE), or the standard
error of prediction (SEP), with a confidence interval and an optional
test against an acceptable error, and returns an object of class
`"htest"`. The defaults give the typical error of Hopkins (2000): the
SEM from a two-way model with systematic differences between items
excluded, \\\sqrt{MS_E}\\.

## Usage

``` r
reli_err_test(
  x = NULL,
  data = NULL,
  measure = NULL,
  item = NULL,
  id = NULL,
  col.names = NULL,
  stat = c("SEM", "SEE", "SEP"),
  scale = c("raw", "log", "mean"),
  model = c("twoway", "oneway"),
  type = c("consistency", "agreement"),
  method = c("anova", "reml"),
  conf.level = 0.95,
  null.value = NULL,
  alternative = c("less", "greater", "two.sided"),
  boot = FALSE,
  boot_ci = c("auto", "perc", "bca", "stud", "basic"),
  replicates = 1999,
  se_method = c("jackknife", "influence", "normal")
)
```

## Arguments

- x:

  A formula of the form `values ~ item | id`, where `values` is the
  measurement, `item` identifies the trial, rater or occasion, and `id`
  identifies the subject. Use either `x`, the `measure`/`item`/`id`
  arguments, or `col.names`.

- data:

  A data frame containing the variables.

- measure, item, id:

  Column names (character) of the measurement, the item (trial, rater,
  or occasion) and the subject identifier, for data in long format.

- col.names:

  Column names (character) of the items, for data in wide format (one
  row per subject).

- stat:

  `"SEM"` (default), `"SEE"`, or `"SEP"`. See Details.

- scale:

  The scale the statistic is reported on: `"raw"` (default, units of the
  measurement), `"log"` (sympercents: 100 times the statistic of the
  log-transformed measurements), or `"mean"` (percentage of the grand
  mean). The last two are coefficients of variation; see Details.

- model:

  `"twoway"` (default) when every subject is measured on the same set of
  items (e.g. test-retest trials), or `"oneway"` when each subject is
  measured by a different set of items.

- type:

  `"consistency"` (default) excludes systematic differences between
  items from the error; `"agreement"` includes them. Only `"agreement"`
  is defined for `model = "oneway"`.

- method:

  `"anova"` (default) estimates the variance components from the
  analysis of variance and requires complete, balanced data. `"reml"`
  fits a linear mixed model and allows missing measurements.

- conf.level:

  Confidence level of the interval (default 0.95).

- null.value:

  An acceptable error to test against, on the scale given by `scale`
  (units, sympercents, or percent of the mean). The default, `NULL`,
  gives the estimate and interval without a test.

- alternative:

  `"less"` (default), `"greater"`, or `"two.sided"`. The default tests
  whether the error is smaller than `null.value`. Used only when
  `null.value` is given; without a test the interval is two-sided.

- boot:

  If `TRUE`, use a nonparametric bootstrap that resamples subjects for
  the interval and p-value. Default `FALSE`.

- boot_ci:

  Bootstrap interval type: `"auto"` (default), `"perc"` (percentile),
  `"bca"`, `"stud"` (studentized), or `"basic"`. With
  `method = "anova"`, `"auto"` uses `"bca"` for the two-way agreement
  statistics and `"stud"` otherwise (`"perc"` when
  `se_method = "normal"`); with `method = "reml"` it uses `"perc"`. See
  Details. Used only when `boot = TRUE`.

- replicates:

  Number of bootstrap replicates (default 1999).

- se_method:

  How the standard error of the log of the statistic is estimated:
  `"jackknife"` (default; delete one subject at a time, accurate in
  simulations but n refits with `method = "reml"`), `"influence"` (a
  per-subject sandwich estimate; cheap but too small with few subjects),
  or `"normal"` (normal theory; too small with skewed or heavy-tailed
  data, and cannot be used by the studentized bootstrap). It is reported
  in `stderr` and used by the studentized bootstrap; it does not change
  the analytic interval or test. For the two-way agreement statistics,
  the jackknife and influence SEs add the normal-theory variance of the
  item mean square, treating the items as random (as the hybrid
  bootstrap does). See
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
  for the simulations.

## Value

A list of class `c("reli_htest", "htest")` containing:

- statistic:

  The \\\chi^2\\ (SEM) or z (SEE, SEP) statistic, when a test was
  requested without the bootstrap.

- parameter:

  The degrees of freedom of the \\\chi^2\\ test.

- p.value:

  The p-value, when a test was requested.

- conf.int:

  The confidence interval.

- estimate:

  The estimate, named by `stat`.

- stderr:

  Standard errors of the estimate (named by `stat`) and of its log
  (`log`), from `se_method`. With `boot = TRUE`, also `boot`: the
  standard deviation of the bootstrap estimates.

- null.value:

  The acceptable error tested against, if any.

- alternative:

  The alternative hypothesis.

- method:

  A description of the statistic and the inference.

- data.name:

  A description of the data.

- fit:

  The fitted variance components, mean squares and model.

- data:

  The data in long format.

- settings:

  The settings used, for the methods.

- boot:

  Bootstrap details (`NULL` unless `boot = TRUE`).

## Details

**Choosing `model` and `type`.** Choose them as for the ICC (see
"Choosing the form" in
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)):
`type = "agreement"` when systematic differences between items count as
error, `"consistency"` when they are checked and reported separately.
Report the error statistic that matches the ICC.

**Statistics.** With \\MS\\ the mean squares of the subjects-by-items
analysis of variance (n subjects, k items):

|  |  |  |  |  |
|----|----|----|----|----|
| `stat` | `model` | `type` | Estimate | Analytic inference |
| SEM | twoway | consistency | \\\sqrt{MS_E}\\ | exact \\\chi^2\\, df \\(n-1)(k-1)\\ |
| SEM | twoway | agreement | \\\sqrt{(MS_J + (n-1) MS_E)/n}\\ | Satterthwaite \\\chi^2\\ |
| SEM | oneway | agreement | \\\sqrt{MS_W}\\ | exact \\\chi^2\\, df \\n(k-1)\\ |
| SEE | any | any | \\SD \sqrt{ICC(1 - ICC)}\\ | delta method, log scale |
| SEP | any | any | \\SD \sqrt{1 - ICC^2}\\ | delta method, log scale |

The consistency SEM is the typical error of Hopkins (2000) and the
\\\sqrt{MS_E}\\ preferred by Weir (2005); the one-way SEM is the
within-subject SD of Bland and Altman. The agreement SEM adds the
variance between items to the error; with complete data it equals the
one-way SEM, but its degrees of freedom differ.

**SEE and SEP** (Weir, 2005, eqs. 11 and 15) use the single-measure ICC
of the same `model` and `type` (ICC(C,1), ICC(A,1) or ICC(1)). The SEE
is the standard error of a subject's estimated true score; the SEP is
used to judge whether a change from the estimated true score is real.
Both depend on how much subjects differ from each other, so the SEM is
generally the better description of measurement error.

**Total SD.** SD is the total SD matched to the denominator of the ICC:
\\SD^2 = \sigma^2\_{subjects} + \sigma^2\_{error}\\ for consistency,
plus \\\sigma^2\_{items}\\ for agreement. With this SD, \\SD\sqrt{1 -
ICC}\\ equals the SEM exactly, which is why there is no separate
ICC-based SEM. In mean squares (complete data): \\SD^2 = (MS_B +
(k-1)MS_E)/k\\ (consistency), \\(MS_B + (k-1)MS_E + k(MS_J -
MS_E)/n)/k\\ (agreement), and \\(MS_B + (k-1)MS_W)/k\\ (one-way). It is
not the SD of all the measurements, \\\sqrt{SS\_{total}/(N-1)}\\, used
by Weir (2005) and by
[`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
which estimates none of these variances; so SEE and SEP differ slightly
from Weir's (for his data set A, SD 32.83 vs 31.74, SEE 7.35 vs 7.11,
SEP 10.54 vs 10.20). A negative ICC estimate is treated as zero (SEE =
0, SEP = SD).

**Intervals and tests.** The SEM interval is the square root of the
\\\chi^2\\ interval for the error variance, and the test of \\H_0: SEM =
\\ `null.value` is the \\\chi^2\\ test of the variance, \\\nu \cdot
SEM^2 / null^2\\. For the SEE and SEP the interval is a Wald interval on
the log scale, with a delta-method SE from the normal-theory variances
of the mean squares, and the test is the matching z test. With a
one-sided alternative the interval is one-sided. `method = "reml"` with
missing data uses degrees of freedom based on the observed data, as in
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
and gives a warning.

**Bootstrap.** `boot = TRUE` resamples subjects, as in
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
including the hybrid step for the two-way agreement statistics (the item
mean square of each replicate is multiplied by an independent
\\\chi^2\_{k-1}/(k-1)\\ draw). The studentized interval works on the log
scale. The p-value inverts the interval and is approximate.

In simulations (3 trials, 10 or 30 subjects, ICC 0.5 or 0.9, normal or
skewed and heavy-tailed data, complete data), the analytic intervals
covered 0.93 to 0.97 with normal data for the consistency and one-way
statistics, but only 0.79 to 0.90 with non-normal data (the \\\chi^2\\
interval for a variance is sensitive to heavy tails). The studentized
bootstrap was the best method for the consistency SEM, the SEP and the
one-way SEM (0.89 to 0.96 throughout), and BCa for the agreement SEM and
SEP (mean 0.91), hence `boot_ci = "auto"`. The percentile interval was
too narrow with 10 subjects (as low as 0.54). For the SEE the analytic
interval was best with normal data (0.93 to 0.97); no bootstrap interval
was reliable near an ICC of 0.5, where the SEE hardly changes with the
ICC.

**Agreement statistics with few items.** The agreement SEM and SEP
include the variance between items, which is estimated from only k - 1
degrees of freedom. When it is a large part of the error and k is small,
every interval under-covers: with 3 trials and an item variance about as
large as the error variance (ICC 0.9), the Satterthwaite interval
covered 0.81 to 0.91 with normal data and BCa 0.91 to 0.95 (0.80 with 30
subjects and skewed data). Resampling subjects without the hybrid step
covered only 0.39 to 0.63 in those settings. More items help more than
more subjects.

**Coefficient of variation.** When the error grows with the size of the
measurements, it is better expressed relative to them (Hopkins, 2000;
Atkinson and Nevill, 1998; Weir, 2005). Two scales are available:

- `scale = "log"` fits the model to the natural logarithm of the
  measurements (all must be positive) and reports \\100 \times\\ the
  statistic, in sympercents (Cole, 2000). Because this is a constant
  multiple of the log-scale statistic, the SEM keeps its exact (or
  Satterthwaite) \\\chi^2\\ interval and test. A sympercent \\s\\
  converts to the conventional CV% as \\100\sqrt{\exp((s/100)^2) - 1}\\;
  the two are nearly identical below about 15% (e.g. 10 sympercents is a
  CV of 10.03%). The conversion is monotone, so it can be applied to the
  interval limits too.

- `scale = "mean"` reports \\100 \times\\ statistic / grand mean (the
  "pooled" CV). The interval and test use the delta method on the log
  scale, adding the uncertainty in the grand mean, whose normal-theory
  variance is \\MS_B/(nk)\\ with items fixed (consistency, one-way) and
  \\(MS_B + MS_J - MS_E)/(nk)\\ with items random (two-way agreement).

In simulations like those below (population mean 5), the `"mean"`
interval covered 0.93 to 0.96 with normal data and 30 subjects (except
for the agreement SEM with few items, see below); leaving out the
uncertainty in the grand mean lowered coverage by up to 0.05. With the
bootstrap, the grand mean of each replicate of a two-way agreement
statistic also gets the item mean's normal-theory variation, as in the
hybrid step below.

`null.value` is on the same scale as the estimate. The log scale is
usually preferable: it treats the error as proportional to each
measurement rather than to the overall mean.

**Minimal detectable change.** The minimal detectable change (MDC),
smallest real difference, or repeatability coefficient is a scaled SEM:
\\1.96 \sqrt{2} SEM\\ for 95% (Weir, 2005, eq. 13).
[`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md)
computes it, with its interval and a test, from the data or from an SEM
result of this function. With two trials,
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
gives the same quantity as the half width of the limits of agreement,
with the bias reported separately.

## References

Atkinson, G., & Nevill, A. M. (1998). Statistical methods for assessing
measurement error (reliability) in variables relevant to sports
medicine. Sports Medicine, 26(4), 217-238.
[doi:10.2165/00007256-199826040-00002](https://doi.org/10.2165/00007256-199826040-00002)

Cole, T. J. (2000). Sympercents: symmetric percentage differences on the
100 log_e scale simplify the presentation of log transformed data.
Statistics in Medicine, 19(22), 3109-3125.
[doi:10.1002/1097-0258(20001130)19:22\<3109::AID-SIM558\>3.0.CO;2-F](https://doi.org/10.1002/1097-0258%2820001130%2919%3A22%3C3109%3A%3AAID-SIM558%3E3.0.CO%3B2-F)

Hopkins, W. G. (2000). Measures of reliability in sports medicine and
science. Sports Medicine, 30(1), 1-15.
[doi:10.2165/00007256-200030010-00001](https://doi.org/10.2165/00007256-200030010-00001)

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. Journal of Strength and
Conditioning Research, 19(1), 231-240.
[doi:10.1519/15184.1](https://doi.org/10.1519/15184.1)

## See also

[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md);
[reli_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/reli_htest-methods.md)
for [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).

## Examples

``` r
# Weir (2005) data set A: 8 subjects, 2 trials
weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                   t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
reli_err_test(col.names = c("t1", "t2"), data = weir)
#> 
#>  Standard error of measurement (two-way, consistency)
#> 
#> data:  t1, t2
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>   4.996445 15.380402
#> sample estimates:
#>      SEM 
#> 7.556927 
#> 

# Is the typical error below 10 units?
reli_err_test(col.names = c("t1", "t2"), data = weir, null.value = 10)
#> 
#>  Standard error of measurement (two-way, consistency)
#> 
#> data:  t1, t2
#> X-squared = 3.9975, df = 7, p-value = 0.2199
#> alternative hypothesis: true SEM is less than 10
#> 95 percent confidence interval:
#>   0.00000 13.58094
#> sample estimates:
#>      SEM 
#> 7.556927 
#> 

# Typical error as a coefficient of variation (sympercents)
reli_err_test(col.names = c("t1", "t2"), data = weir, scale = "log")
#> 
#>  Standard error of measurement (two-way, consistency), log scale
#>  (sympercents)
#> 
#> data:  t1, t2
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>   3.327576 10.243176
#> sample estimates:
#> SEM (sympercent) 
#>         5.032829 
#> 

# Standard error of prediction
res <- reli_err_test(col.names = c("t1", "t2"), data = weir, stat = "SEP")
summary(res)
#> The estimated SEP was 10.5 (95% compatibility interval [6.33, 17.6]).
#> Values in this interval are the most compatible with the data, given
#> the model and its assumptions. The estimate is the standard error of
#> prediction (two-way, consistency), in the units of the measurement: the
#> standard error for judging change from a subject's estimated true
#> score, SD * sqrt(1 - ICC^2) with ICC(C,1) and the SD matched to its
#> denominator, excluding systematic differences between items. The data
#> had 8 subjects and 2 items (16 measurements, none missing); variance
#> components were estimated by analysis of variance. The interval uses
#> the delta method on the log scale with normal-theory variances of the
#> mean squares.
```
