# Reliability Across Facets: Repeats and Occasions

**\[experimental\]**

**This function is experimental and may change**; its arguments and name
may change too even if its functionality is retained.

Estimates one reliability coefficient or one standard error of
measurement (SEM) when each subject is measured more than once by each
item (rater, device). For example, 3 operators each scanning 20
participants twice. The extra measurements separate error *within* a
rater from disagreement *between* raters, which
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
and
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
cannot do with one measurement per subject and item. The measurements
can be exchangeable repeats within each subject-by-item cell (the
default), or shared occasions (`occasion`: every rater measured every
subject on day 1 and again on day 2). The model is the
generalizability-theory (G-study) model for that design. It's G-study
capabilities are narrow, which is purposeful at this time. Returns an
object of class `"htest"` with a confidence interval and an optional
test.

## Usage

``` r
reli_facet_test(
  x = NULL,
  data = NULL,
  measure = NULL,
  item = NULL,
  id = NULL,
  occasion = NULL,
  stat = c("ICC", "SEM"),
  facet = c("intra", "inter", "overall"),
  type = c("consistency", "agreement"),
  k_avg = 1,
  m_avg = 1,
  r_avg = 1,
  scale = c("raw", "log", "mean"),
  method = c("anova", "reml"),
  conf.level = 0.95,
  null.value = NULL,
  alternative = c("two.sided", "greater", "less"),
  ci_method = c("auto", "mls", "satterthwaite"),
  boot = FALSE,
  boot_ci = c("auto", "perc", "bca", "stud", "basic"),
  replicates = 1999,
  se_method = c("jackknife", "normal")
)
```

## Arguments

- x:

  A formula of the form `values ~ item | id`, with several rows per
  subject and item. Leave `NULL` to use `measure`, `item` and `id`. Wide
  data are not supported.

- data:

  A data frame containing the variables.

- measure, item, id:

  Column names (character) of the measurement, the item (trial, rater,
  or occasion) and the subject identifier, for data in long format.

- occasion:

  `NULL` (default) when the measurements within each subject-by-item
  cell are exchangeable repeats, or the name of the column identifying
  shared occasions (e.g. day 1 and day 2, the same for every item). See
  "Nested or crossed".

- stat:

  `"ICC"` (default), a reliability coefficient, or `"SEM"`, the standard
  error of measurement. See Details.

- facet:

  Which measurement varies: `"intra"` (default), the same item on
  another repeat or occasion (intra-rater, repeatability); `"inter"`,
  another item (on the same occasion, with `occasion`); or `"overall"`,
  another item on another occasion (the same as `"inter"` without
  `occasion`). See Details.

- type:

  `"consistency"` (default) sets systematic differences between items
  (and occasions) aside, to be reported separately; `"agreement"` counts
  them as error, as in
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md).
  Without `occasion`, `facet = "intra"` has no systematic effects and
  `type` makes no difference.

- k_avg, m_avg, r_avg:

  The planned score (D-study): the mean of `k_avg` items, `m_avg`
  occasions and `r_avg` repeats (defaults 1, a single measurement).
  `k_avg` must be 1 for `facet = "intra"` and `m_avg` must be 1 for
  `facet = "inter"` and without `occasion`; `r_avg` must be 1 for
  crossed data with one measurement per subject, item and occasion.

- scale:

  For `stat = "SEM"`, the scale it is reported on: `"raw"` (default),
  `"log"` (sympercents) or `"mean"` (percentage of the grand mean), as
  in
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md).
  Must be `"raw"` for `stat = "ICC"`.

- method:

  `"anova"` (default) for balanced data (every subject measured by every
  item, on every occasion, the same number of times), or `"reml"` (a
  linear mixed model) for missing cells or unequal numbers of repeats.

- conf.level:

  Confidence level of the interval (default 0.95).

- null.value:

  A value to test against, on the scale of the estimate. The default,
  `NULL`, gives the estimate and interval without a test.

- alternative:

  `"two.sided"` (default), `"greater"` or `"less"`. For an SEM, `"less"`
  tests whether the error is below an acceptable value. Used only when
  `null.value` is given.

- ci_method:

  The analytic interval when it is not exact: `"auto"` (default),
  `"mls"` (modified large-sample) or `"satterthwaite"`. See Details.
  Ignored when the interval is exact or `boot = TRUE`.

- boot:

  If `TRUE`, use a nonparametric bootstrap that resamples subjects for
  the interval and p-value. Default `FALSE`.

- boot_ci:

  Bootstrap interval type: `"auto"` (default), `"perc"`, `"bca"`,
  `"stud"` (studentized) or `"basic"`. With `method = "anova"`, `"auto"`
  uses `"bca"` for an agreement SEM that counts systematic differences
  between items or occasions and `"stud"` otherwise (`"perc"` when
  `se_method = "normal"`); with `method = "reml"` it uses `"perc"`. See
  Details. Used only when `boot = TRUE`.

- replicates:

  Number of bootstrap replicates (default 1999).

- se_method:

  How the standard error on the working scale is estimated:
  `"jackknife"` (default; delete one subject at a time) or `"normal"`
  (normal theory, delta method). Reported in `stderr` and used by the
  studentized bootstrap.

## Value

A list of class `c("reli_htest", "htest")` containing:

- statistic, parameter:

  The F (coefficients) or \\\chi^2\\ (SEM) statistic and its df, when a
  test was requested with an exact or Satterthwaite method (z for SEMs
  with `scale = "mean"`).

- p.value:

  The p-value, when a test was requested.

- conf.int:

  The confidence interval.

- estimate:

  The estimate.

- stderr:

  Standard errors of the estimate and on the working scale (`z`:
  \\-\log(1 - \rho)/2\\ for coefficients, the log for SEMs), from
  `se_method`; with `boot = TRUE`, also `boot`.

- null.value, alternative, method, data.name:

  As for an `htest`.

- components:

  The variance components with intervals and shares.

- nested:

  With `occasion` and `facet = "intra"`: the estimate the repeat model
  (occasions ignored) would give.

- fit:

  The mean squares, degrees of freedom and fitted model.

- data:

  The data in long format.

- settings:

  The settings used, for the methods.

- boot:

  Bootstrap details (`NULL` unless `boot = TRUE`).

## Details

When considered nested versus crossed, ask whether "measurement 2" is
the same event for every item. If each operator simply scanned each
participant twice, in no shared order, the measurements are exchangeable
repeats nested in the subject-by-item cell: leave `occasion = NULL`. If
every operator measured everyone on day 1 and again on day 2, the days
are shared occasions: give `occasion`. Treating occasions as repeats
counts day-to-day shifts as repeat error, so the intra-rater error looks
larger and cannot be split into a systematic shift (fixable) and noise.
With `occasion`, the result (in `nested`) and
[`summary()`](https://rdrr.io/r/base/summary.html) also give what the
repeat model would have estimated.

For the model, all effects are random. Without `occasion`, with n
subjects, k items and r repeats per cell, \$\$y\_{pir} = \mu + p + i +
(pi) + e\_{pir};\$\$ with `occasion` (m occasions), \$\$y\_{pio} = \mu +
p + i + o + (pi) + (po) + (io) + (pio, e),\$\$ where with one
measurement per subject, item and occasion the three-way interaction and
the error are not separable (the residual). Further repeats within each
subject, item and occasion are allowed and add a separate repeat error.
The ANOVA estimators are unconstrained: a negative variance component
estimate is kept (as
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
keeps a negative ICC), so a coefficient can be negative. The component
table in `components` (and
[`summary()`](https://rdrr.io/r/base/summary.html)) shows each component
with an interval and its share of the variance of a single measurement.
The model is the unrestricted (random facets) one used by lme4 and by
McGraw and Wong (1996); for the consistency forms the items may be
regarded as fixed, which changes the interpretation but not the
computation.

Following Brennan (2001), the facet that stays the same is part of the
subject's score, and the facets that vary are error. For a planned score
that is the mean of \\k'\\ items (`k_avg`), \\m'\\ occasions (`m_avg`)
and \\r'\\ repeats (`r_avg`), the error variance (SEM\\^2\\) is:

|  |  |  |  |
|----|----|----|----|
| `facet` | Score | Consistency error | Agreement adds |
| intra, repeats | \\\sigma^2_p + \sigma^2\_{pi}\\ | \\\sigma^2_e/r'\\ | (nothing) |
| intra, occasions | \\\sigma^2_p + \sigma^2\_{pi}\\ | \\(\sigma^2\_{po} + \sigma^2\_{pio,e})/m'\\ | \\(\sigma^2_o + \sigma^2\_{io})/m'\\ |
| inter, repeats | \\\sigma^2_p\\ | \\\sigma^2\_{pi}/k' + \sigma^2_e/(k'r')\\ | \\\sigma^2_i/k'\\ |
| inter, occasions | \\\sigma^2_p + \sigma^2\_{po}\\ | \\(\sigma^2\_{pi} + \sigma^2\_{pio,e})/k'\\ | \\(\sigma^2_i + \sigma^2\_{io})/k'\\ |
| overall, occasions | \\\sigma^2_p\\ | \\\sigma^2\_{pi}/k' + \sigma^2\_{po}/m' + \sigma^2\_{pio,e}/(k'm')\\ | \\\sigma^2_i/k' + \sigma^2_o/m' + \sigma^2\_{io}/(k'm')\\ |

(with further repeats within subject, item and occasion, the residual is
split and its error part is also divided by \\r'\\). The coefficient
(`stat = "ICC"`) is score / (score + error). The intra-rater coefficient
is the correlation between two measurements by the same item, pooled
over items (it assumes the same error for every item;
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
shows the within-cell SD by item). With occasions, the inter-rater
coefficient is for raters on the same occasion, what most inter-rater
studies report; `"overall"` is for any rater on any occasion, and its
coefficients are the generalizability (\\E\rho^2\\) and dependability
(\\\Phi\\) coefficients of G theory. In gauge R&R terms (repeats),
\\\sigma^2_e\\ is repeatability and \\\sigma^2_i + \sigma^2\_{pi}\\
reproducibility.

It is important to know what averaging does for this function. Without
`occasion` and with \\r' = r\\ (the score averages as many repeats as
the study had), the inter-rater consistency coefficient is the
consistency ICC of the cell means, \\(F - 1)/(F - 1 + k/k')\\ with \\F =
MS_P/MS\_{PI}\\, identical to
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
on the cell means; it is not the reliability of a *single* measurement
unless r = 1. Averaging the repeats before calling
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
is therefore only right when the scores used in practice are also means
of r repeats.

An estimand that is a multiple of one mean square (an SEM) or a function
of the ratio of two (a coefficient) has an exact interval and test.
Without `occasion`, the intra SEM (\\\chi^2\\ on \\MS_W\\) and the
inter-rater consistency SEM and coefficient with \\r' = r\\. The others
are approximate:

- `"mls"`: modified large-sample intervals for linear combinations of
  mean squares (Graybill & Wang, 1980; Ting et al., 1990; Burdick &
  Graybill, 1992). For a coefficient \\\rho\\, the interval holds the
  values \\\rho_0\\ for which the MLS interval of \\\sigma^2\_{score} -
  \rho_0 \sigma^2\_{total}\\ contains 0. The p-value inverts the
  interval, so there is no test statistic.

- `"satterthwaite"`: the SEM uses a \\\chi^2\\ distribution with
  Satterthwaite df; a coefficient uses an F test of
  \\\sigma^2\_{score} - \rho_0 \sigma^2\_{total} = 0\\, the positive and
  negative parts each with Satterthwaite df (McGraw & Wong's device for
  ICC(A,1)), and the interval inverts it.

- `"auto"` uses MLS, chosen in simulations (see "Simulations").

SEMs with `scale = "mean"` use a Wald interval on the log scale with a
delta-method SE that includes the uncertainty of the grand mean.

When wanting bootstrapped results, `boot = TRUE` resamples subjects,
each with all its measurements, as in
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md).
Resampling subjects keeps the same items and occasions, so for
statistics that count systematic differences between them as error, the
mean squares of the effects without subjects (items, occasions, items by
occasions) are also multiplied by independent \\\chi^2\_{df}/df\\ draws
(the hybrid step). The p-value inverts the interval and is approximate.

When there is unbalanced data, `method = "reml"` fits the model with
lme4 and keeps the analytic inference, with degrees of freedom from the
observed data and the harmonic mean number of repeats in place of r, and
gives a warning.

In simulations, repeats (no `occasion`): with 10 or 30 subjects, 2, 3 or
5 items, 2 or 3 repeats, four mixes of variance components and normal
data (2000 data sets per setting), the exact intervals covered 0.940 to
0.951 and the MLS intervals at least 0.939 for every estimand (0.95 to
0.96 on average; slightly conservative for \\\Phi\\), hence
`ci_method = "auto"` uses MLS. The Satterthwaite intervals were similar
for the intra-rater and consistency statistics but fell to 0.67 to 0.74
for the agreement SEM and coefficient with 2 items when systematic
differences between items were the main error. With REML and unbalanced
data (30 subjects, 20% of repeats missing, or 1 to 3 repeats per cell),
the MLS intervals covered 0.937 to 0.969.

Occasions (2 or 3 items, 2 or 3 occasions, one measurement per cell,
four mixes including a large day effect and large rater bias, 2000 data
sets per setting): the MLS intervals covered at least 0.934 with 30
subjects and 0.928 with 10, for every facet, type and statistic; the
Satterthwaite intervals fell to 0.69 to 0.79 for the agreement
statistics.

**Warning:** there are uneven tails for agreement statistics. The
agreement statistics include a component estimated from very few degrees
of freedom (items, or occasions: k - 1 or m - 1). The MLS bound that
this component drives is wide, and the other bound is close to nominal.
With 30 subjects and 95% two-sided intervals, the agreement coefficients
fell below their lower limit 0 to 1% of the time and above their upper
limit about 3% (nominal 2.5% each; two-sided coverage up to 0.97 to 0.99
with occasions), and the agreement SEMs fell above their upper limit 0
to 0.6% of the time and below their lower limit 3 to 6%. So the usual
one-sided tests are conservative: a coefficient greater than a threshold
(`alternative = "greater"`) and an SEM less than an acceptable error
(`alternative = "less"`). The opposite one-sided tests can be somewhat
liberal (up to about 6% instead of 2.5% for an agreement SEM greater
than a threshold). With REML and 20% of cells missing, MLS covered 0.922
to 0.979. Treating the occasions as exchangeable repeats (leaving out
`occasion`) gave intra-rater SEM intervals that covered the true
agreement SEM only 0.24 to 0.92 of the time (0.04 to 0.87 for the
consistency SEM, which it overstated by 30% on average).

With skewed, heavy-tailed data every analytic interval under-covered
(0.78 to 0.90 with repeats). The studentized bootstrap was then the best
interval for every statistic except the agreement SEM, for which BCa was
best, hence `boot_ci = "auto"`: with repeats it covered 0.90 to 0.93 for
the intra-rater and consistency statistics with 30 subjects (2 or 3
items, 2 repeats, 300 data sets of 499 replicates per setting). As in
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
agreement statistics with few items (or occasions) and large systematic
differences between them under-covered with every method (0.69 to 0.85
with 2 or 3 items); more items help more than more subjects. Resampling
subjects without the hybrid step covered as little as 0.15 there. With
normal data the analytic intervals were better than any bootstrap
interval. With occasions (2 days), the bootstrap gave the same `"auto"`
choices and helped for the consistency statistics (0.92 to 0.93 with
skewed data), but for the agreement statistics the MLS interval was as
good or better even with skewed data, and every interval under-covered
when a day effect on 2 days dominated the error.

**Other software:** Given the narrow range of application, I want
highlight a few R packages. VCA (variance components for deeper nesting
and laboratory precision designs), rptR (repeatability for non-Gaussian
data), gtheoryr (crossed multi-facet G-studies and D-study design
optimisation), SixSigma (`ss.rr()`, gauge R&R summaries) and MethComp
(method comparison with linked replicates and method-specific variances)
fit related models. This function adds intervals and tests for the
coefficients and the intra- and inter-rater SEMs, and the link to
[`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md).

## References

Brennan, R. L. (2001). Generalizability Theory. Springer.
[doi:10.1007/978-1-4757-3456-0](https://doi.org/10.1007/978-1-4757-3456-0)

Burdick, R. K., & Graybill, F. A. (1992). Confidence Intervals on
Variance Components. Marcel Dekker.

Graybill, F. A., & Wang, C. M. (1980). Confidence intervals on
nonnegative linear combinations of variances. Journal of the American
Statistical Association, 75(372), 869-873.
[doi:10.1080/01621459.1980.10477565](https://doi.org/10.1080/01621459.1980.10477565)

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. Psychological Methods, 1(1), 30-46.
[doi:10.1037/1082-989X.1.1.30](https://doi.org/10.1037/1082-989X.1.1.30)

Ting, N., Burdick, R. K., Graybill, F. A., Jeyaratnam, S., & Lu, T.-F.
C. (1990). Confidence intervals on linear combinations of variance
components that are unrestricted in sign. Journal of Statistical
Computation and Simulation, 35(3-4), 135-143.
[doi:10.1080/00949659008811240](https://doi.org/10.1080/00949659008811240)

## See also

[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
and
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
for one measurement per subject and item;
[`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md);
[reli_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/reli_htest-methods.md)
for [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).

## Examples

``` r
# 12 participants, 3 operators, 2 scans each (exchangeable repeats)
set.seed(2)
scans <- expand.grid(scan = 1:2, operator = c("A", "B", "C"), id = 1:12)
subj <- rnorm(12, 50, 5)
bias <- c(A = 0, B = 1.5, C = -1)
scans$thickness <- subj[scans$id] + bias[scans$operator] +
  rnorm(nrow(scans), 0, 1.5)

# Intra-rater (repeatability) ICC and SEM
reli_facet_test(thickness ~ operator | id, data = scans)
#> 
#>  Intra-rater ICC (same item, another repeat), for a single measurement
#>  with MLS interval
#> 
#> data:  thickness ~ operator | id
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.7084019 0.9523049
#> sample estimates:
#> ICC(intra) 
#>  0.8627703 
#> 
reli_facet_test(thickness ~ operator | id, data = scans, stat = "SEM")
#> 
#>  Intra-rater SEM (same item, another repeat), for a single measurement
#>  with exact interval
#> 
#> data:  thickness ~ operator | id
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  1.467683 2.344365
#> sample estimates:
#> SEM(intra) 
#>   1.804801 
#> 

# Inter-rater agreement for a single scan by any operator
res <- reli_facet_test(thickness ~ operator | id, data = scans,
                       facet = "inter", type = "agreement")
summary(res)
#> The estimated ICC(inter,A) was 0.827 (95% compatibility interval
#> [0.227, 0.937]). Values in this interval are the most compatible with
#> the data, given the model and its assumptions. The estimate is the
#> inter-rater ICC (absolute agreement, another item), for a single
#> measurement, with error from: items, subjects x items, repeats (within
#> cells). The data had 12 subjects, 3 items (72 measurements, 2
#> measurements per subject-by-item cell); variance components were
#> estimated by analysis of variance (unconstrained). The interval is a
#> modified large-sample (MLS) interval for a ratio of linear combinations
#> of mean squares (Graybill & Wang, 1980; Ting et al., 1990).
#> 
#> Variance components:
#>  Component              Variance 95% CI        SD   Percent
#>  subjects               21.0     [10.3, 61.2]  4.58 81.1   
#>  items                  1.63     [0.368, 68.1] 1.28 6.3    
#>  subjects x items       -0.512   [0, 0.716]    0    0.0    
#>  repeats (within cells) 3.26     [2.15, 5.50]  1.80 12.6   

# D-study: the mean of 2 scans by one operator
reli_facet_test(thickness ~ operator | id, data = scans, facet = "inter",
                type = "agreement", r_avg = 2)
#> 
#>  Inter-rater ICC (absolute agreement, another item), for the mean of 2
#>  repeats with MLS interval
#> 
#> data:  thickness ~ operator | id
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.2320697 0.9641056
#> sample estimates:
#> ICC(inter,A) 
#>    0.8841374 
#> 

# If scan 1 and scan 2 were shared days, the same for every operator
day1 <- reli_facet_test(thickness ~ operator | id, data = scans,
                        occasion = "scan", type = "agreement")
day1
#> 
#>  Intra-rater ICC (absolute agreement, same item, another occasion), for
#>  a single measurement with MLS interval
#> 
#> data:  thickness ~ operator | id, occasions: scan
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.2672571 0.9511654
#> sample estimates:
#> ICC(intra,A) 
#>    0.8624128 
#> 
day1$nested
#> $estimate
#> ICC(intra) 
#>  0.8627703 
#> 
#> $components
#>                component   variance      lower      upper       sd   percent
#> 1               subjects 20.9911484 10.3465885 61.2025740 4.581610 81.100437
#> 2                  items  1.6344496  0.3684636 68.1309585 1.278456  6.314784
#> 3       subjects x items -0.5122982  0.0000000  0.7155093 0.000000  0.000000
#> 4 repeats (within cells)  3.2573063  2.1540936  5.4960478 1.804801 12.584779
#> 

# Smallest difference between two operators beyond measurement error
mdc_test(reli_facet_test(thickness ~ operator | id, data = scans,
                         stat = "SEM", facet = "inter",
                         type = "agreement"))
#> 
#>  Minimal detectable change (95%, two-sided) from the inter-rater SEM
#>  (absolute agreement, another item), for a single measurement with MLS
#>  interval
#> 
#> data:  thickness ~ operator | id
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>   4.756345 23.339395
#> sample estimates:
#>      MDC 
#> 5.800605 
#> 
```
