# Minimal Detectable Change

**\[experimental\]**

Estimates the minimal detectable change (MDC), also called the smallest
detectable or real difference, the "minimal difference" (Weir, 2005), or
the repeatability coefficient: the smallest change in one subject's
score between two occasions that exceeds measurement error with a given
confidence. It is a scaled standard error of measurement (SEM), so it
comes with a confidence interval and an optional test against a change
that matters, and is returned as an object of class `"htest"`.

## Usage

``` r
mdc_test(
  x = NULL,
  data = NULL,
  measure = NULL,
  item = NULL,
  id = NULL,
  col.names = NULL,
  mdc.level = 0.95,
  sided = c("two.sided", "one.sided"),
  k_avg = 1,
  conf.level = 0.95,
  null.value = NULL,
  alternative = c("less", "greater", "two.sided"),
  ...
)
```

## Arguments

- x:

  A formula of the form `values ~ item | id`, or the result of
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
  or
  [`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md)
  with `stat = "SEM"`, whose SEM is then used without refitting. Leave
  `NULL` to use `measure`, `item` and `id` (long data) or `col.names`
  (wide data).

- data:

  A data frame containing the variables.

- measure, item, id:

  Column names (character) of the measurement, the item (trial, rater,
  or occasion) and the subject identifier, for data in long format.

- col.names:

  Column names (character) of the items, for data in wide format (one
  row per subject).

- mdc.level:

  The confidence that a change of at least the MDC is not measurement
  error (default 0.95, the "MDC95"). This is not the confidence level of
  the interval, which is `conf.level`.

- sided:

  `"two.sided"` (default) for a change in either direction, or
  `"one.sided"` when only a change in one stated direction (e.g. an
  improvement) is of interest. A one-sided MDC95 uses the same
  multiplier as the two-sided "MDC90".

- k_avg:

  The number of measurements averaged at each occasion (default 1). Use
  it when the score used in practice is the mean of several trials: the
  MDC then shrinks by \\\sqrt{k\_{avg}}\\.

- conf.level:

  Confidence level of the interval (default 0.95).

- null.value:

  A change that matters, on the scale given by `scale`, to test against:
  by default (`alternative = "less"`), whether the MDC is below it, i.e.
  whether a change of that size can be distinguished from measurement
  error. It should come from an external anchor; see Details. The
  default, `NULL`, gives the estimate and interval without a test.

- alternative:

  `"less"` (default), `"greater"`, or `"two.sided"`. The default tests
  whether the error is smaller than `null.value`. Used only when
  `null.value` is given; without a test the interval is two-sided.

- ...:

  Further arguments passed to
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
  when `x` is not an existing result: `scale`, `model`, `type`,
  `method`, `boot`, `boot_ci`, `replicates` and `se_method`.

## Value

A list of class `c("reli_htest", "htest")`, as for
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
containing:

- statistic, parameter, p.value:

  The test of the SEM (equivalently of the MDC), when a `null.value` was
  given.

- conf.int:

  The confidence interval for the MDC.

- estimate:

  The MDC, on the scale given by `scale`.

- stderr:

  Standard errors of the MDC and of its log.

- null.value:

  The change tested against, if any.

- alternative, method, data.name:

  As for an `htest`.

- percent:

  With `scale = "log"`, the MDC and its interval as a percentage
  increase and decrease.

- sem:

  The SEM result the MDC was computed from.

- fit, data, settings, boot:

  As for
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md);
  `settings` also holds the MDC settings and multiplier (`mdc`).

## Details

**Estimate.** A change score is the difference between two measurements,
each with error SEM, so its error SD is \\\sqrt{2} \cdot SEM\\, and
\$\$MDC = \frac{z \cdot \sqrt{2} \cdot SEM}{\sqrt{k\_{avg}}},\$\$ with
\\z\\ the standard normal quantile for `mdc.level` (1.96 for a two-sided
MDC95; Weir, 2005, eq. 13) and \\k\_{avg}\\ the number of measurements
averaged to give the score at each occasion (`k_avg`; 1 for single
measurements). The mean of \\k\_{avg}\\ measurements has error \\SEM /
\sqrt{k\_{avg}}\\, so averaging shrinks the MDC by the same factor. The
SEM is from
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
with the same `model`, `type` and `scale` choices; the defaults give the
typical error of Hopkins (2000), \\\sqrt{MS_E}\\. With two trials the
MDC equals the half-width of the limits of agreement of
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
which also reports the bias between trials.

**Interval and test.** Because the MDC is a fixed multiple of the SEM,
its interval is the SEM interval times that multiple, and the test of
\\H_0: MDC = \\ `null.value` is the SEM test of `null.value` divided by
it (a \\\chi^2\\ test for the SEM, or the bootstrap with `boot = TRUE`).
With `scale = "log"` the MDC is in sympercents; `percent` gives the
equivalent percentage increase and decrease, which differ:
\\100(e^{MDC/100} - 1)\\ and \\100(1 - e^{-MDC/100})\\.

**Differences between raters or devices.** The same reasoning applies to
two measurements of one subject by different items, such as two raters
or two devices, rather than two occasions: the MDC is the difference
between them that measurement error alone would exceed only 5% of the
time (for 95%). When the raters are interchangeable (any rater's score
is used as it is, and raters are thought of as drawn from a pool), use
`type = "agreement"`, so that systematic differences between raters
count as error; the consistency SEM leaves them out and understates how
far apart two raters can be. For two particular raters, the limits of
agreement of
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
report their bias and the spread around it separately. This is the MDC,
not a multiple of the standard error of prediction (SEP): the SEP
concerns a change from a subject's estimated true score (Weir, 2005),
and \\1.96 \cdot SEP\\ does not bound the difference between two raters.

**Repeat measurements.** When each rater measured each subject more than
once, pass an SEM from
[`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md):
with `facet = "intra"` the MDC is for two measurements by the same rater
(on two occasions, with `occasion`), with `facet = "inter"` and
`type = "agreement"` for two measurements by different raters drawn from
the pool. Averaging is then set in
[`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md)
(`k_avg`, `m_avg`, `r_avg`), and `k_avg` here must be 1.

**An individual threshold, not a group one.** The MDC applies to a
single individual's change between two measurements. It is not a
threshold for an average (group) effect. The uncertainty in a mean
change shrinks with the number of subjects (its standard error is about
\\\sqrt{2} \\ SEM / \sqrt{n}\\), so a mean effect well below the MDC can
be estimated precisely and be clearly different from zero. Judge a mean
effect by its own confidence interval. An effect smaller than the MDC is
small relative to one person's measurement noise; that does not make it
noise itself, and it does not show that the effect is absent!

Nor should the MDC be used to count "responders". An individual's
observed change mixes their true response with within-person variation
from occasion to occasion, so the proportion whose change exceeds the
MDC reflects measurement noise as much as real differences between
people: even if every subject had exactly the same true response, some
would exceed the MDC and some would not (Senn, 2004). Real differences
in response are estimated from the variance of the change scores in a
treated group compared with a control group, or from replicated
crossover designs (Atkinson & Batterham, 2015; Senn, 2016).

**The MDC and the smallest worthwhile change.** The MDC describes
measurement noise. It answers the question: how large an observed change
must be before it can be distinguished from error? Whether a change
*matters* is a separate question that the reliability data cannot
answer. There needs to be an external anchor, such as a clinically
important difference or a change that alters competitive outcomes. Give
that value as `null.value` (with the default `alternative = "less"`) to
test whether the MDC is below it.

A "smallest worthwhile change" (SWC) computed from the same reliability
data cannot serve as that anchor. With SWC = 0.2 times the
between-subject SD (Hopkins, 2004), \\SWC / SEM = 0.2 / \sqrt{1 -
ICC}\\, a restatement of the ICC. Hopkins (2004) offers it as a default
(e.g. for fitness tests in team sports) when no relationship to a real
outcome is available, and it should be treated as such a fallback.

Hopkins's (2004) SWC for solo athletes, 0.3 times the CV, is an external
anchor, but only with the right CV: the within-athlete variability of
top athletes' performance from one *competition* to the next, not the
test's own typical error. Simulations (Hopkins, Hawley & Burke, 1999)
showed that an enhancement of 0.3 of that CV gives a top athlete about
one extra win in 10 events. That CV can be estimated with
[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
(`scale = "log"`) from repeated competition results, with competitions
as items, and 0.3 times it given here as `null.value`. Applied to the
test's own CV instead, the rule is circular: \\SWC / MDC = 0.3 / (1.96
\sqrt{2}) \approx 0.11\\ for every test. The anchor also rests on the
one-in-10 convention and on elite fields of evenly matched athletes; for
athletes further back in the field, Hopkins works out the worthwhile
enhancement case by case.

## References

Atkinson, G., & Batterham, A. M. (2015). True and false interindividual
differences in the physiological response to an intervention.
Experimental Physiology, 100(6), 577-588.
[doi:10.1113/EP085070](https://doi.org/10.1113/EP085070)

Hopkins, W. G. (2000). Measures of reliability in sports medicine and
science. Sports Medicine, 30(1), 1-15.
[doi:10.2165/00007256-200030010-00001](https://doi.org/10.2165/00007256-200030010-00001)

Hopkins, W. G. (2004). How to interpret changes in an athletic
performance test. Sportscience, 8, 1-7.

Hopkins, W. G., Hawley, J. A., & Burke, L. M. (1999). Design and
analysis of research on sport performance enhancement. Medicine &
Science in Sports & Exercise, 31(3), 472-485.
[doi:10.1097/00005768-199903000-00018](https://doi.org/10.1097/00005768-199903000-00018)

Senn, S. (2004). Individual response to treatment: is it a valid
assumption? BMJ, 329(7472), 966-968.
[doi:10.1136/bmj.329.7472.966](https://doi.org/10.1136/bmj.329.7472.966)

Senn, S. (2016). Mastering variation: variance components and
personalised medicine. Statistics in Medicine, 35(7), 966-977.
[doi:10.1002/sim.6739](https://doi.org/10.1002/sim.6739)

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. Journal of Strength and
Conditioning Research, 19(1), 231-240.
[doi:10.1519/15184.1](https://doi.org/10.1519/15184.1)

## See also

[`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md)
for the SEM;
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
for the limits of agreement;
[reli_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/reli_htest-methods.md)
for [`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).

## Examples

``` r
# Weir (2005) data set A: 8 subjects, 2 trials
weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                   t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
mdc_test(col.names = c("t1", "t2"), data = weir)
#> 
#>  Minimal detectable change (95%, two-sided) from the standard error of
#>  measurement (two-way, consistency)
#> 
#> data:  t1, t2
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  13.84918 42.63152
#> sample estimates:
#>      MDC 
#> 20.94635 
#> 

# Can a change of 30 units be distinguished from measurement error?
res <- mdc_test(col.names = c("t1", "t2"), data = weir, null.value = 30)
summary(res)
#> The test is not statistically significant (X-squared(7) = 3.41, p =
#> 0.156; MDC = 20.9, 95% upper bound 37.6) at the 0.05 alpha level. The
#> null hypothesis cannot be rejected: at the desired error rate, it
#> cannot be stated that the true MDC is less than 30. The estimate is the
#> minimal detectable change (two-way, consistency), in the units of the
#> measurement, from the SEM excluding systematic differences between
#> items: the smallest change in either direction between two measurements
#> of one subject that exceeds measurement error with 95% confidence, 1.96
#> * sqrt(2) * SEM. It applies to an individual's change, not to an
#> average (group) effect, which should be judged by its own confidence
#> interval. The data had 8 subjects and 2 items (16 measurements, none
#> missing); variance components were estimated by analysis of variance.
#> The interval and chi-square test are exact under normality (chi-square
#> distribution of the error variance).

# From an existing SEM, one-sided, as a percentage (log scale)
# in the result we can access the MDC on the percentage scale (converted from sympercent)
sem <- reli_err_test(col.names = c("t1", "t2"), data = weir, scale = "log")
res_log <- mdc_test(sem, sided = "one.sided")
res_log$percent
#>     change estimate    lower    upper
#> 1 increase 12.42008 8.047983 26.90570
#> 2 decrease 11.04792 7.448527 21.20134
```
