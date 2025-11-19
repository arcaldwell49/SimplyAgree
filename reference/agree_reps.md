# Tests for Absolute Agreement with Replicates

**\[superseded\]**

Development on `agree_reps()` is complete, and for new code we recommend
switching to
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
which is easier to use, has more features, and still under active
development.

agree_nest produces an absolute agreement analysis for data where there
is multiple observations per subject but the mean does not vary within
subjects as described by Zou (2013). Output mirrors that of agree_test
but CCC is calculated via U-statistics.

## Usage

``` r
agree_reps(
  x,
  y,
  id,
  data,
  delta,
  agree.level = 0.95,
  conf.level = 0.95,
  prop_bias = FALSE,
  TOST = TRUE,
  ccc = TRUE
)
```

## Arguments

- x:

  Name of column with first measurement

- y:

  Name of other column with the other measurement to compare to the
  first.

- id:

  Column with subject identifier

- data:

  Data frame with all data

- delta:

  The threshold below which methods agree/can be considered equivalent,
  can be in any units. Equivalence Bound for Agreement.

- agree.level:

  the agreement level required. Default is 95%. The proportion of data
  that should lie between the thresholds, for 95% limits of agreement
  this should be 0.95.

- conf.level:

  the confidence level required. Default is 95%.

- prop_bias:

  Logical indicator (TRUE/FALSE) of whether proportional bias should be
  considered for the limits of agreement calculations.

- TOST:

  Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests
  for the limits of agreement. Default is TRUE.

- ccc:

  Calculate concordance correlation coefficient.

## Value

Returns single list with the results of the agreement analysis.

- `loa`: a data frame of the limits of agreement including the average
  difference between the two sets of measurements, the standard
  deviation of the difference between the two sets of measurements and
  the lower and upper confidence limits of the difference between the
  two sets of measurements.

- `h0_test`: Decision from hypothesis test.

- `ccc.xy`: Lin's concordance correlation coefficient and confidence
  intervals using U-statistics.

- `call`: The matched call.

- `var_comp`: Table of Variance Components.

- `class`: The type of simple_agree analysis.

## References

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
Statistical methods in medical research, 22(6), 630-642.

King, TS and Chinchilli, VM. (2001). A generalized concordance
correlation coefficient for continuous and categorical data. Statistics
in Medicine, 20, 2131:2147.

King, TS; Chinchilli, VM; Carrasco, JL. (2007). A repeated measures
concordance correlation coefficient. Statistics in Medicine, 26,
3095:3113.

Carrasco, JL; Phillips, BR; Puig-Martinez, J; King, TS; Chinchilli, VM.
(2013). Estimation of the concordance correlation coefficient for
repeated measures using SAS and R. Computer Methods and Programs in
Biomedicine, 109, 293-304.

## Examples

``` r
data('reps')
agree_reps(x = "x", y = "y", id = "id", data = reps, delta = 2)
#> Warning: `agree_reps()` was deprecated in SimplyAgree 0.2.0.
#> ℹ Please use `agreement_limit()` instead.
#> Limit of Agreement = 95%
#> Replicate Data Points (true value does not vary)
#> 
#> Hypothesis Test: don't reject h0
#> 
#> ###- Bland-Altman Limits of Agreement (LoA) -###
#>           Estimate Lower CI Upper CI CI Level
#> Bias        0.7152  -0.6667   2.0971     0.95
#> Lower LoA  -2.2317  -7.5482  -0.7295     0.90
#> Upper LoA   3.6622   2.1599   8.9786     0.90
#> 
```
