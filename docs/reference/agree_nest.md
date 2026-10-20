# Tests for Absolute Agreement with Nested Data

**\[superseded\]**

Development on `agree_nest()` is complete, and for new code we recommend
switching to
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
which is easier to use, has more features, and still under active
development.

`agree_nest` produces an absolute agreement analysis for data where
there is multiple observations per subject but the mean varies within
subjects as described by Zou (2013). Output mirrors that of agree_test
but the CCC is Lin's estimator over all complete pairs with a
delete-one-subject jackknife standard error; see
[`ccc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/ccc_test.md).

## Usage

``` r
agree_nest(
  x,
  y,
  id,
  data,
  delta,
  agree.level = 0.95,
  conf.level = 0.95,
  TOST = TRUE,
  prop_bias = FALSE,
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

- TOST:

  Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests
  for the limits of agreement. Default is TRUE.

- prop_bias:

  Logical indicator (TRUE/FALSE) of whether proportional bias should be
  considered for the limits of agreement calculations.

- ccc:

  Calculate concordance correlation coefficient.

## Value

Returns single simple_agree class object with the results of the
agreement analysis.

- `loa`: A data frame of the limits of agreement including the average
  difference between the two sets of measurements, the standard
  deviation of the difference between the two sets of measurements and
  the lower and upper confidence limits of the difference between the
  two sets of measurements.

- `h0_test`: Decision from hypothesis test.

- `ccc.xy`: Lin's concordance correlation coefficient over all complete
  pairs and confidence intervals, as in `ccc_test(data_type = "nest")`.

- `call`: the matched call.

- `var_comp`: Table of Variance Components.

- `class`: The type of simple_agree analysis.

## References

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
Statistical methods in medical research, 22(6), 630-642.

Lin, L. I. (1989). A concordance correlation coefficient to evaluate
reproducibility. Biometrics, 45(1), 255-268.

## Examples

``` r
data('reps')
agree_nest(x = "x", y = "y", id = "id", data = reps, delta = 2)
#> Warning: `agree_nest()` was deprecated in SimplyAgree 0.2.0.
#> ℹ Please use `agreement_limit()` instead.
#> Limit of Agreement = 95%
#> Nested Data Points (true value may vary)
#> 
#> Hypothesis Test: don't reject h0
#> 
#> ###- Bland-Altman Limits of Agreement (LoA) -###
#>           Estimate Lower CI Upper CI CI Level
#> Bias        0.7101  -0.6824   2.1026     0.95
#> Lower LoA  -2.1540  -7.5771  -0.6064     0.90
#> Upper LoA   3.5742   2.0266   8.9973     0.90
#> 
```
