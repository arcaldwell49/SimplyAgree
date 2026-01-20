# Nonparametric Test for Limits of Agreement

**\[stable\]**

`agree_np` A non-parametric approach to limits of agreement. The
hypothesis test is based on binomial proportions within the maximal
allowable differences, and the limits are calculated with quantile
regression.

## Usage

``` r
agree_np(
  x,
  y,
  id = NULL,
  data,
  delta = NULL,
  prop_bias = FALSE,
  TOST = TRUE,
  agree.level = 0.95,
  conf.level = 0.95
)
```

## Arguments

- x:

  Name of column with first measurement.

- y:

  Name of other column with the other measurement to compare to the
  first.

- id:

  Column with subject identifier with samples are taken in replicates.

- data:

  Data frame with all data.

- delta:

  The threshold below which methods agree/can be considered equivalent
  and this argument is required. Equivalence Bound for Agreement or
  Maximal Allowable Difference.

- prop_bias:

  Logical indicator (TRUE/FALSE) of whether proportional bias should be
  considered for the limits of agreement calculations.

- TOST:

  Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests
  for the limits of agreement. Default is TRUE.

- agree.level:

  the agreement level required. Default is 95%. The proportion of data
  that should lie between the thresholds, for 95% limits of agreement
  this should be 0.95.

- conf.level:

  the confidence level required. Default is 95%.

## Value

Returns simple_agree object with the results of the agreement analysis.

- `loa`: A data frame of the limits of agreement.

- `agree`: A data frame of the binomial proportion of results in
  agreement.

- `h0_test`: Decision from hypothesis test.

- `qr_mod`: The quantile regression model.

- `call`: The matched call

## References

Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
comparison studies. In Statistical Methods in Medical Research (Vol. 8,
Issue 2, pp. 135–160). SAGE Publications.
[doi:10.1177/096228029900800204](https://doi.org/10.1177/096228029900800204)

## Examples

``` r
data('reps')
agree_np(x = "x", y = "y", id = "id", data = reps, delta = 2)
#> Warning: Evidence of proportional bias. Consider setting prop_bias to TRUE.
#> Limit of Agreement = 95%
#> Binomial proportions test and quantile regression for LoA
#> 
#>            agreement lower.ci upper.ci
#> % within 2    0.8333   0.5914   0.9453
#> Hypothesis Test: don't reject h0
#> 
#> ###- Quantile Limits of Agreement (LoA) -###
#>           Estimate Lower CI Upper CI CI Level
#> Lower LoA    -1.12  -1.4927  -0.7473     0.90
#> Bias          0.04  -0.5694   0.6494     0.95
#> Upper LoA     2.97   2.2967   3.6433     0.90
#> 
```
