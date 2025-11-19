# Limits of Agreement

**\[maturing\]**

A function for calculating for Bland-Altman limits of agreement based on
the difference between two measurements (difference = x-y). Please note
that the package developer recommends reporting/using tolerance limits
(see `"tolerance_limit"` function).

## Usage

``` r
agreement_limit(
  x,
  y,
  id = NULL,
  data,
  data_type = c("simple", "nest", "reps"),
  loa_calc = c("mover", "blandaltman"),
  agree.level = 0.95,
  alpha = 0.05,
  prop_bias = FALSE,
  log_tf = FALSE,
  log_tf_display = c("ratio", "sympercent"),
  lmer_df = c("satterthwaite", "asymptotic"),
  lmer_limit = 3000
)
```

## Arguments

- x:

  Name of column with first measurement

- y:

  Name of other column with the other measurement to compare to the
  first.

- id:

  Column with subject identifier. Default is "id" if no entry is
  provided.

- data:

  Data frame with all data.

- data_type:

  The type of data structure. Options include "simple" (all independent
  data points), "nest" (nested data) and "reps" (replicated data
  points).

- loa_calc:

  The method by which the limits of agreement confidence intervals are
  calculated. Options are "mover" (Methods of Recovering Variances
  method) or "blandlatman" (Bland-Altman method).

- agree.level:

  the agreement level required. Default is 95%. The proportion of data
  that should lie between the thresholds, for 95% limits of agreement
  this should be 0.95.

- alpha:

  The alpha-level for confidence levels.

- prop_bias:

  Logical indicator (TRUE/FALSE) of whether proportional bias should be
  considered for the limits of agreement calculations.

- log_tf:

  Calculate limits of agreement using log-transformed data.

- log_tf_display:

  The type of presentation for log-transformed results. The differences
  between methods can be displayed as a "ratio" or "sympercent".

- lmer_df:

  Degrees of freedom method, only matters for if data_type is "nest".
  Default is "satterthwaite". The "asymptotic" method is faster but more
  liberal.

- lmer_limit:

  Sample size limit for degrees of freedom method. If number of
  observations exceeds this limit, then the "asymptotic" method is
  utilized.

## Value

Returns single loa class object with the results of the agreement
analysis.

- `loa`: A data frame containing the Limits of Agreement.

- `call`:The matched call.

## Details

The limits of agreement (LoA) are calculated in this function are based
on the method originally detailed by Bland & Atlman (1986 & 1999). The
`loa_calc` allow users to specify the calculative method for the LoA
which can be based on Bland-Altman (1999) (`loa_calc = "blandaltman"`),
or by the more accurate MOVER method of Zou (2013) and Donner & Zou
(2012) (`loa_calc = "mover"`).

## References

MOVER methods:

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
Statistical methods in medical research, 22(6), 630-642.

Donner, A., & Zou, G. Y. (2012). Closed-form confidence intervals for
functions of the normal mean and standard deviation. Statistical Methods
in Medical Research, 21(4), 347-359.

Bland & Altman methods:

Bland, J. M., & Altman, D. (1986). Statistical methods for assessing
agreement between two methods of clinical measurement. The Lancet,
327(8476), 307-310.

Bland, J. M., & Altman, D. (1999). Measuring agreement in method
comparison studies. Statistical methods in medical research, 8(2),
135-160.

Bland, J. M., & Altman, D. G. (1996). Statistics notes: measurement
error proportional to the mean. BMJ, 313(7049), 106.

## Examples

``` r
data('reps')

# Simple
agreement_limit(x = "x", y ="y", data = reps)
#> MOVER Limits of Agreement (LoA)
#> 95% LoA @ 5% Alpha-Level
#> Independent Data Points
#> 
#>    Bias           Bias CI Lower LoA Upper LoA            LoA CI
#>  0.4383 [-0.1669, 1.0436]    -1.947     2.824 [-3.0117, 3.8884]
#> 
#> SD of Differences = 1.217

# Replicates
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "rep")
#> MOVER Limits of Agreement (LoA)
#> 95% LoA @ 5% Alpha-Level
#> Data with Replicates
#> 
#>    Bias           Bias CI Lower LoA Upper LoA            LoA CI
#>  0.7152 [-1.5287, 2.9591]    -2.232     3.662 [-7.5482, 8.9786]
#> 
#> SD of Differences = 1.5036

# Nested
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "nest")
#> Cannot use mode = "satterthwaite" because *lmerTest* package is not installed
#> MOVER Limits of Agreement (LoA)
#> 95% LoA @ 5% Alpha-Level
#> Nested Data
#> 
#>    Bias           Bias CI Lower LoA Upper LoA            LoA CI
#>  0.7046 [-1.5512, 2.9604]    -2.153     3.562 [-7.4979, 8.9071]
#> 
#> SD of Differences = 1.4581
```
