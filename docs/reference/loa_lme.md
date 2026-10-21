# Limits of Agreement with Linear Mixed Effects

**\[deprecated\]**

`loa_lme()` is deprecated, and for new code we recommend switching to
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
which fits the same random-intercept model with `model = "lme"`, gives
analytic prediction and tolerance limits (no bootstrap), and is still
under active development. It takes the two measurements (`x` and `y`)
rather than their difference and average. Supplying `condition` gives
each condition its own residual variance, as `het_var = TRUE` does here.

This function allows for the calculation of (parametric) bootstrapped
limits of agreement when there are multiple observations per subject.

## Usage

``` r
loa_lme(
  diff,
  avg,
  condition = NULL,
  id,
  data,
  type = c("perc", "norm", "basic"),
  conf.level = 0.95,
  agree.level = 0.95,
  replicates = 999,
  prop_bias = FALSE,
  het_var = FALSE
)
```

## Arguments

- diff:

  Column name of the data frame that includes the difference between the
  2 measurements of interest.

- avg:

  Column name of the data frame that includes the average of the 2
  measurements of interest.

- condition:

  Column name indicating different conditions subjects were tested
  under. This can be left missing if there are no differing conditions
  to be tested.

- id:

  Column name indicating the subject/participant identifier

- data:

  A data frame containing the variables within the model.

- type:

  A character string representing the type of bootstrap confidence
  intervals. Only "norm", "basic", and "perc" currently supported.
  Bias-corrected and accelerated, bca, is the default. See
  ?boot::boot.ci for more details.

- conf.level:

  The confidence level required. Default is 95%.

- agree.level:

  The agreement level required. Default is 95%.

- replicates:

  The number of bootstrap replicates. Passed on to the boot function.
  Default is 999.

- prop_bias:

  Logical indicator (default is FALSE) of whether proportional bias
  should be considered for the limits of agreement calculations.

- het_var:

  Logical indicator (default is FALSE) of whether to assume homogeneity
  of variance in each condition.

## Value

Returns single list with the results of the agreement analysis.

- `var_comp`: Table of variance components

- `loa`: A data frame of the limits of agreement including the average
  difference between the two sets of measurements, the standard
  deviation of the difference between the two sets of measurements and
  the lower and upper confidence limits of the difference between the
  two sets of measurements.

- `call`: The matched call.

## References

Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H.,
Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., &
McKinstry, B. (2016). "Application of mixed effects limits of agreement
in the presence of multiple sources of variability: exemplar from the
comparison of several devices to measure respiratory rate in COPD
patients". PLOS One, 11(12), e0168321.
[doi:10.1371/journal.pone.0168321](https://doi.org/10.1371/journal.pone.0168321)

## See also

[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)

## Examples

``` r
# \donttest{
data("recpre_long")
# Instead of loa_lme(), use:
tolerance_limit(data = recpre_long, x = "PM", y = "AM", id = "id",
                condition = "trial_condition", model = "lme")
#> Agreement between Measures (Difference: x-y)
#> 95% CI for Bias; 95% Prediction Interval
#> Tolerance Limits: at least 95% of differences with 95% confidence
#> Model: random intercept for each id (lme); residual SD by condition
#> 
#>  Condition  Bias          Bias CI     SD Prediction Interval  Tolerance Limits
#>    23C/5.5 0.274 [0.0967, 0.4513] 0.2479   [-0.3141, 0.8621] [-0.5643, 1.1123]
#>    33C/5.5 0.239 [0.0948, 0.3832] 0.2016   [-0.2392, 0.7172] [-0.4426, 0.9206]
#>    33C/7.5 0.240 [0.0764, 0.4036] 0.2288   [-0.3028, 0.7828] [-0.5337, 1.0137]
#> 
#> 
# }
```
