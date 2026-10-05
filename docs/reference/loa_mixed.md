# Mixed Effects Limits of Agreement

**\[deprecated\]**

`loa_mixed()` is outdated, and for new code we recommend switching to
[`loa_lme()`](https://aaroncaldwell.us/SimplyAgree/reference/loa_lme.md)
or `tolerance_limit`, which are easier to use, have more features, and
are still under active development.

This function allows for the calculation of bootstrapped limits of
agreement when there are multiple observations per subject.

## Usage

``` r
loa_mixed(
  diff,
  condition,
  id,
  data,
  plot.xaxis = NULL,
  delta,
  conf.level = 0.95,
  agree.level = 0.95,
  replicates = 1999,
  type = "bca"
)
```

## Arguments

- diff:

  column name of the data frame that includes the continuous measurement
  of interest.

- condition:

  column name indicating different conditions subjects were tested
  under.

- id:

  column name indicating the subject/participant identifier

- data:

  A data frame containing the variables within the model.

- plot.xaxis:

  column name indicating what to plot on the x.axis for the Bland-Altman
  plots. If this argument is missing or set to NULL then no plot will be
  produced.

- delta:

  The threshold below which methods agree/can be considered equivalent,
  can be in any units. Equivalence Bound for Agreement.

- conf.level:

  the confidence level required. Default is 95%.

- agree.level:

  the agreement level required. Default is 95%.

- replicates:

  the number of bootstrap replicates. Passed on to the boot function.
  Default is 1999.

- type:

  A character string representing the type of bootstrap confidence
  intervals. Only "norm", "basic", "bca", and "perc" currently
  supported. Bias-corrected and accelerated, bca, is the default. See
  ?boot::boot.ci for more details.

## Value

Returns single list with the results of the agreement analysis.

- `var_comp`: Table of variance components

- `loa`: a data frame of the limits of agreement including the average
  difference between the two sets of measurements, the standard
  deviation of the difference between the two sets of measurements and
  the lower and upper confidence limits of the difference between the
  two sets of measurements.

- `h0_test`: Decision from hypothesis test.

- `bland_alt.plot`: Simple Bland-Altman plot. Red line are the upper and
  lower bounds for shieh test; grey box is the acceptable limits
  (delta). If the red lines are within the grey box then the shieh test
  should indicate 'reject h0', or to reject the null hypothesis that
  this not acceptable agreement between x & y.

- `conf.level`: Returned as input.

- `agree.level`: Returned as input.

## References

Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H.,
Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., &
McKinstry, B. (2016). "Application of mixed effects limits of agreement
in the presence of multiple sources of variability: exemplar from the
comparison of several devices to measure respiratory rate in COPD
patients". Plos One, 11(12), e0168321.
[doi:10.1371/journal.pone.0168321](https://doi.org/10.1371/journal.pone.0168321)
