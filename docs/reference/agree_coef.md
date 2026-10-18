# Agreement Coefficients

**\[superseded\]**

`agree_coef()` has been superseded by
[`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md),
which returns one coefficient as an `htest` with a hypothesis test,
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
`agree_coef()` still returns a table of all four coefficients.

agree_coef produces inter-rater reliability or "agreement coefficients"
as described by Gwet: percent agreement, Gwet's AC1 (AC2 when weighted),
Fleiss' kappa and Krippendorff's alpha.

## Usage

``` r
agree_coef(
  wide = TRUE,
  col.names = NULL,
  measure,
  item,
  id,
  data,
  weighted = FALSE,
  conf.level = 0.95,
  weights = NULL,
  categories = NULL
)
```

## Arguments

- wide:

  Logical value (TRUE or FALSE) indicating if data is in a "wide"
  format. Default is TRUE.

- col.names:

  If wide is equal to TRUE then col.names is a list of the column names
  containing the measurements for reliability analysis.

- measure:

  Name of column containing the measurement of interest.

- item:

  Name of column containing the items. If this is an inter-rater
  reliability study then this would indicate the rater (e.g., rater1,
  rater2, rater3, etc).

- id:

  Column with subject identifier.

- data:

  Data frame with all data.

- weighted:

  Logical value (TRUE or FALSE) indicating whether to weight the
  responses. If TRUE (default is FALSE) then quadratic weights are
  utilized. This option should be set to TRUE for ordinal or continuous
  responses. Ignored when `weights` is given.

- conf.level:

  the confidence level required. Default is 95%.

- weights:

  Optional weighting scheme, as in
  [`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md):
  e.g. `"quadratic"`, `"linear"` or `"ordinal"`, or a custom weight
  matrix. Overrides `weighted`.

- categories:

  All possible rating categories, as in
  [`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md).
  Defaults to the sorted observed ratings (or the levels of a factor).

## Value

A data frame with one row per coefficient and the columns `est`
(estimate), `se` (standard error), `lower.ci` and `upper.ci`.

## Details

The estimates, standard errors and intervals are those of
[`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md);
see there for details. Confidence limits are clipped to the possible
range of each coefficient. When all ratings are in one category the
chance-corrected coefficients are undefined and returned as `NA`.

## References

Gwet, K.L. (2014, ISBN:978-0970806284). "Handbook of Inter-Rater
Reliability", 4th Edition. Advanced Analytics, LLC.

Gwet, K. L. (2008). "Computing inter-rater reliability and its variance
in the presence of high agreement," British Journal of Mathematical and
Statistical Psychology, 61, 29-48.

## See also

[`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md)

## Examples

``` r
data('reps')
agree_coef(data = reps, wide = TRUE, col.names = c("x","y"), weighted = TRUE)
#>                            est         se   lower.ci  upper.ci
#> Percent Agreement    0.9131459 0.07673687 0.75253380 1.0000000
#> Gwet's AC2           0.5073626 0.18354840 0.12319136 0.8915338
#> Fleiss' Kappa        0.5255860 0.17484095 0.15963973 0.8915324
#> Krippendorff's Alpha 0.4771011 0.19501219 0.06566137 0.8885409
```
