# Concordance Correlation Coefficient

Computes Lin's Concordance Correlation Coefficient (CCC) and performs a
hypothesis test, returning an object of class `"htest"`.

## Usage

``` r
ccc_test(
  x,
  y = NULL,
  id = NULL,
  data = NULL,
  data_type = c("simple", "reps", "nest"),
  conf.level = 0.95,
  null.value = NULL,
  alternative = c("two.sided", "greater", "less")
)
```

## Arguments

- x:

  Either a numeric vector of measurements for method 1, or a character
  string naming the column in `data` containing method 1 measurements.

- y:

  Either a numeric vector of measurements for method 2, or a character
  string naming the column in `data` containing method 2 measurements.

- id:

  A character string naming the column in `data` containing subject
  identifiers. Required when `data_type` is `"reps"` or `"nest"`.

- data:

  An optional data frame containing the variables named in `x`, `y`, and
  `id`.

- data_type:

  Character string specifying the data structure: `"simple"` for paired
  data with one observation per subject (default), `"reps"` for
  replicate data where subjects are measured multiple times, `"nest"`
  for nested data structures.

- conf.level:

  Confidence level for the interval, default is 0.95.

- null.value:

  The CCC under the null hypothesis. The default, `NULL`, gives the
  estimate and interval without a test.

- alternative:

  Character string specifying the alternative hypothesis: `"two.sided"`
  (default), `"greater"`, or `"less"`. Used only when `null.value` is
  given (without a test the interval is two-sided); a one-sided
  alternative gives a one-sided interval, as in
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md).

## Value

A list with class `c("ccc_htest", "htest")` containing the following
components:

- statistic:

  The Z-statistic (Fisher-transformed), when a test was requested.

- parameter:

  The sample size (n for simple data, number of subjects for reps/nest),
  when a test was requested.

- stderr:

  The standard errors for CCC and Z-transformed CCC.

- p.value:

  The p-value for the test, when a test was requested.

- conf.int:

  A confidence interval for the CCC.

- estimate:

  The estimated CCC.

- null.value:

  The specified hypothesized value of CCC, if any.

- alternative:

  The alternative hypothesis.

- method:

  A character string indicating the method used.

- data.name:

  A character string giving the names of the data.

- data:

  The data used (`x`, `y`, and `id` for reps/nest), for the methods.

- components:

  For simple data, Lin's decomposition: the Pearson correlation `r`
  (precision) and the bias correction factor `C_b` (accuracy), with CCC
  = r x C_b. `NULL` for reps/nest.

- settings:

  The settings used, for the methods.

## Details

The concordance correlation coefficient measures the agreement between
two measurements of the same quantity. It combines measures of precision
(Pearson correlation) and accuracy (bias correction factor) to determine
how far the observed data deviate from the line of perfect concordance
(45-degree line through the origin).

For simple paired data, Lin's original method is used. For replicate or
nested data, U-statistics are employed following King, Chinchilli, and
Carrasco.

The confidence interval and the hypothesis test use Fisher's
Z-transformation of the CCC: \$\$Z = 0.5 \times \log\left(\frac{1 +
CCC}{1 - CCC}\right)\$\$ By default (`null.value = NULL`) no test is
done.

**Relation to the ICC.** For two methods, the CCC is close to the
absolute-agreement intraclass correlation, ICC(A,1)
(`icc_test(type = "agreement")`); Lin's estimator uses n rather than n -
1 in the variances, and the difference vanishes as n grows (Nickerson,
1997; Carrasco & Jover, 2003). `ccc_test()` is meant for
method-comparison (agreement) studies;
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md)
for reliability studies (see its "Choosing the form" and "Related
coefficients" sections).

**Methods.** [`summary()`](https://rdrr.io/r/base/summary.html)
describes the result in words (including, for simple data, whether a low
CCC comes from poor precision or poor accuracy),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the
identity or Bland-Altman plot, and
[`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
gives diagnostics; see
[ccc_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/ccc_htest-methods.md).

## References

Lin, L. I. (1989). A concordance correlation coefficient to evaluate
reproducibility. Biometrics, 45(1), 255-268.

Carrasco, J. L., & Jover, L. (2003). Estimating the generalized
concordance correlation coefficient through variance components.
Biometrics, 59(4), 849-858.
[doi:10.1111/j.0006-341x.2003.00099.x](https://doi.org/10.1111/j.0006-341x.2003.00099.x)

Nickerson, C. A. E. (1997). A note on "A concordance correlation
coefficient to evaluate reproducibility". Biometrics, 53(4), 1503-1507.
[doi:10.2307/2533516](https://doi.org/10.2307/2533516)

King, T. S., & Chinchilli, V. M. (2001). A generalized concordance
correlation coefficient for continuous and categorical data. Statistics
in Medicine, 20(14), 2131-2147.

King, T. S., Chinchilli, V. M., & Carrasco, J. L. (2007). A repeated
measures concordance correlation coefficient. Statistics in Medicine,
26(16), 3095-3113.

## See also

[ccc_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/ccc_htest-methods.md),
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)

## Examples

``` r
# Simple paired data using vectors
x <- c(1, 2, 3, 4, 5)
y <- c(1.1, 2.2, 2.9, 4.1, 5.2)
ccc_test(x, y)
#> 
#>  Lin's Concordance Correlation Coefficient
#> 
#> data:  x and y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.9556798 0.9993495
#> sample estimates:
#>       CCC 
#> 0.9945839 
#> 

# Using data frame interface
data("reps")
# Simple data
ccc_test(x = "x", y = "y", data = reps)
#> 
#>  Lin's Concordance Correlation Coefficient
#> 
#> data:  x and y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.1275808 0.7236639
#> sample estimates:
#>       CCC 
#> 0.4790783 
#> 

# Test against specific null value
ccc_test(x = "x", y = "y", data = reps,
         null.value = 0.8, alternative = "greater")
#> 
#>  Lin's Concordance Correlation Coefficient
#> 
#> data:  x and y
#> Z = -2.873, n = 18, p-value = 0.998
#> alternative hypothesis: true CCC is greater than 0.8
#> 95 percent confidence interval:
#>  0.1892367 1.0000000
#> sample estimates:
#>       CCC 
#> 0.4790783 
#> 

# Replicate data
ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps")
#> 
#>  Concordance Correlation Coefficient (U-statistics)
#> 
#> data:  x and y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  -0.1595543  0.3588391
#> sample estimates:
#>       CCC 
#> 0.1069017 
#> 

# Nested data
ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "nest")
#> 
#>  Concordance Correlation Coefficient (U-statistics)
#> 
#> data:  x and y
#> 
#> alternative hypothesis: two.sided
#> 95 percent confidence interval:
#>  0.2429167 0.6616278
#> sample estimates:
#>       CCC 
#> 0.4790783 
#> 

# Describe, plot and check
res <- ccc_test(x = "x", y = "y", data = reps)
summary(res)
#> The estimated CCC was 0.479 (95% compatibility interval [0.128,
#> 0.724]). Values in this interval are the most compatible with the data,
#> given the model and its assumptions. The estimate is Lin's concordance
#> correlation coefficient between x and y: how closely the pairs fall on
#> the line of identity. It combines precision (Pearson r = 0.577) and
#> accuracy (bias correction factor C_b = 0.831), CCC = r x C_b. The data
#> had 18 complete pairs. The interval uses Fisher's z transformation with
#> Lin's (1989) delta-method standard error, which assumes the two
#> measurements are bivariate normal.
plot(res)

check(res)

```
