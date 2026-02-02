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
  null.value = 0,
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

  The hypothesized value of CCC under the null hypothesis, default is 0.

- alternative:

  Character string specifying the alternative hypothesis: `"two.sided"`
  (default), `"greater"`, or `"less"`.

## Value

A list with class `"htest"` containing the following components:

- statistic:

  The Z-statistic (Fisher-transformed).

- parameter:

  The sample size (n for simple data, number of subjects for reps/nest).

- stderr:

  The standard errors for CCC and Z-transformed CCC.

- p.value:

  The p-value for the test.

- conf.int:

  A confidence interval for the CCC.

- estimate:

  The estimated CCC.

- null.value:

  The specified hypothesized value of CCC.

- alternative:

  A character string describing the alternative hypothesis.

- method:

  A character string indicating the method used.

- data.name:

  A character string giving the names of the data.

## Details

The concordance correlation coefficient measures the agreement between
two measurements of the same quantity. It combines measures of precision
(Pearson correlation) and accuracy (bias correction factor) to determine
how far the observed data deviate from the line of perfect concordance
(45-degree line through the origin).

For simple paired data, Lin's original method is used. For replicate or
nested data, U-statistics are employed following King, Chinchilli, and
Carrasco.

The hypothesis test is performed using Fisher's Z-transformation of the
CCC: \$\$Z = 0.5 \times \log\left(\frac{1 + CCC}{1 - CCC}\right)\$\$

## References

Lin, L. I. (1989). A concordance correlation coefficient to evaluate
reproducibility. Biometrics, 45(1), 255-268.

King, T. S., & Chinchilli, V. M. (2001). A generalized concordance
correlation coefficient for continuous and categorical data. Statistics
in Medicine, 20(14), 2131-2147.

King, T. S., Chinchilli, V. M., & Carrasco, J. L. (2007). A repeated
measures concordance correlation coefficient. Statistics in Medicine,
26(16), 3095-3113.

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
#> Z = 5.4582, n = 5, p-value = 4.809e-08
#> alternative hypothesis: true CCC is  0
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
#> Z = 2.5303, n = 18, p-value = 0.0114
#> alternative hypothesis: true CCC is  0
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
#> Z = -2.7972, n = 18, p-value = 0.9974
#> alternative hypothesis: true CCC is  0.8
#> 95 percent confidence interval:
#>  0.1275808 0.7236639
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
#> Z = 0.7841, n = 4, p-value = 0.433
#> alternative hypothesis: true CCC is  0
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
#> Z = 3.7336, n = 4, p-value = 0.0001888
#> alternative hypothesis: true CCC is  0
#> 95 percent confidence interval:
#>  0.2429167 0.6616278
#> sample estimates:
#>       CCC 
#> 0.4790783 
#> 
```
