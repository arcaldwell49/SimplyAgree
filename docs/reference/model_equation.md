# LaTeX Equation of a Fitted Model

**\[experimental\]**

Writes the statistical model behind a result as a LaTeX equation: the
mean model, the distributions of the random effects and residuals, and
any variance function or correlation structure. The equation uses
symbols by default; with `use_coefs = TRUE` the estimates are
substituted.

## Usage

``` r
# S3 method for class 'reli_htest'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'simple_mconv'
model_equation(x, use_coefs = FALSE, coef_digits = 3, pair = NULL, ...)

# S3 method for class 'simple_mvc'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'simple_reli'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'tolerance_delta'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

model_equation(x, ...)

# Default S3 method
model_equation(x, ...)

# S3 method for class 'lm'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'lmerMod'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'gls'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'lme'
model_equation(x, use_coefs = FALSE, coef_digits = 3, ...)

# S3 method for class 'model_equation'
print(x, ...)
```

## Arguments

- x:

  A result of
  [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
  [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
  [`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md),
  [`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md),
  [`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
  [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  or
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md),
  or a model fitted with
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html),
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html),
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) or
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html).

- use_coefs:

  Logical. If `TRUE`, substitute the estimates for the symbols (fixed
  effects, standard deviations of the random effects and residuals,
  variance function and correlation parameters). Default is `FALSE`.

- coef_digits:

  Number of decimal places for the estimates. Default is 3.

- ...:

  Not used.

- pair:

  For
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)
  results with `bias = "linear"`: two method names to show the
  regression for one pair only. Default is `NULL` (all pairs).

## Value

A character string of class `c("model_equation", "character")` with the
LaTeX code (without math delimiters).

## Details

The equation is returned as LaTeX code. Printed at the console it shows
the code; in an R Markdown or Quarto document, a chunk that returns it
renders the equation as display math. Equations with more than one line
use an `aligned` environment: the mean model first, then the
distributions.

Parameters that differ by a level (e.g., the method means \\\alpha_m\\
or the ratios \\\delta\\ of a variance function) are written with an
index in the equation; with `use_coefs = TRUE` their estimates are
listed at the end of the line.

The notation follows the vignettes:

- [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md):
  \\d\_{ij}\\ is the difference for subject \\i\\, measurement \\j\\,
  with the fixed effects, the random intercepts (`model = "lme"`), and
  the variance function and correlation structure of the fitted `nlme`
  model. Conditions are indicators \\\mathrm{I}(\cdot)\\ and \\A\\ is
  the average of the two measurements. The model must have been kept
  (`keep_model = TRUE`).

- [`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
  [`reli_err_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_err_test.md),
  [`mdc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/mdc_test.md),
  [`reli_stats()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md),
  [`reli_aov()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_stats.md):
  \\y\_{ij} = \mu + s_i + t_j + e\_{ij}\\ (one-way: without \\t_j\\).
  The same model underlies the consistency and agreement forms; the
  consistency forms do not use \\\sigma^2_t\\.

- [`reli_facet_test()`](https://aaroncaldwell.us/SimplyAgree/reference/reli_facet_test.md):
  an effect for subjects (\\p\\), items (\\i\\), occasions (\\o\\) and
  their interactions, and repeat error \\e\\.

- [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md):
  \\y\_{mir} = \alpha_m + \mu_i + a\_{ir} + c\_{mi} + e\_{mir}\\ for
  method \\m\\, item \\i\\, replicate \\r\\, on the analysis scale of
  the transformation, with the designs without replicates as special
  cases.

- [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md):
  the regression of the differences \\D\\ on the averages \\A\\ for each
  pair (`bias = "linear"`), the scaled model \\y\_{mir} = \alpha_m +
  \beta_m(\mu_i + a\_{ir} + c\_{mi}) + e\_{mir}\\ (`bias = "scaled"`),
  or the variance component model (from a
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  result).

For `lm`, `lmerMod`, `gls` and `lme` models, the fixed effects are
\\\beta_0, \beta_1, \ldots\\, factor levels are indicators, and a factor
with many levels is shown as one indexed effect \\\gamma\\. Random
effects are \\u\\, \\v\\, ... for the grouping factors, indexed
\\j\[i\]\\, \\k\[i\]\\, ... (the group of observation \\i\\). The
variance functions `varIdent`, `varPower`, `varExp` and `varFixed`, and
the correlation structures `corCompSymm`, `corAR1` and `corCAR1` are
written out; others are shown generically. Generalized linear models are
not supported.

## See also

[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md),
[`icc_test()`](https://aaroncaldwell.us/SimplyAgree/reference/icc_test.md),
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md),
[`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)

## Examples

``` r
data('reps')
tl = tolerance_limit(data = reps, x = "x", y = "y", id = "id")
model_equation(tl)
#> \begin{aligned}
#> d_{ij} &= \beta_{0} + e_{ij} \\
#> e_{ij} &\sim N(0,\, \sigma^{2}) \\
#> \operatorname{Cor}(e_{ij}, e_{ij'}) &= \rho, \quad j \neq j' \\
#> & \text{where } d_{ij} = \mathrm{x}_{ij} - \mathrm{y}_{ij}
#> \end{aligned}
model_equation(tl, use_coefs = TRUE)
#> \begin{aligned}
#> d_{ij} &= 0.705 + e_{ij} \\
#> e_{ij} &\sim N(0,\, 1.458^{2}) \\
#> \operatorname{Cor}(e_{ij}, e_{ij'}) &= 0.928, \quad j \neq j' \\
#> & \text{where } d_{ij} = \mathrm{x}_{ij} - \mathrm{y}_{ij}
#> \end{aligned}

fit = lm(mpg ~ wt + factor(cyl), data = mtcars)
model_equation(fit, use_coefs = TRUE, coef_digits = 2)
#> \begin{aligned}
#> \mathrm{mpg}_{i} &= 33.99 - 3.21\,\mathrm{wt}_{i} - 4.26\,\mathrm{I}(\mathrm{cyl}_{i} = \mathrm{6}) - 6.07\,\mathrm{I}(\mathrm{cyl}_{i} = \mathrm{8}) + \varepsilon_{i} \\
#> \varepsilon_{i} &\sim N(0,\, 2.56^{2})
#> \end{aligned}
```
