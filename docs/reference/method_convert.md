# Conversion Equations Between Measurement Methods

**\[experimental\]**

Estimates the linear relation between each pair of methods, as
conversion equations from one method to another with prediction limits.
The bias between methods may be constant (from a
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
fit), change linearly with the level of measurement (regression of the
differences on the averages; Carstensen, 2010, section 4.7), or follow
the scaled model, in which each method is a linear function of a common
true value (Carstensen, 2010, ch. 7).

## Usage

``` r
method_convert(
  data = NULL,
  y,
  method,
  id,
  replicate = NULL,
  bias = c("linear", "scaled"),
  variance = c("constant", "linear"),
  replicates = c("exchangeable", "linked"),
  pred_level = 0.95,
  conf_level = 0.95,
  transform = "none",
  ci_method = c("delta", "boot"),
  estimator = c("altreg", "ml"),
  boot_reps = 999,
  pair_seed = NULL,
  max_iter = 100,
  tol = 1e-06,
  object = NULL,
  keep_model = TRUE
)
```

## Arguments

- data:

  A data frame in long format (one row per measurement). Not needed when
  `object` is supplied.

- y:

  Name of the column with the measurements.

- method:

  Name of the column identifying the method. The first level (if a
  factor) or the first value in sorted order is the reference method;
  differences are reported as earlier minus later level.

- id:

  Name of the column identifying the item (subject).

- replicate:

  Name of the column identifying the replicate. Required when an item is
  measured more than once by the same method.

- bias:

  Model for the bias between methods. "linear" (default): regression of
  the differences on the averages for each pair of methods. "scaled":
  the scaled model, fit by alternating regression or maximum likelihood
  (see `estimator`; requires replicates). See details.

- variance:

  Model for the SD of the differences with `bias = "linear"`: "constant"
  (default) or "linear" in the average.

- replicates:

  Whether replicates are "exchangeable" (default) or "linked"; see
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md).
  Exchangeable replicates are paired at random within item (see
  `pair_seed`), linked replicates by replicate.

- pred_level:

  Coverage of the prediction limits. Default is 0.95.

- conf_level:

  Confidence level for the conversion parameters. Default is 0.95.

- transform:

  Transformation applied to the measurements before analysis: "none"
  (default), "log", "logit", "pctlogit" (logit of a percentage), "sqrt",
  "asinsqrt", or a list with functions `fun` and `inv` (its inverse)
  and, optionally, a `name`. All estimates are on the transformed scale.

- ci_method:

  Method for the confidence intervals of the conversion parameters:
  "delta" (default; delta method) or, for `bias = "scaled"` with
  `estimator = "altreg"`, "boot" (parametric bootstrap). See details.

- estimator:

  For `bias = "scaled"`, how the model is fit: "altreg" (default;
  alternating regression, as in MethComp) or "ml" (maximum likelihood of
  the structural model with the lavaan package, which must be
  installed). Use "ml" for confidence intervals with close to the stated
  coverage. See details.

- boot_reps:

  Number of bootstrap replicates for `ci_method = "boot"`. Default
  is 999. Each replicate refits the model, so this can take several
  minutes; set a seed with
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for reproducible
  intervals.

- pair_seed:

  Seed for the random pairing of exchangeable replicates; the global
  random number generator state is restored afterwards.

- max_iter, tol:

  Iteration limit and convergence tolerance (largest relative change in
  the conversion parameters and SDs) for `bias = "scaled"`. Bootstrap
  refits use a tolerance of at least 1e-4.

- object:

  A
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  fit (`simple_mvc`). If supplied, the data and column arguments are
  ignored and the conversions are derived from its estimates with a
  constant bias (slope 1).

- keep_model:

  Logical. Whether to keep the fitted models. Default is `TRUE`.

## Value

An object of class `simple_mconv`, a list with:

- `conversion`: tibble with one row per ordered pair (`to`, `from`),
  including each method to itself: the intercept `alpha`, slope `beta`,
  prediction SD `sd_pred`, their standard errors and df, and confidence
  limits (`alpha_lower`, `alpha_upper`, `beta_lower`, `beta_upper`; and,
  for the scaled model, `sd_lower`, `sd_upper` with the bootstrap or
  `estimator = "ml"`).

- `limits_par`: list, by ordered pair (named "to\|from"), of the
  parameters used to evaluate the prediction limits at any level.

- `da`: for `bias = "linear"`, tibble of the DA regressions for each
  pair (`a`, `b` and their SEs, `s`, `c_sd`, `d_sd`, df, numbers of
  pairs and items, and whether the SEs are cluster-robust).

- `varcomp`: the variance components (for the scaled model, on each
  method's scale, with bootstrap intervals if requested; for a
  conversion from a
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  fit, its variance components); otherwise `NULL`.

- `vc_fit`: for a conversion from a
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  fit, its bias and limits tables at `pred_level` and `conf_level`, and
  its design (used by the Bland-Altman plot); otherwise absent.

- `scaled`: for the scaled model, the per-method intercepts and slopes,
  the item values, the variance components on the scale of the true
  values, and the iteration history of the alternating regression
  (`trace`; `NULL` with `estimator = "ml"`). With `estimator = "ml"`,
  the item values are the factor scores of the true values.

- `boot`: for `ci_method = "boot"`, the matrix of bootstrap estimates.

- `data`: the processed data.

- `pairs`: list of the paired data used for each pair of methods.

- `transform`: the transformation (functions included).

- `model`: list of the fitted `lm` objects (linear bias), the final
  variance component model (scaled, alternating regression), the lavaan
  fit (scaled, `estimator = "ml"`), or `NULL`.

- `fit_info`: list describing the fit and settings (including the
  estimator, number of iterations, convergence, and failed bootstrap
  refits; with `estimator = "ml"`, also the log-likelihood `logl`, the
  names of zero or negative variance estimates `heywood`, and
  `fit_measures`: chi-square, df, p value, RMSEA and SRMR).

- `call`: the matched call, with evaluated settings.

## Details

**Constant bias** (`object` supplied): the conversion from method `m` to
method `k` is \\y_k = (\alpha_k - \alpha_m) + y_m\\, with prediction SD
\\\sqrt{V\_{mk}}\\ from
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
(Carstensen, 2010, ch. 5), and \\\sqrt{2}\sigma_m\\ for two measurements
by the same method (with replicates).

**Linear bias** (`bias = "linear"`): for each pair, with differences \\D
= y_1 - y_2\\ and averages \\A = (y_1 + y_2)/2\\ on the (transformed)
scale, \\D = a + bA + e\\ is fit by least squares. With
`variance = "constant"`, \\SD(e) = s\\; with `variance = "linear"`,
\\SD(e \mid A) = c' + d'A\\, from regressing the absolute residuals on
\\A\\ (multiplied by \\\sqrt{\pi/2}\\). The relation is converted to
\$\$y_1 = \frac{a}{1 - b/2} + \frac{1 + b/2}{1 - b/2} y_2,\$\$ and the
prediction limits are the lines \\D = (a \pm z c') + (b \pm z d')A\\
(with \\c' = s, d' = 0\\ for constant variance) transformed the same way
(Carstensen, 2010, eq. 4.11); the conversion from \\y_1\\ to \\y_2\\
inverts these lines. This requires \\\|b\| \< 2\\. With constant
variance the prediction SD is \\s/(1 - b/2)\\ (to method 1) and \\s/(1 +
b/2)\\ (to method 2); with linear variance it depends on the level and
is reported as `NA`.

When items contribute several pairs (replicates), the covariance of
\\(a, b)\\ is cluster-robust by item, with (items - 1) degrees of
freedom. Confidence intervals for the conversion intercepts and slopes
use the delta method.

A slope of the differences on the averages does not by itself show that
the methods are related by a slope other than 1. Whenever the two
methods have unequal measurement error variances, cov(D, A) = (var(y1) -
var(y2)) / 2 is not zero, so \\b\\ is biased away from zero (Bland &
Altman, 1999). The scaled model (`bias = "scaled"`) or
errors-in-variables regression
([`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md),
[`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md))
account for the measurement errors.

**Scaled model** (`bias = "scaled"`): measurement `r` of item `i` by
method `m` is \$\$y\_{mir} = \alpha_m + \beta_m(\mu_i + a\_{ir} +
c\_{mi}) + e\_{mir},\$\$ with the variance components of
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
on the scale of the true values \\\mu_i\\ (Carstensen, 2010, eq. 7.1).
With `estimator = "altreg"` (default) it is fit by alternating
regression as in the `AltReg()` function of the MethComp package: each
method is regressed on the current predicted true values \\\mu_i +
a\_{ir} + c\_{mi}\\ (with a random item intercept), and the variance
component model is refit to \\(y - \alpha_m)/\beta_m\\, until the
conversions converge. The conversion from method `m` to `k` is
\\\alpha\_{k\|m} = \alpha_k - \alpha_m\beta_k/\beta_m\\, \\\beta\_{k\|m}
= \beta_k/\beta_m\\, with prediction SD \\\sqrt{\tau_k^2 + \sigma_k^2 +
\beta\_{k\|m}^2(\tau_m^2 + \sigma_m^2)}\\ from the variance components
on each method's scale (\\\omega\\ cancels), and \\\sqrt{2}\sigma_m\\
for a method to itself. As in MethComp, the method-by-item SD is
multiplied by \\I/(I-2)\\. The per-method \\\alpha_m, \beta_m\\ are
reported with the first method as reference (\\\alpha = 0, \beta = 1\\);
the conversions do not depend on this choice.

With `estimator = "ml"`, the true values are also treated as random,
\\\mu_i \sim N(\mu, \sigma\_\mu^2)\\ (the structural model), and the
model is fit by full maximum likelihood with the lavaan package, as a
confirmatory factor model with equality constraints (one row per item;
missing measurements by full-information ML, so the design need not be
balanced). The alternating regression supplies the starting values. The
method-by-item variance is common to all methods on the scale of the
true values, as above, but is not multiplied by \\I/(I-2)\\, so with few
items the prediction SDs are a little smaller (on average by 3% to 5%
with 15 items and about 1% with 60 items, in the simulations below). The
two fits estimate the same model, but they weight the data differently,
and their estimates can differ when the true values are far from normal;
for the oximetry data in MethComp (skewed true values), the slope of
pulse on CO is 0.94 by alternating regression and 0.90 by maximum
likelihood (95% CI 0.80 to 1.00). The summary reports the model's
chi-square test of fit, RMSEA and SRMR. Variance estimates can be zero
or negative (Heywood cases), mostly with fewer than about 20 items; this
gives a warning, and the corresponding SDs are reported as 0.

Confidence intervals for the scaled model with `estimator = "ml"` are
Wald intervals from the ML standard errors (delta method for the
conversions), with t quantiles on \\I - 1\\ degrees of freedom (\\I\\
items); the prediction SD intervals are computed on the log scale. In
simulations (15, 30 or 60 items; 2 or 3 methods; 3 replicates, linked or
exchangeable; balanced data, 15% of the measurements missing, or uniform
or skewed true values; 500 data sets each), 95% intervals for the
conversion intercepts and slopes covered the true values 92% to 97% of
the time (on average 93.6% with 15 items, 94.5% with 30 and 95.1% with
60), and those for the prediction SDs 92% to 95%. Wald intervals with
normal quantiles, or with robust (sandwich) standard errors, covered
less. No intervals are given for the variance components.

With `estimator = "altreg"`, "delta" (default) uses the standard errors
of \\(\alpha_m, \beta_m)\\ from the final regressions of each method on
the predicted true values. These treat the predicted true values as
known, so they are much too narrow: in the same simulations (balanced
data, normal true values), 95% intervals for the conversion intercepts
and slopes covered the true values only 53% to 73% of the time, with no
improvement with more items. They are useful only as a rough indication
of precision. "boot" simulates data from the fitted model with the
observed design (the same items, methods, and replicates, and the fitted
item values), refits the model, and reports percentile intervals for the
conversion parameters, the prediction SDs, and the variance components.
The bootstrap may improve on the delta method, but its coverage has not
been checked by simulation, and each replicate refits the model (999
replicates can take several minutes). MethComp itself gives no intervals
for the alternating regression. The conversions and prediction limits
themselves do not depend on `ci_method`.

All estimates are on the transformed scale;
[`predict()`](https://rdrr.io/r/stats/predict.html) and the conversion
plot back-transform the conversions and limits to the original scale
(Carstensen, 2010, ch. 8).

## References

Carstensen, B. (2010). Comparing Clinical Measurement Methods: A
Practical Guide. Wiley.

Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
comparison studies. Statistical Methods in Medical Research, 8(2),
135-160.

## See also

[simple_mconv-methods](https://aaroncaldwell.us/SimplyAgree/reference/simple_mconv-methods.md),
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md),
[`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md),
[`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md)

## Examples

``` r
dat = sim_methods(n_items = 50, n_reps = 2, methods = c("A", "B"),
                  alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                  mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)

# linear bias
conv = method_convert(dat, y = "y", method = "method", id = "id",
                      replicate = "repl", pair_seed = 1)
conv
#> Conversion Between Methods: Linear Bias
#> y(To) = alpha + beta * y(From); sd = prediction SD
#> Regression of differences on averages, constant SD of the differences
#> 
#>  To From alpha   95% CI             beta   95% CI           sd    
#>  A  A     0.0000                    1.0000                        
#>  A  B    -0.8536 [-1.3802, -0.3269] 0.8345 [0.7978, 0.8712] 0.4429
#>  B  A     1.0228 [ 0.4360,  1.6097] 1.1983 [1.1456, 1.2511] 0.5308
#>  B  B     0.0000                    1.0000                        
#> 
#> CIs: approximate (delta method, cluster-robust by item)
#> A slope of the differences on the averages can arise from unequal measurement errors alone;
#>   see ?method_convert
predict(conv, newdata = c(5, 10, 15), from = "A", to = "B")
#> # A tibble: 3 × 4
#>   from_value   fit lower upper
#>        <dbl> <dbl> <dbl> <dbl>
#> 1          5  7.01  5.97  8.05
#> 2         10 13.0  12.0  14.0 
#> 3         15 19.0  18.0  20.0 

# constant bias, from a variance component fit
fit = method_vc(dat, y = "y", method = "method", id = "id",
                replicate = "repl")
method_convert(object = fit)
#> Conversion Between Methods: Constant Bias
#> y(To) = alpha + beta * y(From); sd = prediction SD
#> 
#>  To From alpha  95% CI             beta 95% CI sd    
#>  A  A     0.000                    1           0.4434
#>  A  B    -3.068 [-3.2464, -2.8895] 1           0.7005
#>  B  A     3.068 [ 2.8895,  3.2464] 1           0.7005
#>  B  B     0.000                    1           0.4343
#> 
#> From method_vc(): slope fixed at 1; intercept CIs are those of the biases

# scaled model (alternating regression)
sc = method_convert(dat, y = "y", method = "method", id = "id",
                    replicate = "repl", bias = "scaled", tol = 1e-4)
sc
#> Conversion Between Methods: Scaled Model
#> y(To) = alpha + beta * y(From); sd = prediction SD
#> 
#>  To From alpha   95% CI             beta   95% CI           sd    
#>  A  A     0.0000                    1.0000                  0.4434
#>  A  B    -0.8468 [-1.1599, -0.5336] 0.8340 [0.8111, 0.8568] 0.4573
#>  B  A     1.0153 [ 0.6670,  1.3637] 1.1991 [1.1662, 1.2319] 0.5484
#>  B  B     0.0000                    1.0000                  0.4343
#> 
#> CIs: delta method, conditional on the predicted true values;
#>   much too narrow (about 55-75% coverage for 95% in simulations);
#>   use estimator = 'ml' for intervals; see ?method_convert
#> Alternating regression: 3 iterations, converged (criterion 5.87e-05)
#> 
#> Variance components (SD, on each method's scale)
#>  Method tau    sigma 
#>  A      0.1504 0.3136
#>  B      0.1803 0.3071

# scaled model by maximum likelihood (needs lavaan), with intervals that
# have close to the stated coverage
if (requireNamespace("lavaan", quietly = TRUE)) {
  method_convert(dat, y = "y", method = "method", id = "id",
                 replicate = "repl", bias = "scaled", estimator = "ml",
                 tol = 1e-4)
}
#> Conversion Between Methods: Scaled Model (maximum likelihood)
#> y(To) = alpha + beta * y(From); sd = prediction SD
#> 
#>  To From alpha   95% CI             beta   95% CI           sd    
#>  A  A     0.0000                    1.0000                  0.4434
#>  A  B    -0.8394 [-1.3054, -0.3733] 0.8334 [0.7994, 0.8675] 0.4533
#>  B  A     1.0071 [ 0.4880,  1.5262] 1.1998 [1.1509, 1.2488] 0.5439
#>  B  B     0.0000                    1.0000                  0.4343
#> 
#> CIs: Wald (maximum likelihood, structural model), t with items - 1 df;
#>   about 92-97% coverage for 95% in simulations with 15-60 items; see ?method_convert
#> Maximum likelihood (lavaan): 22 iterations, converged; log-likelihood -203.15
#> 
#> Variance components (SD, on each method's scale)
#>  Method tau    sigma 
#>  A      0.1443 0.3136
#>  B      0.1732 0.3071
# \donttest{
# parametric bootstrap intervals (use more replicates in practice)
set.seed(1)
method_convert(dat, y = "y", method = "method", id = "id",
               replicate = "repl", bias = "scaled", tol = 1e-4,
               ci_method = "boot", boot_reps = 50)
#> Conversion Between Methods: Scaled Model
#> y(To) = alpha + beta * y(From); sd = prediction SD
#> 
#>  To From alpha   95% CI             beta   95% CI           sd    
#>  A  A     0.0000                    1.0000                  0.4434
#>  A  B    -0.8468 [-1.2489, -0.3245] 0.8340 [0.7921, 0.8622] 0.4573
#>  B  A     1.0153 [ 0.4097,  1.4494] 1.1991 [1.1598, 1.2624] 0.5484
#>  B  B     0.0000                    1.0000                  0.4343
#> 
#> CIs: parametric bootstrap percentile intervals (50 of 50 refits used); approximate, coverage not yet evaluated
#> Alternating regression: 3 iterations, converged (criterion 5.87e-05)
#> 
#> Variance components (SD, on each method's scale)
#>  Method tau    sigma 
#>  A      0.1504 0.3136
#>  B      0.1803 0.3071
# }
```
