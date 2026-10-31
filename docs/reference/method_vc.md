# Variance Component Limits for Comparing Several Methods

**\[experimental\]**

Fits the variance component model of Carstensen (2010) to measurements
of a set of items by two or more methods, with or without replicate
measurements, and reports the bias between each pair of methods, the
variance components, and limits of agreement (or prediction and
tolerance limits) for the difference between single measurements by two
methods on a new item.

## Usage

``` r
method_vc(
  data,
  y,
  method,
  id,
  replicate = NULL,
  replicates = c("exchangeable", "linked"),
  tau = c("common", "method"),
  interval = c("agreement", "tolerance"),
  pred_level = 0.95,
  conf_level = 0.95,
  bound_type = NULL,
  transform = "none",
  include_omega = FALSE,
  keep_model = TRUE,
  control = nlme::lmeControl(returnObject = TRUE)
)
```

## Arguments

- data:

  A data frame in long format (one row per measurement). Wide data can
  be converted with
  [`mc_long()`](https://aaroncaldwell.us/SimplyAgree/reference/mc_long.md).

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

- replicates:

  Whether replicates are "exchangeable" (default) or "linked".
  Replicates are linked when all methods measure an item on the same
  occasion (replicate `r` by every method shares the occasion), so
  measurements on the same occasion share an item-by-replicate effect.
  This is a property of the design, not of the data.

- tau:

  Whether the method-by-item variance is "common" to all methods
  (default) or differs by "method". Method-specific values require at
  least three methods.

- interval:

  Type of limits: "agreement" (default) for limits of agreement with
  confidence bounds, or "tolerance" for prediction limits and tolerance
  limits, as in
  [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md).

- pred_level:

  The proportion of differences the limits should contain (the agreement
  level). Default is 0.95.

- conf_level:

  Confidence level for the bias, the variance components, and the
  confidence bounds of the limits. Default is 0.95.

- bound_type:

  Which claim the confidence bounds of the limits support, "iu" or
  "joint", as in
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  and
  [`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md).
  The default (`NULL`) is "iu" for `interval = "agreement"` and "joint"
  for `interval = "tolerance"`. See details.

- transform:

  Transformation applied to the measurements before analysis: "none"
  (default), "log", "logit", "pctlogit" (logit of a percentage), "sqrt",
  "asinsqrt", or a list with functions `fun` and `inv` (its inverse)
  and, optionally, a `name`. All estimates are on the transformed scale.

- include_omega:

  Logical. With linked replicates, whether the item-by-replicate
  variance (omega) is included in the variance of a difference. Default
  is `FALSE`, for methods applied to an item on the same occasion, where
  it cancels. Use `TRUE` when the methods would be applied on different
  occasions in practice.

- keep_model:

  Logical. Whether to keep the fitted model in the returned object.
  Default is `TRUE`.

- control:

  Control values for
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html); see
  [`nlme::lmeControl()`](https://rdrr.io/pkg/nlme/man/lmeControl.html).

## Value

An object of class `simple_mvc`, a list with:

- `bias`: tibble with the bias (`method1` - `method2`) for each pair of
  methods, its standard error, df, and confidence limits.

- `limits`: tibble with, for each pair, the bias, the SD of the
  difference (`sd`) and its df (`df_sd`), and the limits (`lower_loa`,
  `upper_loa`, `lower_loa_ci`, `upper_loa_ci` for agreement; the
  standard error of prediction `sep`, the prediction limits `lower.PL`,
  `upper.PL`, and the tolerance limits `lower.TL`, `upper.TL` for
  tolerance).

- `varcomp`: tibble of the variance components (`tau`, `omega`,
  `sigma`), as SDs and variances, with confidence limits and df.

- `repeatability`: tibble with, for each method, the residual SD
  (`sigma`), the repeatability coefficient (`rc`) with confidence
  limits, and for linked replicates the reproducibility coefficient
  (`rpc`) with confidence limits; `NULL` without replicates. See
  details.

- `alpha`: list with the method means averaged over items (`alpha`) and
  their covariance matrix (`vcov`).

- `vc_cov`: approximate covariance matrix of the estimated variances in
  `varcomp`.

- `diagnostics`: fitted values, Pearson residuals, and predicted random
  effects, used by
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).

- `data`: the processed data (columns `y`, `y_t` (transformed),
  `method`, `id`, `repl`).

- `rep_table`: the number of items by method and number of replicates.

- `transform`: the transformation (functions included).

- `model`: the fitted `lme` or `gls` model (`NULL` if
  `keep_model = FALSE`).

- `design`: list describing the design and settings.

- `call`: the matched call, with evaluated settings.

## Details

Measurement `r` of item `i` by method `m` is modeled (Carstensen, 2010,
eq. 5.1 and 5.2) as \$\$y\_{mir} = \alpha_m + \mu_i + a\_{ir} +
c\_{mi} + e\_{mir},\$\$ with fixed item effects \\\mu_i\\,
method-by-item effects \\c\_{mi} \sim N(0, \tau_m^2)\\, residuals
\\e\_{mir} \sim N(0, \sigma_m^2)\\ with a separate SD for each method,
and, for linked replicates only, item-by-replicate effects \\a\_{ir}
\sim N(0, \omega^2)\\ common to all methods. The model is fit by REML
with [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html).

What can be estimated depends on the design:

- **Replicates**: all components. With two methods, only a common
  \\\tau\\ can be estimated.

- **One measurement per item and method, two methods**: only the
  variance of the difference is identifiable. The model is fit with
  [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) with a common
  residual SD, which is the paired-difference analysis of
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  with `data_type = "simple"`.

- **One measurement per item and method, three or more methods**: the
  method-specific residual SDs (Grubbs-type estimates) absorb the
  method-by-item variation, and repeatability cannot be estimated. The
  items are random effects in this case, because with fixed item effects
  the likelihood is unbounded (one residual SD can shrink to zero).

The bias for methods `m` and `k` is \\\alpha_m - \alpha_k\\, with
containment degrees of freedom (the number of method-by-item cells,
minus the number of items and methods, plus one; \\(I-1)(M-1)\\ with
complete data). The variance of the difference between single
measurements by the two methods on a new item (on the same occasion) is
\$\$V\_{mk} = \tau_m^2 + \tau_k^2 + \sigma_m^2 + \sigma_k^2,\$\$ plus
\\2\omega^2\\ with `include_omega = TRUE`. The limits of agreement are
\\\alpha_m - \alpha_k \pm z \sqrt{V\_{mk}}\\, with \\z = \Phi^{-1}((1 +
\code{pred\\level})/2)\\. Note that MethComp uses a multiplier of 2.

The uncertainty of the variance components is obtained from the
approximate covariance matrix of the variance parameters of the model
(the delta method). Confidence intervals for the SDs are Wald intervals
on the log scale. \\V\_{mk}\\ is given Satterthwaite degrees of freedom
(`df_sd`), \\2V^2/\mathrm{var}(\hat V)\\, which account for the
covariance between the estimated components. With one measurement per
item by two methods these are exactly \\n - 1\\.

With `interval = "tolerance"`, the prediction limits are \\\alpha_m -
\alpha_k \pm t \cdot SEP\\, with \\SEP = \sqrt{V\_{mk} + SE^2}\\ (\\SE\\
the standard error of the bias) and \\t\\ the \\(1 +
\code{pred\\level})/2\\ quantile of the t distribution with `df_sd`
degrees of freedom, as in
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md).
They are expected to contain a single new difference with probability
`pred_level` (a beta-expectation tolerance interval), but are not a
confidence statement, and they do not depend on `bound_type`.

The confidence bounds of the limits follow
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
(`interval = "agreement"`, MOVER bounds) or
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)
(`interval = "tolerance"`, the tolerance limits), with the same meaning
of `bound_type`:

- "iu": a one-sided `conf_level` bound on each limit, for an
  intersection-union test against a maximal allowable difference.

- "joint": joint confidence for both limits (agreement), or a
  `pred_level`-content, `conf_level`-confidence tolerance interval
  (tolerance).

In simulations with 15 to 60 items, 2 or 3 replicates, 2 or 3 methods,
and linked or exchangeable replicates (1000 data sets each; 95%
targets), the bias CIs had coverage of 0.94 to 0.96. The one-sided
bounds of the limits of agreement had 0.92 to 0.96, the joint bounds
0.91 to 0.95, and the joint tolerance limits 0.92 to 0.96; the lowest
values (about 0.91 to 0.93) were with 15 items, where the bounds are
slightly liberal. The RC intervals had 0.94 to 0.98, and the RpC
intervals 0.93 to 0.95.

With replicates, the repeatability coefficient of method `m`, \\RC_m = z
\sqrt{2} \sigma_m\\ (Carstensen, 2010, ch. 9), is the limit that the
absolute difference between two measurements by the method on the same
item, under the same conditions, should not exceed with probability
`pred_level`. Its confidence limits are those of \\\sigma_m\\, scaled.
For linked replicates, the reproducibility coefficient \\RpC_m = z
\sqrt{2} \sqrt{\sigma_m^2 + \omega^2}\\ is the same limit for two
measurements on different occasions.

## References

Carstensen, B. (2010). Comparing Clinical Measurement Methods: A
Practical Guide. Wiley.

Carstensen, B., Simpson, J., & Gurrin, L. C. (2008). Statistical models
for assessing agreement in method comparison studies with replicate
measurements. The International Journal of Biostatistics, 4(1).

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
Statistical Methods in Medical Research, 22(6), 630-642.

## See also

[simple_mvc-methods](https://aaroncaldwell.us/SimplyAgree/reference/simple_mvc-methods.md),
[`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md),
[`sim_methods()`](https://aaroncaldwell.us/SimplyAgree/reference/sim_methods.md),
[`mc_long()`](https://aaroncaldwell.us/SimplyAgree/reference/mc_long.md),
[`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
[`tolerance_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/tolerance_limit.md)

## Examples

``` r
# three methods, linked replicates
dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
                  alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
                  sigma = c(0.3, 0.5, 0.4), replicates = "linked",
                  seed = 1)
fit = method_vc(dat, y = "y", method = "method", id = "id",
                replicate = "repl", replicates = "linked")
fit
#> Variance Component Limits of Agreement (LoA)
#> 95% LoA @ 95% Confidence
#> Linked replicates; common method-by-item variance (tau) (30 items, 3 methods)
#> 
#>  Methods Bias    Bias CI            Lower LoA Upper LoA LoA CI           
#>  A - B   -0.4480 [-0.6638, -0.2323] -1.9263   1.0303    [-2.1832, 1.2872]
#>  A - C    0.3192 [ 0.1116,  0.5268] -1.0550   1.6934    [-1.3089, 1.9474]
#>  B - C    0.7672 [ 0.5473,  0.9871] -0.7629   2.2973    [-1.0262, 2.5606]
#>  SD    
#>  0.7542
#>  0.7011
#>  0.7807
#> 
#> LoA CI: one-sided 95% bounds (outer ends of 90% CIs) for an intersection-union test;
#>   not a joint 95% interval for the LoA
#> 
#> Variance Components (SD)
#>  Component SD     95% CI          
#>  tau       0.3452 [0.2651, 0.4496]
#>  omega     0.3473 [0.2689, 0.4487]
#>  sigma (A) 0.3261 [0.2483, 0.4282]
#>  sigma (B) 0.4735 [0.3845, 0.5830]
#>  sigma (C) 0.3833 [0.3031, 0.4847]
#> 
#> Repeatability Coefficients (95%)
#>  Method RC     RC CI            RpC    RpC CI          
#>  A      0.9038 [0.6883, 1.1868] 1.3205 [1.1046, 1.5787]
#>  B      1.3124 [1.0659, 1.6160] 1.6277 [1.3933, 1.9014]
#>  C      1.0624 [0.8401, 1.3435] 1.4337 [1.2217, 1.6825]
#> RC: limit for the difference between two measurements by a method under the same conditions
#> RpC: the same limit across replicate occasions (includes the item-by-replicate variation)

# two methods, one measurement per item (the paired-difference analysis)
dat2 = sim_methods(n_items = 40, methods = c("A", "B"), alpha = c(0, 0.3),
                   sigma = 0.5, seed = 2)
method_vc(dat2, y = "y", method = "method", id = "id")
#> Variance Component Limits of Agreement (LoA)
#> 95% LoA @ 95% Confidence
#> One measurement per item and method (40 items, 2 methods)
#> 
#>  Methods Bias    Bias CI            Lower LoA Upper LoA LoA CI           
#>  A - B   -0.4588 [-0.7076, -0.2101] -1.9831   1.0654    [-2.3905, 1.4728]
#>  SD    
#>  0.7777
#> 
#> LoA CI: one-sided 95% bounds (outer ends of 90% CIs) for an intersection-union test;
#>   not a joint 95% interval for the LoA
#> 
#> Variance Components (SD)
#>  Component SD     95% CI          
#>  sigma     0.5499 [0.4505, 0.7061]
#> sigma: residual SD of the paired-difference model (SD of the differences / sqrt(2))
```
