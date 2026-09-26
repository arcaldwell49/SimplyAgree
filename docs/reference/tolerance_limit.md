# Tolerance Limits from an Agreement Study

**\[maturing\]**

A function for calculating tolerance limits for the difference between
two measurements (difference = x-y). This is a procedure that should
produce results similar to the Bland-Altman limits of agreement. See
vignettes for more details.

## Usage

``` r
tolerance_limit(
  data,
  x,
  y,
  id = NULL,
  condition = NULL,
  time = NULL,
  pred_level = 0.95,
  tol_level = 0.95,
  conf_level = 0.95,
  tol_method = c("analytic", "boot_cal"),
  bound_type = c("joint", "iu"),
  prop_bias = FALSE,
  log_tf = FALSE,
  log_tf_display = c("ratio", "sympercent"),
  model = c("gls", "lme"),
  cor_type = c("sym", "car1", "ar1", "none"),
  correlation = NULL,
  weights = NULL,
  keep_model = TRUE,
  replicates = 999
)
```

## Arguments

- data:

  A data frame containing the variables.

- x:

  Name of the column for the first measurement.

- y:

  Name of the column for the second measurement.

- id:

  Name of the column for the subject ID. With `model = "lme"`, two
  column names can be given for nested random intercepts, outer level
  first (e.g., `c("golfer", "club")` for clubs within golfers); see
  "Model assumptions" in details.

- condition:

  Name of the column indicating different conditions subjects were
  tested under. This can be left missing if there are no differing
  conditions to be tested. Supplying a condition adds a separate
  residual variance for each condition
  ([`nlme::varIdent`](https://rdrr.io/pkg/nlme/man/varIdent.html)); see
  "Model assumptions" in details.

- time:

  Name of the column indicating the time points. Only necessary if the
  data is from time series or repeated measures collection.

- pred_level:

  Prediction level for the prediction interval, which is also the
  content (the proportion of differences to be covered, beta) for the
  tolerance limits. Default is 95%.

- tol_level:

  Confidence level (gamma) for the tolerance limits. Default is 95%. See
  `bound_type` for what this confidence refers to.

- conf_level:

  Confidence level for the confidence interval of the bias
  (`lower.CL`/`upper.CL`). Default is 95%. This does not affect the
  prediction or tolerance limits. For a two one-sided tests (TOST)
  procedure on the bias at level alpha, use `conf_level = 1 - 2 * alpha`
  (e.g., 0.90).

- tol_method:

  Method for calculating the tolerance limits. Options are "analytic"
  (default) for closed-form limits and "boot_cal" (experimental) for the
  closed-form limits calibrated by a parametric bootstrap. The analytic
  limits are recommended; see details. Both target the interval set by
  `bound_type`. The previous names, "approx" and "perc", are deprecated
  aliases for "analytic" and "boot_cal".

- bound_type:

  Which claim the tolerance limits (`lower.TL`/`upper.TL`) support.
  "joint" (default) gives a beta-content, gamma-confidence tolerance
  interval: with confidence `tol_level`, at least `pred_level` of all
  differences lie within the limits. "iu" gives equal-tailed bounds: a
  one-sided `tol_level` confidence bound on each of the (1 -
  `pred_level`)/2 and (1 + `pred_level`)/2 percentiles of the
  differences. The "iu" bounds are intended for an intersection-union
  test of agreement against a maximal allowable difference and are not a
  joint `tol_level` interval. See details.

- prop_bias:

  Whether to include a proportional bias term in the model. Determines
  whether proportional bias should be considered for the
  prediction/tolerance limits calculations. Note that a slope of the
  differences on the average can appear without any true proportional
  bias when the two methods have unequal measurement error variances;
  see "Model assumptions" in details.

- log_tf:

  Calculate limits of agreement using log-transformed data.

- log_tf_display:

  The type of presentation for log-transformed results. The differences
  between methods can be displayed as a "ratio" or "sympercent".

- model:

  The type of model for the differences. "gls" (default) is a marginal
  generalized least squares model
  ([`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html)) with the
  correlation structure set by `cor_type`. "lme" is a linear mixed model
  ([`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)) with a random
  intercept for each `id`, which requires `id`; with two `id` columns it
  has nested random intercepts (outer level, and inner level within
  outer). With compound symmetry and no variance function both give the
  same fit; "lme" differs when serial correlation (`cor_type = "ar1"` or
  `"car1"`) or a variance function (`condition` or `weights`) is added,
  because it keeps a persistent subject effect and applies the variance
  function to the residuals only. See "Model assumptions" in details.

- cor_type:

  The type of correlation structure. "sym" is for Compound Symmetry,
  "car1" is for continuous autocorrelation structure of order 1, or
  "ar1" for autocorrelation structure of order 1. The autoregressive
  options ("ar1" and "car1") have no persistent subject effect; see
  "Model assumptions" in details. With `model = "lme"`, "sym" fits the
  random intercept alone, "ar1" and "car1" add serial correlation of the
  residuals within `id` on top of the random intercept, and "none" is
  not allowed.

- correlation:

  an optional corStruct object describing the within-group correlation
  structure that overrides the default setting. See the documentation of
  corClasses for a description of the available corStruct classes. If a
  grouping variable is to be used, it must be specified in the form
  argument to the corStruct constructor. Defaults to NULL. With
  `model = "lme"`, this is the correlation of the residuals within the
  innermost level of `id` (on top of the random intercepts) and must be
  grouped by it using the internal names: `id` for one level (e.g.,
  `nlme::corAR1(form = ~ time | id)`) or `id/id_2` for two (e.g.,
  `nlme::corAR1(form = ~ time | id/id_2)`).

- weights:

  an optional varFunc object or one-sided formula describing the
  within-group heteroskedasticity structure that overrides the default
  setting. If given as a formula, it is used as the argument to
  varFixed, corresponding to fixed variance weights. See the
  documentation on varClasses for a description of the available varFunc
  classes. Variance covariates must use the internal column names
  (`avg`, `condition`, `x`, `y`, `time`) or `fitted(.)`. Currently
  `varIdent`, `varFixed`, `varExp`, and `varPower` are supported. If the
  variance depends on `avg`, the limits are reported at the minimum,
  median, and maximum of `avg` even when `prop_bias = FALSE`. With
  `model = "lme"`, the variance function applies to the residuals only;
  the random-intercept variance is common to all observations.

- keep_model:

  Logical indicator to retain the fitted model (`gls` or `lme`). Useful
  when working with large data and the model is very large.

- replicates:

  The number of bootstrap replicates. Passed on to the boot function.
  Default is 999.

## Value

Returns single `tolerance_delta` class object with the results of the
agreement analysis with a prediction interval and tolerance limits.

- `limits`: A data frame containing the prediction/tolerance limits.
  Columns include `bias` (estimated mean difference), `SEM` (standard
  error of the bias), `SD` (residual standard deviation of a single
  difference at that row, from the variance function if one is in the
  model), `SEP` (standard error of prediction, `sqrt(SD^2 + SEM^2)`),
  `SD.df` (degrees of freedom of the residual variance), `SD.upper`
  (one-sided `tol_level` upper confidence bound for SD),
  `SD.between`/`SD.nested`/`SD.within` (between-subject, nested (inner
  level), and within-subject components of SD, under compound symmetry
  or with `model = "lme"`; `NA` otherwise, and `SD.nested` is `NA`
  without nesting), `lower.CL`/`upper.CL` (confidence limits for the
  bias), `lower.PL`/`upper.PL` (prediction limits),
  `lower.TL`/`upper.TL` (tolerance limits), and, for
  `tol_method = "boot_cal"`, `lower.TL.level`/`upper.TL.level` (the
  calibrated nominal levels at which the analytic limits were computed).

- `model`: The fitted `gls` or `lme` model; NULL if keep_model set to
  FALSE. The data (with the internal column names `x`, `y`, `delta`,
  `avg`, `id` (the row number if not supplied), and, if supplied,
  `condition` and `time`), correlation structure, and variance function
  are stored with the model, so
  [`update()`](https://rdrr.io/r/stats/update.html) and
  [`nlme::getData()`](https://rdrr.io/pkg/nlme/man/getData.html) can be
  used on it directly.

- `call`: The matched call.

## Details

The tolerance limits calculated in this function are based on the papers
by Francq & Govaerts (2016), Francq, et al. (2019), and Francq, et al.
(2020). The formulas, including how the degrees of freedom are
determined for clustered data, are given in
[`vignette("agreement_analysis", package = "SimplyAgree")`](https://aaroncaldwell.us/SimplyAgree/articles/agreement_analysis.md).

The output contains three kinds of interval, which support different
claims:

- **Prediction interval** (`lower.PL`/`upper.PL`): a beta-expectation
  tolerance interval. It is expected to contain a single future
  difference, from a new subject, with probability `pred_level`,
  averaged over repeated studies. It is not a confidence statement: any
  single prediction interval may cover less than `pred_level` of the
  differences.

- **Tolerance limits, `bound_type = "joint"`** (`lower.TL`/`upper.TL`):
  a beta-content, gamma-confidence tolerance interval. With confidence
  `tol_level`, at least `pred_level` of all differences lie within the
  limits. If the limits lie within a maximal allowable difference of
  plus or minus delta, one can conclude (at level 1 - `tol_level`) that
  at least `pred_level` of the differences lie within plus or minus
  delta (Lin, 2000). The tails do not need to be split equally.

- **Tolerance limits, `bound_type = "iu"`**: a one-sided `tol_level`
  bound on the lower and on the upper (1 -/+ `pred_level`)/2 percentile
  of the differences, i.e., on the limits of agreement. If both bounds
  lie within plus or minus delta, one can reject, at level 1 -
  `tol_level`, that either limit of agreement lies outside plus or minus
  delta (intersection-union test). Both bounds hold together only about
  1 - 2 \* (1 - `tol_level`) of the time, so they should not be reported
  as a joint `tol_level` interval. These target the same quantities as
  the default confidence bounds from
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  (numerically close but not identical, because the models differ).

The "joint" and "iu" limits test different hypotheses (coverage versus
each tail), so they do not nest: the "iu" bounds can be wider than the
"joint" limits, and a skewed distribution can pass one test and fail the
other.

With `tol_method = "analytic"`:

- "joint" uses the approximation of Howe (1969) as described by Francq
  et al. (2020), with the standard error of prediction (SEP) in place of
  the sample standard deviation. For independent data this matches
  Howe's approximation exactly.

- "iu" uses the exact noncentral t bound for independent data, and
  otherwise the MOVER bound of Zou (2013).

- Both use a one-sided upper confidence bound for the residual standard
  deviation (SD). With independent data this is the usual chi-square
  bound. With compound symmetry (`cor_type = "sym"`), the between- and
  within-subject variance components are combined with the MOVER, as in
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md)
  with `data_type = "nest"`. With other correlation structures, the
  bound uses an effective degrees of freedom (`SD.df`) from the
  approximate covariance of the variance parameters.

The analytic limits are exact for independent data (without a variance
function). In simulations (95% target), they were close to nominal with
compound symmetry (about 0.95, with 10 to 20 subjects) and with AR(1)
correlation (about 0.95). With `condition` (a separate residual variance
per condition) combined with compound symmetry, the variance components
for each condition are bounded using that condition's cluster sizes and
degrees of freedom; coverage was then about 0.96. Other variance
functions (`weights`) use the cluster sizes of the whole data set and
have not been checked by simulation.

With `tol_method = "boot_cal"`, the analytic limits are calibrated by a
parametric bootstrap (Loh, 1987; Beran, 1987). New data are simulated
from the fitted model (Francq et al., 2019), the model is refit to each
replicate, and the analytic limits are computed for each replicate over
a range of nominal confidence levels. The level is chosen at which the
replicates achieve the target confidence for the fitted model:

- "joint": a proportion `tol_level` of the replicate intervals contain
  at least `pred_level` of the fitted distribution of the differences.

- "iu": separately for each bound, a proportion `tol_level` of the
  replicate bounds lie beyond the fitted limit of agreement.

The limits for the observed data are the analytic limits at the
calibrated levels, which are returned as `lower.TL.level` and
`upper.TL.level`. `tol_method = "boot_cal"` is **experimental**. The
calibration is judged against the fitted model, so it treats the
estimated correlation as known. With few subjects this adds its own
error: in simulations with 10 to 15 subjects (95% target), coverage
ranged from about 0.93 (compound symmetry, with or without `condition`)
to about 0.96 (AR(1) correlation), while the analytic limits were 0.95
to 0.965 in the same settings. The analytic limits are therefore
recommended; the bootstrap may be useful as a check for models whose
analytic limits have not been evaluated (e.g., other variance functions
or `prop_bias = TRUE`).

For clustered data (`id` supplied), all limits refer to a new difference
from a new subject: the SD includes both the between- and within-subject
variance.

### Model assumptions

With `model = "gls"` (default), the model is a marginal (generalized
least squares) model fit with
[`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html). For the default
setup (`cor_type = "sym"` without a variance function), compound
symmetry with a non-negative correlation gives the same likelihood as a
random-intercept model, and targets the marginal distribution of a
single difference from a randomly chosen subject. With `model = "lme"`,
the model is a linear mixed model fit with
[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) with a random
intercept for each `id`; with compound symmetry and no variance function
it gives the same fit and limits. With `model = "lme"`, the degrees of
freedom for the bias are the containment degrees of freedom (subjects -
1 for the models fit here), because the Satterthwaite approximation for
`lme` models in emmeans can fail. The other options carry assumptions
that should be checked:

- **Autoregressive correlation** (`cor_type = "ar1"` or `"car1"`): with
  `model = "gls"`, the correlation between two measurements from the
  same subject decays towards zero as they get further apart in time. A
  persistent subject-specific bias (e.g., a subject by method
  interaction) instead makes all measurements from that subject equally
  correlated. When such an effect exists, the autoregressive structures
  miss most of the long-range correlation, so the standard error of the
  bias is too small and the limits are too narrow; the bootstrap does
  not correct this, because it simulates from the same model. In one
  simulation (20 subjects with 19 measurements each, and a random
  subject effect), the 95% confidence interval for the bias covered the
  true value 74% of the time with AR(1), versus 96% with compound
  symmetry. With `model = "lme"`, the serial correlation is added to the
  residuals on top of a random intercept, which keeps the persistent
  subject effect. In simulations with a random subject effect and AR(1)
  residuals (15 subjects, 8 measurements each; 95% target), coverage of
  the joint tolerance limits was about 0.89 with
  `model = "gls", cor_type = "ar1"` and about 0.95 with
  `model = "lme", cor_type = "ar1"`. Use the autoregressive options with
  `model = "gls"` only when no persistent subject effect is expected.
  The fits can be compared with
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) on the returned models (a
  REML comparison is valid because the fixed effects are the same).

- **Variance functions with compound symmetry** (`condition`, or
  `weights` together with `cor_type = "sym"`): in `gls`, the correlation
  applies to the standardized residuals, so the covariance between two
  measurements from the same subject is rho \* sigma_i \* sigma_j. The
  between-subject variance therefore scales with the variance function
  (e.g., it is larger in a condition with a larger residual SD), rather
  than being common to all conditions. The marginal variance of each
  condition, which drives the limits, is not directly affected, but the
  standard error of each condition's bias and the reported variance
  components are. With `model = "lme"`, the variance function applies to
  the residuals only, and the between-subject variance is common to all
  conditions. In simulations where that was true (12 subjects; 95%
  target), the joint tolerance limits had coverage of about 0.96 with
  `model = "gls"` and about 0.97 with `model = "lme"`.

- **Clustering levels**: `model = "gls"` has a single grouping factor
  (`id`). With `model = "lme"`, `id` can name two nested levels (e.g.,
  `id = c("golfer", "club")` for shots with several clubs per golfer),
  giving random intercepts for the outer level and for the inner level
  within the outer level. Setting `id` to the inner level alone (e.g., a
  golfer-by-club identifier) should be avoided: it drops the correlation
  across inner levels within the same outer level, which understates the
  uncertainty. In simulations (15 subjects with 2 to 4 settings each;
  95% target), the joint tolerance limits had coverage of about 0.88 to
  0.91 with the inner level as `id`, versus about 0.96 with nested
  random intercepts. Grouping by the outer level only (`id = "golfer"`)
  also gave close to nominal coverage (about 0.96), because the
  inner-level variance is then absorbed into the residual; the nested
  model additionally reports the inner-level variance component
  (`SD.nested`). More than two levels are not supported.

- **Variance components**: the SD of a single difference splits into
  between-subject (`SD.between`), inner-level (`SD.nested`, with two
  `id` columns), and within-subject (`SD.within`) components. With
  `model = "gls"` and compound symmetry these are sqrt(rho) \* SD and
  sqrt(1 - rho) \* SD; with `model = "lme"`, `SD.between` is the
  random-intercept SD and `SD.within` the residual SD. They can be used,
  for example, for the point estimate of limits for the mean of m
  measurements per subject, bias +/- z \* sqrt(SD.between^2 +
  SD.within^2 / m) (without residual correlation), or for the
  between-subject floor of agreement. They are point estimates only.

- **Proportional bias** (`prop_bias = TRUE`): a non-zero slope of the
  differences on the average can appear without any true proportional
  bias. Whenever the two methods have unequal measurement error
  variances, cov(difference, average) = (var(x) - var(y)) / 2 is not
  zero (Bland & Altman, 1999), so the slope should not be read as
  proportional bias on its own. Errors-in-variables methods such as
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md)
  or
  [`pb_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/pb_reg.md)
  are better suited to assessing proportional bias.

## References

Francq, B. G., & Govaerts, B. (2016). How to regress and predict in a
Bland–Altman plot? Review and contribution based on tolerance intervals
and correlated‐errors‐in‐variables models. Statistics in mMdicine,
35(14), 2328-2358.

Francq, B. G., Lin, D., & Hoyer, W. (2019). Confidence, prediction, and
tolerance in linear mixed models. Statistics in Medicine, 38(30),
5603-5622.

Francq, B. G., Berger, M., & Boachie, C. (2020). To tolerate or to
agree: A tutorial on tolerance intervals in method comparison studies
with BivRegBLS R Package. Statistics in Medicine, 39(28), 4334-4349.

Howe, W. G. (1969). Two-sided tolerance limits for normal
populations—some improvements. Journal of the American Statistical
Association, 64(326), 610-620.

Lin, L. I. (2000). Total deviation index for measuring individual
agreement with applications in laboratory performance and
bioequivalence. Statistics in Medicine, 19(2), 255-270.

Loh, W. Y. (1987). Calibrating confidence coefficients. Journal of the
American Statistical Association, 82(397), 155-162.

Beran, R. (1987). Prepivoting to reduce level error of confidence sets.
Biometrika, 74(3), 457-468.

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
Statistical Methods in Medical Research, 22(6), 630-642.

## Examples

``` r
data('reps')

# Simple
tolerance_limit(x = "x", y ="y", data = reps)
#> Agreement between Measures (Difference: x-y)
#> 95% CI for Bias; 95% Prediction Interval
#> Tolerance Limits: at least 95% of differences with 95% confidence
#> Model: GLS, independent differences
#> 
#>    Bias           Bias CI Prediction Interval Tolerance Limits
#>  0.4383 [-0.1669, 1.0436]   [-2.1998, 3.0764] [-2.993, 3.8697]
#> 
#> 

# Nested
tolerance_limit(x = "x", y ="y", data = reps, id = "id")
#> Agreement between Measures (Difference: x-y)
#> 95% CI for Bias; 95% Prediction Interval
#> Tolerance Limits: at least 95% of differences with 95% confidence
#> Model: GLS, compound symmetry within id
#> 
#>    Bias           Bias CI Prediction Interval  Tolerance Limits
#>  0.7046 [-1.5572, 2.9664]   [-4.4687, 5.8779] [-8.3471, 9.7562]
#> 
#> 

# Nested, random intercept (mixed) model
tolerance_limit(x = "x", y ="y", data = reps, id = "id", model = "lme")
#> Agreement between Measures (Difference: x-y)
#> 95% CI for Bias; 95% Prediction Interval
#> Tolerance Limits: at least 95% of differences with 95% confidence
#> Model: random intercept for each id (lme)
#> 
#>    Bias           Bias CI Prediction Interval  Tolerance Limits
#>  0.7046 [-1.5511, 2.9603]    [-4.4548, 5.864] [-8.3471, 9.7562]
#> 
#> 

# Nested random intercepts (e.g., measurements within settings within subjects)
# \donttest{
reps2 = reps
reps2$setting = rep(1:2, length.out = nrow(reps2))
tolerance_limit(x = "x", y = "y", data = reps2, id = c("id", "setting"),
                model = "lme")
#> Agreement between Measures (Difference: x-y)
#> 95% CI for Bias; 95% Prediction Interval
#> Tolerance Limits: at least 95% of differences with 95% confidence
#> Model: nested random intercepts for id and setting within id (lme)
#> 
#>    Bias           Bias CI Prediction Interval  Tolerance Limits
#>  0.7046 [-1.5511, 2.9603]    [-4.4548, 5.864] [-8.3473, 9.7565]
#> 
#> 
# }
```
