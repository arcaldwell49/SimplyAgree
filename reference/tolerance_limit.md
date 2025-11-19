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
  tol_method = c("approx", "perc"),
  prop_bias = FALSE,
  log_tf = FALSE,
  log_tf_display = c("ratio", "sympercent"),
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

  Name of the column for the subject ID.

- condition:

  Name of the column indicating different conditions subjects were
  tested under. This can be left missing if there are no differing
  conditions to be tested.

- time:

  Name of the column indicating the time points. Only necessary if the
  data is from time series or repeated measures collection.

- pred_level:

  Prediction level for the prediction interval. Default is 95%.

- tol_level:

  Tolerance level for the tolerance limit (i.e., the CI of the
  prediction limit). Default is 95%.

- tol_method:

  Method for calculating the tolerance interval. Options are "approx"
  for a chi-square based approximation and "perc" for a parametric
  percentile bootstrap method.

- prop_bias:

  Whether to include a proportional bias term in the model. Determines
  whether proportional bias should be considered for the
  prediction/tolerance limits calculations.

- log_tf:

  Calculate limits of agreement using log-transformed data.

- log_tf_display:

  The type of presentation for log-transformed results. The differences
  between methods can be displayed as a "ratio" or "sympercent".

- cor_type:

  The type of correlation structure. "sym" is for Compound Symmetry,
  "car1" is for continuous autocorrelation structure of order 1, or
  "ar1" for autocorrelation structure of order 1.

- correlation:

  an optional corStruct object describing the within-group correlation
  structure that overrides the default setting. See the documentation of
  corClasses for a description of the available corStruct classes. If a
  grouping variable is to be used, it must be specified in the form
  argument to the corStruct constructor. Defaults to NULL.

- weights:

  an optional varFunc object or one-sided formula describing the
  within-group heteroskedasticity structure that overrides the default
  setting. If given as a formula, it is used as the argument to
  varFixed, corresponding to fixed variance weights. See the
  documentation on varClasses for a description of the available varFunc
  classes.

- keep_model:

  Logical indicator to retain the GLS model. Useful when working with
  large data and the model is very large.

- replicates:

  The number of bootstrap replicates. Passed on to the boot function.
  Default is 999.

## Value

Returns single `tolerance_delta` class object with the results of the
agreement analysis with a prediction interval and tolerance limits.

- `limits`: A data frame containing the prediction/tolerance limits.

- `model`: The GLS model; NULL if keep_model set to FALSE.

- `call`: The matched call.

## Details

The tolerance limits calculated in this function are based on the papers
by Francq & Govaerts (2016), Francq, et al. (2019), and Francq, et al.
(2020). When `tol_method` is set to "approx", the tolerance limits are
calculated using the approximation detailed in Francq et al. (2020).
However, these are only an approximation and conservative. Therefore, as
suggested by Francq, et al. (2019), a parametric bootstrap approach can
be utilized to calculate percentile tolerance limits
(`tol_method = "perc"`).

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

## Examples

``` r
data('reps')

# Simple
tolerance_limit(x = "x", y ="y", data = reps)
#> Agreement between Measures (Difference: x-y)
#> 95% Prediction Interval with 95% Tolerance Limits
#> 
#>    Bias           Bias CI Prediction Interval Tolerance Limits
#>  0.4383 [-0.1669, 1.0436]   [-2.1998, 3.0764] [-2.993, 3.8697]
#> 
#> 

# Nested
tolerance_limit(x = "x", y ="y", data = reps, id = "id")
#> Agreement between Measures (Difference: x-y)
#> 95% Prediction Interval with 95% Tolerance Limits
#> 
#>    Bias           Bias CI Prediction Interval   Tolerance Limits
#>  0.7046 [-1.5571, 2.9663]   [-4.4686, 5.8778] [-8.6101, 10.0192]
#> 
#> 
```
