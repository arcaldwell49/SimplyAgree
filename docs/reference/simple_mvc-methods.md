# Methods for simple_mvc objects

Methods defined for objects returned from
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md).

## Usage

``` r
# S3 method for class 'simple_mvc'
predict(object, newdata, from = NULL, to = NULL, level = NULL, ...)

# S3 method for class 'simple_mvc'
print(x, digits = 4, scale = c("analysis", "original"), at = NULL, ...)

# S3 method for class 'simple_mvc'
summary(object, ...)

# S3 method for class 'summary.simple_mvc'
print(x, digits = 4, ...)

# S3 method for class 'simple_mvc'
confint(object, parm = c("all", "bias", "sd"), level = NULL, ...)

# S3 method for class 'simple_mvc'
coef(object, ...)

# S3 method for class 'simple_mvc'
vcov(object, ...)

# S3 method for class 'simple_mvc'
plot(
  x,
  type = c("ba", "matrix", "vc"),
  pair = NULL,
  pair_seed = NULL,
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  delta = NULL,
  ...
)

# S3 method for class 'simple_mvc'
check(
  x,
  pair_seed = NULL,
  error.ratio = NULL,
  bias_line = c("adjusted", "expected"),
  ...
)
```

## Arguments

- newdata, from, to:

  Passed to the `predict` method of
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md)
  results: values of the `from` method (original scale) to convert to
  the `to` method, with prediction limits.

- level:

  For `confint`, the confidence level, by default the `conf_level` used
  in
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md);
  other levels are recomputed from the stored standard errors and
  degrees of freedom. For `predict`, the coverage of the prediction
  limits (default: the `pred_level` of the fit).

- ...:

  further arguments passed through, see description of return value for
  details.

- x, object:

  Object of class `simple_mvc`, as returned from
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md).

- digits:

  The number of digits to print.

- scale:

  For `print`, the scale of the bias, limits, and repeatability
  coefficients: "analysis" (default), the scale set by `transform`, or
  "original", back-transformed to the units of the measurements at the
  level(s) in `at`. Ignored without a transformation. See details.

- at:

  For `print` with `scale = "original"`, the level(s), in original
  units, at which to express the results. Default (`NULL`) is the median
  of the item averages (computed on the analysis scale and
  back-transformed). Supplying `at` implies `scale = "original"`.

- parm:

  Which parameters to include: "all" (default), "bias", or "sd"
  (variance component SDs).

- type:

  Type of plot. "ba" (default): a Bland-Altman plot of the differences
  for one pair of methods, with the bias and limits drawn as points to
  the left of the data, as in
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md):
  the whiskers show the confidence interval of the bias and the
  confidence bounds of the limits of agreement (with
  `interval = "tolerance"`, the points are the prediction limits and the
  whiskers extend to the tolerance limits). "matrix": all pairs, with
  the methods named on the diagonal, each pair of methods plotted
  against each other above it (identity line dashed; constant-bias
  conversion line with prediction limits on the original scale), and
  Bland-Altman plots below it. "vc": estimates and confidence intervals
  of the variance component SDs and the repeatability coefficients.

- pair:

  Character vector naming the two methods to plot, in the order of the
  difference (first minus second). Default is the first pair.

- pair_seed:

  Seed for the random pairing of exchangeable replicates in the plot or
  in
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md).
  The global random number generator state is restored afterwards. With
  exchangeable replicates and no seed, the pairing changes from call to
  call, and so do the plotted differences and the
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  p-values (when items have unequal numbers of replicates by the two
  methods); set `pair_seed` for reproducible results.

- geom:

  For `type = "ba"`, the geometry used to display the data points:
  "geom_point" (default), "geom_bin2d", "geom_density_2d",
  "geom_density_2d_filled", or "stat_density_2d", as in
  [loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md).

- delta:

  For `type = "ba"`, the maximal allowable difference, drawn as dashed
  lines: one value (drawn at `delta` and `-delta`, or, for ratios with
  `transform = "log"`, at `delta` and `1/delta`) or two values, on the
  plotted scale.

- error.ratio:

  For `check`, an optional known ratio of the measurement error
  variances of the two methods, var(error of the first) / var(error of
  the second), as in
  [`dem_reg()`](https://aaroncaldwell.us/SimplyAgree/reference/dem_reg.md),
  on the analysis scale. Used only with two methods and one measurement
  per item.

- bias_line:

  For `check` with replicates or `error.ratio`, the dashed line on the
  level dependence panel: "adjusted" (default; the error-adjusted slope)
  or "expected" (the slope expected from unequal measurement error
  alone). See "Choosing `bias_line`" in
  [loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md)
  for the pros and cons.

## Value

- `print`:

  Prints the biases, limits, and variance components. With
  `transform = "log"`, biases and limits are shown as ratios; with
  `scale = "original"`, in original units at the level(s) in `at`.

- `coef`:

  Named vector: method means averaged over items (`alpha_<method>`) and
  the variance component SDs (`tau`, `omega`, `sigma_<method>`, on the
  transformed scale).

- `vcov`:

  Covariance matrix of the method means.

- `summary`:

  An object of class `summary.simple_mvc` whose print method shows the
  design (number of items by method and number of replicates), biases,
  variance components with confidence limits and df, limits, and
  repeatability coefficients.

- `confint`:

  Matrix of confidence limits for the biases and the variance component
  SDs.

- `plot`:

  A ggplot object (`type = "ba"` or `"vc"`) or a patchwork object
  (`type = "matrix"`).

- `check`:

  A patchwork of diagnostic plots, as for
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
  with what to look for in each subtitle and the tests in the captions:
  normal QQ plots of the standardized residuals by method and of the
  predicted random effects (Shapiro-Wilk tests, or Kolmogorov-Smirnov
  with 5000 or more values); and, for each pair, the homogeneity of
  variance of the differences (square root of the absolute standardized
  differences against the averages, Breusch-Pagan test) and proportional
  bias (centred differences against the averages, with a regression
  line). The tests for each pair use item means, so replicates are not
  counted more than once. With replicates, the test of the slope is
  corrected for unequal measurement error (and, with `tau = "method"`,
  unequal method-by-item variances), and the error-adjusted slope is
  drawn dashed; see "Proportional bias diagnostic" in
  [loa-methods](https://aaroncaldwell.us/SimplyAgree/reference/loa-methods.md).
  A message suggests alternatives when the slope or the Breusch-Pagan
  test has p \< 0.05. The proportional bias results are attached as the
  attribute `"prop_bias"`, one row per pair (named in `group`); `beta`
  is the scale of the second method of the pair relative to the first.

## Details

With a transformation, a constant bias and constant limits on the
analysis scale correspond, in original units, to a bias and limits that
depend on the level. `print(x, scale = "original")` expresses them at a
given level \\t\\ (on the analysis scale): a difference \\d\\ between
the methods, centered on that level, is \\h(d) = g^{-1}(t + d/2) -
g^{-1}(t - d/2)\\ in original units, where \\g^{-1}\\ is the inverse
transformation. For a fixed level, \\h\\ is increasing in \\d\\, so the
limits, their confidence bounds, and the prediction and tolerance limits
keep their coverage and confidence exactly, and the back-transformed
bias is the median difference at that level (not the mean). The
repeatability coefficients are converted in the same way. The SDs and
variance components stay on the analysis scale. Differences are centered
on the level of the two measurements, as on the x-axis of a Bland-Altman
plot, unlike [`predict()`](https://rdrr.io/r/stats/predict.html), which
starts from a measurement by one method.
