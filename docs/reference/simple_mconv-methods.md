# Methods for simple_mconv objects

Methods defined for objects returned from
[`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md).

## Usage

``` r
# S3 method for class 'simple_mconv'
print(x, digits = 4, ...)

# S3 method for class 'simple_mconv'
summary(object, ...)

# S3 method for class 'summary.simple_mconv'
print(x, digits = 4, ...)

# S3 method for class 'simple_mconv'
check(x, ...)

# S3 method for class 'simple_mconv'
coef(object, ...)

# S3 method for class 'simple_mconv'
confint(object, parm = c("all", "alpha", "beta"), level = NULL, ...)

# S3 method for class 'simple_mconv'
predict(object, newdata, from = NULL, to = NULL, level = NULL, ...)

# S3 method for class 'simple_mconv'
plot(
  x,
  type = c("conversion", "ba"),
  pair = NULL,
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  delta = NULL,
  ...
)
```

## Arguments

- x, object:

  Object of class `simple_mconv`, as returned from
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md).

- digits:

  The number of digits to print.

- ...:

  further arguments passed through, see description of return value for
  details.

- parm:

  Which parameters to include: "all" (default), "alpha" (intercepts), or
  "beta" (slopes).

- level:

  For `confint`, the confidence level (default: the `conf_level` of the
  fit). For `predict`, the coverage of the prediction limits (default:
  the `pred_level` of the fit).

- newdata:

  Values of the `from` method on the original scale: a numeric vector,
  or a data frame with a column named after the `from` method.

- from, to:

  Names of the methods to convert from and to. Default: from the first
  method to the second.

- type:

  Type of plot. "conversion" (default): the `to` method against the
  `from` method on the original scale, with the observed pairs, the line
  of identity (dashed), and the conversion with its prediction limits.
  "ba": a Bland-Altman plot of the differences (first minus second
  method of `pair`) with the bias and prediction limits implied by the
  model, as lines that change with the level (linear bias or scaled
  model). For a conversion from a
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)
  fit, the bias is constant and the plot is that of the fit (see
  [simple_mvc-methods](https://aaroncaldwell.us/SimplyAgree/reference/simple_mvc-methods.md)):
  the bias and limits are points with their confidence intervals, to the
  left of the data.

- pair:

  Character vector of two method names. For `type = "conversion"`,
  `c(from, to)`; for `type = "ba"`, the difference is first minus
  second. Default is the first two methods.

- geom, delta:

  For `type = "ba"`, the geometry used to display the data points and
  the maximal allowable difference; see
  [simple_mvc-methods](https://aaroncaldwell.us/SimplyAgree/reference/simple_mvc-methods.md).

## Value

- `print`:

  Prints the conversion equations (intercept, slope, and prediction SD)
  with confidence intervals.

- `coef`:

  Named vector of the conversion parameters, `alpha[to|from]` and
  `beta[to|from]`.

- `confint`:

  Matrix of confidence limits for the conversion parameters: percentile
  intervals from the bootstrap estimates for `ci_method = "boot"`,
  otherwise from the standard errors and the t distribution.

- `predict`:

  A tibble with the values converted from `from` to `to` on the original
  scale: `from_value`, `fit`, and the prediction limits `lower` and
  `upper`. Values outside the domain of the transformation give `NA`.

- `plot`:

  A ggplot object.

- `summary`:

  An object of class `summary.simple_mconv` whose print method shows the
  fit details (iterations and convergence for the scaled model,
  bootstrap failures; with `estimator = "ml"`, the log-likelihood, any
  zero or negative variance estimates, and the model fit: chi-square
  test, RMSEA and SRMR), the design, the number of pairs used, the
  regressions of the differences on the averages (linear bias) or the
  per-method intercepts and slopes (scaled), the conversions, and the
  variance components.

- `check`:

  A patchwork of diagnostic plots, as for
  [`agreement_limit()`](https://aaroncaldwell.us/SimplyAgree/reference/agreement_limit.md),
  with what to look for in each subtitle and the tests in the captions.
  Linear bias, for each pair: a normal QQ plot of the standardized
  residuals of the regression of the differences on the averages
  (Shapiro-Wilk test); the square root of the absolute standardized
  residuals against the averages (Breusch-Pagan test; with
  `variance = "linear"`, the residuals are divided by the fitted SD);
  and the residuals against the averages (RESET test of linearity). The
  last two use item means, so replicates are not counted more than once.
  Scaled model: normal QQ plots of the standardized residuals of the
  final variance component model by method (Shapiro-Wilk tests), the
  square root of their absolute values against the fitted true values
  (Breusch-Pagan tests), and the intercepts and slopes at each iteration
  of the alternating regression. With `estimator = "ml"`, the residuals
  are from the factor scores of the true values, item-by-replicate and
  method-by-item effects, and the iteration panel is replaced by a
  caption with the convergence and model fit. Not available for
  conversions from
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md);
  use
  [`check()`](https://aaroncaldwell.us/SimplyAgree/reference/simple_agree-methods.md)
  on that fit.
