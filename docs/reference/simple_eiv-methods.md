# Methods for simple_eiv objects

Methods defined for objects returned from error-in-variables models
(e.g., `dem_reg`, `pb_reg`).

## Usage

``` r
# S3 method for class 'simple_eiv'
print(x, ...)

# S3 method for class 'simple_eiv'
formula(x, ...)

# S3 method for class 'simple_eiv'
model.frame(formula, data = NULL, na.action = na.pass, ...)

# S3 method for class 'simple_eiv'
summary(object, ...)

# S3 method for class 'simple_eiv'
confint(object, parm, level = NULL, ...)

# S3 method for class 'simple_eiv'
plot(
  x,
  x_name = NULL,
  y_name = NULL,
  interval = c("none", "confidence"),
  level = NULL,
  n_points = 100,
  data = NULL,
  ...
)

# S3 method for class 'simple_eiv'
check(x, data = NULL, ...)

plot_joint(x, ...)

# S3 method for class 'simple_eiv'
plot_joint(
  x,
  ideal_slope = 1,
  ideal_intercept = 0,
  show_intervals = TRUE,
  n_points = 100,
  ...
)

# S3 method for class 'simple_eiv'
vcov(object, ...)

# S3 method for class 'simple_eiv'
coef(object, ...)

# S3 method for class 'simple_eiv'
fitted(object, type = c("y", "x", "both"), data = NULL, ...)

# S3 method for class 'simple_eiv'
residuals(object, type = c("optimized", "x", "y", "raw_y"), data = NULL, ...)

# S3 method for class 'simple_eiv'
predict(
  object,
  newdata = NULL,
  interval = c("none", "confidence"),
  level = NULL,
  se.fit = FALSE,
  data = NULL,
  ...
)
```

## Arguments

- ...:

  further arguments passed through.

- formula:

  A simple_eiv object (for model.frame method)

- data:

  Optional data frame. Required if model was fitted with `model = FALSE`
  and `newdata` is NULL.

- na.action:

  Function for handling NA values (default: na.pass)

- object, x:

  object of class `simple_eiv` from dem_reg or pb_reg function.

- parm:

  A specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  Confidence level for intervals (default uses the model's conf.level).

- x_name:

  Name for x-axis label (optional).

- y_name:

  Name for y-axis label (optional).

- interval:

  Type of interval calculation. Can be "none" (default), or "confidence"

- n_points:

  Number of points to use for drawing the joint confidence region
  (default = 100).

- ideal_slope:

  The hypothesized slope value to test against (default = 1)

- ideal_intercept:

  The hypothesized intercept value to test against (default = 0)

- show_intervals:

  Logical. If TRUE, shows individual confidence intervals as well.

- type:

  Type of residuals to return. Options are "optimized" (default), "x",
  "y", or "raw_y".

- newdata:

  An optional data frame containing values of X at which to predict. If
  omitted, the fitted values are returned.

- se.fit:

  Logical. If TRUE, standard errors of predictions are returned. Note:
  For Passing-Bablok regression without bootstrap, standard errors are
  not available and this argument is ignored with a warning.

## Value

- `print`:

  Prints short summary of the EIV regression model.

- `summary`:

  Prints detailed summary.

- `plot`:

  Returns a plot of the regression line and data.

- `check`:

  Returns plots of residuals.

- `plot_joint`:

  Returns plot of joint confidence region (Deming only).

- `predict`:

  Predicts Y values for new X values.

- `fitted`:

  Extracts fitted values.

- `residuals`:

  Extracts residuals.

- `coef`:

  Extracts model coefficients.

- `vcov`:

  Extracts variance-covariance matrix.
