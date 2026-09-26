# Simulate Deming Regression Power

**\[experimental\]**

Functions for conducting power analysis and sample size determination
for Deming regression in method comparison studies. These functions help
determine the sample size needed to detect specified biases
(proportional and/or constant) between two measurement methods.

Estimates statistical power to detect deviations from the line of
identity using simulated data with known properties.

## Usage

``` r
deming_power_sim(
  n_sims = 1000,
  sample_size = 50,
  x_range = c(10, 100),
  x_dist = "uniform",
  actual_slope = 1.05,
  actual_intercept = 0,
  ideal_slope = 1,
  ideal_intercept = 0,
  y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
  x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
  weighted = FALSE,
  conf.level = 0.95
)
```

## Arguments

- n_sims:

  Number of simulation iterations. Default is 1000.

- sample_size:

  Sample size (number of paired observations) per simulation.

- x_range:

  Numeric vector of length 2 specifying min and max of X values (e.g.,
  c(10, 100)).

- x_dist:

  Character specifying distribution of X values: "uniform", "central",
  or "right_skewed".

- actual_slope:

  Actual slope used to generate data (e.g., 1.05 for 5% proportional
  bias).

- actual_intercept:

  Actual intercept used to generate data.

- ideal_slope:

  Hypothesized slope to test against (typically 1 for identity line).

- ideal_intercept:

  Hypothesized intercept to test against (typically 0 for identity
  line).

- y_var_params:

  List with Y variance parameters: beta1, beta2, J, type. Type can be
  "constant", "proportional", or "power". For power function: sigma^2 =
  (beta1 + beta2\*U)^J

- x_var_params:

  List with X variance parameters (same structure as y_var_params).

- weighted:

  Logical. Use weighted Deming regression? Default is FALSE.

- conf.level:

  Confidence level for tests. Default is 0.95.

## Value

A list of class "deming_power" containing:

- power_ci_slope:

  Power based on slope confidence interval

- power_ci_intercept:

  Power based on intercept confidence interval

- power_either_ci:

  Power when either CI detects difference

- power_joint:

  Power based on joint confidence region

- settings:

  List of simulation settings

- advantage:

  Difference in power between joint region and CIs

## Details

Power Analysis for Deming Regression

This function generates simulated datasets with specified error
characteristics and tests whether confidence intervals and joint
confidence regions detect deviations from hypothesized values. The joint
confidence region typically provides higher statistical power,
especially when the X-range is narrow (high slope-intercept
correlation).

The variance functions allow flexible modeling of heteroscedastic
errors:

- constant: sigma^2 = beta1

- proportional: sigma^2 = beta1 \* U^2 (constant CV%)

- power: sigma^2 = (beta1 + beta2\*U)^J

## References

Sadler, W.A. (2010). Joint parameter confidence regions improve the
power of parametric regression in method-comparison studies.
Accreditation and Quality Assurance, 15, 547-554.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple example: detect 5% proportional bias with constant variance
power_result <- deming_power_sim(
  n_sims = 500,
  sample_size = 50,
  x_range = c(10, 100),
  actual_slope = 1.05,
  ideal_slope = 1.0,
  y_var_params = list(beta1 = 25, beta2 = 0, J = 1, type = "constant"),
  x_var_params = list(beta1 = 20, beta2 = 0, J = 1, type = "constant")
)
print(power_result)

# More complex: heteroscedastic errors
power_result2 <- deming_power_sim(
  n_sims = 500,
  sample_size = 75,
  x_range = c(1, 100),
  actual_slope = 1.03,
  y_var_params = list(beta1 = 0.5, beta2 = 0.05, J = 2, type = "power"),
  x_var_params = list(beta1 = 0.4, beta2 = 0.04, J = 2, type = "power"),
  weighted = TRUE
)
} # }
```
