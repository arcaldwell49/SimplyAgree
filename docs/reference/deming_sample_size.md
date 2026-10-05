# Determine Required Sample Size for Deming Regression

\#' **\[experimental\]**

Automatically determines the minimum sample size needed to achieve
target statistical power for detecting specified bias in method
comparison studies using Deming regression.

## Usage

``` r
deming_sample_size(
  target_power = 0.9,
  initial_n = 20,
  max_n = 500,
  n_sims = 500,
  use_joint = TRUE,
  step_size = 5,
  ...
)
```

## Arguments

- target_power:

  Desired statistical power (e.g., 0.80 or 0.90). Default is 0.90.

- initial_n:

  Starting sample size for search. Default is 20.

- max_n:

  Maximum sample size to try. Default is 500.

- n_sims:

  Number of simulations per sample size tested. Default is 500.

- use_joint:

  Logical. If TRUE, optimizes for joint region power; if FALSE, for CI
  power.

- step_size:

  Step size for sample size increments. Default is 5.

- ...:

  Additional arguments passed to deming_power_sim()

## Value

A list of class "deming_sample_size" containing:

- n_required_ci:

  Required N for confidence intervals

- n_required_joint:

  Required N for joint confidence region

- target_power:

  Target power level

- power_curve:

  Data frame with N and power for both methods

- reduction_n:

  Sample size reduction using joint method

- reduction_pct:

  Percentage reduction

## Details

This function performs a grid search over sample sizes to find the
minimum N needed to achieve the target power. It tests both confidence
interval and joint confidence region approaches, allowing comparison of
required sample sizes.

Using joint confidence regions typically requires 20-50% fewer samples
than confidence intervals when the measurement range is narrow (max:min
ratio \< 10:1).

## Examples

``` r
if (FALSE) { # \dontrun{
# Determine N needed for 90% power to detect 5% bias
sample_size_result <- deming_sample_size(
  target_power = 0.90,
  initial_n = 30,
  max_n = 200,
  n_sims = 500,
  x_range = c(20, 200),
  actual_slope = 1.05,
  ideal_slope = 1.0,
  y_var_params = list(beta1 = 1, beta2 = 0.05, J = 2, type = "power"),
  x_var_params = list(beta1 = 0.8, beta2 = 0.04, J = 2, type = "power")
)

print(sample_size_result)
plot(sample_size_result)
} # }
```
