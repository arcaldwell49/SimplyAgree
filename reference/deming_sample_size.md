# Determine Required Sample Size for Deming Regression

**\[experimental\]**

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
  use_joint = NULL,
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

  Which target ends the search. `NULL` (default) searches until both the
  joint-region and the confidence-interval targets are reached, so both
  sample sizes (and the reduction) are reported. `TRUE` stops at the
  joint-region target and `FALSE` at the confidence-interval target; the
  other sample size is then reported only if it was reached first.

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

Using joint confidence regions typically requires far fewer samples than
confidence intervals when the measurement range is narrow (max:min ratio
\< 10:1).

Each power is a simulation estimate (Monte Carlo SE about 1.3 percentage
points at 90% power with `n_sims = 500`), and the search takes the first
N whose estimate reaches the target, which tends to pick N slightly too
small. In simulations the true power at the returned N was close to the
target on average but below 85% in up to 20% of runs. Use a larger
`n_sims` (or check the chosen N with
[`deming_power_sim()`](https://aaroncaldwell.us/SimplyAgree/reference/deming_power.md))
when the sample size matters. N is never below `initial_n`.

## Examples

``` r
# \donttest{
# Determine N needed for 90% power to detect 5% bias with the joint
# region (use_joint = TRUE stops there; n_sims = 500 or more in practice)
sample_size_result <- deming_sample_size(
  target_power = 0.90,
  initial_n = 10,
  max_n = 200,
  n_sims = 200,
  step_size = 10,
  use_joint = TRUE,
  x_range = c(20, 200),
  actual_slope = 1.05,
  ideal_slope = 1.0,
  y_var_params = list(beta1 = 1, beta2 = 0.05, J = 2, type = "power"),
  x_var_params = list(beta1 = 0.8, beta2 = 0.04, J = 2, type = "power")
)
#> Searching for N to achieve 90% power...
#>   Testing N = 10... CI: 13.0%, Joint: 33.5%
#>   Testing N = 20... CI: 18.5%, Joint: 63.0%
#>   Testing N = 30... CI: 27.5%, Joint: 80.5%
#>   Testing N = 40... CI: 38.5%, Joint: 90.0%
#> 
#> Target power achieved!

print(sample_size_result)
#> 
#> === Deming Regression Sample Size Determination ===
#> 
#> Target Power: 90%
#> 
#> Required Sample Sizes:
#>   Confidence Intervals:  Not reached by N = 40 (search stopped at the joint target) 
#>   Joint Region:          N = 40
plot(sample_size_result)

# }
```
