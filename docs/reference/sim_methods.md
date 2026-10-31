# Simulate Method Comparison Data

**\[experimental\]**

Simulates measurements of a set of items by several methods, with or
without replicates, from the variance component model of Carstensen
(2010) with a linear (scaled) relation between methods. Useful for
planning studies, checking methods by simulation, and examples.

## Usage

``` r
sim_methods(
  n_items = 50,
  n_reps = 1,
  methods = c("A", "B"),
  alpha = 0,
  beta = 1,
  mu_mean = 0,
  mu_sd = 1,
  tau = 0,
  omega = 0,
  sigma = 1,
  replicates = c("exchangeable", "linked"),
  p_missing = 0,
  transform = "none",
  seed = NULL,
  mu_values = NULL
)
```

## Arguments

- n_items:

  Number of items (subjects).

- n_reps:

  Number of replicate measurements of each item by each method (before
  any are removed by `p_missing`).

- methods:

  Character vector of method names (at least two). The first is the
  reference method.

- alpha, beta:

  Intercepts and slopes of each method on the common item scale (see
  details). Scalars are recycled to `length(methods)`.

- mu_mean, mu_sd:

  Mean and SD of the true item values `mu_i`, which are drawn from a
  normal distribution.

- tau:

  SD of the method-by-item interaction `c_mi` on the item scale. A
  scalar (common) or one value per method.

- omega:

  SD of the item-by-replicate effect `a_ir` (common to all methods, on
  the item scale). Only used when `replicates = "linked"`.

- sigma:

  Residual (measurement error) SD of each method, on that method's
  scale. A scalar or one value per method.

- replicates:

  "exchangeable" (default) or "linked". With linked replicates, all
  methods measure an item on the same occasions, which share the effect
  `a_ir`.

- p_missing:

  Probability that each measurement is removed at random. At least one
  measurement of each item by each method is kept.

- transform:

  Transformation (see
  [`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md)).
  If not "none", the data are simulated on the transformed scale and
  back-transformed, so all parameters refer to the transformed scale.

- seed:

  Optional seed. The global random number generator state is restored
  afterwards.

- mu_values:

  Optional numeric vector of length `n_items` with fixed true item
  values `mu_i`, used instead of drawing them (`mu_mean` and `mu_sd` are
  then ignored). Used for the parametric bootstrap in
  [`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md).

## Value

A tibble with columns `id`, `method` (a factor, levels in the order of
`methods`), `repl`, and `y`, with attribute `truth` (see details).

## Details

Measurement `r` of item `i` by method `m` is \$\$y\_{mir} = \alpha_m +
\beta_m (\mu_i + a\_{ir} + c\_{mi}) + e\_{mir},\$\$ with \\\mu_i \sim
N(\mu, \sigma\_\mu^2)\\, \\a\_{ir} \sim N(0, \omega^2)\\ (linked
replicates only), \\c\_{mi} \sim N(0, \tau_m^2)\\, and \\e\_{mir} \sim
N(0, \sigma_m^2)\\ (Carstensen, 2010, eq. 7.1). With all \\\beta_m =
1\\, this is the constant bias model (eq. 5.1/5.2) used by
[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md).

The `truth` attribute holds the parameters and the quantities derived
from them, on the (transformed) analysis scale:

- `params`: the parameters, one row per method, with the y-scale SDs
  `tau_y` (\\\beta_m \tau_m\\) and `omega_y` (\\\beta_m \omega\\).

- `pairs`: for each pair of methods (`method1` - `method2`), the bias at
  `mu_mean`, the SD of the difference between single measurements of the
  same item (on the same occasion, for linked replicates), and the
  corresponding 95% limits of agreement. The bias is constant over items
  only when the slopes are equal (`constant_bias`).

- `repeatability`: for each method, `sigma`, the 95% repeatability
  coefficient \\1.96 \sqrt{2} \sigma_m\\, and, for linked replicates,
  the reproducibility coefficient \\1.96 \sqrt{2} \sqrt{\sigma_m^2 +
  \beta_m^2 \omega^2}\\.

- `conversion`: for each ordered pair (`to`, `from`), the conversion
  intercept, slope, and prediction SD (Carstensen, 2010, section 7.1).

## References

Carstensen, B. (2010). Comparing Clinical Measurement Methods: A
Practical Guide. Wiley.

## See also

[`method_vc()`](https://aaroncaldwell.us/SimplyAgree/reference/method_vc.md),
[`method_convert()`](https://aaroncaldwell.us/SimplyAgree/reference/method_convert.md),
[`mc_long()`](https://aaroncaldwell.us/SimplyAgree/reference/mc_long.md)

## Examples

``` r
# three methods, linked replicates, constant bias
dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
                  alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
                  sigma = c(0.3, 0.5, 0.4), replicates = "linked",
                  seed = 1)
head(dat)
#> # A tibble: 6 × 4
#>      id method  repl      y
#>   <int> <fct>  <int>  <dbl>
#> 1     1 A          1 -0.284
#> 2     1 A          2 -0.229
#> 3     1 A          3 -1.09 
#> 4     1 B          1  1.03 
#> 5     1 B          2  1.73 
#> 6     1 B          3 -0.567
attr(dat, "truth")$pairs
#> # A tibble: 3 × 7
#>   method1 method2  bias    sd lower_loa upper_loa constant_bias
#>   <chr>   <chr>   <dbl> <dbl>     <dbl>     <dbl> <lgl>        
#> 1 A       B        -0.5 0.721    -1.91      0.913 TRUE         
#> 2 A       C         0.2 0.656    -1.09      1.49  TRUE         
#> 3 B       C         0.7 0.768    -0.805     2.21  TRUE         
```
