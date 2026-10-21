# Model-Based Kappa for Ordinal Ratings from Many Raters

**\[experimental\]**

Estimates the model-based kappa \\\kappa_m\\ of Nelson and Edwards
(2015) and Nelson, Mitani and Edwards (2017), and please cite these
papers if this function is utilized. This is a chance-corrected measure
of exact agreement between raters who classify subjects on an ordinal
scale. It is computed from an ordinal (cumulative probit) generalized
linear mixed model with crossed random effects for subjects and raters,
so it describes the populations of raters and subjects that were
sampled, and it is not affected by the prevalence of the categories.

Requires the ordinal package.

## Usage

``` r
ord_kappa_test(
  x = NULL,
  data = NULL,
  measure = NULL,
  item = NULL,
  id = NULL,
  col.names = NULL,
  categories = NULL,
  conf.level = 0.95,
  null.value = NULL,
  alternative = c("two.sided", "greater", "less"),
  group = NULL,
  group_type = c("rater", "subject")
)
```

## Arguments

- x:

  A formula of the form `rating ~ rater | id`. Leave `NULL` to use
  `measure`, `item` and `id` (long data) or `col.names` (wide data).

- data:

  A data frame.

- measure, item, id:

  Names of the columns with the ratings, the raters, and the subjects
  (long data).

- col.names:

  Names of the columns with each rater's ratings (wide data, one row per
  subject).

- categories:

  All categories of the ordinal scale, in order. Defaults to the levels
  of a factor, and otherwise to the sorted observed ratings. Character
  ratings need `categories` to set their order. The number of
  categories, C, enters \\\kappa_m\\ through the chance agreement 1/C.

- conf.level:

  Confidence level of the interval (default 0.95).

- null.value:

  The hypothesized value of \\\kappa_m\\, in \[0, 1). `NULL` (default)
  gives no test. Not used with `group`.

- alternative:

  The alternative hypothesis: `"two.sided"` (default), `"greater"` or
  `"less"`. Without a `null.value` the interval is two-sided.

- group:

  Optional name of a column with a two-level rater or subject
  characteristic (e.g. experienced or inexperienced raters, younger or
  older patients), constant within each rater (or subject). The two
  groups are then compared; see Details. The order of the levels is that
  of a factor, and otherwise sorted. Requires long data.

- group_type:

  Whether `group` describes the `"rater"`s (default) or the
  `"subject"`s.

## Value

A list of class `c("cac_htest", "htest")` containing:

- statistic:

  The z statistic (only with a `null.value`), or with `group`, the
  likelihood ratio statistic.

- p.value:

  The p-value (only with a `null.value` or `group`).

- conf.int:

  The confidence interval for \\\kappa_m\\, or with `group`, for the
  difference between the groups (second minus first).

- estimate:

  The estimated \\\kappa_m\\, or with `group`, one per group.

- stderr:

  The delta-method standard error(s).

- null.value:

  The hypothesized value.

- alternative:

  The alternative hypothesis.

- method:

  A description of the method.

- data.name:

  A description of the data.

- fit:

  The fitted model (`model`), the variance components (`sigma2_subject`,
  `sigma2_rater`), `rho` and its standard error, the thresholds, the
  observed agreement at the fitted thresholds (`p0`), the number of
  categories `C`, the rater conditional modes and their conditional
  variances (`rater_effects`), and the numbers of subjects and raters.
  With `group`, `fit` also has the null model (`null_model`), the
  likelihood ratio test (`lrt`), the fixed effect of the group (`beta`),
  one variance per group, and a table of subgroup kappas with standard
  errors and intervals (`subgroups`).

- data:

  The ratings in long format.

- settings:

  The settings used, for the methods.

## Details

**Model.** The rating of subject i by rater j falls in category c when a
latent variable falls between thresholds \\\alpha\_{c-1}\\ and
\\\alpha_c\\: \$\$P(Y\_{ij} \le c \mid u_i, v_j) = \Phi(\alpha_c - u_i -
v_j),\$\$ with subject effects \\u_i \sim N(0, \sigma^2_u)\\ and rater
effects \\v_j \sim N(0, \sigma^2_v)\\. It is fitted by the Laplace
approximation with
[`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html).

**Kappa.** Observed agreement \\p_0\\ is the probability that two raters
put the same subject in the same category. Chance agreement is taken at
its minimum, \\p_c = 1/C\\, reached when the thresholds split the latent
scale into C equally likely categories, and \\p_0\\ is evaluated at
those thresholds. Then \$\$\kappa_m = \frac{p_0 - 1/C}{1 - 1/C},\$\$
which depends only on C and \\\rho = \sigma^2_u / (\sigma^2_u +
\sigma^2_v + 1)\\, the share of the latent variance due to differences
between subjects. \\\kappa_m\\ is 0 when \\\rho = 0\\ and approaches 1
as \\\rho\\ approaches 1. Because the thresholds drop out, \\\kappa_m\\
does not depend on how common each category is. The observed agreement
at the fitted thresholds is also returned (`fit$p0`).

**Inference.** The standard error is from the delta method with the
covariance matrix of the variance components from the model Hessian
(Nelson et al., 2017). The interval and test are formed on the logit of
\\\rho\\ and mapped back to \\\kappa_m\\, so the interval stays within
\[0, 1\]; Nelson et al. (2017) used the Wald interval \\\hat\kappa_m \pm
1.96\\ se\\, which can be found from `estimate` and `stderr`. The
p-value is from a z test of \\\rho = \rho_0\\, where \\\rho_0\\ gives
the `null.value` of \\\kappa_m\\.

**Design.** Raters and subjects are treated as random samples from their
populations. Missing ratings are allowed. At least three raters are
needed to estimate the rater variance (Nelson et al., 2017); with few
raters or subjects the variance components, and so the interval, are
imprecise.

**Comparing groups.** With `group`, the group enters the model as a
fixed effect (shifting the thresholds, e.g. for a higher prevalence of
severe ratings in older patients), and the rater (or subject) variance
may differ between the two groups (Nelson et al., 2017). The result
gives \\\kappa_m\\ within each group, with an interval for their
difference, and the test is the likelihood ratio test of a common
variance. For rater groups, `fit$subgroups` also gives \\\kappa_m\\
between a rater from each group; chance agreement is then minimised
numerically over common thresholds, and the fixed effect matters.
Subgroup intervals are formed on the logit scale.

Nelson et al. (2017) add a random slope for the group indicator and test
its variance and correlation against a 50:50 mixture of chi-square
distributions with 1 and 2 df. Because the group is constant within each
rater (or subject), only the variance in each group is identified, not
the intercept-slope covariance, and the Hessian of that model is
singular. The equivalent model with a separate variance for each group
is fitted instead: it has the same likelihood and the same kappas, with
one parameter fewer. Equal variances lie inside its parameter space, so
the likelihood ratio statistic is compared with a chi-square
distribution with 1 df.

## References

Nelson, K. P., & Edwards, D. (2015). Measures of agreement between many
raters for ordinal classifications. Statistics in Medicine, 34(23),
3116-3132. [doi:10.1002/sim.6546](https://doi.org/10.1002/sim.6546)

Nelson, K. P., Mitani, A. A., & Edwards, D. (2017). Assessing the
influence of rater and subject characteristics on measures of agreement
for ordinal ratings. Statistics in Medicine, 36(20), 3181-3199.
[doi:10.1002/sim.7323](https://doi.org/10.1002/sim.7323)

## See also

[cac_htest-methods](https://aaroncaldwell.us/SimplyAgree/reference/cac_htest-methods.md)
for [`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html);
[`cac_test()`](https://aaroncaldwell.us/SimplyAgree/reference/cac_test.md)
for Gwet's AC1/AC2, Fleiss' kappa and Krippendorff's alpha.

## Examples

``` r
# \donttest{
if (requireNamespace("ordinal", quietly = TRUE)) {
  # Simulated ratings of 60 subjects by 8 raters on a 5-point scale
  set.seed(42)
  d <- expand.grid(id = 1:60, rater = paste0("R", 1:8))
  latent <- rnorm(60, sd = 1.5)[d$id] + rnorm(8, sd = 0.5)[d$rater] +
    rnorm(nrow(d))
  d$rating <- cut(latent, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE)
  res <- ord_kappa_test(rating ~ rater | id, data = d)
  res
  summary(res)
  plot(res, type = "raters")

  # Do the first 4 raters agree better than the other 4?
  d$exp <- ifelse(d$rater %in% c("R1", "R2", "R3", "R4"), "exp", "inexp")
  res_g <- ord_kappa_test(rating ~ rater | id, data = d, group = "exp")
  res_g
  res_g$fit$subgroups
}
#>       subgroup   kappa_m         se     lower     upper
#> 1          exp 0.2784536 0.03412543 0.2166932 0.3499523
#> 2        inexp 0.2545958 0.03882610 0.1861483 0.3377664
#> 3 exp vs inexp 0.2658583 0.03431575 0.2041858 0.3382413
# }
```
