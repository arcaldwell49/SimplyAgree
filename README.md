
# SimplyAgree <a href="https://aaroncaldwell.us/SimplyAgree/"><img src="man/figures/logo.png" align="right" height="118" alt="SimplyAgree website" /></a>

<!-- badges: start -->

[![DOI](https://joss.theoj.org/papers/10.21105/joss.04148/status.svg)](https://doi.org/10.21105/joss.04148)
[![Codecov test
coverage](https://codecov.io/gh/arcaldwell49/SimplyAgree/branch/master/graph/badge.svg)](https://app.codecov.io/gh/arcaldwell49/SimplyAgree?branch=master)
[![R-CMD-check](https://github.com/arcaldwell49/SimplyAgree/workflows/R-CMD-check/badge.svg)](https://github.com/arcaldwell49/SimplyAgree/actions)
[![documentation](https://img.shields.io/badge/website-active-blue)](https://aaroncaldwell.us/SimplyAgree/)
<!-- badges: end -->

`SimplyAgree` is an R package and [jamovi](https://www.jamovi.org/)
module for method comparison, agreement, and reliability studies. It
estimates Bland-Altman limits of agreement, tolerance limits,
concordance correlation coefficients (CCC), intraclass correlation
coefficients (ICC), and Deming/Passing-Bablok regression, and plans
sample sizes for agreement studies. Simple paired data, replicate
measurements, and nested (repeated measures) designs are supported. Full
documentation and vignettes are on the [package
website](https://aaroncaldwell.us/SimplyAgree/).

## Which function should I use?

| Goal | Function | Data |
|----|----|----|
| Bland-Altman limits of agreement | `agreement_limit()` | simple, replicate, or nested |
| Tolerance limits (prediction-oriented agreement) | `tolerance_limit()` | simple, repeated, or nested; optional conditions |
| Concordance correlation coefficient (CCC) | `ccc_test()` | simple, replicate, or nested |
| Non-parametric limits of agreement | `agree_np()` | simple or replicate |
| Reliability: ICC, SEM, coefficient of variation | `reli_stats()`, `reli_aov()` | repeated measures (long or wide) |
| Deming / Passing-Bablok regression | `dem_reg()`, `pb_reg()` | paired, optionally with replicates |
| Power and sample size | `power_agreement_exact()`, `blandPowerCurve()`, `agree_expected_half()`, `agree_assurance()`, `deming_sample_size()` | study planning |

`agree_test()`, `agree_reps()`, and `agree_nest()` still work but are
superseded by `agreement_limit()`. The `jmv*()` functions exist for the
jamovi module and are not intended for use in R. `loa_mixed()` is
defunct; use `loa_lme()` for mixed-model limits of agreement.

# Background

`SimplyAgree` is an R package, and [jamovi](https://www.jamovi.org/)
module, designed to simplify agreement and reliability analyses for
researchers. The package implements rigorous statistical methods for
method comparison studies, providing both classical and modern
approaches to assessing measurement agreement.

## Core Functionality

The package provides two primary approaches for assessing agreement
between measurement methods:

### 1. Limits of Agreement: `agreement_limit()`

The `agreement_limit()` function implements Bland-Altman style limits of
agreement analysis. This approach:

- Computes confidence intervals for the range of agreement between two
  methods
- Supports both simple and nested/repeated measures designs
- Implements exact procedures based on Shieh (2019) and Jan & Shieh
  (2018)
- Handles design effects for clustered data
- Provides multiple interval estimation methods (exact, MOVER, Zou’s
  method)

This is the preferred function when you want to describe what proportion
of differences fall within specified bounds or when making inferences
about the central region of the paired-difference distribution.

### 2. Tolerance Intervals: `tolerance_limit()`

The `tolerance_limit()` function creates statistical tolerance intervals
that:

- Construct intervals expected to contain a specified proportion of
  future observations
- Implement exact equal-tailed tolerance intervals
- Support confidence intervals for ranges of percentiles
- Handle both simple and complex study designs
- Provide sample size and power calculations

Tolerance intervals are most appropriate when making simultaneous
inferences about pairs of percentile limits or when you need
prediction-oriented intervals for future measurements.

## Additional Features

Beyond the two core functions, `SimplyAgree` provides:

- **Reliability Analysis**: `reli_stats()` and `reli_aov()` functions
  for comprehensive reliability assessment
- **Power Analysis**: `power_agreement_exact()` and related functions
  for sample size determination in agreement studies
- **Error-in-Variables**: supports “Deming” and “Passing-Bablok”
  regression methods for method comparison
- **Visualization**: Built-in plotting capabilities for Bland-Altman
  plots and related visualizations
- **Flexible Design Support**: Handles simple, nested, and repeated
  measures designs with appropriate variance adjustments

## Installing SimplyAgree

Install the released version from CRAN:

``` r
install.packages("SimplyAgree")
```

Or the development version from
[GitHub](https://github.com/arcaldwell49/SimplyAgree):

``` r
devtools::install_github("arcaldwell49/SimplyAgree")
```

## Quick Start Example

The `temps` data set has rectal (`trec_pre`) and esophageal (`teso_pre`)
temperatures measured on the same participants across several trials
(`id` identifies the participant).

``` r
library(SimplyAgree)
data(temps)

# Bland-Altman limits of agreement for nested (repeated measures) data
agreement_limit(x = "trec_pre", y = "teso_pre", id = "id",
                data = temps, data_type = "nest")
```

    ## MOVER Limits of Agreement (LoA)
    ## 95% LoA @ 5% Alpha-Level
    ## Nested Data
    ## 
    ##    Bias          Bias CI Lower LoA Upper LoA            LoA CI
    ##  0.1908 [0.1191, 0.2625]   -0.1507    0.5324 [-0.2307, 0.6124]
    ## 
    ## LoA CI: one-sided 95% bounds (outer ends of 90% CIs) for an intersection-union test against a maximal allowable difference;
    ##   not a joint 95% interval for the LoA
    ## SD of Differences = 0.1743

``` r
# Tolerance limits, allowing the differences to vary by time of day
tolerance_limit(data = temps, x = "trec_pre", y = "teso_pre",
                id = "id", condition = "tod")
```

    ## Agreement between Measures (Difference: x-y)
    ## 95% CI for Bias; 95% Prediction Interval
    ## Tolerance Limits: at least 95% of differences with 95% confidence
    ## Model: GLS, compound symmetry within id; residual SD by condition
    ## 
    ##  Condition   Bias          Bias CI     SD Prediction Interval  Tolerance Limits
    ##         AM 0.1537 [0.0595, 0.2478] 0.1878   [-0.2919, 0.5993] [-0.3674, 0.6748]
    ##         PM 0.2280 [0.1342, 0.3218] 0.1520     [-0.216, 0.672] [-0.1938, 0.6498]

``` r
# Concordance correlation coefficient
ccc_test(x = "trec_pre", y = "teso_pre", id = "id",
         data = temps, data_type = "nest")
```

    ## 
    ##  Concordance Correlation Coefficient (U-statistics)
    ## 
    ## data:  trec_pre and teso_pre
    ## Z = 8.8359, n = 10, p-value < 2.2e-16
    ## alternative hypothesis: true CCC is  0
    ## 95 percent confidence interval:
    ##  0.4392869 0.6291813
    ## sample estimates:
    ##       CCC 
    ## 0.5410955

The `agreement_limit()` and `tolerance_limit()` results have `plot()`
methods for Bland-Altman plots and `check()` methods for assumption
checks.

# Contributing

We are happy to receive bug reports, suggestions, questions, and (most
of all) contributions to fix problems and add features. Pull Requests
for contributions are encouraged.

Here are some simple ways in which you can contribute (in the increasing
order of commitment):

- Read and correct any inconsistencies in the documentation
- Raise issues about bugs or wanted features
- Review code
- Add new functionality

## Code of Conduct

Please note that the SimplyAgree project is released with a [Contributor
Code of
Conduct](https://aaroncaldwell.us/SimplyAgree/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Acknowledgements

Logo artwork courtesy of Chelsea Parlett Pelleriti.

# References

The functions in this package are largely based on the following works:

Bland, J. M., & Altman, D. G. (1986). Statistical methods for assessing
agreement between two methods of clinical measurement. *The Lancet*,
327(8476), 307-310. <https://doi.org/10.1016/S0140-6736(86)90837-8>

Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
comparison studies. *Statistical Methods in Medical Research*, 8(2),
135-160. <https://doi.org/10.1177/096228029900800204>

Carrasco, JL, et al. (2013). Estimation of the concordance correlation
coefficient for repeated measures using SAS and R. *Computer Methods and
Programs in Biomedicine*, 109, 293-304.
<https://doi.org/10.1016/j.cmpb.2012.09.002>

Francq, B. G., Berger, M., & Boachie, C. (2020). To tolerate or to
agree: A tutorial on tolerance intervals in method comparison studies
with BivRegBLS R Package. *Statistics in Medicine*, 39(28), 4334-4349.
<https://doi.org/10.1002/sim.8709>

Francq, B. G., Lin, D., & Hoyer, W. (2019). Confidence, prediction, and
tolerance in linear mixed models. *Statistics in Medicine*, 38(30),
5603-5622. <https://doi.org/10.1002/sim.8386>

Jan, S. L., & Shieh, G. (2018). The Bland-Altman range of agreement:
Exact interval procedure and sample size determination. *Computers in
Biology and Medicine*, 100, 247-252.
<https://doi.org/10.1016/j.compbiomed.2018.06.020>

King, TS and Chinchilli, VM. (2001). A generalized concordance
correlation coefficient for continuous and categorical data. *Statistics
in Medicine*, 20, 2131-2147. <https://doi.org/10.1002/sim.845>

King, TS, Chinchilli, VM, and Carrasco, JL. (2007). A repeated measures
concordance correlation coefficient. *Statistics in Medicine*, 26,
3095-3113. <https://doi.org/10.1002/sim.2778>

Lin, L. I. (1989). A concordance correlation coefficient to evaluate
reproducibility. *Biometrics*, 45(1), 255-268.
<https://doi.org/10.2307/2532051>

Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between
Two Methods of Measurement by Bland-Altman Method. *The International
Journal of Biostatistics*, 12(2).
<https://doi.org/10.1515/ijb-2015-0039>

Parker, R. A., et al. (2016). Application of mixed effects limits of
agreement in the presence of multiple sources of variability: exemplar
from the comparison of several devices to measure respiratory rate in
COPD patients. *PloS One*, 11(12), e0168321.
<https://doi.org/10.1371/journal.pone.0168321>

Shieh, G. (2019). Assessing agreement between two methods of
quantitative measurements: Exact test procedure and sample size
calculation. *Statistics in Biopharmaceutical Research*, 12(3), 352-359.
<https://doi.org/10.1080/19466315.2019.1677495>

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. *The Journal of Strength
& Conditioning Research*, 19(1), 231-240.
<https://doi.org/10.1519/15184.1>

Zou, G. Y. (2013). Confidence interval estimation for the Bland-Altman
limits of agreement with multiple observations per individual.
*Statistical Methods in Medical Research*, 22(6), 630-642.
<https://doi.org/10.1177/0962280211402548>
