
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
coefficients (ICC), measurement error and minimal detectable change,
chance-corrected agreement for categorical ratings (kappa, Gwet’s AC1,
Krippendorff’s alpha), and Deming/Passing-Bablok regression, and plans
sample sizes for agreement and reliability studies. Simple paired data,
replicate measurements, and nested (repeated measures) designs are
supported. Full documentation and vignettes are on the [package
website](https://aaroncaldwell.us/SimplyAgree/).

## Which function should I use?

| Goal | Function | Data |
|----|----|----|
| Bland-Altman limits of agreement | `agreement_limit()` | simple, replicate, or nested |
| Tolerance limits (prediction-oriented agreement) | `tolerance_limit()` | simple, repeated, or nested; optional conditions |
| Concordance correlation coefficient (CCC) | `ccc_test()` | simple, replicate, or nested |
| Non-parametric limits of agreement | `agree_np()` | simple or replicate |
| Reliability: intraclass correlation (ICC) | `icc_test()` | repeated measures (long or wide) |
| Measurement error: SEM, SEE, SEP, coefficient of variation | `reli_err_test()` | repeated measures |
| Minimal detectable change (MDC) | `mdc_test()` | repeated measures |
| Reliability across facets (repeats and occasions) | `reli_facet_test()` | multi-facet designs |
| Categorical agreement: Gwet’s AC1, kappa, Krippendorff’s alpha | `cac_test()` | categorical or ordinal ratings |
| Model-based kappa for many raters | `ord_kappa_test()` | ordinal ratings |
| Deming / Passing-Bablok regression | `dem_reg()`, `pb_reg()` | paired, optionally with replicates |
| Power and sample size for agreement | `power_agreement_limit()`, `power_agreement_exact()`, `blandPowerCurve()`, `agree_expected_half()`, `agree_assurance()` | study planning |
| Power and sample size for reliability | `power_icc()`, `precision_icc()` | study planning |
| Power and sample size for Deming regression | `deming_sample_size()` | study planning |

Some older functions still work but are superseded: use
`agreement_limit()` instead of `agree_test()`, `agree_reps()`, and
`agree_nest()`; `icc_test()` and `reli_err_test()` instead of
`reli_stats()` and `reli_aov()`; and `cac_test()` instead of
`agree_coef()`. `loa_lme()` is deprecated in favor of
`tolerance_limit(model = "lme")`, and `loa_mixed()` is defunct. The
`jmv*()` functions exist for the jamovi module and are not intended for
use in R.

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
- Supports simple, replicate and nested designs
- Handles design effects for clustered data
- Uses the MOVER method of Zou (2013) by default, or the classic
  Bland-Altman (1999) intervals (`loa_calc`)

Exact procedures based on Shieh (2019) and Jan & Shieh (2018) are
available in `agree_test()` and the power functions.

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

Tolerance intervals are most appropriate when making simultaneous
inferences about pairs of percentile limits or when you need
prediction-oriented intervals for future measurements.

## Additional Features

Beyond the two core functions, `SimplyAgree` provides:

- **Concordance**: `ccc_test()` for the concordance correlation
  coefficient with simple, replicate and nested data (with an optional
  subject bootstrap, `boot = TRUE`, recommended for replicate data with
  few subjects)
- **Reliability Analysis**: `icc_test()` (intraclass correlations;
  replaces the superseded `reli_stats()` and `reli_aov()`),
  `reli_err_test()` (SEM, SEE, SEP and coefficients of variation) and
  `mdc_test()` (minimal detectable change)
- **Categorical Agreement**: `cac_test()` for chance-corrected agreement
  coefficients (Gwet’s AC1, kappa, Krippendorff’s alpha and more) and
  `ord_kappa_test()` for model-based kappa with ordinal ratings and many
  raters
- **Power Analysis**: `power_agreement_exact()` and related functions
  for sample size determination in agreement studies, and `power_icc()`
  / `precision_icc()` for reliability studies
- **Error-in-Variables**: supports “Deming” and “Passing-Bablok”
  regression methods for method comparison
- **Visualization**: Built-in plotting capabilities for Bland-Altman
  plots and related visualizations
- **Flexible Design Support**: Handles simple, nested, and repeated
  measures designs with appropriate variance adjustments

## Installing SimplyAgree

You can install the released version of `SimplyAgree` from
[CRAN](https://cran.r-project.org/package=SimplyAgree) with:

``` r
install.packages("SimplyAgree")
```

Or the most up-to-date version from
[GitHub](https://github.com/arcaldwell49/SimplyAgree) with:

``` r
devtools::install_github("arcaldwell49/SimplyAgree")
```

## Quick Start Example

``` r
library(SimplyAgree)

# Example data: rectal (trec) and esophageal (teso) temperatures,
# several trials per subject
data(temps)

# Limits of agreement analysis
agreement_limit(x = "trec_pre",
                y = "teso_pre",
                id = "id",
                data = temps,
                data_type = "nest",
                agree.level = 0.95)
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
# Tolerance interval analysis
tolerance_limit(x = "trec_pre",
                y = "teso_pre",
                id = "id",
                data = temps,
                tol_level = 0.95)
```

    ## Agreement between Measures (Difference: x-y)
    ## 95% CI for Bias; 95% Prediction Interval
    ## Tolerance Limits: at least 95% of differences with 95% confidence
    ## Model: GLS, compound symmetry within id
    ## 
    ##    Bias          Bias CI Prediction Interval  Tolerance Limits
    ##  0.1908 [0.1191, 0.2625]   [-0.2098, 0.5915] [-0.2532, 0.6349]
    ## 
    ## SD of Differences = 0.1743
    ## Between-Subject SD = 0.0773; Within-Subject SD = 0.1562

``` r
# Concordance correlation coefficient
ccc_test(x = "trec_pre",
         y = "teso_pre",
         id = "id",
         data = temps,
         data_type = "nest")
```

    ## 
    ##  Concordance Correlation Coefficient (nested pairs)
    ## 
    ## data:  trec_pre and teso_pre
    ## 
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  0.3216463 0.7053779
    ## sample estimates:
    ##       CCC 
    ## 0.5410955

``` r
# Reliability analysis: Shrout & Fleiss (1979), 6 subjects rated by 4 judges
sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6), J2 = c(2, 1, 4, 1, 5, 2),
                 J3 = c(5, 3, 6, 2, 6, 4), J4 = c(8, 2, 8, 6, 9, 7))
icc_test(col.names = c("J1", "J2", "J3", "J4"), data = sf)
```

    ## 
    ##  Two-way, consistency, single-measure ICC [Shrout & Fleiss ICC(3,1)]
    ## 
    ## data:  J1, J2, J3, J4
    ## 
    ## alternative hypothesis: two.sided
    ## 95 percent confidence interval:
    ##  0.3424648 0.9458583
    ## sample estimates:
    ##  ICC(C,1) 
    ## 0.7148407

Most results have `plot()` methods (Bland-Altman and line-of-identity
plots) and `check()` methods for assumption diagnostics.

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

## Development and use of AI

`SimplyAgree` is developed and maintained by a single developer.
Generative AI tools, primarily Anthropic’s Claude Opus models, allow me
to maintain the package more thoroughly and respond to users more
quickly than I could alone. I use AI tools in four ways:

- **Testing:** stress testing code to identify bugs and edge cases.
- **Issue triage:** a first-pass evaluation of reported issues before I
  investigate them myself.
- **Simulation:** writing and running simulations to examine performance
  under specific conditions.
- **Documentation:** drafting user-facing documentation from the source
  code, which I then check for accuracy and edit.

The statistical methods, package design, and architectural decisions are
my own. I review every AI-assisted code change, and all changes must
pass the package’s test suite before release. I am responsible for the
correctness of this package. If you find an error, please [open an
issue](https://github.com/arcaldwell49/SimplyAgree/issues).

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

Carrasco, J. L., & Jover, L. (2003). Estimating the generalized
concordance correlation coefficient through variance components.
*Biometrics*, 59(4), 849-858.
<https://doi.org/10.1111/j.0006-341x.2003.00099.x>

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

Gwet, K. L. (2014). *Handbook of Inter-Rater Reliability* (4th ed.).
Advanced Analytics, LLC.

Jan, S. L., & Shieh, G. (2018). The Bland-Altman range of agreement:
Exact interval procedure and sample size determination. *Computers in
Biology and Medicine*, 100, 247-252.
<https://doi.org/10.1016/j.compbiomed.2018.06.020>

Lin, L. I. (1989). A concordance correlation coefficient to evaluate
reproducibility. *Biometrics*, 45(1), 255-268.
<https://doi.org/10.2307/2532051>

Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between
Two Methods of Measurement by Bland-Altman Method. *The International
Journal of Biostatistics*, 12(2).
<https://doi.org/10.1515/ijb-2015-0039>

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. *Psychological Methods*, 1(1),
30-46. <https://doi.org/10.1037/1082-989X.1.1.30>

Parker, R. A., et al. (2016). Application of mixed effects limits of
agreement in the presence of multiple sources of variability: exemplar
from the comparison of several devices to measure respiratory rate in
COPD patients. *PloS One*, 11(12), e0168321.
<https://doi.org/10.1371/journal.pone.0168321>

Shieh, G. (2019). Assessing agreement between two methods of
quantitative measurements: Exact test procedure and sample size
calculation. *Statistics in Biopharmaceutical Research*, 12(3), 352-359.
<https://doi.org/10.1080/19466315.2019.1677495>

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. *Psychological Bulletin*, 86(2), 420-428.
<https://doi.org/10.1037/0033-2909.86.2.420>

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. *The Journal of Strength
& Conditioning Research*, 19(1), 231-240.
<https://doi.org/10.1519/15184.1>

Zou, G. Y. (2013). Confidence interval estimation for the Bland-Altman
limits of agreement with multiple observations per individual.
*Statistical Methods in Medical Research*, 22(6), 630-642.
<https://doi.org/10.1177/0962280211402548>
