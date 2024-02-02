SimplyAgree R Package
================

<img src="https://raw.githubusercontent.com/arcaldwell49/SimplyAgree/master/images/sticker.png" width="150" height="150" />

*Artwork courtesy of Chelsea Parlett Pelleriti*

<!-- badges: start -->

[![DOI](https://joss.theoj.org/papers/10.21105/joss.04148/status.svg)](https://doi.org/10.21105/joss.04148)
[![Codecov test
coverage](https://codecov.io/gh/arcaldwell49/SimplyAgree/branch/master/graph/badge.svg)](https://app.codecov.io/gh/arcaldwell49/SimplyAgree?branch=master)
[![R-CMD-check](https://github.com/arcaldwell49/SimplyAgree/workflows/R-CMD-check/badge.svg)](https://github.com/arcaldwell49/SimplyAgree/actions)
[![documentation](https://img.shields.io/badge/website-active-blue)](https://aaroncaldwell.us/SimplyAgree/)
<!-- badges: end -->

Please see the package’s
[website](https://aaroncaldwell.us/SimplyAgree/) for updates, vignettes,
and other details about the package.

# Background

`SimplyAgree` is an R package, and [jamovi](https://www.jamovi.org/)
module, created to make agreement and reliability analyses easier for
the average researcher. The functions within this package include simple
tests of agreement (`agree_test`), agreement analysis for nested
(`agree_nest`) and replicate data (`agree_reps`), and provide robust
analyses of reliability (`reli_stats`). In addition, this package
contains a set of functions to help when planning studies looking to
assess measurement agreement (`blandPowerCurve`).

## Installing SimplyAgree

You can install the most up-to-date version of `SimplyAgree` from
[GitHub](https://github.com/arcaldwell49/SimplyAgree) with:

``` r
devtools::install_github("arcaldwell49/SimplyAgree")
```

# Contributing

We are happy to receive bug reports, suggestions, questions, and (most
of all) contributions to fix problems and add features. Pull Requests
for contributions are encouraged.

Here are some simple ways in which you can contribute (in the increasing
order of commitment):

-   Read and correct any inconsistencies in the documentation
-   Raise issues about bugs or wanted features
-   Review code
-   Add new functionality

## Code of Conduct

Please note that the SimplyAgree project is released with a [Contributor
Code of
Conduct](https://aaroncaldwell.us/SimplyAgree/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

# References

The functions in this package are largely based on the following works:

Lin L (1989). A concordance correlation coefficient to evaluate
reproducibility. *Biometrics* 45: 255 - 268.
<https://doi.org/10.2307/2532051>

Shieh, G. (2019). Assessing agreement between two methods of
quantitative measurements: Exact test procedure and sample size
calculation. *Statistics in Biopharmaceutical Research*, 1-8.
<https://doi.org/10.1080/19466315.2019.1677495>

Parker, R. A., et al (2016). Application of mixed effects limits of
agreement in the presence of multiple sources of variability: exemplar
from the comparison of several devices to measure respiratory rate in
COPD patients. Plos one, 11(12), e0168321.
<https://doi.org/10.1371/journal.pone.0168321>

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman
limits of agreement with multiple observations per individual.
*Statistical methods in medical research*, 22(6), 630-642.
<https://doi.org/10.1177/0962280211402548>

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. *The Journal of Strength
& Conditioning Research*, 19(1), 231-240.

Lu, Meng-Jie, et al (2016). “Sample Size for Assessing Agreement between
Two Methods of Measurement by Bland−Altman Method” *The International
Journal of Biostatistics*, 12(2),
<https://doi.org/10.1515/ijb-2015-0039>

King, TS and Chinchilli, VM. (2001). A generalized concordance
correlation coefficient for continuous and categorical data. *Statistics
in Medicine*, 20, 2131:2147.

King, TS, Chinchilli, VM, and Carrasco, JL. (2007). A repeated measures
concordance correlation coefficient. *Statistics in Medicine*, 26,
3095:3113.

Carrasco, JL, et al. (2013). Estimation of the concordance correlation
coefficient for repeated measures using SAS and R. *Computer Methods and
Programs in Biomedicine*, 109, 293-304.
