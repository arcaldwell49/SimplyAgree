# CLAUDE.md - SimplyAgree Package Guide

## Package Overview
**SimplyAgree** (v0.3.0) - Flexible and Robust Agreement and Reliability Analyses

An R package for method comparison studies implementing Bland-Altman limits of agreement, tolerance intervals, concordance correlation coefficients, and reliability statistics (ICCs). Supports simple paired data, repeated measures, and nested data structures.

- CRAN: https://cran.r-project.org/package=SimplyAgree
- Docs: https://aaroncaldwell.us/SimplyAgree/
- License: GPL-3

## Development Commands
```r
library(devtools)
devtools::load_all()    # Load package for interactive dev
devtools::document()    # Generate docs from roxygen2
devtools::test()        # Run testthat tests
devtools::check()       # Full R CMD check (must pass with 0 errors/warnings/notes)
devtools::build()       # Build package tarball
devtools::install()     # Install locally
```

No Makefile. RStudio shortcuts: Ctrl+Shift+L (load), Ctrl+Shift+D (document), Ctrl+Shift+T (test).

## Code Style
- **Tidyverse-style** coding (pipes, dplyr verbs)
- **Naming**: snake_case for functions (`agree_test`, `reli_stats`, `ccc_test`)
- **Parameters**: Use `conf.level`, `agree.level` (with dots, matching base R)
- **Roxygen2**: Markdown enabled (`Roxygen: list(markdown = TRUE)` in DESCRIPTION)
- **Lifecycle badges**: Use `lifecycle::badge()` for function status

### Function Patterns
- Use `match.arg()` for character arguments with choices
- Validate inputs early with informative `stop()` messages
- Store call with `match.call()` for reproducibility
- Return named lists with `structure(..., class = "classname")`

### Base R Class Conventions
- **htest class**: Use for hypothesis tests (see `ccc_test.R` for example)
  - Required elements: `statistic`, `parameter`, `p.value`, `conf.int`, `estimate`, `null.value`, `alternative`, `method`, `data.name`
- **Custom classes**: `simple_agree`, `simple_reli`, `simple_eiv`, `loa`, `loa_mermod`, `tolerance_delta`

## S3 Methods
Methods are organized in `R/methods.*.R` files:

| Class | print | plot | check | Other |
|-------|-------|------|-------|-------|
| simple_agree | Yes | Yes (ggplot2) | Yes | - |
| simple_reli | Yes | Yes (ggplot2) | Yes | - |
| simple_eiv | Yes | Yes | Yes | coef, confint, fitted, predict, residuals, summary, vcov |
| loa, loa_mermod | Yes | Yes (ggplot2) | Yes | - |
| tolerance_delta | Yes | Yes (ggplot2) | Yes | - |
| htest | Inherited from stats | - | - | - |

### Plot Methods
- All use **ggplot2** for visualization
- Bland-Altman plots: `type = 1` (default)
- Line-of-identity plots: `type = 2`
- Support `geom` parameter for density options
- Optional smoothing via `smooth_method`

### Check Methods
- Generic defined: `check <- function(x, ...) UseMethod("check")`
- Returns diagnostic plots (normality, heteroskedasticity, proportional bias)
- Uses `patchwork::wrap_plots()` for multi-panel output

## Package Structure
```
R/
├── agreement_limit.R      # Main LoA function (recommended)
├── agree_test.R           # Simple agreement (superseded)
├── agree_reps.R           # Repeated measures agreement
├── agree_nest.R           # Nested data agreement
├── agree_np.R             # Non-parametric agreement
├── agree_coef.R           # Agreement coefficients
├── ccc_test.R             # CCC hypothesis test (htest)
├── reli_stats.R           # Reliability (ICC, SEM, CV)
├── tolerance_limit.R      # Tolerance intervals
├── dem_reg.R              # Deming regression
├── pb_reg.R               # Passing-Bablok regression
├── loa_lme.R              # Mixed-effects LoA
├── power_*.R              # Power/sample size functions
├── methods.*.R            # S3 methods by class
├── *_functions.R          # Internal helpers
└── jmv*.R                 # jamovi interface

tests/testthat/
├── test-*.R               # One file per function/feature

vignettes/
├── agreement_analysis.Rmd
├── reliability_analysis.Rmd
├── power_sample_size_vignette.Rmd
├── Deming.Rmd
└── agree_tests.Rmd
```

## Testing
**Framework**: testthat (edition 3)

```r
devtools::test()                    # Run all tests
devtools::test(filter = "ccc")      # Run specific test file
covr::package_coverage()            # Check coverage
```

### Testing Standards
- **Target**: 100% coverage (minimum 90%)
- **Tolerance**: Use tight tolerances for published results: `expect_equal(..., tolerance = 1e-5)`
- **Validation**: Test against known published values where available
- **Structure**: One `test_that()` per behavior, use `context()` for grouping

### Test Patterns
```r
test_that("function returns correct class", {
  result <- my_function(x, y)
  expect_s3_class(result, "expected_class")
})

test_that("results match published values", {
  result <- my_function(x, y)
  expect_equal(result$estimate, 0.95, tolerance = 1e-5)
})

test_that("input validation works", {
  expect_error(my_function(x), "error message pattern")
})
```

## Documentation
**Roxygen2 with markdown** - Do not edit `man/` or `NAMESPACE` directly.

### Standard Tags
```r
#' @title Short Title
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Longer description...
#'
#' @param x Description of x
#' @param conf.level Confidence level (default 0.95)
#'
#' @return A list with class \code{"classname"} containing:
#'   \item{estimate}{The point estimate}
#'   \item{conf.int}{Confidence interval}
#'
#' @examples
#' data("reps")
#' result <- my_function(x = reps$x, y = reps$y)
#'
#' @references
#' Author (Year). Title. Journal. \doi{10.xxxx/xxxxx}
#'
#' @seealso \code{\link{related_function}}
#' @export
```

### Family Tags
Not extensively used, but consider for grouping related functions.

## Dependencies

### Imports (Core)
- **ggplot2**: All plotting
- **lme4**: Mixed models (`lmer`)
- **nlme**: GLS models (`gls`, `lme`)
- **emmeans**: Marginal means for CIs
- **boot**: Bootstrap methods
- **dplyr/tidyr**: Data manipulation
- **patchwork**: Combining plots

### Suggests (Optional)
- **testthat**: Testing
- **covr**: Coverage
- **knitr/rmarkdown**: Vignettes
- **deming**: Reference implementation
- **pbkrtest**: Kenward-Roger df

### Key Internal Uses
- `lme4::lmer()` for mixed-effects reliability models
- `nlme::gls()` for heterogeneous variance models
- `boot::boot()` for bootstrap CIs
- `emmeans::ref_grid()` for LoA confidence intervals

## References

### Directory Structure
- `/references/general/` - Persistent documentation (tracked in git)
- `/references/working/` - Branch-specific papers/notes (gitignored)

**Check `/references/working/` first** when implementing new methods for source material and methodology papers.

## Agreement/Reliability Context

### Core Concepts
- **Agreement** (absolute): Do methods give the same values? (Bland-Altman LoA)
- **Reliability** (relative): Do methods rank subjects consistently? (ICC)
- **Concordance**: Combined precision + accuracy (CCC)

### Key Methods
| Method | Function | Data Type |
|--------|----------|-----------|
| Bland-Altman LoA | `agreement_limit()` | Simple, repeated, nested |
| Tolerance Intervals | `tolerance_limit()` | Simple, repeated, nested |
| CCC | `ccc_test()` | Simple, repeated, nested |
| ICC | `reli_stats()`, `reli_aov()` | Repeated measures |
| Deming Regression | `dem_reg()` | Paired |
| Passing-Bablok | `pb_reg()` | Paired |

### Data Structures
- **Simple**: One observation per subject, two methods
- **Reps**: Multiple replicate measurements, true value constant
- **Nest**: Nested design, true value may vary

### Function Lifecycle
- `agreement_limit()` - **Recommended** for new code
- `agree_test()` - **Superseded** (works but not actively developed)
- `loa_lme()` - Current; replaces deprecated `loa_mixed()`
