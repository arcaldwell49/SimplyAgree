## Submission

This is an update from 0.3.0 (the current CRAN version) to 0.4.1. Its main changes:

- New functions for reliability (`icc_test()`, `reli_err_test()`, `mdc_test()`, `reli_facet_test()`, `power_icc()`, `precision_icc()`), categorical agreement (`cac_test()`, `ord_kappa_test()`), the concordance correlation coefficient (`ccc_test()`), and agreement power (`power_agreement_limit()`).
- Several statistical corrections that change results (described in NEWS.md).
- `agree_np()` no longer hangs on data with many ties. Its bootstrap fits each resample by Frisch-Newton instead of quantreg's simplex, which could cycle without end.

The test suite has been trimmed for CRAN: slow simulation and bootstrap checks are skipped with `skip_on_cran()`.

## Test environments

- local macOS, R 4.5.1
- win-builder: R-devel and R-release
- GitHub Actions: Ubuntu, macOS and Windows (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes on win-builder and GitHub Actions.



## Reverse dependencies

There are no reverse dependencies on CRAN.
