# TODO

Open items noticed during development. Each entry says what was seen, why
it may matter, and how to check it.

## `calc_loa_nest()`: variance decomposition in the LoA bounds

Noticed 2026-10-31 while adding `tdi()`. **Confirmed by simulation**: the default MOVER bounds covered 0.85-0.94 instead of 0.95, and Zou's decomposition covers 0.94-0.97. Being fixed against v0.4.0 on branch `fix-loa-nest-bounds` (off `master`; study in `references/general/loa_nest_sims/` there). Remove this entry once that is merged here.

**What.** In `R/agreement_limit.R`, `calc_loa_nest()` (data_type = "nest")
splits the variance of a single difference, `total = between + within`
(from `lmer(delta ~ 1 + (1 | id))`), into two pieces for the MOVER bound and
for the Bland-Altman `var_loa`:

- `between_variance` on `n_sub - 1` df
- `(1 - 1/mh) * within_variance` on `n_obs - n_sub` df

The quantity that has `n_sub - 1` df is the variance of the subject mean
differences, `between + within/mh`, not `between` alone. So `within/mh`
appears in neither piece: the pieces add to `between + (1 - 1/mh) within`,
not to `total`, and the SE of the bias (`between_variance * infl / n_sub`)
also leaves out `within/mh`.

**Why it may matter.** Both the bias SE and the uncertainty added to the
limits are too small when the within-subject variance is large relative to
the between-subject variance and subjects have few pairs. The confidence
bounds of the limits could then be liberal. With large `between_variance`
the difference is small.

**What the other functions do.**

- `calc_loa_reps()` uses `d_var`, the variance of the subject mean
  differences, on `n_sub - 1` df (consistent).
- `tolerance_limit()` with `id` and compound symmetry (`sd_bound_info()`)
  uses the share `rho + (1 - rho)/mh`, i.e. `between + within/mh`
  (consistent). It gave the same SD as `data_type = "nest"` in the `tdi()`
  checks, so its bounds are a direct comparison.
- `tdi()` computes `df_sd` and its variance components for nest data from
  `between + within/mh` and `(1 - 1/mh) within`.

**How to check.**

1. Compare `lower_loa_ci` / `upper_loa_ci` of
   `agreement_limit(data_type = "nest")` with the IU bounds of
   `tolerance_limit(id = , cor_type = "sym", bound_type = "iu")` on the same
   data, for small and large between/within ratios.
2. Simulate coverage of the LoA confidence bounds (nested data, 10-40
   subjects, 2-5 pairs each, between/within ratio from 0.1 to 10) with the
   current and the corrected decomposition. `references/general/tdi_sims/`
   has a nested-data generator to start from.
3. If the corrected version covers better, change `calc_loa_nest()` (and its
   `var_loa`), update the vignette's "Nested" calculation steps, and add a
   NEWS entry.

## Planned features

- `bls_reg()` (renamed from `cbls_reg()`): done, including tolerance limits, the simultaneous band, variance functions, per-subject variances, the log scale, `error_ratio_profile()` and CBLS in `method_convert()`. What is left is under "Still open" and "Gaps against BivRegBLS and MethComp" in `bls_reg_roadmap.md`.
- Within-subject (trending) agreement for 0.5.0: the within-subject spread of the average and a within-subject residual plot, 4-quadrant/polar options in `plot()` for `loa`, `tolerance_delta` and `simple_mvc`, and documentation (change LoA via `mdc_test()`, the non-unit slope limitation, the `dem_reg(id)`/`pb_reg(id)` assumption). Plan, evidence and open questions are in `trending_agreement_plan.md`; check scripts are in `trending_checks/`. Not started.

## Next steps (handoff, 2026-11-01)

State of branch `MethComp`: everything up to `bls_reg()` (rename),
`error_ratio_profile()`, `linked_reps_test()`, per-subject variances and
`transform = "log"` in `bls_reg()` is implemented, documented (NEWS,
pkgdown, Deming and Part 1 vignettes) and tested. Work through these in
order:

**Unresolved before step 1: slow `R CMD check` tests (2026-11-01).** After
the `bls_reg()` rename, `devtools::check()` passed everything up to the
tests (install, vignettes, examples with and without `--run-donttest` all
OK), but the tests step ran for over 30 minutes without finishing (the
whole check took 8m 41s on the previous round) and was stopped. The test
process was busy, not hung, and no other R process was running. The
filtered tests (`bls_reg|error_ratio|method_convert|linked_reps|eiv|tdi`,
649 expectations) all pass, and `linked_reps_test`'s tests take about 12 s,
so the slow file is probably elsewhere. Possibly related: that run reported
`covr` as not installed, which earlier runs did not, so it may have used a
different library. To find it, time each test file with `NOT_CRAN=true`,
e.g. a loop over `list.files("tests/testthat", "^test-")` calling
`testthat::test_file(f, reporter = "silent", package = "SimplyAgree")` and
recording the elapsed time. Then run `devtools::check()` before the
simulation.

Lead: timing each file (same R) gave under 20 s for every file from
`test-agree_coef.R` to `test-cac_test.R`, then `test-call_args.R` ran for
several minutes without finishing. That file was not changed in this work;
it renders ggplots to `grDevices::pdf(NULL)`. The run used R 4.5.3 (the
newest R under `C:/Program Files/R`, picked automatically), whose library
lacks `covr`, so suspect the environment (R version, graphics device or
package versions) before the code. Run `test-call_args.R` alone first.

1. **Run the `linked_reps_test()` simulation** on a machine with many
   cores: `references/general/linked_reps_sims/README.md` (question,
   criteria, how to run, about 22 core-hours). Then write up the results
   there and add the size and power to NEWS, the `linked_reps_test()` docs
   and the Part 1 vignette.
2. **Random raters in `method_vc()`** (gap 5 in `bls_reg_roadmap.md`, which
   lists the design questions). Proposed design, not yet agreed with the
   maintainer; confirm before starting:
   - scope: `method_vc()` only (not `method_convert()` for now);
   - model: methods as a random effect with crossed method-by-item and
     item-by-replicate effects and a common residual SD, fitted with lme4
     (already imported), since nlme does not do crossed effects easily;
   - output: a rater SD in `varcomp`, and one limits row for two randomly
     chosen raters (bias 0, SD sqrt(2 (sigma_rater^2 + tau^2 + sigma^2)),
     plus 2 omega^2 with `include_omega`) instead of one row per pair;
   - bounds: MOVER with the rater variance on M - 1 df by default, an
     optional parametric bootstrap, and a warning with fewer than about 5
     raters;
   - downstream: print/summary/plot/check/`tdi()` for `simple_mvc`; a
     coverage simulation like `method_vc()`'s.
3. **After the `fix-loa-nest-bounds` PR is merged into master**: merge
   master into this branch, resolve the `NEWS.md` and `calc_loa_nest()`
   conflicts (take master's fix), and remove the `calc_loa_nest()` TODO
   comment and the entry at the top of this file.
4. Lower priority (in `bls_reg_roadmap.md`): linked replicates for
   `method_convert(estimator = "cbls")`; variance functions in
   `method_convert()`; method-specific equation error with three or more
   methods; the per-ratio ellipses and an MD-plot version of
   `error_ratio_profile()`.

Environment notes:

- Suggests needed for the full tests and vignettes: BivRegBLS, MethComp,
  TDIagree, lavaan, ordinal, deming, pbkrtest (and covr for coverage).
- `devtools::check()` takes about 9 minutes with the slow tests
  (`NOT_CRAN` set by devtools). On Windows outside RStudio, set
  `RSTUDIO_PANDOC` to RStudio's pandoc folder (e.g.
  `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`) or the
  vignettes fail with "Pandoc is required".
- The `docs/` folder has pkgdown changes from before this work; do not
  include them in commits.
