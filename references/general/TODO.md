# TODO

Open items noticed during development. Each entry says what was seen, why
it may matter, and how to check it.

## Planned features

- `bls_reg()` (renamed from `cbls_reg()`): done, including tolerance limits, the simultaneous band, variance functions, per-subject variances, the log scale, `error_ratio_profile()` and CBLS in `method_convert()`. What is left is under "Still open" and "Gaps against BivRegBLS and MethComp" in `bls_reg_roadmap.md`.
- Within-subject (trending) agreement for 0.5.0: plan, decisions (2026-11-02) and evidence in `trending_agreement_plan.md`; check scripts in `trending_checks/`. Done: (a) the within-item reliability in `simple_mvc` output (logit delta-method CIs; coverage in `method_vc_reliability_sims/`) and a `print()` note for a residual SD at zero (a warning would fire in up to 30% of good fits); (b) `plot(type = "quadrant"/"polar")` for `simple_mvc` (linked), `tolerance_delta` (id) and `loa` (nest), in `R/trend_plot_functions.R`, with `exclusion`, `relative`, `expected`. (c) documentation: test-retest assumption in `icc_test()`, `reli_err_test()`, `mdc_test()`; `id` notes in `dem_reg()`, `pb_reg()`; `tolerance_limit()` mean-of-m caveat; "Trending agreement" section in the agreement vignette; changes within items in the `method_vc` vignette; reliability and power vignette notes. Done for 0.5.0.

## Next steps (handoff, updated 2026-11-01)

State of branch `MethComp`: everything up to `bls_reg()` (rename),
`error_ratio_profile()`, `linked_reps_test()`, per-subject variances and
`transform = "log"` in `bls_reg()` is implemented, documented (NEWS,
pkgdown, Deming and Part 1 vignettes) and tested. Done since the handoff:

- **Slow `R CMD check` tests: fixed.** Not the R 4.5.3 environment:
  quantreg's bootstrap (`summary(se = "boot")`, simplex fits) cycled without
  end on some resamples of the tied issue #50 data in `test-call_args.R`,
  depending on the random number state left by the last test of
  `test-bls_reg.R` (only reached with BivRegBLS installed). `agree_np()` and
  its plot now use their own Frisch-Newton xy bootstrap
  (`rq_boot_vcov()` in `R/quantile_funs.R`). The full check took 9 minutes.
- **`linked_reps_test()` simulation: run and written up**
  (`linked_reps_sims/README.md`, NEWS, docs, Part 1 vignette). Size and
  power fine; the permuted limits are up to 4% narrower than an
  exchangeable fit's.
- **master merged** (the `fix-loa-nest-bounds` fix of `calc_loa_nest()`).
  NEWS keeps master's 0.4.1 bug-fix section below the 0.5.0 section.
- **`agree_np(prop_bias = FALSE, TOST = TRUE)`: LoA bounds now at `conf2`**
  (they were computed at `conf.level` but labelled `conf2`). The plot's
  bands (`get_qemm()`) now match the printout (2026-11-02): the stored
  model (it refit `delta ~ mean` even with `prop_bias = FALSE`), the
  limits at `conf2`, and the stored bootstrap covariance (`qr_vcov`).

- **Random raters in `method_vc()`: first version done** (design agreed
  2026-11-01; gap 5 in `bls_reg_roadmap.md`). `methods = c("fixed",
  "random")` and `sigma = c("common", "method")`; nlme with a rater
  `pdIdent` block for a constant outer group; one limits row for two random
  raters; agreement limits only. Tests in `test-method_vc_random.R`;
  coverage study in `method_vc_random_sims/` (bounds 0.94 to 1.00 with the
  MOVER over the components, which replaced the Satterthwaite chi-square
  for random raters). MethComp's `BA.est(random.raters = TRUE)` on `sbp`
  stops at a rater variance of 0 with a lower REML likelihood, so the fit
  was checked against `lme4::lmer()` instead.
- **`tdi()` for random raters uses the components** (2026-11-02): GPQ
  draws each component on its own df and `"tolerance"` uses the component
  MOVER, so it equals `upper_loa_ci` at p = pred_level (tested);
  `"delta"` errors for random raters. GPQ coverage 0.94 to 1.00 in the
  random-raters simulation (`method_vc_random_sims/README.md`).
- **Rater SD interval: MOVER** (2026-11-02): `mc_vc_rater_ci()`, v = S2 -
  c with S2 on M - 1 df and c (the items' part of the method-mean variance)
  on its delta-method df; coverage 0.94 to 0.98 (was 0.49 to 0.95 with the
  chi-square on M - 1 df; Satterthwaite df were tried and not enough). The
  limits and `tdi()` keep M - 1 df for the rater variance.
- **Variance components at zero: refit** (2026-11-02; random and fixed
  methods): `mc_vc_refit_boundary()` refits without every rater, tau or
  omega below 1e-3 times sigma (`mc_vc_zero_tol`), kept if the REML
  likelihood is not lower; dropped components are 0 with interval 0 to NA
  (rater: MOVER from 0) and add nothing to the bounds (agreed). Random-
  raters simulation: 998 to 1000 data sets per cell with bounds (was 826
  to 1000). Tests in `test-method_vc_boundary.R`. Known limit: with 3
  raters a large rater SD estimated at zero is missed (cell 8: 0.923); the
  MOVER rater term instead of 0 did not help there. The fixed-method
  simulations were rerun with the refit: method_vc coverage changed by at
  most 0.006 and fits without bounds fell from up to 2.2% to at most 0.6%
  (the documented ranges hold); `tdi()` on mvc fits failed in at most 4 of
  1000 (was up to 46), coverage 0.93 to 0.98; `linked_reps_test()` size
  and power unchanged.

- **Random-rater tolerance limits** (2026-11-02): `interval = "tolerance"`
  with `methods = "random"`. Bias 0 is known, so the tolerance limits are
  ±z U for both bound types and equal `upper_loa_ci` (tested); prediction
  limits ±t SD on the components' Satterthwaite df (now also `df_sd` for
  random raters), expected content 0.94 to 0.95 in the random-raters
  simulation.

Remaining, in order:

1. Random-rater follow-up: an optional parametric bootstrap (which might
   address the 3-rater case above, where a large rater SD estimated at
   zero gives bound coverage 0.92).
2. Lower priority (in `bls_reg_roadmap.md`): linked replicates for
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
