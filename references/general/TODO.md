# TODO

Open items noticed during development. Each entry says what was seen, why
it may matter, and how to check it.

## Planned features

- `bls_reg()` (renamed from `cbls_reg()`): done, including tolerance limits, the simultaneous band, variance functions, per-subject variances, the log scale, `error_ratio_profile()` and CBLS in `method_convert()`. What is left is under "Still open" and "Gaps against BivRegBLS and MethComp" in `bls_reg_roadmap.md`.
- Within-subject (trending) agreement for 0.5.0: the within-subject spread of the average and a within-subject residual plot, 4-quadrant/polar options in `plot()` for `loa`, `tolerance_delta` and `simple_mvc`, and documentation (change LoA via `mdc_test()`, the non-unit slope limitation, the `dem_reg(id)`/`pb_reg(id)` assumption). Plan, evidence and open questions are in `trending_agreement_plan.md`; check scripts are in `trending_checks/`. "Step 0" was run (2026-11-01): `method_vc(replicates = "linked")` covers the analysis, so the work is documentation, the new plot types, a within-person reliability with CIs in `simple_mvc` output, and possibly a boundary-σ warning. Not started.

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
  LoA bands (`get_qemm()`) still use the outer end of a `conf.level`
  interval for the limits, for both `prop_bias` settings; not changed.

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
  `"delta"` errors for random raters. The GPQ bound was not simulated.

Remaining, in order:

1. Random-rater follow-ups (in `method_vc_random_sims/README.md`):
   - the rater SD interval (M - 1 df) undercovers when the rater SD is small
     (0.79 to 0.91); try Satterthwaite df from the method mean square;
   - no bounds when the rater SD is estimated at zero (up to 10% with 3
     raters); e.g. refit without the rater effect for the other
     components and use a one-sided bound for the rater variance;
   - tolerance limits and an optional parametric bootstrap.
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
