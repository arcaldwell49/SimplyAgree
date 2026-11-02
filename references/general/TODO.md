# TODO

Open items noticed during development. Each entry says what was seen, why
it may matter, and how to check it.

## `agree_np(prop_bias = FALSE)`: confidence level of the LoA bounds

Noticed 2026-11-01 while fixing the bootstrap hang (see the TODO comment in
`R/agree_np.R`). Not yet fixed; it changes reported results, so decide first.

**What.** The bounds of the limits of agreement are reported with
`ci.level = conf2` (one-sided, the outer ends of a `1 - 2 * (1 - conf.level)`
interval), but they are computed at `conf.level`, like the bias. Before
the bootstrap rewrite the same thing happened through a typo
(`df_coef2 = rbind(df_coef, temp)` instead of `rbind(df_coef2, temp)`) and
`process_rq(conf.level = conf.level)` for the limits.

**Why it matters.** The LoA bounds are narrower than their label says.

**How to check.** Compute the limits' bounds with `conf.level = conf2` in
`rq_boot_coef()` for the limit rows and compare with the `prop_bias = TRUE`
branch, which already uses `conf2`; add a NEWS entry under "Results that
change".

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

Remaining, in order:

1. **Random raters in `method_vc()`** (gap 5 in `bls_reg_roadmap.md`, which
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
2. The `agree_np(prop_bias = FALSE)` confidence level (entry at the top of
   this file); needs a decision.
3. Lower priority (in `bls_reg_roadmap.md`): linked replicates for
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
