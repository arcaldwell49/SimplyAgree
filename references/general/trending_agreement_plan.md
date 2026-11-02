# Within-subject (trending) agreement: plan for 0.5.0

Status: done 2026-11-02 (step 0, then items (a) reliability, (b) plots, (c) documentation; see Decisions). Item 4 (a within-subject slope in `sim_methods()`) was not needed: the vignette example builds its data directly. To be folded into the MethComp work on branch `MethComp`
(see `TODO.md` and `bls_reg_roadmap.md`).

## Motivation

With repeated measures of a changing true value (`agreement_limit(data_type = "nest")`,
`tolerance_limit(id =, model = "lme")`, `method_vc(replicates = "linked")`), the variance of the differences
splits into a between-subject part (a subject-specific bias) and a within-subject part. Users increasingly want
the within-subject part on its own: "do the two methods agree on *changes*?" (trending agreement). The usual
answers are:

- the within-subject SD of the differences and a "change LoA", z·√2·σ_w;
- the within-subject spread of the measured quantity (the range of changes the result applies to);
- 4-quadrant (concordance) and polar plots.

Enevoldsen et al. (2026, *Anesth Analg*, doi:10.1213/ANE.0000000000008389) and their BAtrending package
argue for the first two. SimplyAgree already fits the same model; what is missing is mostly reporting,
plots and documentation.

## What already exists

| Quantity | Where it is now |
|---|---|
| Bias, σ_between, σ_within, LoA of the differences | `agreement_limit(data_type = "nest")` (`lmer(delta ~ 1 + (1|id))`), `tolerance_limit(model = "lme")` (`SD.between`, `SD.within`; point estimates) |
| Change LoA, z·√2·σ_w of D, with a CI | `mdc_test(D ~ occasion | id, model = "oneway", method = "reml")` on D = x − y. Matches 1.96·√2·`SD.within` from `tolerance_limit(model = "lme")` on `reps` (1.08276 vs 1.08278) and gives a chi-square CI (`trending_checks/mdc_check.R`) |
| Per-method repeatability coefficient z·√2·σ_m (one method's ability to measure change) | `method_vc()` `rc` (with CIs) |
| Within-subject spread of the true value | `method_vc(replicates = "linked")`: `omega` (item-by-replicate SD, with CIs) |
| Serial correlation, condition-specific variance | `tolerance_limit(model = "lme", cor_type = "ar1"/"car1", time =, condition =)` |
| Log scale / ratio display | `log_tf`, `log_tf_display`; `transform` in `method_vc()` |

Open point for `method_vc()`: with two methods, linked replicates and one measurement per method per occasion,
check whether σ_1 and σ_2 are separately identified, or only σ_1² + σ_2². The second is enough for the change LoA
of the pair, but not for the per-method `rc`.

## A known limitation: a non-unit within-subject slope

If method y responds to within-subject changes in the true value with slope b ≠ 1, the systematic error
(b − 1)·Δtrue is absorbed into σ_within. The change LoA then depends on the study's within-subject range, and a
device that does not track at all can look acceptable when that range is small. This affects every function above.

Simulation (`trending_checks/issue_checks.R`; 200 subjects × 6 occasions; both error SDs 0.25):

| within-subject SD of true value | true within slope | change LoA | pooled LMM slope of D on A | `dem_reg(id)` / `pb_reg(id)` slope | Deming on subject-centred data, known ratio |
|---|---|---|---|---|---|
| 0.3 | 1 | 1.02 | 0.05 | 1.05 / 1.06 | 1.07 |
| 0.3 | 0 | **1.30** | −0.10 | 1.05 / 1.03 | 0.03 |
| 1.5 | 0 | 4.44 | −1.83 | 1.02 / 0.83 | 0.01 |

What the existing slope tools see:

- `prop_bias` and `check()` for nested data use a pooled random-intercept slope, which is dominated by
  between-subject variation when the within-subject range is small.
- `check.tolerance_delta` with `id`, `pb_reg(id)` and `dem_reg(id)` use subject means, so they only see
  between-subject slopes.
- `dem_reg(id)` also estimates the error ratio from within-subject variances. That is invalid when the true value
  changes: it estimated ratios from 0.32 to 37.8 when the truth was 1 or 0.17.
- `method_convert(bias = "scaled", replicates = "linked")` scales the occasion effect, but with the same slope as
  the between-subject true value. It returned β = 1.13 (0.96, 1.30) for a true within-subject slope of 0.
- The slope of D on the subject-centred average is confounded by unequal error variances: 0.79 for a device that
  tracks perfectly when its error SD is 0.6.
- Deming on subject-centred data recovers the within-subject slope, but only with a known error ratio. With one
  measurement per method per occasion, the slope and the ratio are not separately identified (3 moments, 4 unknowns).
  Replicates within an occasion, or an external error ratio, are needed.

So this release documents the limitation and gives a recipe; it does not add a slope function.

## Step 0: check what the existing methods already give (do this first)

Most of what trending agreement asks for may already be estimable. The change LoA is `mdc_test()` on D. Within-person
reliability is ω² / (ω² + σ_m²) from `method_vc(replicates = "linked")`, or `icc_test()` with device as the rater on
subject-centred data. If those cover it, the work below is mainly documentation and worked examples, not new code.
Answer these with a script in `trending_checks/` (simulated data from `issue_checks.R`: tracking, partial and
non-tracking devices, small and large within-subject ranges):

1. **`method_vc(replicates = "linked")` with two methods and one measurement per method per occasion.** Does it fit,
   and are σ_1, σ_2 and ω separately identified? (Within subject there are 3 moments, var(x_c), var(y_c) and
   cov(x_c, y_c), and 3 unknowns, ω², σ_1² and σ_2², so they should be identified *given a unit slope*; to be
   confirmed.) Does z·√2·√(σ_1² + σ_2²) match the change LoA from `mdc_test()` on D?
2. **Within-person reliability.** Compare ω² / (ω² + σ_m²) from `method_vc()` with `icc_test()` / `reli_err_test()`
   on subject-centred data (device as `item`, subject–occasion as `id`). How far off are the centred-data CIs
   (N − 2 instead of about N − n − 1 residual df)?
3. **Non-tracking.** With a within-subject slope of 0, what do `method_vc()` and the centred ICC report? Expectation
   (unverified): cov(x_c, y_c) ≈ 0, so ω → 0 and the true within-subject change is pushed into σ_ref, i.e. the misfit
   is visible as an implausible ω and σ split rather than as a small change LoA. Does `linked_reps_test()` or
   `check()` flag it?
4. **Gaps no existing function fills.** Expected: the quadrant and polar plots, the within-subject residual plot,
   and ω for users of `agreement_limit(nest)` / `tolerance_limit(lme)` (who could instead be pointed to
   `method_vc()`).

### Results (2026-11-01, `trending_checks/step0_checks.R`)

100 subjects × 6 occasions, one data set per row; error SD of ref 0.25; method-by-item SD 0.25.

| within SD of true value | within slope | alt error SD | ω | σ_ref | σ_alt | change LoA `method_vc` / `mdc_test` | reliability `method_vc` / centred ICC (CI) | `linked_reps_test` p |
|---|---|---|---|---|---|---|---|---|
| 0.3 | 1 | 0.25 | 0.32 | 0.25 | 0.27 | 1.009 / 1.009 | 0.60 / 0.60 (0.55, 0.65) | – |
| 1.5 | 1 | 0.25 | 1.58 | 0.23 | 0.29 | 1.009 / 1.009 | 0.97 / 0.97 (0.97, 0.98) | – |
| 0.3 | 0.5 | 0.25 | 0.23 | 0.33 | 0.22 | 1.092 / 1.092 | 0.39 / 0.39 (0.33, 0.46) | 0.05 |
| 1.5 | 0.5 | 0.25 | 0.83 | **0.87** | **0.00** | 2.399 / 2.399 | 0.65 / 0.77 (0.73, 0.80) | 0.05 |
| 0.3 | 0 | 0.25 | 0.05 | 0.40 | 0.27 | 1.324 / 1.324 | 0.02 / 0.02 (−0.06, 0.10) | 0.35 |
| 1.5 | 0 | 0.25 | **0.00** | **1.57** | 0.27 | 4.428 / 4.430 | 0.00 / 0.00 (−0.08, 0.08) | 0.60 |
| 0.3 | 1 | 0.60 | 0.32 | 0.24 | 0.65 | 1.907 / 1.907 | 0.30 / 0.30 (0.23, 0.37) | – |
| 1.5 | 1 | 0.60 | 1.58 | 0.24 | 0.65 | 1.907 / 1.907 | 0.91 / 0.91 (0.90, 0.93) | – |

(`linked_reps_test` was run with only 19 permutations, so 0.05 is its smallest possible p.)

Coverage of the centred-ICC 95% CI (300 data sets each, unit slope): 0.89 (20 subjects × 3 occasions),
0.92 (20 × 6), 0.86 (60 × 3), 0.89 (20 × 3, larger ω).

1. **Yes.** With two methods and one measurement per method per occasion, `method_vc(linked)` fits, and with a unit
   slope it recovers ω, σ_ref and σ_alt separately. Its change LoA, z·√2·√(σ_ref² + σ_alt²), equals `mdc_test()` on D
   to three decimals. Open question 5 is answered.
2. **Same point estimate, worse CIs.** ω²/(ω² + mean σ²) from `method_vc()` equals the centred ICC whenever the fit is
   not at a boundary. The centred-ICC CIs undercover (0.86–0.92), worse with fewer occasions per subject, because the
   lost subject df are ignored. So the centred-ICC recipe is fine for a point estimate but not for inference.
   `method_vc()` has the covariance of the variance components (`vc_cov`) needed for a delta-method CI, but does not
   report the reliability.
3. **Non-tracking is visible, but as a misfit, and nothing flags it.**
   - Slope 0: ω → 0, the reference method's error SD absorbs the true change (σ_ref 1.57 against a true 0.25), and the
     reliability → 0. So the reliability does catch a device that does not track; the change LoA alone did not
     when the range was small (1.32).
   - Slope 0.5, wide range: the misfit is worse. σ_alt hits 0 (boundary) and σ_ref is 0.87, so the *non-tracking*
     device looks error-free. Here the `method_vc()` reliability (0.65) and the centred ICC (0.77, which assumes equal
     error SDs) disagree.
   - The tell-tale sign is an implausible σ_ref (compared with the reference's known repeatability) or a σ at 0.
   - `linked_reps_test()` does not detect it: it tests whether replicates are linked (ω > 0), so with slope 0 it
     is *not* significant and would suggest treating replicates as exchangeable.
4. **Gaps confirmed:** the quadrant and polar plots, the within-subject residual plot, a within-person reliability
   with a valid CI, and a diagnostic for a non-unit within-subject slope.

**Outcome.** `method_vc(replicates = "linked")` is the trending analysis: ω is the range of applicability, `rc` and the
pair's change LoA come from it, and its point estimate of the within-person reliability matches the ICC. Item 1's
extra model for the average is dropped (users of `agreement_limit(nest)` / `tolerance_limit(lme)` are pointed to
`method_vc()`). New code reduces to:

- a within-person reliability per method and for the pair in `simple_mvc` output (linked only), with delta-method
  CIs from `vc_cov` (a coverage check is needed);
- the plots (item 2), on `simple_mvc` first;
- possibly a warning in `method_vc()` when a σ is at the boundary with linked replicates, pointing to a non-unit
  slope as one cause.

Everything else is documentation and worked examples (item 3), including how a non-tracking device shows up in
`method_vc()` output and why `linked_reps_test()` does not detect it.

**Decision rule (as written before the checks).**

- If 1–3 hold, drop item 1's new model fit: document `method_vc(replicates = "linked")` as the trending analysis,
  with ω as the range of applicability, `rc` and the pair's change LoA, and at most add the within-person reliability
  to `simple_mvc`'s print/summary as a derived quantity. Item 3 (documentation and examples) becomes the main
  deliverable; item 2 (plots) is the only clearly new code.
- If `method_vc()` cannot be used for this design (e.g. σ_1 and σ_2 not identified, or fitting fails with one
  measurement per method per occasion), keep item 1 as written for `agreement_limit()` and `tolerance_limit()`.

## Work items

Items 1 and 4 depend on the outcome of step 0.

### 1. Within-subject spread of the measured quantity, and the within-subject plot

These come as a pair: the plot's x-axis is the residuals of the model for the average.

- Fit `avg ~ 1 + (1|id)` alongside the difference model in `agreement_limit(data_type = "nest")` and
  `tolerance_limit(model = "lme")`. Use the log scale under `log_tf`.
- Report the mean and the between- and within-subject SDs of the average in print and in an element
  (e.g. `applicability`). For `method_vc(replicates = "linked")`, point to the existing `omega` instead.
- `plot(x, type = "within")` for `loa` (nest), `tolerance_delta` (lme) and `simple_mvc` (linked):
  - conditional residuals of D against conditional residuals of the average, on the same axis scale as the BA plot;
  - change LoA lines at ±z·σ_w (the residual scale; ±z·√2·σ_w is the change scale);
  - an optional acceptable-change band (`delta_change`, divided by √2 for the residual scale).
- Add a QQ plot of the random effects to `check()` for these fits.
- Tests: the residual SDs match the reported components; the plot builds for all three classes; log scale.

### 2. Concordance (4-quadrant) and polar plots in the existing plot methods

No new function. New `type` options for `plot()` on `loa`, `tolerance_delta` and `simple_mvc`:

```r
plot(x, type = "quadrant", exclusion = 0, relative = FALSE)
plot(x, type = "polar",    exclusion = 0, relative = FALSE)
```

- **Changes** are successive differences within `id`, ordered by:
  - `time` for `tolerance_limit()`;
  - row order within `id` for `agreement_limit()` (document this, or add a `time` argument);
  - the replicate column for `method_vc(replicates = "linked")`.

  Error for exchangeable replicates and for data without repeated measures.
- **Quadrant**: change by x against change by y, with the identity line and the exclusion zone. The concordance rate
  and the number of excluded pairs go in the caption.
- **Polar**: radius = mean change, angle from the 45° line (Critchley et al., 2011), with angular bias and limits.
- **Exclusion zone**: an explicit argument defaulting to 0 (no exclusion). `relative = TRUE` uses percentage changes.
- **Optional overlay**: the expected concordance from the variance components, Φ(Δ/(√2·σ_w)) at a change Δ,
  which ties the plot to the model. Check: change LoA 1.5 and Δ = 1 give 0.904, as in the paper's Figure 4.
- **No CI by default.** If one is added, it must resample subjects (changes within a subject are correlated).
- **Docs caveats**:
  - The rate depends on the exclusion zone and on the size of the changes.
  - The polar exclusion by mean change drops highly discordant pairs.
  - The common 92% and ±30° thresholds have no statistical basis.
  - Both plots, like the change LoA, miss a device that does not track when the within-subject range is small.

### 3. Documentation

- **State the test–retest assumption of the reliability functions explicitly.** `icc_test()`, `reli_err_test()`
  and `mdc_test()` (with occasion or trial as `item`) assume one true value per subject, measured repeatedly, so
  differences between occasions are pure error. When the true value changes between occasions (trending data,
  e.g. cardiac output during an intervention), that assumption fails:
  - on one device's readings, the "SEM" is √(ω² + σ_m²), real change plus device error;
  - the ICC with occasion as `item` counts real change as error.

  Where it goes:
  - a short "Assumptions" paragraph in the Details of each of the three functions, pointing to
    `method_vc(replicates = "linked")` for repeated measures of a changing value;
  - a sentence near the start of the reliability vignette;
  - the trending subsection of the agreement vignette, with a table of which call estimates what. It should include
    the two calls that do work: `mdc_test()` on D (the change LoA, because the real change cancels in D) and
    `icc_test()` on subject-centred data with device as `item` (a point estimate only; see step 0).
- **Agreement vignette, nested section**: a "trending agreement" subsection with
  - the change LoA via `mdc_test()` on D (with code), and the per-method `rc` from `method_vc()`;
  - what σ_within contains, including the non-unit slope case and the simulation table above;
  - the subject-centred Deming recipe: centre x and y within subject, `dem_reg(y_c ~ x_c, error.ratio = )` with a
    ratio from an external repeatability study, and `error_ratio_profile()` when it is unknown. Caveats: the fit
    uses N − 2 residual df instead of N − n − 1, so SEs are too small (about 10% with 6 measurements per subject),
    and the full CI over all ratios is often wide.
- **`dem_reg()` and `pb_reg()`**: state that `id` assumes exchangeable replicates of one true value, that
  `dem_reg(id)` fits subject means (a between-subject slope), and that neither is valid for repeated measures of a
  changing value. Point to `method_vc()` / `method_convert()` for linked designs.
- **`tolerance_limit()` docs** (the "Variance components" paragraph): the `SD.within^2 / m` formula for the mean of m
  measurements assumes SD.within is noise that averages out; a slope component at the same occasion does not.
- **Power vignette** (around the ICC-of-differences discussion): remove the duplicated "Caveat emptor" paragraph,
  and note that a non-unit slope also ends up in the within-subject variance.
- NEWS entries for items 1 and 2.

### 4. Optional: simulation support

- A within-subject slope argument to `sim_methods()` (separate from the slope on μ_i), so that the non-tracking case
  can be simulated in tests and in the vignette. Currently one β_m scales both μ_i and the occasion effect
  (`R/sim_methods.R:175`).

## Deferred / not planned

- **A `change_limit()` function.** For independent data it is `mdc_test()` on D. What it would add is the
  lag-dependent form under AR(1)/CAR(1), z·√(2σ²_w(1 − ρ^lag)) (√2·σ_w is the long-lag limit), and per-condition
  forms (√(σ²_c + σ²_c′)). Revisit if autocorrelated monitor data becomes a real use case; the CI would need a
  coverage simulation (delta method on (log σ_w, atanh ρ) from `apVar`, or Satterthwaite df).
- **A within-subject slope function.** Needs a design that identifies it: replicates within an occasion, or a scaled
  model with separate slopes for μ_i and the occasion effects (e.g. a lavaan variant of
  `method_convert(estimator = "ml")`). Research-level work; out of scope for 0.5.0.
- **Percentage error** (Critchley: 1.96·SD/mean). Field-specific, depends on the level; `log_tf` or
  `reli_err_test(scale = "mean")` is the principled alternative.
- **Bootstrap CIs for these quantities** (as in BAtrending). The analytic chi-square / MOVER bounds are used instead.

## Decisions (2026-11-02)

Agreed with the user after step 0; these replace the open questions below.

- **Item 1 dropped** (step 0 outcome): no model for the average in `agreement_limit()` / `tolerance_limit()`;
  ω from `method_vc(replicates = "linked")` is the range of applicability. No within-subject residual plot.
- **Plots (item 2): quadrant and polar** as `plot(type = "quadrant")` / `plot(type = "polar")` for all three classes:
  - `simple_mvc` with linked replicates (order = replicate);
  - `tolerance_delta` with `id` (order = `time`, else row order within `id`);
  - `loa` with `data_type = "nest"` (row order within `id`, documented; no new `time` argument).

  Error for exchangeable replicates and data without repeated measures. The expected-concordance overlay is an
  option, off by default. **Done (b)**: `R/trend_plot_functions.R`; the exclusion zone is on the mean change for
  both plots; `expected = TRUE` adds the expected concordance to the caption (σ_w from the model for `simple_mvc` and
  `tolerance_delta` with one `SD.within`, else the pooled within-id SD of the differences); tests in
  `test-trend_plots.R` (including the paper's 0.904 check).
- **Within-person reliability** in `simple_mvc` output (linked only): ω² / (ω² + σ_m²) per method and
  ω² / (ω² + mean σ²) for the pair, with delta-method CIs from `vc_cov`, checked by a coverage simulation before
  release. **Done (a)**: logit delta-method CIs (the F interval was no better); "average" and common-SD rows covered
  0.93–0.96; per-method rows as low as 0.86 with two methods and a large ω, so all rows are reported with a caveat
  (agreed after the simulation; `method_vc_reliability_sims/`).
- **Residual SD at zero:** first agreed as a warning in `method_vc(replicates = "linked")`; changed after the
  simulation to a `print()` note naming both causes (σ_m small relative to ω, or a non-unit within-subject slope),
  because with two methods and a large ω a σ_m is at zero in up to 30% of fits where both methods track.
  `dem_reg(id)` / `pb_reg(id)`: documentation only.
- Item 3 (documentation) as written; item 4 (a within-subject slope in `sim_methods()`) as needed for tests and the
  vignette example.

Implementation order: (a) reliability + boundary warning in `method_vc()` with the coverage simulation;
(b) the change extraction and quadrant/polar plots, `simple_mvc` first, then `tolerance_delta` and `loa`;
(c) documentation and the vignette subsection.

## Follow-up: a within-subject facet in `reli_facet_test()` (proposed 2026-11-02; done 2026-11-02)

**Done.** `facet = "within"`: score sigma2_po, consistency error sigma2_pio,e / k', agreement adds sigma2_io / k'
("inter" with the subject-level effects p, pi, i removed by centring; sigma2_o in neither, as for "inter"). The
consistency coefficient is a ratio of MS_po and MS_pio, so its interval is exact. Decisions: no change LoA (left to
`mdc_test()` on D and `method_vc()`); `m_avg` must be 1; `k_avg` and `r_avg` allowed. Checks: on the scenarios below it
gives 0.974 / 0.603 (tracking, within SD 1.5 / 0.3) and -0.003 / 0.020 (not tracking), against `method_vc(sigma =
"common")` 0.974 / 0.599 / 0.011 / 0.018 (the latter with wide boundary intervals). Coverage
(`reli_sims/within_coverage.R`, `within_coverage.csv`; 64 settings x 1000): ANOVA 0.933-0.977, REML with 20% missing
0.942-0.997.

`reli_facet_test(occasion =)` (experimental) with devices as `item` and occasions as `occasion` fits the same
decomposition as `method_vc(replicates = "linked")`: subject-by-occasion (po) is the real within-subject change
(ω²), subject-by-item (pi) is τ², and the residual is the device error (pooled over devices). But none of its
facets reports the trending coefficient. Check (`trending_checks/facet_check.R`; 100 subjects × 6 occasions,
one data set per row):

| Scenario | `facet = "intra"` | `facet = "inter"` | (po) / ((po) + residual) |
|---|---|---|---|
| tracks, within SD 1.5 | 0.21 | 0.97 | ≈ 0.97 |
| does not track, within SD 1.5 | 0.37 | 0.38 | ≈ 0 |
| **does not track, within SD 0.3** | **0.88** | **0.85** | **0.02** |

With unit slope the components were recovered: (po) SD 1.58 (true 1.5), (pi) 0.20 (0.18), residual 0.26 (0.25).

- `"intra"` counts (po), the real change, as error: the test–retest assumption again.
- `"inter"` keeps the between-subject variance in the score, so with a small within-subject range it misses a device
  that does not track.

**Proposal:** `facet = "within"` (requires `occasion`), with score σ²_po and error σ²_pio,e / k' (consistency;
agreement adds (σ²_o + σ²_io) / k' only if shared occasions are meaningful). This reuses the existing MLS /
Satterthwaite / bootstrap intervals and the `components` table, and gives reliability users (raters × occasions) the
same coefficient that `simple_mvc` now reports. `method_vc(linked)` stays the recommended route for method
comparison (separate error SDs per device, fixed bias, limits in measurement units).

To decide or check before starting:

- the formula for unbalanced data (`method = "reml"`) and repeats within subject × item × occasion (residual split);
- whether to also report the change LoA, z·√2·√(2σ²_pio,e), from the pooled residual, or leave that to
  `method_vc()` / `mdc_test()` on D;
- docs: occasions are assumed shared across subjects (day 1 is the same event for everyone); for monitoring data
  where occasion r of one subject has nothing to do with occasion r of another, σ²_o and σ²_io should be ≈ 0 and
  only the consistency form is meaningful;
- interval coverage: a short simulation in the style of `method_vc_reliability_sims/`, including the boundary case
  (σ²_po near 0);
- cross-check against the `simple_mvc` pair reliability, ω² / (ω² + mean σ²), on the same data.

## Open questions (answered 2026-11-02; see Decisions)

1. Item 1: report the average's SDs as point estimates only (matching `SD.between`/`SD.within`), or with chi-square CIs?
2. Item 2: add a `time` argument to `agreement_limit()`, or rely on row order within `id`?
3. Item 2: include the polar plot, or only the quadrant plot? Draw the expected-concordance overlay by default?
4. Item 3: should `dem_reg(id)` / `pb_reg(id)` also warn when replicates are clearly not exchangeable, or is the
   docs change enough?
5. `method_vc()` with two methods and linked replicates: are σ_1 and σ_2 separately identified (see above)?
   Step 0, question 1 answers this: yes, given a unit slope.
6. If step 0 shows `method_vc()` covers the analysis, is a within-person reliability in `simple_mvc` output worth
   adding, or do docs pointing to `icc_test()` on centred data suffice?

## References

- Enevoldsen J, et al. (2026). Trending agreement: Bland–Altman is all you need. *Anesth Analg*.
  doi:10.1213/ANE.0000000000008389. Package: BAtrending (GitHub `JohsEnevoldsen/batrending`).
- Critchley LA, Lee A, Ho AM-H (2010). A critical review of the ability of continuous cardiac output monitors to
  measure trends in cardiac output. *Anesth Analg* 111:1180–1192.
- Critchley LA, Yang XX, Lee A (2011). Assessment of trending ability of cardiac output monitors by polar plot
  methodology. *J Cardiothorac Vasc Anesth* 25:536–546.
- Saugel B, Grothe O, Wagner JY (2015). Tracking changes in cardiac output: statistical considerations on the
  4-quadrant plot and the polar plot methodology. *Anesth Analg* 121:514–524.
- Carstensen B (2010). *Comparing Clinical Measurement Methods: A Practical Guide*. Wiley.
