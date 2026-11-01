# Within-subject (trending) agreement: plan for 0.5.0

Status: proposed, 2026-11-01. Not started. To be folded into the MethComp work on branch `MethComp`
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

## Work items

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

## Open questions

1. Item 1: report the average's SDs as point estimates only (matching `SD.between`/`SD.within`), or with chi-square CIs?
2. Item 2: add a `time` argument to `agreement_limit()`, or rely on row order within `id`?
3. Item 2: include the polar plot, or only the quadrant plot? Draw the expected-concordance overlay by default?
4. Item 3: should `dem_reg(id)` / `pb_reg(id)` also warn when replicates are clearly not exchangeable, or is the
   docs change enough?
5. `method_vc()` with two methods and linked replicates: are σ_1 and σ_2 separately identified (see above)?

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
