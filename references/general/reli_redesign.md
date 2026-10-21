# Reliability Suite Redesign

Branch: `reli-update` · Status: phases 1–8 done (engine, `icc_test()`,
`reli_err_test()` with raw/log/mean scales, methods, `power_icc()`,
`precision_icc()`, legacy rewire, jamovi, NEWS, vignette, check and
coverage, plot redesign); next: the PR
· Last updated: 2026-10-17

## 0. Handoff: current state and next steps

Read this section first. The rest of the document is the full spec plus a
chronological record of decisions and simulation results (mostly in
Section 9 under each phase); where they disagree, this section and the code
are current.

### Where things are

| Item | Location |
|---|---|
| Engine (data prep, ANOVA/REML fits, ICC/SEM/SD helpers, CIs and tests, SEs, bootstrap) | `R/reli_engine.R` (internal, `@noRd`) |
| `icc_test()` | `R/icc_test.R`, `man/icc_test.Rd` |
| `reli_err_test()` (SEM/SEE/SEP; raw, log, mean scales) | `R/reli_err_test.R`, `man/reli_err_test.Rd` |
| `gstudy_test()` (repeats within cells, G theory) | `R/gstudy_test.R`, `R/gstudy_engine.R`, `tests/testthat/test-gstudy_test.R`, sims `reli_sims/gstudy_*.R` (Section 14) |
| `power_icc()`, `precision_icc()` | `R/power_icc.R`, `man/power_icc.Rd`, `man/precision_icc.Rd`, `tests/testthat/test-power_icc.R` |
| `summary()`, `plot()`, `check()` for `reli_htest` | `R/methods.reli_htest.R`, `man/reli_htest-methods.Rd` |
| Tests | `tests/testthat/test-reli_engine.R`, `test-icc_test.R`, `test-reli_err_test.R`, snapshots in `tests/testthat/_snaps/` (all passing on 2026-10-16) |
| Simulation scripts behind every table here | `references/general/reli_sims/` (run from the package root) |
| pkgdown reference entry | `_pkgdown.yml`, "Reliability" section |
| Legacy functions (superseded, rewired onto the engine) | `R/reli_stats.R`, `R/methods.simple_reli.R`, `R/jmvreli.b.R` (jamovi, now on `icc_test()`/`reli_err_test()`) |

Commits on `reli-update` since `master`: `3826d5d` (phase 1), then
`f7fcc97`, `7b685ee`, `f6768b4`, `ae74d46`, `8fd0d35`, `aec1a82` (phase 2).
`R CMD check` (phase 7): 0 errors, 0 warnings; environment NOTEs only. Phase 1–2 note, kept for the record: one NOTE that predated this work
(`calc_loa_reps`: undefined globals `mxi`, `myi` in `R/agree_reps.R`), plus
an occasional environment NOTE ("unable to verify current time").

### `icc_test()` as built

```r
icc_test(x = NULL, data = NULL, measure = NULL, item = NULL, id = NULL,
         col.names = NULL,
         model = c("twoway", "oneway"), type = c("consistency", "agreement"),
         unit = c("single", "average"), method = c("anova", "reml"),
         conf.level = 0.95, null.value = NULL,
         alternative = c("two.sided", "greater", "less"),
         boot = FALSE, boot_ci = c("auto", "perc", "bca", "stud", "basic"),
         replicates = 1999, se_method = c("jackknife", "influence", "normal"))
```

Returns `c("reli_htest", "htest")` with `statistic` (F; omitted when
bootstrapping), `parameter`, `p.value`, `conf.int`, `estimate`, `stderr`
(`ICC`, `z`, plus `boot` when bootstrapping), `null.value`, `alternative`,
`method`, `data.name`, and extras `fit`, `data`, `settings`, `boot`.

### Settled design (with the evidence behind each)

- **Default ICC(C,1)** (Shrout & Fleiss ICC(3,1)): Hopkins (2000) and Weir
  (2005, full text) recommend it once systematic change is checked; Koo & Li
  (2016) prefer agreement. Section 6.1.
- **Intervals two-sided at α/2** (Shrout & Fleiss and McGraw & Wong, both
  read directly). Exact for one-way/consistency, Satterthwaite for agreement.
  Agreement test and interval are not exact inverses (documented). Sections
  2, 6.1.
- **Average-measure forms** = Spearman-Brown projection of the single form
  to all k items (identical with complete data; fixes the estimand with
  missing data). Section 3, phase-1 notes in Section 9.
- **ANOVA default; REML for missing data** with observed-data df and a
  warning. Coverage .91–.97 up to 30% missing. Section 3.
- **Bootstrap = resample subjects;** two-way agreement forms add a
  chi-square(k − 1)/(k − 1) multiplier on MSJ per replicate ("hybrid"),
  because subject resampling alone treats items as fixed. Section 9, phase 2.
- **`se_method` default `"jackknife"`** (accurate in every scenario; closed
  form for ANOVA, n refits for REML). `"influence"` is cheaper but 5–30% too
  small with few subjects, skew or MAR missingness; `"normal"` is fine only
  with normal data. Section 9, phase 2.
- **`boot_ci = "auto"`**: `stud` (jackknife SE) for ANOVA, `perc` for REML.
  `stud` with REML warns (influence-studentized under-covered at moderate
  ICCs with missing data). BCa never beat percentile. Section 9, phase 2.
- **No qualitative ICC labels; no MDC/RC options; CV folded into
  `reli_err_test(scale = )`; matched total SD; legacy functions superseded,
  not deprecated, with their ICC CIs corrected to two-sided.** Sections 6,
  7, 11.

- **`null.value = NULL` default in both functions** (estimate + two-sided
  interval; a test only against a stated null). `summary(style = )`:
  hypothesis (Neyman–Pearson) vs estimation (Greenland: compatibility
  intervals, S-values); `"auto"` picks by whether there is a p-value.
  Section 9, phase 3.
- **`reli_err_test()`:** SEM χ² intervals, SEE/SEP log-Wald (normal-theory
  delta; `scale = "log"` (sympercents) or `"mean"` (with Var(ȳ))); hybrid
  bootstrap for agreement statistics; `boot_ci = "auto"` =
  `bca` (agreement) / `stud` (otherwise) for ANOVA, `perc` for REML.
  Section 9, phase 3.

### Next steps

**Phase 8 done 2026-10-17** (decisions below in Section 9); next is the PR.

**Phase 8: reassess the reliability plot** (user, 2026-10-16), before the
PR. `plot.reli_htest()` (and `plot.simple_reli()`, jamovi) currently draws
each subject's measurements across items, coloured by subject, with a
legend. Explore alternatives on the Shrout & Fleiss and Weir data and
decide with the user before implementing. Candidates: a refined profile
plot (grey subject lines, item means with intervals, no legend); a
trial-vs-trial scatter with the identity line (k = 2) or a pairs matrix;
within-subject SD (or |difference|) against the subject mean, to show
whether the error grows with the mean (the raw vs log `scale` choice);
subject means ranked with ±SEM bars (between-subject spread against
measurement error, i.e. the ICC); residuals by item (systematic trial
effects, consistency vs agreement); a variance-component bar.

Then the PR from `reli-update` to `master` (description from this doc and
NEWS). Manual checks only the user can do: the jamovi module in jamovi,
and a read of the rendered vignette.

**G-theory extension (2026-10-20): built.** `reli_facet_test()` (first
built as `gstudy_test()`; the name is experimental and may change) for
repeat measurements within subject × item cells (Section 14, results 14.8)
and for shared occasions, `occasion` (Section 15, results 15.9). Open:
the MLS agreement coefficients with occasions are conservative (C6 not
met on that side; try the G*/H* terms). Manual checks left: a read of the
rendered vignette section "Repeat measurements and occasions".

### Open TODOs

- Influence SE: tuned small-sample correction (B) or an `"auto"` SE (C),
  only if the jackknife default proves a problem.
- Bootstrap under the null (proper test instead of CI inversion).
- MDC converter (`as_mdc()`), D-study / Spearman-Brown projection,
  average-measure SEM, RMS-of-subject-CVs, simulation-based power for the
  agreement ICC. Section 10.
- Unverified: McGraw & Wong Table 8 agreement-test coefficients (OCR
  unreadable; confirmed via Stata and simulation), the McGraw & Wong
  correction page, Cicchetti (1994) cutoffs. Section 12.
- In TOSTER (separate package): `describe_htest()` should use
  `inherits(htest, "htest")` so subclasses work.

### Working conventions for this branch

- New test files use `testthat::local_edition(3)`; the package's
  DESCRIPTION has no edition set and older tests use edition 2.
- Slow simulation tests run only with `SIMPLYAGREE_SLOW_TESTS=true`
  (`devtools::test()` runs `skip_on_cran()` tests locally).
- State acceptance criteria before running a simulation that decides a
  default, and record the table and the decision here.
- Long simulations: use `parallel` with progress logging (see
  `reli_sims/boot_coverage2.R`); REML jackknife runs are slow (about
  100 ms per lmer fit).
- On Windows with Git Bash, R code passed through shell heredocs can lose
  backslashes; use `fixed = TRUE` patterns or write scripts with an editor.

## 1. Goal

Replace `reli_stats()` / `reli_aov()` (one function returning everything) with
focused, `htest`-style functions that each estimate one quantity with a
proper confidence interval and an optional test against a meaningful null:

| Function | Estimates | Replaces |
|---|---|---|
| `icc_test()` | One ICC (McGraw & Wong taxonomy) | ICC table in `reli_stats()` |
| `reli_err_test()` | SEM, SEE, or SEP, in raw units, sympercents (log scale) or % of the mean (CV) | SEM/SEE/SEP/CV in `reli_stats()` |
| `power_icc()` | Power / sample size to test an ICC | new |
| `precision_icc()` | Sample size for a target ICC CI width | new |

The estimating functions share one internal data-preparation and model-fitting engine. Bug
fixes in the current code (Section 2) are folded into the redesign rather than
patched separately.

## 2. Problems in the current implementation

Confirmed by running R unless noted. File refs are to the pre-redesign code.

1. **ICC CIs are 90%, not 95%.** `qf(1 - alpha, ...)` used where two-sided
   needs `qf(1 - alpha/2, ...)` (`R/reli_stats.R:200-221`, same block in
   `reli_aov`, its bootstrap function, and jamovi). Simulated coverage of the
   ICC3 interval at `conf.level = .95` was 0.888. Shrout & Fleiss data: ICC1
   lower bound -0.097 here vs -0.133 in `psych::ICC`. Vignette formulas repeat
   the error. Each bound is a valid one-sided `conf.level` bound (the
   intended use: judging the lower bound, like a one-sided F test), but the
   pair is labelled as a `conf.level` two-sided interval, which it is not.
   **Sources checked (2026-10-15):**
   - Shrout & Fleiss (1979), read directly, pp. 424-426 (Eqs. 4-5, 7, 8-9 and
     the k-rater versions): every interval uses F at 1 − ½α and is called "a
     (1 − α)·100% confidence interval". Two-sided.
   - McGraw & Wong (1996), read directly (supplied PDF, text extracted
     2026-10-15): Table 7 is titled "Upper and Lower Limits on 1 − α
     Confidence Intervals" and every limit uses the (1 − ½α) × 100th F
     percentile. Two-sided. Their F tests (Table 8) are described for both
     one-tailed (Ha: ρ > ρ0) and two-tailed use; for two-tailed, reject when
     the smaller of P(F ≤ Fobs) and P(F ≥ Fobs) is ≤ α/2 (i.e.
     p = 2 × min). The Stata `icc` manual gives the same formulas.
   - psych history: psych's ICC docs said that from 1.4.2 the intervals used
     "(1-alpha)% at both tails", claimed to agree with Shrout & Fleiss. That
     is the convention `reli_stats` copied, but the attribution is wrong per
     the paper. psych 1.9.12 changed it again; current psych (2.6.3) uses
     `qf(1 - alpha/2)`.
2. **`print.simple_reli` breaks with variable arguments.** `conf.level = cl` or
   `other_ci = oc` gives the wrong branch or "non-numeric argument". Same root
   cause as the issue fixed for agreement functions in e4a8ded.
3. **`reli_aov` silently accepts unbalanced long data.** Only explicit `NA`
   values trigger subject removal; a missing row produces sequential (type I)
   SS with balanced-design df and CIs.
4. **`reli_stats` plugs REML variance components into balanced-design
   formulas.** MS are reconstructed from expected-MS identities, then
   McGraw-Wong F tests/CIs applied with balanced df. With missing data this is
   not valid; with complete data it equals ANOVA (no benefit). REML also
   truncates item variance at 0, shifting ICC2 for two-trial designs.
5. **Chi-square CIs for SEE/SEP (and ICC-based SEM) use error df.** These are
   functions of ICC and total SD, not a residual variance.
6. **Vangel CV interval applied with error df to a within-subject CV.** Vangel
   (1996) is for s/x̄ from one normal sample.
7. **`cv_calc = "residuals"`** is RMS of residuals over N (conditional
   residuals for lmer): a downward-biased version of √MSE.
8. Smaller:
   - `se_type = "MSE"` silently uses ICC3 for SEE/SEP; undocumented in Rd.
   - `reli_aov` truncates item variance at 0 but not subject variance.
   - `p11`, `p21`, `stat.final` computed and never used.
   - ~300 lines duplicated across `reli_stats`, `reli_aov`, bootstrap function.
   - `wide = FALSE` with no `measure` gives "argument missing", not the
     intended message.
9. **Unverified:** inside `bootMer`, `reli_mod_mse()` takes the mean from
   `insight::get_response(mod.lmer)`; may return the original, not simulated,
   response. Moot after the rewire, but check if any of that code survives.

### Documentation errors (fix in new docs; legacy Rd too if cheap)

- `@param wide` says default TRUE; it is FALSE.
- "Standard MSE of measurement / the Estimate / Predictions" (find/replace of
  "error").
- `@return`: lists `n.items` (actual `n.item`); SEM/SEE/SEP described as lists
  (are data-frame rows); says "agreement analysis"; `lmer` element holds an
  `aov` for `reli_aov`.
- Shrout & Fleiss is **1979**, pp. **420-428** (Rd says 1976, "420-3428").
- Details: "Hann & Meeker, 1991" vs cited "Hahn & Meeker 2011"; neither matches a real edition (1991 1st ed. Hahn & Meeker; 2017 2nd ed. Meeker, Hahn & Escobar); "Vangal's"; "u uses".
- `simple_reli-methods` Rd: "summary of the Limits of Agreement", "objects
  returned from the agree functions".
- Vignette: ICC(2,k) numerator written MSB−MSW (code MSB−MSE); CV CI formula
  missing `+2` and multiplies instead of adds; ICC(1,1) CI uses n_j where its
  notation means n_k; `ICC(_{3,1)}` typo; CI formulas show F₁₋α.
- `tests/testthat/test-reli_stats.R`: `context("agree_test")`;
  `expect_equivalent(test1$icc$icc, test1$icc$icc)` is a tautology;
  `context()`/`expect_equivalent()` deprecated in edition 3; no CI checks
  against published values (why bug 1 survived).

## 3. Engine choice: ANOVA vs REML

- Balanced, complete data, no boundary: REML = ANOVA estimates. lmer adds
  nothing.
- McGraw-Wong F tests and CIs are ANOVA theory (exact or Satterthwaite).
- REML truncation at 0 matters most in two-trial test-retest (the most common
  design).
- ANOVA: fast, never fails to converge, matches SPSS/`psych`/`irr`.
- REML's real advantages: missing/unbalanced data (MAR), covariates, more
  facets (G-theory), heterogeneous variances. Only realised if inference is
  model-based too.

**Decision:** single engine with `method = c("anova", "reml")`, default
`"anova"`. Unbalanced or incomplete data with `"anova"` → error pointing to
`method = "reml"` (no silent switch). `"reml"` with balanced data uses the same
analytic inference as `"anova"` (the MS are recoverable and estimates coincide
unless a variance component is on the boundary).

### REML with unbalanced data: F inference with observed-data df

`"reml"` with unbalanced data keeps the analytic F/χ² inference but with df
computed from the data actually observed (not the nominal complete-data df
that `psych::ICC(lmer = TRUE)` and the current `reli_stats()` use), plus a
warning. `boot = TRUE` is the opt-in alternative.

- Subject df: `n_obs - 1` (subjects with at least one observation).
- Error df: `N - n_obs - k + 1` (N = observations present).
- Items per subject in the MS reconstruction and ICC formulas:
  `kbar = N / n_obs`.
- Satterthwaite df (agreement forms, agreement SEM) computed from the same
  observed-data quantities.
- **Verify** the correction for every ICC form and SEM row during phase 1; it
  has only been simulated for ICC(C,1).
- **Warning** whenever REML is used with unbalanced data, no fixed %
  threshold: report the % of missing cells, say the F/χ² intervals and
  p-values are approximate and may be liberal as missingness increases, and
  suggest `boot = TRUE`. The documentation reports the simulation below as a
  guide rather than a rule (where coverage slips depends on n, k, the ICC and
  the missingness pattern).

Simulation (2026-10-15; ICC(C,1) = 0.7, item SD 0.3, MCAR, 400 reps per row,
95% two-sided F interval):

| n | k | % missing | nominal df (psych / current) | observed-data df |
|---|---|---|---|---|
| 20 | 3 | 0 | 94.0 | 94.0 |
| 20 | 3 | 15 | 92.0 | 94.2 |
| 20 | 3 | 30 | 88.0 | 92.7 |
| 30 | 2 | 10 | 91.7 | 94.0 |
| 30 | 2 | 20 | 90.0 | 95.3 |
| 10 | 4 | 20 | 92.5 | 94.8 |

Turn this into a fuller coverage study (all ICC forms, MAR as well as MCAR,
more reps) as a `skip_on_cran()` test and a vignette table.

**Phase-1 coverage study (2026-10-15)**, engine as implemented: n = 20, k = 3,
σ²_id = 1, σ²_items = 0.09, σ²_e = 0.43; 400 reps per row (MC SE 0.011);
MAR = later trials more likely missing for subjects with high trial-1 values;
one-way rows simulate rater effects unique to each cell. Script:
`reli_sims/coverage_reml.R`.

| Missing | 1 | k | C1 | Ck | A1 | Ak | SEM one-way | SEM C | SEM A |
|---|---|---|---|---|---|---|---|---|---|
| 0% | .932 | .932 | .940 | .940 | .932 | .932 | .943 | .953 | .963 |
| 15% MCAR | .960 | .960 | .958 | .958 | .965 | .965 | .958 | .945 | .963 |
| 30% MCAR | .948 | .948 | .925 | .925 | .940 | .940 | .948 | .935 | .943 |
| 15% MAR | .930 | .930 | .935 | .935 | .940 | .940 | .935 | .955 | .958 |
| 30% MAR | .935 | .935 | .917 | .917 | .912 | .912 | .925 | .935 | .945 |

- The first run found the average-measure forms falling to .855–.895 at 30%
  missing. Cause: they were computed for `kbar` (the average number of items
  observed per subject) instead of all k. Fixed by defining every
  average-measure estimate, interval and test as the Spearman-Brown projection
  of the single-measure form to k (algebraically identical with balanced
  data; tested). Table above is after the fix.
- Observed-data df verified for all ICC forms and SEM rows. Slightly liberal
  at 30% MAR (.912–.917), consistent with the warning.
- Phase 1 used `kbar = N / n`. The classical unbalanced one-way ANOVA uses
  k0 = (N − Σ nᵢ² / N) / (n − 1); not needed given these results.

## 4. Architecture

```
R/reli_engine.R        .reli_prepare(), .reli_fit(), CI/test helpers
R/icc_test.R           icc_test()
R/reli_err_test.R      reli_err_test()
R/power_icc.R          power_icc()
R/methods.reli_htest.R check(), plot() for reli_htest
R/reli_stats.R         superseded wrappers
```

### `.reli_prepare()`

Accepts any of the three interfaces (Section 5) and returns:

- `df`: long data frame with `values` (numeric), `items` (factor), `id`
  (factor);
- `n` (subjects), `k` (items), `N` (non-missing observations);
- `balanced` (every subject has every item exactly once, no `NA`);
- `data.name` string for `htest`.

Validates early: numeric response, no `NA` in `id`/`item`, at least 2 subjects
and 2 items, no duplicate subject × item cells (error; aggregating is the
user's job).

### `.reli_fit(prep, method)`

Returns a list of class `reli_fit`:

- `method`, `balanced`, `n`, `k`, `N`;
- `ms`: named vector `MSB`, `MSJ`, `MSE`, `MSW` and df `dfB`, `dfJ`, `dfE`,
  `dfW` (ANOVA, or balanced REML where they are recoverable);
- `vc`: variance components `id`, `items`, `resid` (reported truncated at 0;
  ICC/SEM formulas use the MS directly);
- `sd_total(model, type)`: **matched to the ICC's denominator**, i.e. the
  observed-score variance that the chosen ICC is a proportion of:
  - consistency: `sigma2_id + sigma2_resid` = `(MSB + (k - 1) MSE) / k`;
  - agreement: `sigma2_id + sigma2_items + sigma2_resid`
    = `(MSB + (k - 1) MSE + k (MSJ - MSE) / n) / k`;
  - one-way: `sigma2_between + sigma2_within` = `(MSB + (k - 1) MSW) / k`.
  (Expressions in MS for balanced data; with REML use the variance
  components.)
  Rationale (decided 2026-10-15): SEM = SD·√(1−ρ), SEE = SD·√(ρ(1−ρ)) and
  SEP = SD·√(1−ρ²) are classical test theory identities that hold only when
  SD² is the denominator of ρ. Then SD²·(1 − ICC) equals the error variance
  exactly (consistency → MSE, agreement → (MSJ + (n−1)MSE)/n, one-way → MSW).
  The observed SD √(SS_total/(N−1)) (Weir 2005; legacy `reli_stats()`)
  estimates no ICC's denominator: E[SS_total/(N−1)] = σ²_e +
  σ²_id·k(n−1)/(nk−1) + σ²_items·n(k−1)/(nk−1), e.g. 93% of the subject and
  53% of the trial variance when n = 8, k = 2. Weir's observation that his
  eq. 8 SEM is "modestly affected by between-subjects variability" is a
  symptom of that mismatch. Documentation must state the definition, give
  the formulas, explain why it differs from Weir and from `sd(values)`, and
  give Weir comparisons (data set A: matched SD 32.83 vs observed 31.74; SEE
  7.35 vs 7.11; SEP 10.54 vs 10.20, all from the raw data);
- `grand_mean`;
- `fit`: `lm` (ANOVA) or `lmerMod` (REML) object, used by `check()`/`plot()`
  and bootstrap.

ANOVA path computes SS in closed form from row/column/grand means (no `aov()`).

### CI and test helpers

Small, individually tested functions:

- two-sided intervals always use `alpha/2`; one-sided alternatives return
  one-sided bounds (as `t.test()` does);
- McGraw-Wong F statistics and CIs per ICC form;
- χ² interval and test for a variance with given df;
- Satterthwaite df for linear combinations of MS;
- delta method over the MS vector (MS treated as independent scaled χ²,
  `Var(MS) = 2 E[MS]^2 / df`);
- nonparametric subject-resampling bootstrap and leave-one-subject-out
  jackknife (below);
- bootstrap intervals and p-values for `bca`, `perc`, `basic`, `stud`.

### Bootstrap inference

Used when `boot = TRUE` in `icc_test()` or `reli_err_test()`, with either
method (balanced or not). Never forced.

- Arguments `boot = FALSE`,
  `boot_ci = c("auto", "perc", "bca", "stud", "basic")`, `replicates = 1999`,
  `se_method = c("jackknife", "influence", "normal")`, identical in both
  functions. **As implemented (2026-10-16):** `"auto"` = `stud` for
  `method = "anova"` (with `se_method`), `perc` for `"reml"` or when
  `se_method = "normal"`. See Section 9, phase 2, for the studies behind
  this.

**Resampling: nonparametric, by subject.**

- Each replicate draws n subjects with replacement, keeping all of each
  subject's observations, gives duplicates new ids, and refits with the same
  `method`. Works unchanged with unbalanced data.
- Rationale: a parametric bootstrap simulates from the normal model, so with
  balanced data it adds little over the F/χ² theory. The point of
  bootstrapping reliability statistics is robustness to non-normality and
  heteroskedasticity, which subject resampling provides.
- No parametric bootstrap option (`lme4::bootMer` not used).
- Replicates that fail (e.g. a non-converging REML fit, or a resample with
  < 2 distinct subjects) are dropped and counted; report the count in the
  output and warn if more than a small fraction fail.

**Interval types (mirror TOSTER 0.9.0, especially `boot_cor_test()`):**

- `perc`, `basic`: standard.
- `bca`: bias correction `z0` from the bootstrap distribution; acceleration
  `acc` from a leave-one-subject-out jackknife (n extra fits; trivial for
  ANOVA, n lmer refits for REML; dropping a subject keeps balanced data
  balanced). Mirror TOSTER's `bca_params()`.
- `stud`: **superseded design note** — the original plan below used the
  normal-theory (trigamma) SE, which turned out to be constant across
  resamples, so `stud` reduced to the basic interval on the z scale. As
  implemented, each replicate is studentized with its own `se_method` SE
  (jackknife or influence; `"normal"` is an error). The text below is kept
  for the record.
  Original plan: studentize every replicate with the analytic SE already computed
  for that statistic, on a variance-stabilising working scale, then
  back-transform (as TOSTER does on the Fisher z scale for correlations). No
  nested bootstrap. Supported by Ukoumunne et al. (2003): bootstrap-t on a
  variance-stabilising transformation of the ICC gave the closest to nominal
  coverage (standard methods under-covered with 10 clusters). Caveat: their
  setting was cluster trials with ICC ≤ 0.3, not high reliability values;
  our coverage study must check high ICCs.
  - ICC (one-way, consistency): `z = 0.5 * log((1 + (k - 1) * rho) / (1 - rho))`.
    With the plug-in ICC this is exactly `0.5 * log(F_obs)`. Use the exact
    variance `Var(z) = 0.25 * (trigamma(df1/2) + trigamma(df2/2))`, not the
    approximation `0.5 * (1/df1 + 1/df2)`, which is too small at small n
    (pre-check simulation, n = 10, k = 2: observed SD 0.355, exact 0.353,
    approximation 0.333). Agreement ICC: delta method from the MS on the same
    scale. Verify the average-measure versions.
  - SEM from MS (rows 1–3): log scale, `Var(log SEM) ≈ 1 / (2 * nu)`.
  - SEE, SEP, and `scale = "mean"`: log scale, delta-method SE.
  - REML with unbalanced data: same formulas with observed-data df (SE only
    needs to track each replicate's variability).
  - Replicates where the SE is not finite or ≤ 0 (boundary estimates) are
    dropped from `tvec`, as TOSTER does; count them.

**p-values: invert the chosen interval**, following TOSTER's `boot_pvalue()`
dispatch (`.pval_perc()`, `.pval_basic()`, `.pval_bca()`, `.pval_stud()`).
With bootstrap estimates `b`, observed estimate `est`, null `r0`, `B`
replicates:

- percentile: two-sided `phat = (sum(b < r0) + 0.5 * sum(b == r0)) / B`,
  `p = 2 * min(phat, 1 - phat)`; `"greater"`: `p = 1 - sum(b >= r0) / B`;
  `"less"`: `p = 1 - sum(b <= r0) / B`.
- basic: same on the reflected values `2 * est - b`.
- bca: `p0 = (sum(b < r0) + 0.5) / (B + 1)`, `u = qnorm(p0) - z0`,
  `p_bca = pnorm((u * (1 - acc * z0) - z0) / (1 + acc * u))`; two-sided
  `2 * min(p_bca, 1 - p_bca)`, `"greater"` `p_bca`, `"less"` `1 - p_bca`.
- stud: `t_obs = (est - r0) / se_obs` on the working scale (null transformed
  too); two-sided `2 * min(mean(t >= t_obs), mean(t <= t_obs))`,
  `"greater"` `mean(t >= t_obs)`, `"less"` `mean(t <= t_obs)`.
- (TOSTER's percentile one-sided branches drop the 0.5 tie weight that the
  two-sided and basic branches use; harmless for continuous estimates, but
  keep the tie handling consistent here.)

Common to all types:

- The p-value is consistent with the reported interval (rejects at `alpha`
  exactly when the CI excludes `null.value`).
- `statistic` and `parameter` are omitted (no reference distribution);
  `print.htest` handles their absence. Output also carries the bootstrap SE
  and the number of usable replicates.
- `method` string says "nonparametric (subject) bootstrap" and the interval
  type, e.g. "(BCa)", "(studentized)".
- **Documentation must state clearly** that these p-values are approximate:
  they come from inverting a bootstrap interval, not from a test whose null
  distribution is known, and their accuracy depends on the bootstrap
  interval's coverage (which can be poor with few subjects or estimates near
  the parameter boundary).

### Return class

`c("reli_htest", "htest")`. Prints via `print.htest`. Extra element `fit`
(the `reli_fit`) supports:

- `check.reli_htest()`: normality and heteroskedasticity diagnostics (port of
  `check_reli()`);
- `plot.reli_htest()`: subject profiles across items (port of
  `plot.simple_reli()`);
- `summary.reli_htest()`: plain-language description (below).

### `summary.reli_htest()`

Modelled on TOSTER's `describe_htest()` (0.9.0) but written internally (no
TOSTER dependency; same wording style). Returns an object of class
`summary.reli_htest` with the text in `$text` and the parts used to build it;
its print method writes the paragraph. `print()` stays `print.htest`.

The paragraph covers:

- **Test sentence** (when there is a p-value), `describe_htest()` style:
  significance at alpha = 1 − `conf.level`, statistic with df and p (omitted
  for bootstrap results), estimate with CI, and the decision about the
  alternative.
- **What was estimated, in words:** e.g. "two-way, consistency,
  single-measure ICC (Shrout & Fleiss ICC(3,1))"; for errors the stat and
  scale, e.g. "typical error (SEM) in sympercents (100 × SD of
  log-transformed values)" or "SEM as a percentage of the grand mean".
- **Design:** n subjects, k trials, ANOVA or REML, % missing; with REML and
  missing data, the approximation caveat.
- **Inference:** exact F, Satterthwaite or delta method; or bootstrap
  interval type, usable replicates, and that the p-value is approximate.
- **Estimation only** (`null.value = NULL`): no test sentence, e.g. "The SEM
  was 1.01 units (95% CI [0.82, 1.31])."
- **One-sided CIs** worded as bounds, e.g. "95% lower bound 0.41".
- **No qualitative benchmark labels** (e.g. Koo & Li 2016 "poor" to
  "excellent"). Published cutoffs disagree and are usually misapplied to the
  point estimate. The `icc_test()` documentation says so and shows the
  formal alternative: test against the threshold, e.g.
  `icc_test(null.value = 0.75, alternative = "greater")`. Weir (2005)
  supports this: "there is certainly no consensus as to what constitutes a
  good ICC", and an ICC's magnitude depends on between-subjects variability.

Note for TOSTER: `describe_htest()` checks
`if (!(class(htest) %in% c("htest")))`, which errors (length-2 condition, R ≥
4.2) for any htest subclass such as `c("reli_htest", "htest")`. Fix there
with `inherits(htest, "htest")`.

Possible follow-up outside this branch: give `ccc_test()` results the same
summary.

Store evaluated settings (not the call) so printing/plotting never
re-evaluates user expressions (avoids bug 2).

## 5. Input interface (all functions)

1. **Formula:** `f(values ~ item | id, data = df)`
2. **Long:** `f(measure = "values", item = "item", id = "id", data = df)`
3. **Wide:** `f(col.names = c("t1", "t2", "t3"), data = df)`, rows are subjects.
   Giving `col.names` means wide; there is no `wide` argument.

Supplying more than one interface → error.

## 6. Function specifications

### 6.1 `icc_test()`

```r
icc_test(x, data, measure, item, id, col.names,
         model = c("twoway", "oneway"),
         type = c("consistency", "agreement"),
         unit = c("single", "average"),
         method = c("anova", "reml"),
         conf.level = 0.95,
         null.value = 0,
         alternative = c("two.sided", "greater", "less"),
         boot = FALSE,
         boot_ci = c("auto", "perc", "bca", "stud", "basic"),
         replicates = 1999,
         se_method = c("jackknife", "influence", "normal"))
```

- Default ICC(C,1) (= Shrout & Fleiss ICC(3,1)). Pre-check (2026-10-15):
  - **Hopkins (2000), confirmed:** the abstract says changes in the mean
    should be eliminated from estimates of within-subject variation and kept
    as a separate measure; his sportsci.org reliability calculator computes
    the ICC from a two-way ANOVA (subjects, trials) as (F − 1)/(F + k − 1),
    i.e. ICC(3,1). He does not use the words "consistency"/"agreement".
  - **Weir (2005), full text confirmed:** the choice between models 2 and 3
    hinges on including or excluding systematic error (random vs fixed only
    changes interpretation, not calculation). If ICC(3,1) is reported, the
    trials effect must be shown to be trivial (F test and effect size). His
    "Practical applications" section recommends: run the two-way ANOVA, check
    the trials effect, fix the measurement schedule if needed, then use
    ICC(3,1) "since it is most closely tied to the MS_E calculation of the
    SEM". Direct support for the default, conditional on checking systematic
    error.
  - **Counterpoint, confirmed:** Koo & Li (2016) say absolute agreement should
    always be chosen for test-retest and intrarater studies; Qin et al.
    (2019) also recommend ICC(A,1).
  - **Rd wording:** present the default as Hopkins's and Weir's approach
    (systematic change checked and reported separately, e.g. with the trials
    F test or `t.test()`), and that others (Koo & Li) prefer absolute
    agreement, `type = "agreement"`. Do not present it as consensus.
  - McGraw & Wong: average-measure forms ICC(C,k)/ICC(A,k) are not defined
    for their Case 3 (two-way mixed with interaction). Mention in the Rd.
  - Matches `reli_err_test()` defaults.
- `model = "oneway"` only supports agreement; explicit
  `type = "consistency"` with one-way → error.
- Mapping to Shrout & Fleiss: ICC(1,1)/(1,k) = oneway; ICC(2,·) = twoway
  agreement; ICC(3,·) = twoway consistency. Document the mapping table.
- Default `alternative = "two.sided"` (shared with `power_icc()`). Docs and
  examples show `alternative = "greater"` for the common "is reliability
  above ρ₀?" question: the CI becomes `[L, 1]` with L the one-sided
  `conf.level` lower bound, matching the test.
- **Test (McGraw & Wong 1996, Table 8; the agreement and consistency forms
  below match the Stata `icc` manual; still verify against the paper):**
  - one-way: `F = (MSB/MSW) * (1 - r0) / (1 + (k - 1) r0)`, df `(n-1, n(k-1))`;
    average: `F = (MSB/MSW) * (1 - r0)`.
  - consistency: same with MSE, df `(n-1, (n-1)(k-1))`.
  - agreement single: `F = MSB / (a MSJ + b MSE)` with
    `a = k r0 / (n (1 - r0))`, `b = 1 + k r0 (n - 1) / (n (1 - r0))`;
    average: `a = r0 / (n (1 - r0))`, `b = 1 + r0 (n - 1) / (n (1 - r0))`;
    denominator df by Satterthwaite.
- **CI:** McGraw-Wong exact (oneway, consistency) or approximate (agreement)
  intervals, two-sided at `alpha/2`.
- **REML with unbalanced data:** same F test and CI with observed-data df,
  plus the warning (Section 3).
- **`boot = TRUE`:** subject-resampling bootstrap CI (`bca`, `stud`, `perc`,
  `basic`) and approximate p-value by CI inversion (Section 4, "Bootstrap
  inference"); `statistic`/`parameter` omitted. Works with `"anova"` and
  `"reml"`. Document that REML bootstrapping means `replicates` lmer refits
  (plus n for the BCa jackknife) and can be slow.
- `htest` fields: `statistic` (F), `parameter` (df1, df2), `p.value`,
  `conf.int`, `estimate` (named e.g. `"ICC(C,1)"`), `null.value`,
  `alternative`, `method` (e.g. "Two-way, consistency, single measure ICC"),
  `data.name`, plus `fit`.

### 6.2 `reli_err_test()`

```r
reli_err_test(x, data, measure, item, id, col.names,
              stat = c("SEM", "SEE", "SEP"),
              scale = c("raw", "log", "mean"),
              model = c("twoway", "oneway"),
              type = c("consistency", "agreement"),
              method = c("anova", "reml"),
              conf.level = 0.95,
              null.value = NULL,
              alternative = c("less", "greater", "two.sided"),
              boot = FALSE,
              boot_ci = c("auto", "perc", "bca", "stud", "basic"),
              replicates = 1999,
              se_method = c("jackknife", "influence", "normal"))
```

Defaults return Hopkins' typical error: SEM, raw units, two-way, consistency.

#### Base estimates (`scale = "raw"`)

| # | `stat` | Settings | Estimate | Inference |
|---|---|---|---|---|
| 1 | SEM | twoway, consistency | √MSE | exact χ², df = (n−1)(k−1) |
| 2 | SEM | twoway, agreement | √[(MSJ + (n−1)MSE)/n] = √(σ²_items + σ²_e) | Satterthwaite χ² |
| 3 | SEM | oneway | √MSW (Bland & Altman σ_w) | exact χ², df = n(k−1) |
| 4 | SEE | ICC from model/type | SD_total·√(ICC(1−ICC)) | delta (default) or bootstrap |
| 5 | SEP | ICC from model/type | SD_total·√(1−ICC²) | delta (default) or bootstrap |

SD_total is matched to the ICC's denominator (see `.reli_fit()`).

Notes:

- Rows 2 and 3 are numerically identical with balanced data; only df differ
  (one-way treats MSW as exact χ²).
- **No ICC-based SEM option (`from_icc` dropped).** With the matched SD,
  SD·√(1 − ICC) is algebraically identical to rows 1–3 (verified on Weir's
  data: 7.56, 7.56, 2.17 = √MSE for data sets A, B, C). The only version that
  differs is Weir's eq. 8 with the observed SD, which mixes a mismatched SD
  with the ICC. The Rd explains this; legacy `reli_stats(se_type = ...)`
  keeps the old calculation.
- Weir (2005) is the source for rows 4–5 (full text confirmed):
  - His eq. 8, SEM = SD·√(1 − ICC), "most references". He prefers √MSE from
    the two-way ANOVA (following Hopkins) because it does not depend on which
    ICC is used and is not affected by between-subjects variability; the
    one-way √MSW is biased high when the trials effect is large.
  - Row 4: his eq. 11, SD·√(ICC(1 − ICC)), labelled SEM_TS, "which some have
    referred to as the standard error of estimate"; the SE for a CI around a
    subject's estimated true score X̄ + ICC·(S − X̄). With the matched SD
    (consistency) it equals √(σ²_id·σ²_e / (σ²_id + σ²_e)), the classical
    true-score SD given an observed score.
  - Row 5: his eq. 15, SEP = SD·√(1 − ICC²), for judging real change around
    the estimated true score.
  - His SD is the observed SD of all scores, √(SS_total/(N − 1)) (data set
    A: 31.74); ours is matched, so rows 4–5 will not reproduce his numbers
    exactly (see `.reli_fit()`).
- Rows 4–5 depend on between-subjects heterogeneity (through the ICC and
  SD); document why rows 1–3 are generally preferred for describing
  measurement error.
- Row 2 Satterthwaite df:
  `nu = (MSJ + (n-1) MSE)^2 / (MSJ^2/(k-1) + ((n-1) MSE)^2/((n-1)(k-1)))`.
- `boot = TRUE` replaces the analytic inference for any row (χ² for rows 1–3,
  delta for rows 4–5). See Section 4, "Bootstrap inference".
- REML with unbalanced data: rows 1–3 use χ² with observed-data df (error df
  `N - n_obs - k + 1`, Satterthwaite from observed quantities for row 2;
  verify row 3's one-way df), rows 4–5 use the delta method on the same
  quantities, plus the Section 3 warning.

#### `scale`: coefficient of variation (replaces `cv_test()`)

- **`"raw"` (default):** measurement units.
- **`"log"`:** (Weir 2005 also recommends a log transformation when error
  grows with the size of the scores.) The engine is fitted on `log(values)`; the estimate is reported
  in **sympercents** (Cole 2000): `100 * sigma_log`. Because this is a
  constant multiple of the log-scale SD, rows 1–3 keep exact (or
  Satterthwaite) χ² inference. Requires all values > 0 (error otherwise).
  Documentation gives the conversion to the conventional CV%,
  `100 * sqrt(exp(sigma_log^2) - 1)`, notes the two are nearly identical below
  ~15%, and cites Hopkins (2000) and Atkinson & Nevill (1998) for the
  log-transformed typical error. The conversion is monotone, so users can
  convert the CI limits too.
- **`"mean"`:** `100 * error / grand_mean` (the current "pooled" CV). Inference
  by delta method including the uncertainty in the grand mean, treated as
  independent of the MS under normality:
  - consistency (items fixed): `Var(ybar) = MSB / (n k)`;
  - agreement / oneway (items random): `Var(ybar) = (MSB + MSJ - MSE) / (n k)`
    (verify; for oneway use the one-way expectation).
  Bootstrap with `boot = TRUE`.
- Applies to all three stats.
- The `htest` estimate name reflects the scale, e.g. `"SEM (sympercent)"`,
  `"SEM (% of mean)"`.
- Dropped from the current code: `cv_calc = "residuals"`, the Vangel interval.
- `@concept coefficient of variation` and a vignette section titled with that
  phrase so users searching for "CV" find this function.

#### Test

- `null.value` is an acceptable-error threshold **on the reported scale**
  (units, sympercents, or % of mean), with `alternative = "less"` by default.
- Rows 1–3 (raw or log): χ² test on the variance,
  `statistic = nu * est^2 / null.value^2`, df `nu`.
- Rows 4–5, and `scale = "mean"`: delta-method z test, or the bootstrap
  CI-inversion p-value when `boot = TRUE`.
- `null.value = NULL` → estimate and CI only (no statistic or p-value).

#### MDC, SDD and RC

**Update 2026-10-19:** superseded by `mdc_test()` (see Section 10). The notes
below are kept for the terminology and the SEP caveat.

**No multiplier, no MDC/RC option.** `@details` and the vignette explain that
MDC, SDD and RC are scaled SEMs and point users to the options below.
Terminology to use in the docs (pre-check): Weir (2005) calls it the
"minimum difference" (MD; section heading "Minimal differences needed to be
considered real"), MD = SEM × 1.96 × √2 (his eq. 13, confirmed in full
text), with SEM = √MSE and a different z allowed. Beckerman et al. (2001)
the "smallest real difference" (SRD); Haley & Fragala-Pinkham (2006) the
"minimal detectable change" (MDC); Bland & Altman the repeatability
coefficient. Weir also notes the MD "is not precisely accurate": following
Charter and Dudek, the SEP-based interval around the estimated true score
(X̄ + ICC·(S − X̄) ± 1.96·SEP) is the correct one for judging real change.
Mention this in the vignette section.

- two trials: `agreement_limit(x = trial1, y = trial2)` gives the exact MDC₉₅
  as the LoA half-width (for k = 2, MSE = var(d)/2), with bias reported
  separately; `tolerance_limit()` for a bound that accounts for SEM
  uncertainty;
- three or more trials:
  `qnorm(0.975) * sqrt(2) * c(res$estimate, res$conf.int)`.

### 6.3 `power_icc()` and `precision_icc()`

Two functions modelled on `stats::power.t.test()` (not on the package's
`power_agreement_exact()`): exactly one argument is `NULL` and is solved for
(`uniroot()` for continuous arguments, integer search for `k`); return class
`"power.htest"` so `stats` prints it; non-integer `n` reported with a NOTE to
round up (as `power.t.test()` does); `k` returned as the smallest integer
≥ 2 that meets the target.

**Why exact computation is available:** for the one-way and consistency ICCs,
under true ICC ρ the observed F ratio is a scaled central F:
`F_obs ~ [(1 + (k-1) rho) / (1 - rho)] * F(n - 1, df2)`, with
`df2 = n(k-1)` (one-way) or `(n-1)(k-1)` (consistency). Power is one `pf()`
call; the CI is a monotone function of F, so the distribution of its width is
known too.

```r
power_icc(n = NULL, k = 2, rho0 = 0, rho1 = NULL,
          sig.level = 0.05, power = NULL,
          model = c("oneway", "twoway"),          # twoway = consistency
          alternative = c("two.sided", "greater", "less"),
          method = c("exact", "approximate"))

precision_icc(n = NULL, k = 2, rho = NULL, width = NULL,
              conf.level = 0.95, assurance = 0.5,
              model = c("oneway", "twoway"),
              method = c("exact", "approximate"))
```

- `power_icc()`: exactly one of `n`, `k`, `rho1`, `power`, `sig.level` is
  `NULL`.
- `alternative` uses the test notation of `icc_test()` (not
  `power.t.test()`'s `"one.sided"`), same default `"two.sided"`, so a design
  planned with the defaults matches the analysis run with the defaults.
  Error if `rho1` is on the wrong side of `rho0` for `"greater"`/`"less"`.
  Solving for `rho1` under `"two.sided"` returns the solution above `rho0`,
  with a NOTE pointing to `alternative = "less"` for the one below.
- `precision_icc()`: exactly one of `n`, `k`, `width`, `assurance` is `NULL`.
  `width` is the full CI width; `assurance` is the probability the realised
  width is ≤ `width` (0.5 = median width).
- **`method = "exact"` (default):** power from the scaled central F;
  assurance by numerical integration over the F distribution of the CI width.
- **`method = "approximate"`:** the published formulas, Walter, Eliasziw &
  Donner (1998) for power/n and Zou (2012) for precision. Solving for other
  arguments uses `uniroot()` on the approximate function. Both are one-way
  model methods: `"approximate"` is only available with `model = "oneway"`
  (error otherwise).
  - Walter et al.: exact power from Donner & Eliasziw (1987) is what
    `"exact"` computes; their closed form (one-sided α):
    `n = 1 + 2k (z_{1-α} + z_{1-β})^2 / ((log C0)^2 (k - 1))`,
    `C0 = (1 + k θ0) / (1 + k θ1)`, `θ = ρ / (1 - ρ)` (our n = subjects,
    k = trials). **Their paper swaps the letters** (n = observations per
    subject, k = subjects); so does PASS. Note this in the Rd.
  - Zou (2012), half-width ω with assurance 1 − γ:
    `N = 1 + [(A z_{1-α/2} + sqrt((A z_{1-α/2})^2 + 4 ω A |B| z_{1-γ} z_{1-α/2})) / (ω sqrt(2K(K-1)))]^2`,
    `A = (1 - ρ)(1 + (K - 1)ρ)`, `B = K - 2 + 2ρ(1 - K)` (`|B|` needed to
    reproduce the table). Lower one-sided limit: same closed form as
    Walter et al. with assurance in place of power.
  - **Caveat (resolved 2026-10-16):** Mondal et al. (2024) report that the
    variance expression in Zou's equation (3) is incorrect; it is the log-F
    (ZFρ) variance, which the Zou sample-size formula used here does not
    involve. See Section 9, phase 5.
  - Our `width` is the full CI width; Zou's ω is the half-width
    (`width = 2 * omega`). Document the conversion.
- Validation: exact power formula matches simulation (pre-check: 0.563 vs
  0.563, 0.884 vs 0.885). Test targets (one-way, α = .05 one-sided,
  power/assurance .80 unless stated; letters in our notation):
  - Walter et al. table (via PASS): ρ0 = .2, ρ1 = .3, k = 2 → n = 543.16,
    rounded 544 (exact power at 544: 0.8003).
  - Walter closed form (computed, not from the paper): (ρ0, ρ1, k) =
    (.6, .8, 2) → 38.61; (.4, .6, 2) → 86.12; (.7, .9, 2) → 17.90;
    (.8, .9, 2) → 45.29; (.5, .7, 3) → 39.60.
  - PASS exact power, ρ0 = .2, ρ1 = .3: (n, k) = (50, 2) 0.18333;
    (50, 3) 0.29534; (50, 5) 0.45522; (150, 4) 0.74538.
  - Zou Table 1: 95% CI, ρ = .6, ω = .1 (width .2), assurance .80, k = 2 →
    n = 183. PASS, assurance .90, ρ = .6, ω = .1: k = 2 → 196, k = 4 → 96,
    k = 6 → 77, k = 8 → 70.
  - Zou lower limit: 95% one-sided, ρ1 = .6, ρ0 = .5, k = 2, assurance .80 →
    n = 300 (Fisher approximation); PASS exact → 301.
- **Agreement ICC not supported.** No exact F distribution exists; McGraw &
  Wong (1996, footnote 3) note that because the denominator df of the Type A
  tests are sample-dependent, their power "cannot be determined exactly".
  Rd `@details` says so, cites this, and points to the TODO (simulation-based
  power).

## 7. Legacy functions and jamovi

- `reli_stats()` and `reli_aov()`: badge `superseded` (no warning). Rd
  description points to `icc_test()` and `reli_err_test()` (including
  `scale = "log"`/`"mean"` for the CV); deprecation can come in a later
  release. Rewire internally onto the new engine so the output keeps the
  `simple_reli` structure with bugs 1–7 fixed (`reli_aov` = engine with
  `"anova"`). Keep SEE, SEP and the existing `se_type`/`cv_calc` arguments for
  compatibility; map `cv_calc = "residuals"` to `"MSE"` with a warning.
- **ICC CI correction in the legacy functions:** change to two-sided
  `conf.level` intervals (`qf(1 - alpha/2)`), matching Shrout & Fleiss,
  McGraw & Wong, psych, irr, SPSS and Stata. Rationale: the functions cite
  those papers, and the label must mean what those papers mean (decided
  after reading Shrout & Fleiss; earlier plan was documentation-only).
  - NEWS entry explains the change and the one-sided-bounds reasoning
    behind the old output.
  - Old numbers are exactly reproducible: old output at `conf.level = c`
    equals new output at `conf.level = 1 - 2 * (1 - c)` (e.g. old .95 =
    new .90). Say this in NEWS and the Rd.
  - Point users who want the one-sided lower bound to
    `icc_test(alternative = "greater")`.
  - jamovi follows the same change.
- `simple_reli` methods: fix bug 2 (store evaluated settings).
- `jmvreli`: call the engine and new functions directly.

## 8. Testing

Use testthat edition 3 (`expect_equal(..., tolerance = 1e-5)`, no
`context()`).

- **Published values:** Shrout & Fleiss (1979) 6 × 4 data (already in tests):
  ICC(1,1) .17, (2,1) .29, (3,1) .71, (1,4) .44, (2,4) .62, (3,4) .91.
  McGraw & Wong (1996) worked examples for CIs and F tests against ρ₀ ≠ 0.
  Weir (2005) data sets (n = 8, k = 2; from his Tables 1 and 6):
  - A: trial 1 `146 148 170 90 157 156 176 205`,
    trial 2 `140 152 152 99 145 153 167 218`.
  - B: trial 1 `166 168 160 150 147 146 156 155`,
    trial 2 `160 172 142 159 135 143 147 168`.
  - C: trial 1 as A, trial 2 `161 162 189 100 175 171 195 219`.
  - Published values reproduced exactly by our formulas (pre-check):
    SS tables (A: 14,689.8 / 30.2 / 399.8 / 15,119.8; C: 15,925 / 961 / 33 /
    16,919); MSW 53.75 and MSE 57.1 (A, B); MSE 4.71 and MSW 124.25 (C);
    Table 4 ICCs for A (.95 .97 .95 .97 .95 .97) and B (.56 .72 .55 .71 .54
    .70); C: ICC(1,1) .896, ICC(2,1) .901; SEM √MSE 7.6 (A) and 2.2 (C);
    one-way SEM 7.3 (A) and 11.1 (C); observed SD 31.74 (A).
  - Weir's MD 21.07, SEE 6.92 and SEP 9.91 use rounded inputs (SEM 7.6,
    ICC .95). Exact values from the same data: MD 20.95, SEE 7.11, SEP 10.20
    (with observed SD and ICC(3,1) = .947). Test against the exact values,
    and against his numbers only with his rounded inputs.
  - Weir reports ICC(3,1) = 0.998 for data set C; the data give 0.996 (0.998
    is ICC(3,k)). Do not use 0.998 as a target.
- **Cross-checks:** `psych::ICC(lmer = FALSE)` and `irr::icc()` for every ICC
  form, estimate and CI (Suggests only).
- **Coverage simulation:** small simulation per ICC form confirming ~95%;
  `skip_on_cran()`.
- **Engine:** ANOVA = REML on balanced, non-boundary data; unbalanced data with
  `"anova"` errors; duplicate cells error; formula/long/wide interfaces give
  identical results.
- **REML unbalanced:** observed-data df computed correctly; warning fires
  whenever data are unbalanced and reports % missing; coverage study for every
  ICC form and SEM row (`skip_on_cran()`), extending the Section 3 table.
- **Bootstrap p-values:** match TOSTER's `.pval_perc()`, `.pval_basic()`,
  `.pval_bca()`, `.pval_stud()` on fixed bootstrap vectors; p < alpha exactly
  when the CI excludes the null; `statistic`/`parameter` absent; `method`
  string names the resampling and interval type.
- **`summary()`:** text for test vs estimation-only results, two-sided vs
  one-sided CIs, analytic vs bootstrap, balanced vs REML with missing data;
  snapshot tests (`expect_snapshot()`) for the printed paragraph.
- **Bootstrap mechanics:** resampled duplicates get new ids; unbalanced data
  work; failed replicates dropped and counted; jackknife leaves out whole
  subjects; studentized working-scale SEs match the analytic SEs.
- **Bootstrap coverage study** (`skip_on_cran()`), used to choose the default
  `boot_ci`: all four types × ICC forms and SEM rows, small n (e.g. 10–30),
  ICC near 0 and near 1, normal and non-normal (skewed, heavy-tailed) data,
  balanced and missing. Compare with the analytic intervals.
- **Matched SD:** for each ICC form, `sd_total^2 * (1 - ICC)` equals the
  corresponding error variance (MSE, agreement SEM², MSW) to machine
  precision; Weir data-set values for SEE/SEP with the matched SD (A: 7.35,
  10.54).
- **`reli_err_test`:** rows 2 and 3 equal with balanced data; for k = 2, MDC
  recipe equals `agreement_limit()` LoA half-width; χ² CI against hand
  calculation; delta vs bootstrap agree roughly.
- **`scale`:** `"log"` equals 100 × SEM of `log(values)` with identical χ²
  CI scaling; error on non-positive values; `"mean"` estimate against hand
  calculation; `null.value` interpreted on the reported scale.
- **Legacy:** `print()` works with variable `conf.level`/`other_ci`; output
  structure unchanged.
- **`power_icc` / `precision_icc`:** reproduce the Section 6.3 test
  targets (`"approximate"` matches the published closed forms; `"exact"`
  matches PASS exact values); `"exact"` close to them and matches a
  simulation of `icc_test()` (`skip_on_cran()`); solving round-trips for every
  solvable argument (n → power → n, k → power → k); exactly-one-`NULL` errors;
  result prints via `print.power.htest`.

## 9. Phases

1. Engine, helpers (including bootstrap p-value), input interface + tests
   against published values. **Done 2026-10-15:** `R/reli_engine.R`,
   `tests/testthat/test-reli_engine.R` (edition 3 via `local_edition()`;
   157 tests, plus slow simulation tests gated by
   `SIMPLYAGREE_SLOW_TESTS=true` rather than only `skip_on_cran()`, because
   `devtools::test()` runs `skip_on_cran()` tests locally). Notes for
   phase 2:
   - The bootstrap coverage study (default `boot_ci`) needs the statistic
     functions, so it moves to phases 2–3. **Done for ICCs in phase 2**
     (below).
2. **Phase 2 done 2026-10-15:** `R/icc_test.R`, `R/methods.reli_htest.R`
   (`summary`, `print.summary`, `plot`, `check`), `tests/testthat/test-icc_test.R`,
   `_pkgdown.yml` reference entry. Agreement-form test/interval mismatch:
   reported as published (McGraw & Wong) and documented in `@details`.
   **ICC bootstrap coverage study** (k = 3, σ²_items = 0.1, 250 sims × 499
   replicates per cell, MC SE 0.014; "non-normal" = chi-square(3) subject
   effects and t(5) errors, except ICC = .5 where both were t(5); script
   `reli_sims/boot_coverage.R`):

   | Form | n | ICC | data | F | BCa | stud | perc | basic |
   |---|---|---|---|---|---|---|---|---|
   | C1 | 10 | .5 | normal | .948 | .888 | .896 | .884 | .772 |
   | C1 | 30 | .5 | normal | .960 | .920 | .928 | .928 | .900 |
   | C1 | 10 | .9 | normal | .932 | .892 | .892 | .880 | .732 |
   | C1 | 30 | .9 | normal | .944 | .888 | .916 | .916 | .864 |
   | C1 | 10 | .5 | non-normal | .908 | .864 | .848 | .856 | .724 |
   | C1 | 30 | .5 | non-normal | .868 | .888 | .856 | .912 | .792 |
   | C1 | 10 | .9 | non-normal | .828 | .804 | .768 | .808 | .740 |
   | C1 | 30 | .9 | non-normal | .816 | .900 | .884 | .912 | .824 |
   | A1 | 10 | .5 | normal | .932 | .828 | .816 | .844 | .732 |
   | A1 | 30 | .5 | normal | .964 | .920 | .924 | .928 | .868 |
   | A1 | 10 | .9 | normal | .928 | .764 | .780 | .828 | .672 |
   | A1 | 30 | .9 | normal | .908 | .704 | .732 | .708 | .680 |
   | A1 | 10 | .5 | non-normal | .916 | .836 | .796 | .824 | .704 |
   | A1 | 30 | .5 | non-normal | .896 | .896 | .876 | .892 | .812 |
   | A1 | 10 | .9 | non-normal | .872 | .768 | .780 | .748 | .660 |
   | A1 | 30 | .9 | non-normal | .900 | .796 | .780 | .780 | .692 |

   - **Default `boot_ci = "perc"`** (decided from this study): best
     bootstrap coverage on non-normal data with n = 30 (.912), and its lower
     bound was too high in at most 2.8% of samples for C1 (nominal 2.5%);
     BCa and stud were close but slightly worse; basic was poor everywhere.
     This does not match Ukoumunne et al. (2003), whose setting was small
     ICCs in cluster trials.
   - F intervals are best with normal data; the bootstrap helps only for
     non-normal data with ~30+ subjects. Every bootstrap type under-covers
     with 10 subjects.
   - **Agreement forms:** subject resampling treats items as fixed, so with
     random items it ignores item-sampling uncertainty (A1, ICC .9: .70–.80
     even at n = 30). Documented in `@details`, and `icc_test()` warns when
     `boot = TRUE` is used with a two-way agreement ICC (decided
     2026-10-15: warn, not block, because the bootstrap is appropriate when
     items are fixed). One-way ICCs do not warn: there is no crossed item
     effect to ignore.
   - **Alternatives that keep items random** (2026-10-15 simulation, A1,
     n = 30, ICC(C,1) = .9, σ²_items = 0.1, 250 sims × 499 replicates,
     percentile intervals; non-normal = chi-square(3) subject and item
     effects, t(5) errors; script `reli_sims/agree_boot.R`):

     | k | data | F | subject only | two-way (pigeonhole) | hybrid |
     |---|---|---|---|---|---|
     | 3 | normal | .888 | .740 | .820 | .924 |
     | 6 | normal | .932 | .764 | .908 | .932 |
     | 3 | non-normal | .772 | .744 | .880 | .900 |
     | 6 | non-normal | .760 | .812 | .904 | .904 |

     - Two-way ("pigeonhole") resampling: subjects and items resampled
       independently (McCullagh 2000; Owen 2007). Works at k = 6, too coarse
       at k = 3 (only 3^3 item draws; duplicated items).
     - **Hybrid:** resample subjects, and in each replicate multiply the
       resampled MSJ by an independent χ²(k − 1)/(k − 1) draw, adding the
       normal-theory sampling variation of the item mean square that subject
       resampling leaves out. Best of the four in every cell, including with
       skewed item effects. Proposed to replace subject-only resampling for
       two-way agreement forms (and remove the warning); needs the full grid
       (n = 10, ICC .5) before adopting. Under fixed items it would be
       conservative, consistent with the F interval, which also treats items
       as random.
     - References to verify: McCullagh, P. (2000). Resampling and
       exchangeable arrays. Bernoulli, 6(2), 285-301. Owen, A. B. (2007). The
       pigeonhole bootstrap. Annals of Applied Statistics, 1(2), 386-411.
   - **Full hybrid grid (2026-10-15)**, A1, percentile, 250 sims × 499
     replicates, σ²_items = 0.1; script `reli_sims/agree_boot_grid.R`:

     | n | ICC | k | data | F | subject | pigeonhole | hybrid |
     |---|---|---|---|---|---|---|---|
     | 10 | .5 | 3 | normal | .972 | .876 | .980 | .912 |
     | 30 | .5 | 3 | normal | .968 | .924 | .916 | .952 |
     | 10 | .9 | 3 | normal | .948 | .840 | .952 | .948 |
     | 30 | .9 | 3 | normal | .912 | .740 | .792 | .940 |
     | 10 | .5 | 6 | normal | .936 | .840 | .984 | .864 |
     | 30 | .5 | 6 | normal | .956 | .904 | .968 | .920 |
     | 10 | .9 | 6 | normal | .928 | .776 | .968 | .916 |
     | 30 | .9 | 6 | normal | .944 | .824 | .912 | .964 |
     | 10 | .5 | 3 | non-normal | .904 | .848 | .972 | .880 |
     | 30 | .5 | 3 | non-normal | .848 | .844 | .928 | .888 |
     | 10 | .9 | 3 | non-normal | .848 | .776 | .944 | .880 |
     | 30 | .9 | 3 | non-normal | .808 | .780 | .844 | .920 |
     | 10 | .5 | 6 | non-normal | .860 | .772 | .988 | .800 |
     | 30 | .5 | 6 | non-normal | .832 | .848 | .984 | .868 |
     | 10 | .9 | 6 | non-normal | .800 | .688 | .908 | .812 |
     | 30 | .9 | 6 | non-normal | .804 | .800 | .940 | .928 |

     Pre-set criteria: (1) hybrid ≥ subject-only in every cell: met (16/16);
     (2) non-normal, hybrid ≥ F − 0.02: failed 2/8 (both n = 10); (3) n = 30
     coverage in .92–.98: failed 2/8 (ICC .5, non-normal: .888, .868, where F
     was .848, .832). Remaining shortfall matches the general weakness of
     subject resampling with skewed data at moderate ICCs (also seen for C1).
     **Decision (user, 2026-10-15): adopt the hybrid** for two-way agreement
     forms; warning removed; limitations documented. Implemented:
     `.hybrid_item_ms()`; `.reli_boot()` now calls `stat_fun(prep, resample)`
     so only bootstrap replicates are perturbed (not t0 or the jackknife).

   - **Studentized interval was degenerate.** For the one-way and
     consistency forms the trigamma SE depends only on df, so it is constant
     across balanced resamples and `stud` reduced to the basic interval on
     the z scale (verified: identical limits). TOSTER (master) hit the same
     issue and moved to data-based influence-function SEs. Check of SEs for z
     (3000 sims; C1):

     | scenario | true SD | trigamma | influence | jackknife |
     |---|---|---|---|---|
     | normal, n 10, k 3 | .304 | .303 | .208 | .321 |
     | normal, n 30, k 3 | .160 | .163 | .144 | .166 |
     | non-normal, n 10, k 3 | .406 | .303 | .232 | .400 |
     | non-normal, n 30, k 3 | .244 | .163 | .187 | .235 |
     | non-normal, n 30, k 6 | .232 | .146 | .174 | .220 |

     Influence function per subject on z = ½ log(MSB/MSE): ψ_i =
     ½(ψ_B,i/MSB − ψ_E,i/MSE), ψ_B,i = k·m_i² − MSB (m_i = subject mean
     deviation after centering items), ψ_E,i = Σ_j r_ij²/(k−1) − MSE (r =
     residual after row and column effects); SE = √Σψ²/n. A crude HC4
     adaptation overshot (.357 at n 10). In a bootstrap-t, a consistent
     multiplicative SE bias cancels; what matters is tracking variability.

   - **SE validation with missing data (2026-10-15)**, mean SE / true SD of
     z, 600 sims per cell, k = 3, n = 20 / 40, REML for missing cells
     (script `reli_sims/se_validate.R`):

     | ICC(C,1) | influence | jackknife | normal |
     |---|---|---|---|
     | complete, normal | .87 / .92 | 1.02 / 1.00 | .99 / .98 |
     | complete, non-normal | .79 / .85 | 1.03 / .98 | .73 / .70 |
     | 15–30% MCAR, normal | .78–.91 | .98–1.04 | .95–.99 |
     | 15–30% MCAR, non-normal | .70–.85 | .99–1.04 | .71–.76 |
     | 15–30% MAR, normal | .75–.88 | .99–1.02 | .87–.93 |
     | 15–30% MAR, non-normal | .69–.77 | .89–1.00 | .60–.66 |

     ICC(A,1): influence .75–1.00, jackknife .93–1.08, normal good only with
     normal data. The ICC(1) rows of that run were invalid (data simulated
     with crossed item effects); rerun with one-way data (`reli_sims/se_validate_oneway.R`).
     The observed-cell influence approximation does not break with missing
     data; it shares the influence SE's general small-sample shortfall,
     worse with skew and MAR. The REML-score sandwich backup is shelved
     (sandwich estimators share the same small-sample bias).
   - **Decision (user, 2026-10-15): default `se_method = "jackknife"`**
     (option A). Accurate everywhere; cheap for ANOVA (closed-form
     leave-one-out); n refits once for REML. `stud` + REML requires
     `"influence"` (error otherwise).
     **TODO if it proves a problem:** (B) influence default with a tuned
     small-sample correction (as TOSTER's HC4-style leverage inflation);
     (C) an `"auto"` default (jackknife, except influence for REML +
     bootstrap + stud).
   - **One-way SE validation rerun** (one-way data, n = 20, 300 sims;
     `reli_sims/se_validate_oneway.R`): influence .76–.93, jackknife .95–1.11, normal
     .96–1.05 (normal data) and .71–.82 (non-normal). Same pattern as two-way.
   - **Combined bootstrap coverage study (2026-10-15)**, k = 3, σ²_items =
     0.1, B = 499; complete data 200 sims (MC SE .015), 20% MCAR with REML
     150 sims (MC SE .018); script `reli_sims/boot_coverage2.R`. Coverage:

     | form | n | ICC | data | miss | F | perc | bca | stud infl | stud jack |
     |---|---|---|---|---|---|---|---|---|---|
     | C1 | 10 | .5 | normal | 0 | .925 | .875 | .890 | .930 | .930 |
     | C1 | 30 | .5 | normal | 0 | .955 | .945 | .925 | .945 | .945 |
     | C1 | 10 | .9 | normal | 0 | .940 | .910 | .910 | .945 | .950 |
     | C1 | 30 | .9 | normal | 0 | .965 | .960 | .945 | .960 | .965 |
     | C1 | 10 | .5 | non-normal | 0 | .910 | .910 | .895 | .930 | .945 |
     | C1 | 30 | .5 | non-normal | 0 | .900 | .945 | .915 | .950 | .955 |
     | C1 | 10 | .9 | non-normal | 0 | .850 | .855 | .835 | .865 | .905 |
     | C1 | 30 | .9 | non-normal | 0 | .830 | .905 | .870 | .915 | .940 |
     | C1 | 10 | .5 | normal | .2 | .980 | .947 | .959 | .880 | — |
     | C1 | 30 | .5 | normal | .2 | .967 | .947 | .953 | .953 | — |
     | C1 | 10 | .9 | normal | .2 | .953 | .913 | .927 | .960 | — |
     | C1 | 30 | .9 | normal | .2 | .893 | .880 | .887 | .913 | — |
     | C1 | 10 | .5 | non-normal | .2 | .967 | .907 | .908 | .800 | — |
     | C1 | 30 | .5 | non-normal | .2 | .793 | .873 | .859 | .807 | — |
     | C1 | 10 | .9 | non-normal | .2 | .880 | .907 | .880 | .933 | — |
     | C1 | 30 | .9 | non-normal | .2 | .867 | .920 | .920 | .940 | — |
     | A1 | 10 | .5 | normal | 0 | .935 | .890 | .895 | .905 | .910 |
     | A1 | 30 | .5 | normal | 0 | .955 | .945 | .920 | .935 | .930 |
     | A1 | 10 | .9 | normal | 0 | .950 | .950 | .900 | .925 | .940 |
     | A1 | 30 | .9 | normal | 0 | .930 | .955 | .945 | .945 | .950 |
     | A1 | 10 | .5 | non-normal | 0 | .930 | .910 | .900 | .930 | .945 |
     | A1 | 30 | .5 | non-normal | 0 | .875 | .940 | .915 | .945 | .945 |
     | A1 | 10 | .9 | non-normal | 0 | .835 | .890 | .840 | .895 | .905 |
     | A1 | 30 | .9 | non-normal | 0 | .805 | .935 | .870 | .895 | .900 |
     | A1 | 10 | .5 | normal | .2 | .987 | .927 | .926 | .873 | — |
     | A1 | 30 | .5 | normal | .2 | .960 | .953 | .967 | .960 | — |
     | A1 | 10 | .9 | normal | .2 | .933 | .960 | .933 | .940 | — |
     | A1 | 30 | .9 | normal | .2 | .907 | .920 | .887 | .907 | — |
     | A1 | 10 | .5 | non-normal | .2 | .947 | .907 | .923 | .820 | — |
     | A1 | 30 | .5 | non-normal | .2 | .807 | .880 | .859 | .847 | — |
     | A1 | 10 | .9 | non-normal | .2 | .847 | .947 | .913 | .927 | — |
     | A1 | 30 | .9 | non-normal | .2 | .807 | .933 | .873 | .907 | — |

     - Complete data: jackknife-studentized best overall for C1 (.905–.965);
       for A1 roughly tied with percentile, whose lower bound is too high
       less often (perc .005–.035 vs stud .015–.07 of samples).
     - Missing data (REML): influence-studentized unreliable at ICC .5
       (.80–.88); percentile steadier (.87–.95).
     - BCa never beat percentile; F under-covers with non-normal data.
     - **Decision (user, 2026-10-16): `boot_ci = "auto"` default** →
       `stud` (with `se_method`, default jackknife) for `method = "anova"`,
       `perc` for `"reml"` and whenever `se_method = "normal"`. Explicit
       `boot_ci = "stud"` with REML warns (under-coverage at moderate ICCs).
       Implemented and tested; Rd Details updated with these results.
   - **`se_method = c("jackknife", "influence", "normal")`** (originally
     planned with `"influence"` as default; see decision above):
     - `icc_test()` always returns `stderr = c(ICC = , z = )` (htest
       convention, as `t.test()` and `ccc_test()`); ICC-scale SE by delta
       from z. Documented as informational: the non-bootstrap interval and
       test stay F-based.
     - `"influence"`: cheap everywhere; reported `stderr` is biased low with
       few subjects (≈0.68× at n 10, 0.90× at n 30) — document; consider a
       tuned small-sample correction later.
     - `"jackknife"`: delete-one-subject; closed-form-cheap for ANOVA, n lmer
       fits for REML.
     - `"normal"`: trigamma / normal-theory delta; `boot_ci = "stud"` with
       `"normal"` is an error (constant SE cannot studentize).
     - REML with missing data, `"influence"`: observed-cell moment
       approximation (per-subject item-adjusted mean deviation and
       within-subject residual variance, centred at REML estimates); validate
       with MCAR/MAR. Backup if it fails: REML-score sandwich (e.g.
       `merDeriv::estfun()`; check crossed-effects support).
     - Agreement forms: influence for MSB/MSE plus normal-theory variance
       2·MSJ²/dfJ for MSJ, consistent with the hybrid.
     - `reli_err_test()` gets the same argument.
     - Combined coverage study (perc, bca, stud × each SE; complete and
       15–30% missing; normal/non-normal; n 10/30) sets the defaults for
       `boot_ci` and `se_method`.
   - Agreement-form tests and intervals are not exact inverses (different
     Satterthwaite df at ρ0 vs ρ̂). On the Shrout & Fleiss data (n = 6),
     testing at the ICC(A,1) lower limit gives p = 0.0015, not 0.05; the
     upper limit gives 0.052. Exact forms invert exactly (tested). Decide in
     phase 2 whether `icc_test()` reports this as is (McGraw & Wong) or
     documents the mismatch.
3. `reli_err_test()` (raw scale). **Done 2026-10-16:** `R/reli_err_test.R`,
   `tests/testthat/test-reli_err_test.R`, `man/reli_err_test.Rd`, pkgdown
   entry; `summary()` generalised (`.describe_error_estimand()`,
   `.describe_error_inference()`). The `scale` argument is not in the
   signature yet (phase 4). Decisions (user, 2026-10-16, unless noted):
   - **Interval without a test is two-sided.** `alternative` defaults to
     `"less"` but shapes the interval only when `null.value` is given; with
     `null.value = NULL` the stored `alternative` is `"two.sided"`.
   - **SEE/SEP analytic interval: Wald on the log scale** with the
     normal-theory delta SE (`.err_se_log(..., "normal")`), back-transformed;
     the z test uses the same SE, so test and interval invert exactly.
   - `se_method` does not change the analytic interval or test (as in
     `icc_test()`); it sets `stderr = c(<stat>, log)` and the SE used by the
     studentized bootstrap (log scale). `"normal"` for the SEM rows is the
     exact 0.5·√trigamma(ν/2).
   - A negative ICC estimate is truncated at 0 in SEE/SEP (SEE = 0,
     SEP = SD) (implementation choice; documented in the Rd).
   - Refactor: `.icc_se_z()` now calls a generic `.subject_se()` and
     `.jackknife_var()` (any statistic of the mean squares), and the
     `boot_ci = "auto"` resolution and `stud` checks moved to
     `.resolve_boot_ci()`, shared by both functions. ICC tests unchanged and
     passing.
   - Weir (2005) targets reproduced: SEM 7.56 (A), 2.17 (C); one-way 7.33
     (A), 11.15 (C); matched SEE 7.35 and SEP 10.54 (A). (His SS table is
     rounded: error SS is 399.75 in the data, not 399.8; tests compute the
     mean squares from the data.)
   - **Error-statistic coverage study (2026-10-16)**, k = 3, σ²_id = 1,
     σ²_items = 0.1, σ²_e from ICC(C,1); ANOVA, complete data; 300 sims ×
     499 replicates per cell (MC SE .013); non-normal = chi-square(3)
     subject and item effects, t(5) errors; SEM_1 uses one-way data (a
     random item effect in every cell). Script
     `reli_sims/err_boot_coverage.R` (about 10 minutes on 10 cores).
     Columns: `an` = analytic (χ² for SEM, log-Wald normal-theory delta for
     SEE/SEP); `waldjk` = log-Wald with the jackknife SE (not an option,
     recorded for information); bootstrap `perc`, `bca`, `stud` (jackknife
     SE, log scale), all hybrid for the agreement rows; `subj` =
     subject-only percentile.

     **Pre-set criteria:** (1) hybrid ≥ subject-only − 0.02 in every cell
     and ≥ .92 with normal data at n = 30; (2) keep `auto` = `stud` for
     ANOVA if stud mean coverage ≥ perc mean − 0.01 and no n = 30 cell
     below .88; (3) analytic SEE/SEP log-Wald ≥ .92 with normal data at
     n = 30.

     | stat | n | ICC | data | an | waldjk | perc | bca | stud | subj |
     |---|---|---|---|---|---|---|---|---|---|
     | SEM_C | 10 | .5 | normal | .953 | .943 | .823 | .897 | .947 | — |
     | SEM_C | 30 | .5 | normal | .960 | .920 | .893 | .883 | .933 | — |
     | SEM_C | 10 | .9 | normal | .943 | .940 | .793 | .827 | .937 | — |
     | SEM_C | 30 | .9 | normal | .933 | .940 | .907 | .910 | .947 | — |
     | SEM_C | 10 | .5 | non-normal | .867 | .853 | .720 | .757 | .907 | — |
     | SEM_C | 30 | .5 | non-normal | .810 | .903 | .827 | .877 | .917 | — |
     | SEM_C | 10 | .9 | non-normal | .847 | .870 | .707 | .740 | .893 | — |
     | SEM_C | 30 | .9 | non-normal | .847 | .923 | .850 | .900 | .950 | — |
     | SEM_A | 10 | .5 | normal | .963 | .947 | .947 | .967 | .937 | .870 |
     | SEM_A | 30 | .5 | normal | .950 | .947 | .917 | .947 | .917 | .857 |
     | SEM_A | 10 | .9 | normal | .910 | .867 | .870 | .950 | .903 | .627 |
     | SEM_A | 30 | .9 | normal | .837 | .827 | .833 | .920 | .853 | .453 |
     | SEM_A | 10 | .5 | non-normal | .873 | .880 | .877 | .910 | .903 | .763 |
     | SEM_A | 30 | .5 | non-normal | .857 | .897 | .903 | .933 | .920 | .810 |
     | SEM_A | 10 | .9 | non-normal | .840 | .853 | .853 | .897 | .887 | .550 |
     | SEM_A | 30 | .9 | non-normal | .683 | .720 | .730 | .800 | .763 | .400 |
     | SEM_1 | 10 | .5 | normal | .947 | .880 | .847 | .847 | .910 | — |
     | SEM_1 | 30 | .5 | normal | .937 | .910 | .900 | .877 | .907 | — |
     | SEM_1 | 10 | .9 | normal | .930 | .897 | .867 | .880 | .913 | — |
     | SEM_1 | 30 | .9 | normal | .960 | .957 | .913 | .943 | .963 | — |
     | SEM_1 | 10 | .5 | non-normal | .890 | .877 | .787 | .823 | .897 | — |
     | SEM_1 | 30 | .5 | non-normal | .837 | .907 | .863 | .877 | .927 | — |
     | SEM_1 | 10 | .9 | non-normal | .897 | .860 | .813 | .827 | .910 | — |
     | SEM_1 | 30 | .9 | non-normal | .880 | .920 | .870 | .877 | .923 | — |
     | SEE_C | 10 | .5 | normal | .966 | .965 | .680 | .737 | .845 | — |
     | SEE_C | 30 | .5 | normal | .970 | .970 | .847 | .877 | .870 | — |
     | SEE_C | 10 | .9 | normal | .933 | .953 | .737 | .793 | .937 | — |
     | SEE_C | 30 | .9 | normal | .940 | .933 | .897 | .900 | .923 | — |
     | SEE_C | 10 | .5 | non-normal | .900 | .934 | .543 | .638 | .861 | — |
     | SEE_C | 30 | .5 | non-normal | .860 | .933 | .717 | .800 | .839 | — |
     | SEE_C | 10 | .9 | non-normal | .857 | .872 | .627 | .687 | .902 | — |
     | SEE_C | 30 | .9 | non-normal | .847 | .923 | .813 | .887 | .930 | — |
     | SEP_C | 10 | .5 | normal | .943 | .940 | .757 | .837 | .933 | — |
     | SEP_C | 30 | .5 | normal | .953 | .937 | .867 | .893 | .923 | — |
     | SEP_C | 10 | .9 | normal | .937 | .943 | .773 | .800 | .930 | — |
     | SEP_C | 30 | .9 | normal | .947 | .940 | .907 | .917 | .930 | — |
     | SEP_C | 10 | .5 | non-normal | .803 | .850 | .637 | .700 | .907 | — |
     | SEP_C | 30 | .5 | non-normal | .787 | .890 | .800 | .847 | .913 | — |
     | SEP_C | 10 | .9 | non-normal | .837 | .873 | .653 | .713 | .890 | — |
     | SEP_C | 30 | .9 | non-normal | .850 | .927 | .827 | .890 | .943 | — |
     | SEP_A | 10 | .5 | normal | .950 | .957 | .930 | .973 | .937 | .833 |
     | SEP_A | 30 | .5 | normal | .947 | .950 | .930 | .943 | .907 | .857 |
     | SEP_A | 10 | .9 | normal | .853 | .860 | .850 | .950 | .903 | .583 |
     | SEP_A | 30 | .9 | normal | .813 | .833 | .830 | .913 | .853 | .440 |
     | SEP_A | 10 | .5 | non-normal | .817 | .853 | .843 | .927 | .900 | .717 |
     | SEP_A | 30 | .5 | non-normal | .810 | .890 | .867 | .913 | .903 | .777 |
     | SEP_A | 10 | .9 | non-normal | .830 | .857 | .840 | .890 | .880 | .537 |
     | SEP_A | 30 | .9 | non-normal | .653 | .717 | .713 | .797 | .767 | .390 |

     Mean coverage (an / waldjk / perc / bca / stud): SEM_C .895 / .912 /
     .815 / .849 / .929; SEM_A .864 / .867 / .866 / .915 / .885; SEM_1
     .910 / .901 / .858 / .869 / .919; SEE_C .909 / .936 / .733 / .790 /
     .888; SEP_C .882 / .912 / .777 / .825 / .921; SEP_A .834 / .865 / .850
     / .913 / .881. Subject-only percentile: SEM_A .666, SEP_A .642.

     Against the criteria:
     - (1) Hybrid vs subject-only: first part met everywhere (subject-only
       collapses to .39–.87, worst at ICC .9 where the item variance is a
       large share of the error). Second part fails at ICC .9 (SEM_A .833,
       SEP_A .830), but the analytic Satterthwaite interval fails there
       too (.837, .813): with k = 3 the item mean square has 2 df and
       dominates the error variance, which no method captures well. BCa
       (hybrid) is the best method for both agreement rows (.913–.950 with
       normal data).
     - (2) Fails as written: stud has n = 30 cells below .88 (SEM_A and
       SEP_A at ICC .9: .853, .763–.767; SEE_C at ICC .5: .839, .870), but
       perc is worse in each of those cells. stud beats perc on average for
       every statistic and is the best method for SEM_C, SEM_1 and SEP_C;
       BCa beats stud for SEM_A and SEP_A in 7 of 8 cells.
     - (3) Met for SEE_C and SEP_C (.940–.970); fails for SEP_A at ICC .9
       (.813), the same item-variance problem as (1).
     - Not anticipated: the analytic intervals (including the exact χ² SEM
       interval) under-cover with non-normal data (.79–.87; kurtosis of the
       errors), while a log-Wald interval with the jackknife SE is close to
       nominal for the consistency rows (.85–.97). SEE near ICC .5 is
       poorly suited to the bootstrap (SEE is flat in the ICC there); the
       analytic interval is best for SEE.
     - **Decisions (user, 2026-10-16):** adopt the hybrid for the agreement
       SEM/SEP and document the few-items limitation (Rd "Agreement
       statistics with few items"); `reli_err_test()` `boot_ci = "auto"` =
       `bca` for two-way agreement statistics and `stud` otherwise with
       ANOVA, `perc` with REML (`.resolve_boot_ci(agreement_bca = )`);
       `icc_test()` keeps its rule. Results summarised in the Rd Details.
     - TODO (not decided): offer the log-Wald interval with the jackknife SE
       as an analytic option for non-normal data (e.g. a `ci_method`
       argument); it beat the χ²/normal-delta intervals with non-normal
       data at little cost.
   - **`null.value = NULL` default in `icc_test()` too** (user,
     2026-10-16): no F test unless a null is given; without a test the
     interval is two-sided whatever `alternative` says (same rule as
     `reli_err_test()`). Tests that need the F test now pass
     `null.value = 0`.
   - **`summary(style = c("auto", "hypothesis", "estimation"))`** (user,
     2026-10-16). `"hypothesis"`: the existing Neyman–Pearson / TOSTER
     wording. `"estimation"` (Greenland): "compatibility interval" /
     "compatibility bound", no α or decision; with a p-value, the S-value
     s = −log₂ p for the tested hypothesis (the complement of
     `alternative`: "at least", "at most" or "equal to" the null), with the
     coin-toss analogy (round(s) heads in a row) and "(approximate, from
     the bootstrap)" for bootstrap p-values. `"auto"` = hypothesis if
     there is a p-value, else estimation; `"hypothesis"` without a p-value
     falls back to estimation with a message. Returned object gains
     `style` and `s.value`. Snapshot tests added (`tests/testthat/_snaps/`).
     References: Greenland (2019), Rafi & Greenland (2020) (metadata
     checked on CrossRef). TODO: a p-value function / S-values at several
     nulls (e.g. `summary(null.values = )`); same summary for
     `ccc_test()`.
4. `reli_err_test()` `scale = "log"` / `"mean"`. **Implemented
   2026-10-16** (`scale = c("raw", "log", "mean")` after `stat`):
   - `"log"`: the engine is fitted to `log(values)` (error if any value
     ≤ 0); estimate, interval and `stderr` are 100 × the log-scale values,
     so the SEM keeps exact/Satterthwaite χ² inference and the χ² test uses
     `null.value / 100`. `data` keeps the original measurements (for
     `plot()`); `fit` is on the log scale. Label `"SEM (sympercent)"`.
   - `"mean"`: 100 × statistic / grand mean (error if the grand mean ≤ 0).
     Every statistic (SEM too) uses the log-Wald interval and z test with
     Var(log est) = Var(log stat) + Var(ȳ)/ȳ², `.grand_mean_var()`:
     MSB/(nk) (consistency, one-way), (MSB + max(MSJ − MSE, 0))/(nk)
     (two-way agreement); nk = N with missing data. Label
     `"SEM (% of mean)"`.
   - Subject-level SEs include the grand mean generically: `.subject_se()`
     adds an influence column for ȳ (`.grand_mean_influence()`: n·nᵢ(ȳᵢ −
     ȳ)/N) with the numerical gradient of g in ȳ, and `.jackknife_var()`
     updates the leave-one-out grand mean; with random items it adds
     g_ȳ²·(MSJ − MSE)/(nk). The gradient is exactly 0 for statistics that
     do not use ȳ, so ICC SEs are unchanged (tested).
   - Hybrid bootstrap for the mean scale (`.hybrid_item_ms(shift_mean =
     TRUE)`): each replicate's grand mean also gets an independent
     N(0, (MSJ − MSE)/(nk)) draw, the item-mean variation that subject
     resampling misses.
   - Docs: Rd "Coefficient of variation" section (sympercent ↔ CV%
     conversion, Var(ȳ) formulas, log preferred), `@concept coefficient of
     variation`, Atkinson & Nevill (1998) and Cole (2000) references;
     summary says "in sympercents …" / "as a percentage of the grand mean"
     and mentions the grand mean in the inference sentence.
   - **`scale = "mean"` coverage study (2026-10-16)**, same design as the
     phase-3 study with population mean μ = 5 (truth = 100 × statistic /
     μ); 300 sims × 499 replicates (MC SE .013); script
     `reli_sims/err_mean_coverage.R` (about 10 minutes). Columns: `an` =
     analytic log-Wald with Var(ȳ); `nogm` = the same without Var(ȳ) (for
     information); `waldjk` = log-Wald with the jackknife SE; bootstrap
     (hybrid with the grand-mean shift for SEM_A); `subj` = subject-only
     percentile.

     **Pre-set criteria:** (1) analytic ≥ .92 with normal data at n = 30
     for SEM_C, SEP_C, SEM_1 and SEM_A at ICC .5; (2) keep the Var(ȳ) term
     if dropping it lowers normal-data coverage; (3) keep the phase-3
     `auto` rule unless another method beats it by > .02 in mean coverage.

     | stat | n | ICC | data | an | nogm | waldjk | perc | bca | stud | subj |
     |---|---|---|---|---|---|---|---|---|---|---|
     | SEM_C | 10 | .5 | normal | .943 | .930 | .950 | .853 | .893 | .957 | — |
     | SEM_C | 30 | .5 | normal | .933 | .897 | .903 | .873 | .883 | .930 | — |
     | SEM_C | 10 | .9 | normal | .947 | .933 | .923 | .793 | .837 | .943 | — |
     | SEM_C | 30 | .9 | normal | .950 | .927 | .947 | .917 | .923 | .940 | — |
     | SEM_C | 10 | .5 | non-normal | .853 | .833 | .867 | .747 | .787 | .910 | — |
     | SEM_C | 30 | .5 | non-normal | .833 | .783 | .910 | .843 | .877 | .917 | — |
     | SEM_C | 10 | .9 | non-normal | .853 | .827 | .870 | .753 | .773 | .903 | — |
     | SEM_C | 30 | .9 | non-normal | .850 | .827 | .917 | .867 | .880 | .930 | — |
     | SEM_A | 10 | .5 | normal | .950 | .923 | .943 | .943 | .967 | .943 | .887 |
     | SEM_A | 30 | .5 | normal | .943 | .937 | .953 | .943 | .950 | .943 | .887 |
     | SEM_A | 10 | .9 | normal | .877 | .857 | .870 | .873 | .943 | .917 | .657 |
     | SEM_A | 30 | .9 | normal | .833 | .827 | .847 | .857 | .920 | .867 | .487 |
     | SEM_A | 10 | .5 | non-normal | .883 | .863 | .893 | .893 | .933 | .920 | .803 |
     | SEM_A | 30 | .5 | non-normal | .880 | .843 | .930 | .927 | .947 | .930 | .843 |
     | SEM_A | 10 | .9 | non-normal | .867 | .843 | .880 | .883 | .917 | .903 | .613 |
     | SEM_A | 30 | .9 | non-normal | .713 | .690 | .767 | .773 | .857 | .800 | .447 |
     | SEP_C | 10 | .5 | normal | .940 | .917 | .933 | .803 | .877 | .947 | — |
     | SEP_C | 30 | .5 | normal | .927 | .877 | .910 | .853 | .883 | .913 | — |
     | SEP_C | 10 | .9 | normal | .947 | .910 | .930 | .763 | .830 | .937 | — |
     | SEP_C | 30 | .9 | normal | .943 | .923 | .947 | .907 | .903 | .930 | — |
     | SEP_C | 10 | .5 | non-normal | .843 | .800 | .860 | .670 | .723 | .890 | — |
     | SEP_C | 30 | .5 | non-normal | .837 | .777 | .877 | .803 | .853 | .903 | — |
     | SEP_C | 10 | .9 | non-normal | .843 | .817 | .867 | .720 | .720 | .900 | — |
     | SEP_C | 30 | .9 | non-normal | .850 | .823 | .903 | .853 | .873 | .927 | — |
     | SEM_1 | 10 | .5 | normal | .947 | .907 | .900 | .860 | .867 | .930 | — |
     | SEM_1 | 30 | .5 | normal | .960 | .913 | .920 | .890 | .900 | .920 | — |
     | SEM_1 | 10 | .9 | normal | .943 | .913 | .903 | .863 | .863 | .917 | — |
     | SEM_1 | 30 | .9 | normal | .953 | .940 | .947 | .930 | .950 | .960 | — |
     | SEM_1 | 10 | .5 | non-normal | .890 | .843 | .890 | .813 | .840 | .927 | — |
     | SEM_1 | 30 | .5 | non-normal | .857 | .813 | .907 | .873 | .887 | .933 | — |
     | SEM_1 | 10 | .9 | non-normal | .910 | .880 | .867 | .823 | .837 | .910 | — |
     | SEM_1 | 30 | .9 | non-normal | .887 | .873 | .927 | .903 | .907 | .923 | — |

     Mean coverage (an / nogm / waldjk / perc / bca / stud): SEM_C .895 /
     .870 / .911 / .831 / .857 / .929; SEM_A .868 / .848 / .885 / .887 /
     .929 / .903 (subject-only .703); SEP_C .891 / .855 / .903 / .797 /
     .833 / .918; SEM_1 .918 / .885 / .907 / .870 / .881 / .927.

     Against the criteria: (1) met: .927–.960 (SEM_A at ICC .9 under-covers,
     .833–.877, as in phase 3); (2) met: dropping Var(ȳ) lowers coverage in
     15 of 16 normal cells, by up to .05, so the term stays; (3) met: stud is
     best for SEM_C, SEP_C and SEM_1 and BCa for SEM_A, the phase-3 rule.
     The analytic intervals again under-cover with non-normal data (.71–.91),
     as on the raw scale. The hybrid with the grand-mean shift beats
     subject-only resampling for SEM_A in every cell. **No changes needed;
     phase 4 accepted** (pending the user's review).
5. `power_icc()` and `precision_icc()`. **Done 2026-10-16:**
   `R/power_icc.R`, `tests/testthat/test-power_icc.R`, Rd files, pkgdown
   entries. Signatures as in Section 6.3.
   - **Exact power:** the scaled central F (one `pf()` per tail; two-sided
     = both tails at α/2). **Exact assurance:** the interval width is
     unimodal in log F, so P(width ≤ w) = P(F ≤ f_lo) + P(F ≥ f_hi), found
     with `optimize()` and two `uniroot()`s (`.icc_assurance_exact()`).
     A deterministic test checks it against a dense grid over the F
     distribution.
   - **Approximate:** the Walter closed form inverted for power (α/2 for
     two-sided); Zou's n formula, with assurance from its algebraic
     inverse (`.icc_assurance_zou()`; below the median-width n it returns
     0.5 with a warning, since the formula does not cover assurance < 0.5).
     Solving `width` or `k` with `"approximate"` inverts through n.
   - **Solving:** `uniroot()` for n (lower end 2; if the target is already
     met at n = 2, 2 is returned), rho1, sig.level and width; integer
     search 2–1000 for k (tolerance 1e-8 so round trips at the boundary
     work). NOTE lines: round up n; two-sided rho1 root above rho0; n/k
     meaning (and width = upper − lower for `precision_icc()`).
   - **Mondal et al. (2024), read 2026-10-16** (PMC10981208; authors
     Mondal, Vanbelle, Cassese & Candel; metadata checked on CrossRef):
     the error they report is in Zou's eq. 3, the variance of the log-F
     (ZFρ) statistic, which our approximate method does not use. They also
     note that Zou derived his formula with n − 1 and the half-width, so
     their own Wald/Fisher coefficients differ slightly (e.g. 469 vs 470
     for ρ .7, width .1, assurance .9). They recommend the Searle (F)
     interval, which is our exact method. Zou's formula is kept as
     published (it reproduces Zou's and PASS's tables).
   - **All Section 6.3 targets reproduced:** Walter 543.16 and the five
     closed-form values; PASS exact power .18333, .29534, .45522, .74538,
     and .8003 at n = 544; Zou n = 183; PASS assurance .9 n = 196, 96, 77,
     70 (Zou formula); lower limit 300 (approximate) and 301 (exact).
     **Additional targets from Mondal et al. (Searle/F, full width):**
     assurance .9: n = 473 (ρ .7, width .1) and 81 (ρ .8, width .2);
     median width: 403 (ρ .7, width .1); testing: 162 (ρ₀ .7, ρ₁ .8,
     power .9, one-sided). Exact n for the PASS assurance table: 195.3,
     93.8, 74.8, 67.0 (k = 2, 4, 6, 8), i.e. the Zou approximation is
     slightly conservative for k > 2.
   - Slow tests (`SIMPLYAGREE_SLOW_TESTS=true`): exact power vs simulated
     `icc_test()` rejection rate; median width vs simulated `icc_test()`
     interval widths (a first 2000-rep run gave .527, 2.4 MC SE; reruns
     gave .499 with simulated F ratios and .504 with 6000 fits, so the
     test uses 4000 reps).
6. Supersede and rewire `reli_stats()`/`reli_aov()`; jamovi; NEWS; vignette
   rewrite (including "Coefficient of variation" and "MDC, SDD and the
   repeatability coefficient" sections); fix all documentation errors in
   Section 2. **Done 2026-10-16.** Decisions (user, 2026-10-16):
   - **Legacy bootstrap → engine subject bootstrap** in both functions
     (`.legacy_other_boot()`: subject resampling with the hybrid step,
     `.hybrid_item_ms(shift_mean = TRUE)`; `type` = perc / basic / norm,
     norm = est − bias ± z·SE as `boot.ci`). `bootMer`, the duplicated
     `boot::boot` loop, `reli_mod_mse()` (`R/reli_functions.R`, deleted),
     `cv_ci()` and `sigma_ci()` are gone. Bug 9 is moot.
   - **`type = "chisq"` = analytic, name kept:** exact χ² for √MSE;
     log-scale delta method (MSB, MSJ, MSE and the grand mean, with
     Var(ȳ) = MSB/(nk)) for the CV, ICC-based SEM, SEE and SEP
     (`.legacy_other_analytic()`).
   - **`reli_aov()` drops incomplete subjects with a message**
     (`.complete_subjects()`), catching missing rows too (bug 3).
   - **jamovi switched to the new functions:** six `icc_test()` calls
     (ANOVA, or REML with a note when cells are missing), variance
     components from the ICC(C,1) fit, SEM/SEE/SEP from `reli_err_test()`
     with intervals (matched SD), CV in sympercents (`% of mean` if any
     value ≤ 0; omitted if the mean ≤ 0); plot state = the ICC(C,1)
     `reli_htest`. Table definitions (`.r.yaml`, `jmvreli.h.R`) unchanged.
   - Legacy definitions kept: observed SD √(SS_total/(N − 1)) for
     SEE/SEP/ICC-based SEM, written in mean squares
     (dfB·MSB + dfJ·MSJ + dfE·MSE, so the delta method applies);
     `se_type = "MSE"` uses ICC3 for SEE/SEP (now documented); negative
     ICCs truncated at 0. `cv_calc = "residuals"` → `"MSE"` with a warning.
   - Output structure kept (`icc`, `lmer`, `anova`, `var_comp`, `n.id`,
     `n.item`, `call`, `cv`, `SEM`, `SEE`, `SEP`). `lmer` is now an `lm`
     (not `aov`) for `reli_aov()`; the call stores evaluated
     `conf.level`/`other_ci` (bug 2). Variance components now come from
     the engine (fixes `reli_aov()`'s item variance MSW − MSE and the
     missing truncation of the subject variance).
   - Long data with duplicate subject × item cells now errors (engine
     rule); noted in NEWS. The `reps` data have 5% missing cells, so
     `reli_stats(reps)` now warns; `test-plot_state.R` suppresses it.
   - Tests: `test-reli_stats.R` rewritten in edition 3 (structure, S&F
     two-sided CIs, conf.level mapping, REML = ANOVA, Weir SEM/SEE/SEP,
     cv_calc, analytic and bootstrap intervals, missing data, print with
     variables, jamovi). The `agree_coef()` assertions moved unchanged to
     `test-agree_coef.R`. jmvcore resolves `vars` as a quosure: pass a
     literal vector in tests.
   - NEWS: "Reliability" section at the top of 0.3.1. Vignette rewritten
     around `icc_test()`/`reli_err_test()`/`power_icc()`; corrected
     formulas (α/2, ICC(2,k) numerator, Satterthwaite a/b form); new refs
     in `vignettes/refs.bib` (Hopkins, Koo & Li, Cole, Atkinson & Nevill,
     Greenland, Rafi & Greenland, Walter et al., Zou, Mondal et al.).
     `agree_tests.Rmd` now points to the new functions.
7. `devtools::check()` clean; coverage ≥ 90% on new files. **Done
   2026-10-16.** `covr` 3.6.5 installed on the Windows machine (user
   approved). Line coverage from the fast tests: `reli_err_test.R` 100%,
   `reli_engine.R` 98.0%, `methods.reli_htest.R` 97.8%, `reli_stats.R`
   97.8%, `icc_test.R` 97.6%, `power_icc.R` 96.4%, `jmvreli.b.R` 90.1% before
   a test of its CV fallbacks was added (the jamovi `.plot` render cannot be
   reached from tests). All slow tests pass with
   `SIMPLYAGREE_SLOW_TESTS=true`. The pre-existing `calc_loa_reps`
   `mxi`/`myi` NOTE is fixed (`R/globals.R`). `R CMD check`: 0 errors, 0
   warnings, 2 environment NOTEs (`ggeffects`/`deming` not installed;
   "unable to verify current time").

8. **Reliability plot (phase 8), done 2026-10-17.** Seven candidates were
   rendered on the Shrout & Fleiss data, Weir data set A and a simulated
   n = 30, k = 3 set with a learning effect and multiplicative error
   (scratch script, not kept): A current (per-subject colours, legend: the
   legend is useless at n = 30); B refined profile; C trial vs trial with
   identity (clear for k = 2, busy for k = 4); D within-subject SD vs
   subject mean (decisive at n = 30, noise at n = 6–8); E subjects ranked
   by mean (closest picture of the ICC); F deviations by item (item
   effects); G variance-component shares (unreadable small slices;
   dropped). **Decisions (user):**
   - `plot.reli_htest(type = c("profile", "subjects", "items", "pairs"))`,
     default `"profile"` (B): grey subject lines and points, item means with
     95% CIs, no legend. Item means are adjusted for subjects (average of
     the fitted `lm(values ~ id + items)` over the full grid, SE from its
     vcov), which equals the raw means with complete data and removes the
     bias from who is missing otherwise. `"subjects"` (E; subject labels
     hidden above 15 subjects), `"items"` (F), `"pairs"` (C; incomplete
     pairs dropped).
   - `check.reli_htest()` gains a third panel (D): within-subject SD
     against the subject mean with a linear trend and Kendall's tau, on the
     fitted scale (log values for `scale = "log"`, where the "consider
     scale = log" hint is dropped). Omitted when fewer than 3 subjects have
     2+ measurements.
   - jamovi follows automatically (its plot state is an `icc_test()`
     result). Legacy `plot.simple_reli()` unchanged.
   - Tests in `test-icc_test.R`; vignette section "Plots and
     diagnostics"; NEWS line.

9. **`ccc_test()` methods (2026-10-17).** For consistency with `icc_test()`
   (the CCC is close to ICC(A,1) for two methods; kept separate because it
   is meant for agreement studies). Decisions (user): class
   `c("ccc_htest", "htest")` storing `data`, `settings` and, for simple
   data, Lin's `components` (r, C_b); `null.value = NULL` default, keyword
   `alternative` (it was stored as a sentence, so `print()` showed "true CCC
   is  0.5"), one-sided Fisher-z interval for a one-sided alternative
   (`.ccc_ci()`; two-sided values unchanged, tested). `summary.ccc_htest()`
   reuses the shared `.resolve_summary_style()`/`.htest_result_text()`
   (factored out of `summary.reli_htest()`; its snapshots unchanged);
   `plot(type = c("identity", "ba"))` (identity: line of identity plus
   Deming line; ba: `plot()` of an `agreement_limit()` refit);
   `check()`: normality and heteroskedasticity of the differences
   (`.loa_check_panels()`, split out of `check.loa()`), linearity (residuals
   of y on x vs fitted, quadratic RESET test), and for simple data a Q-Q of
   squared Mahalanobis distances against chi-square(2). Proportional bias
   was dropped from the CCC check (user, 2026-10-18): a slope ≠ 1 lowers
   C_b, so it is part of what the CCC measures, not an assumption.
   `check()` for `reli_htest` (two-way models) gained an additivity panel
   with **Mandel's (1961) column-regression test** (user, 2026-10-18):
   item-specific slopes on the estimated subject effects of
   `lm(values ~ id + items)`, k − 1 df, built from fitted values as in
   Milliken & Graybill (1970) so it stays exact with missing cells
   (`.mandel_test()`); the panel plots additive-model residuals against the
   subject effect, one line per item. Tukey's 1-df test was tried first
   and rejected: it only detects the multiplicative pattern (slope ∝ item
   mean) and missed an item with 1.6× scale but the same mean (p = 0.80 vs
   Mandel p ≈ 5e-28). Check (scratch script; 2000 reps, n = 30, k = 3, 3
   missing cells, additive normal data): type I error Tukey 0.051, Mandel
   0.0525; multiplicative items: both detect; k = 2: identical p-values.
   Shared helpers in `R/other_functions.R`: `.mandel_test()`,
   `.fitted_square_test()` (RESET, used by the CCC linearity panel),
   `plot_resid_fitted()`. No `se_method` or
   bootstrap (TODO below). Bug fixed on the way: the fallback SE for simple
   data used all non-missing x and y rather than complete pairs, so with
   missing values the reported SE and z test did not match the CCC and its
   interval.

10. **`unit = "average"` for `power_icc()`/`precision_icc()` (user,
    2026-10-18).** Exact, via Spearman-Brown: the ICC(k)/ICC(C,k) test is
    the single-measure F test at the projected values (power computed on
    the single scale; `rho1` solved there and projected back), and the
    average-measure interval has limits 1 − a/F and 1 − 1/(bF), so its
    width (a − 1/b)/F is monotone in F and the assurance is one F
    probability (tested against a dense grid and against `icc_test()`'s
    interval width). `k` cannot be solved for with `"average"` (it defines
    the estimand); the Zou approximation is single-measure only. The Rd
    gained "Planning other designs": agreement ICC (consistency result is
    an optimistic lower bound on n, worst with 2–3 items), missing data
    (inflate n by 1/(1 − p)), non-normal data (allow a margin).
    Simulation-based power for the agreement ICC stays a TODO (below).

11. **Cell missingness in power/precision: approximate method checked
    (2026-10-18).** Method: the exact scaled-F calculation with observed-data
    quantities, N = nk(1 − p), kbar = N/n, error df N − n − k + 1 (two-way)
    or N − n (one-way), as `icc_test(method = "reml")` uses. Script
    `reli_sims/power_missing.R`: 32 cells (one-way ICC(1) and two-way
    ICC(C,1); n 15/40; k 3/5; none, 15% MCAR, 30% MCAR, 30% MAR, item 1
    always observed), 1000 reps of the REML analysis under ρ₁ (complete-data
    power 0.60 against ρ₀ = 0.6, one-sided α = .05) and under ρ₀. Pre-set
    criteria: power within ±0.03 (MCAR), size 0.035–0.065, median 95% CI
    width within 5%.

    | | two-way | one-way |
    |---|---|---|
    | approx − sim power, MCAR 15–30% | −0.018 to +0.006 | −0.024 to +0.033 |
    | approx − sim power, MAR 30% | +0.004 to +0.017 | −0.020 to +0.016 |
    | approx − sim power, no missing (approx exact) | −0.010 to −0.002 | −0.011 to +0.019 |
    | median width approx / sim | 0.979–1.017 | 0.978–1.018 |
    | sim size, MCAR 15–30% | 0.051–0.065 | 0.043–0.069 |
    | sim size, MAR 30% | 0.058–0.073 | 0.055–0.071 |

    Power: met within MC error (MC SE 0.016; one cell 0.033, and the
    complete-data cells, where the approximation is exact, scatter by up to
    0.019). Width: met easily, MAR included. Size: the REML analysis itself
    is slightly liberal at 30% missing (as in the phase-1 coverage study);
    the approximation predicts power for the test as it behaves. MAR here
    removed ~31–33% of cells for a nominal 30%; using the observed
    proportion changes the approximation by < 0.01. Conclusion: good enough
    to offer as an explicitly approximate option without simulation.
    **Implemented (user, 2026-10-18):** `missing = 0` in `power_icc()` and
    `precision_icc()` (`.icc_design()` gives kbar, df1, df2; used by the
    power, width and assurance helpers; with `unit = "average"` the
    projection uses all k items and the F scale uses kbar, as
    `icc_test()` does). With `missing > 0`: the method string says
    "observed-data df; approximate", a NOTE describes the assumption, the
    result gains a `missing` element, `method = "approximate"` (Walter/Zou)
    is refused, and a warning is given above 30% (the studied range).
    Whole-subject loss is planned by reducing n (documented).

## 10. TODO (future, not this branch)

- ~~Cell-level missingness in `power_icc()`/`precision_icc()`~~ — done
  2026-10-18 as `missing = 0` (approximate method, validated in Section 9,
  item 11). Remaining: patterns beyond the study (more than 30% missing;
  missingness concentrated in some subjects or items; MNAR) would still
  need simulation, alongside the agreement-ICC simulation TODO.

- **`se_method` and subject bootstrap for `ccc_test()`.** Lin's SE (simple
  data) is a normal-theory delta method and probably under-covers with
  non-normal data and few subjects, as the F interval did for the ICC; the
  U-statistic SE (reps/nest) is already nonparametric. A jackknife SE and
  a subject bootstrap (reusing `.reli_boot()`-style helpers) would need a
  coverage study before choosing defaults.

- **Bootstrap under the null** for `boot = TRUE`: resample data transformed
  so the parameter equals `null.value` (or simulate from a model constrained
  to it, e.g. ICC = ρ₀ with nuisance variance components re-estimated) and
  compare the observed estimate with that null distribution. A proper test,
  replacing the approximate CI-inversion p-value. Needs a way to impose the
  null on subject-resampled data or a constrained fit; research first.
- ~~**MDC converter**~~ — done 2026-10-19 as the standalone `mdc_test()`
  (`R/mdc_test.R`): from data or an existing SEM result, `mdc.level`,
  `sided`, `k_avg` (same number of trials averaged at both occasions; unequal
  `k1`/`k2` not offered), scaled interval, test against an anchored change
  via the SEM test of `null.value / multiplier`, percent increase/decrease on
  the log scale. Docs state that the MDC is an individual threshold (not for
  group effects, not for counting responders; Senn 2004) and why no SWC is
  computed (0.2 SD is a restatement of the ICC; 0.3 × the test's own TE is a
  fixed 11% of the MDC; Hopkins's 0.3 × CV is anchored only because it uses
  the competition CV; Hopkins et al. 1999, Hopkins 2004, read from
  user-supplied PDFs).
- **Individual responses (instead of an MDC "responder" classifier).** A
  helper that flags each subject as improved/unchanged/worse by the MDC was
  considered and rejected: observed changes mix true response with
  within-person variation, so the share exceeding the MDC reflects noise as
  much as real heterogeneity (Senn 2004, 2016). The defensible tool is an
  estimate of the SD of individual responses,
  sqrt(SD²_change,treatment − SD²_change,control), with a CI (Atkinson &
  Batterham 2015; Hopkins 2015, "Individual responses made easy"), and
  pointers to replicated crossover designs. Research the CI (it can be
  negative under the square root) before building.
- **D-study / Spearman-Brown projection:** ICC (and SEM of the mean) for k′
  items from fitted variance components; multi-facet G-theory via lmer.
  The repeats-within-cells case (r:(p × i)) is designed in Section 14.
- **Average-measure SEM** (SEM/√k).
- **Power/precision for the agreement ICC:** simulation-based (no exact F
  distribution); `power_icc()`/`precision_icc()` currently cover one-way and
  consistency only.
- **RMS-of-subject-CVs** (Hyslop & White 2009): not SEM-based, so it doesn't
  fit `reli_err_test()`; add only if requested (mostly used in lab assay work).

## 11. Decision log

| Decision | Outcome |
|---|---|
| Split into focused functions | `icc_test`, `reli_err_test` (+ `power_icc`) |
| CV | Folded into `reli_err_test(scale = "log" / "mean")`; no `cv_test()` |
| Return class | `c("reli_htest", "htest")` |
| Default engine | `method = "anova"`; unbalanced → error pointing to `"reml"` |
| REML + unbalanced inference | F/χ² with observed-data df plus a warning (no fixed % threshold); `boot = TRUE` opt-in |
| Bootstrap | `boot`/`boot_ci`/`replicates` in both `icc_test()` and `reli_err_test()`; p-value by CI inversion (TOSTER style), documented as approximate; bootstrap-under-null is a TODO |
| Bootstrap resampling | Nonparametric, by subject; no parametric option |
| Bootstrap interval types | `auto`, `perc`, `bca`, `stud`, `basic` (as TOSTER); `stud` divides each replicate by its own `se_method` SE on the z = 0.5 log F scale (`"normal"` not allowed: constant SE); `bca` acceleration from leave-one-subject-out jackknife |
| Default `boot_ci` | `"auto"` (2026-10-16): `stud` for ANOVA, `perc` for REML or `se_method = "normal"`; explicit `stud` + REML warns. `reli_err_test()`: `bca` for two-way agreement statistics with ANOVA (phase-3 study) |
| Agreement-form bootstrap | Hybrid: subject resampling plus a chi-square(k − 1)/(k − 1) multiplier on MSJ per replicate (2026-10-15) |
| `se_method` | `c("jackknife", "influence", "normal")`, default `"jackknife"` (2026-10-15, option A); `stderr = c(ICC, z)` always, plus `boot` (SD of bootstrap estimates) when `boot = TRUE`; `stud` + REML requires `"influence"`; TODO B (tuned influence correction) and C (`"auto"` SE) only if needed |
| Plain-language description | `summary.reli_htest()`, internal `describe_htest()`-style describer; no TOSTER dependency; `style = c("auto", "hypothesis", "estimation")`, estimation = compatibility intervals + S-values (2026-10-16) |
| Default `null.value` | `NULL` in `icc_test()` and `reli_err_test()`: estimate and two-sided interval; test only when a null is given (2026-10-16) |
| SEE/SEP analytic interval | Wald on the log scale, normal-theory delta SE; negative ICC truncated at 0 (2026-10-16) |
| Qualitative benchmark labels | None (option A); docs point to testing against a threshold instead |
| Input interface | Formula + long + wide (`col.names`) |
| `reli_stats`/`reli_aov` | Supersede for now (changed from deprecate); deprecate later |
| New-function lifecycle badge | `maturing` (already used in 7 places in the package) |
| SD_total (SEE, SEP) | Matched to the chosen ICC's denominator (not observed SD, not one fixed model-based SD); `from_icc` dropped because the ICC-based SEM then equals rows 1–3; legacy keeps the observed SD |
| `icc_test()` default | ICC(C,1) / ICC(3,1), per Hopkins (confirmed) and Weir (conditional, abstract); Rd notes Koo & Li prefer absolute agreement |
| SEE | Keep |
| Change in mean | Not included (`t.test()` covers it) |
| MDC / SDD / RC | Not estimated; documented as scaled SEM / LoA special cases; converter is a TODO |
| Multiplier argument | Rejected |
| ICC-based SE inference | Delta method default, `boot = TRUE` option |
| `reli_err_test` defaults | SEM, raw scale, twoway, consistency |
| ICC sample size | Two functions, `power_icc()` and `precision_icc()`, `power.t.test()` style, `power.htest` class |
| Solve for `k` | Yes, in both |
| Power/precision method | `method = c("exact", "approximate")`; exact default, approximate = Walter / Zou formulas |
| Agreement ICC power | Not supported; documented; simulation-based is a TODO |
| `alternative` in `power_icc()` | Test notation (`"two.sided"`, `"greater"`, `"less"`); default `"two.sided"`, shared with `icc_test()`; two-sided `rho1` solve returns the root above `rho0` with a NOTE |
| Legacy ICC CIs | Change to two-sided (Shrout & Fleiss / McGraw & Wong convention), with NEWS and the `conf.level` mapping that reproduces old numbers |
| D-study | TODO only; for repeats within cells, designed (not built) in Section 14 (2026-10-20) |

## 12. Open questions

None blocking. The default `boot_ci` is decided by the phase-1 coverage study.

### Pre-check results (2026-10-15)

Numerical checks (simulation and algebra, script `reli_sims/precheck_verify.R`):

| Check | Result |
|---|---|
| Type I error of all six F tests (H0 ρ = 0.3, n = 15, k = 3, 4000 reps) | 0.044–0.052 (nominal 0.05) |
| Two-sided CIs, all six forms, Shrout & Fleiss data | Identical to psych 2.6.3 |
| Satterthwaite v: S&F eq. 7 form vs McGraw-Wong a,b form | Identical |
| ICC(A,k) CI: Stata form vs Spearman-Brown of ICC(A,1) bounds | Identical (errata issue moot) |
| `Var(ybar)`: consistency `MSB/(nk)`, agreement `(MSB+MSJ−MSE)/(nk)`, one-way `MSB/(nk)` | Match simulation within ~2% |
| Studentizing variance on 0.5·log F scale | Use exact trigamma form (Section 4) |
| Exact power via scaled central F vs simulation | 0.563/0.563, 0.884/0.885 |
| k = 2: 1.96·√2·√MSE = 1.96·SD(diff) | Exact |
| Agreement SEM² = MSW; doc Satterthwaite ν = general form | Exact |
| Matched SD·√(1 − ICC(3,1)) = √MSE (Weir A, B, C) | 7.56, 7.56, 2.17 = √MSE; observed-SD version 7.31, 7.36, 2.16 |
| Sympercent vs CV% (5, 10, 15, 25) | 5.00, 10.03, 15.08, 25.40 |

Literature checks: see the notes in Sections 2, 4, 6.1, 6.2, 6.3 and the
references. Weir (2005) and McGraw & Wong (1996) were read in full from PDFs
the user supplied (2026-10-15; the PDFs and extracted text are not in the
repo, since they are copyrighted; obtain them via the DOIs below if needed).
Still unverified:

- McGraw & Wong Table 8 coefficients (a, b, c, d for Type A tests): the PDF
  text layer is too garbled to read them. They rest on the Stata manual plus
  the null simulation above (nominal Type I error). Structure matches.
- McGraw & Wong correction (p. 390): not in the supplied PDF. Our intervals
  match psych and Stata, so it is unlikely to matter.
- Cicchetti (1994) cutoffs (secondary sources only; not used).

## 13. References

All metadata below checked against CrossRef on 2026-10-15 unless noted.

- Atkinson, G., & Nevill, A. M. (1998). Statistical methods for assessing
  measurement error (reliability) in variables relevant to sports medicine.
  Sports Medicine, 26(4), 217-238. doi:10.2165/00007256-199826040-00002
- Beckerman, H., Roebroeck, M. E., Lankhorst, G. J., Becher, J. G.,
  Bezemer, P. D., & Verbeek, A. L. M. (2001). Smallest real difference, a
  link between reproducibility and responsiveness. Quality of Life Research,
  10(7), 571-578. doi:10.1023/A:1013138911638
- Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
  comparison studies. Statistical Methods in Medical Research, 8(2), 135-160.
  doi:10.1177/096228029900800204 (use this, not the legacy 10.1191 DOI)
- Cicchetti, D. V. (1994). Guidelines, criteria, and rules of thumb for
  evaluating normed and standardized assessment instruments in psychology.
  Psychological Assessment, 6(4), 284-290. doi:10.1037/1040-3590.6.4.284
  (only needed if the docs mention benchmark disagreement)
- Cole, T. J. (2000). Sympercents: symmetric percentage differences on the
  100 log_e scale simplify the presentation of log transformed data.
  Statistics in Medicine, 19(22), 3109-3125.
  doi:10.1002/1097-0258(20001130)19:22<3109::AID-SIM558>3.0.CO;2-F
  (a 2002 follow-up, Cole & Kryakin, Stat Med 21(15):2287-2290, exists;
  check whether it is an erratum)
- Donner, A., & Eliasziw, M. (1987). Sample size requirements for
  reliability studies. Statistics in Medicine, 6(4), 441-448. (Source of the
  doi:10.1002/sim.4780060404 (source of the exact power used by Walter et
  al.; metadata checked on CrossRef 2026-10-16)
- Greenland, S. (2019). Valid p-values behave exactly as they should: Some
  misleading criticisms of p-values and their resolution with S-values. The
  American Statistician, 73(sup1), 106-114.
  doi:10.1080/00031305.2018.1529625
- Haley, S. M., & Fragala-Pinkham, M. A. (2006). Interpreting change scores
  of tests and measures used in physical therapy. Physical Therapy, 86(5),
  735-743. doi:10.1093/ptj/86.5.735
- Hopkins, W. G. (2000). Measures of reliability in sports medicine and
  science. Sports Medicine, 30(1), 1-15. doi:10.2165/00007256-200030010-00001
  (comment and reply in 30(5), 375-381). Also sportsci.org reliability pages:
  https://www.sportsci.org/resource/stats/precision.html,
  https://www.sportsci.org/resource/stats/relycalc.html
- Hyslop, N. P., & White, W. H. (2009). Estimating precision using duplicate
  measurements. Journal of the Air & Waste Management Association, 59(9),
  1032-1039. doi:10.3155/1047-3289.59.9.1032
- Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
  intraclass correlation coefficients for reliability research. Journal of
  Chiropractic Medicine, 15(2), 155-163. doi:10.1016/j.jcm.2016.02.012
  (erratum: 2017, 16(4), 346, doi:10.1016/j.jcm.2017.10.001)
- McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some intraclass
  correlation coefficients. Psychological Methods, 1(1), 30-46.
  doi:10.1037/1082-989X.1.1.30. Correction: 1(4), 390,
  doi:10.1037/1082-989X.1.4.390 (paper read in full from a user-supplied
  PDF; the correction was not included)
- Meeker, W. Q., Hahn, G. J., & Escobar, L. A. (2017). Statistical Intervals:
  A Guide for Practitioners and Researchers (2nd ed.). Wiley.
  doi:10.1002/9781118594841. (The 1991 1st edition is Hahn & Meeker,
  "Statistical Intervals: A Guide for Practitioners", doi:10.1002/9780470316771;
  the legacy Rd's "Hahn & Meeker (2011)" matches neither.)
- Mondal, D., Vanbelle, S., Cassese, A., & Candel, M. J. J. M. (2024).
  Review of sample size determination methods for the intraclass
  correlation coefficient in the one-way analysis of variance model.
  Statistical Methods in Medical Research, 33(3), 532-553.
  doi:10.1177/09622802231224657 (read 2026-10-16; see phase 5 notes)
- Qin, S., et al. (2019). Quality of Life Research, 28(4), 1029-1033.
  doi:10.1007/s11136-018-2076-0 (recommends ICC(A,1); full metadata not yet
  checked)
- Rafi, Z., & Greenland, S. (2020). Semantic and cognitive tools to aid
  statistical science: replace confidence and significance by compatibility
  and surprise. BMC Medical Research Methodology, 20, 244.
  doi:10.1186/s12874-020-01105-9
- Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
  assessing rater reliability. Psychological Bulletin, 86(2), 420-428.
  doi:10.1037/0033-2909.86.2.420 (interval formulas read from the paper)
- StataCorp. Stata Base Reference Manual: `icc` (Methods and formulas;
  secondary source for McGraw & Wong's interval and test formulas).
  https://www.stata.com/manuals/ricc.pdf
- Ukoumunne, O. C., Davison, A. C., Gulliford, M. C., & Chinn, S. (2003).
  Non-parametric bootstrap confidence intervals for the intraclass
  correlation coefficient. Statistics in Medicine, 22(24), 3805-3821.
  doi:10.1002/sim.1643
- Vangel, M. G. (1996). Confidence intervals for a normal coefficient of
  variation. The American Statistician, 50(1), 21-26.
  doi:10.1080/00031305.1996.10473537 (legacy docs only)
- Walter, S. D., Eliasziw, M., & Donner, A. (1998). Sample size and optimal
  designs for reliability studies. Statistics in Medicine, 17(1), 101-110.
  doi:10.1002/(SICI)1097-0258(19980115)17:1<101::AID-SIM727>3.0.CO;2-E
- Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass
  correlation coefficient and the SEM. Journal of Strength and Conditioning
  Research, 19(1), 231-240. doi:10.1519/15184.1 (read in full from a
  user-supplied PDF)
- Zou, G. Y. (2012). Sample size formulas for estimating intraclass correlation
  coefficients with precision and assurance. Statistics in Medicine, 31(29),
  3972-3981. doi:10.1002/sim.5466

References for Section 14 (G-theory design; **metadata not yet checked**
against CrossRef; verify before citing in an Rd):

- Brennan, R. L. (2001). Generalizability Theory. Springer.
- Burdick, R. K., & Graybill, F. A. (1992). Confidence Intervals on Variance
  Components. Marcel Dekker.
- Burdick, R. K., Borror, C. M., & Montgomery, D. C. (2005). Design and
  Analysis of Gauge R&R Studies. SIAM/ASA. (Parts × operators × replicates
  is this design; repeatability = σ²_e, reproducibility = σ²_i + σ²_pi.)
- Cappelleri, J. C., & Ting, N. (2003). A modified large-sample approach to
  approximate interval estimation for a particular intraclass correlation
  coefficient. Statistics in Medicine, 22(11), 1861-1877. (Check.)
- Graybill, F. A., & Wang, C.-M. (1980). Confidence intervals on nonnegative
  linear combinations of variances. Journal of the American Statistical
  Association, 75(372), 869-873.
- Jiang, Z. (2018). Using the linear mixed-effect model framework to
  estimate generalizability variance components in R: A lme4 package
  application. Methodology, 14(3), 133-142. (Check.)
- Satterthwaite, F. E. (1946). An approximate distribution of estimates of
  variance components. Biometrics Bulletin, 2(6), 110-114.
- Shavelson, R. J., & Webb, N. M. (1991). Generalizability Theory: A Primer.
  Sage.
- Software (14.7; cite the package's `citation()` output at release):
  VCA (Schuetzenmeister & Dufey), rptR (Stoffel, Nakagawa & Schielzeth,
  2017, Methods in Ecology and Evolution 8(11), 1639-1644; check),
  SixSigma (Cano, Moguerza & Prieto Corcoba), gtheoryr (Tyagi), gtheory
  (Moore; archived), cccrm (Carrasco et al.).

## 14. Replicate measurements: a G-theory extension (`gstudy_test()`, built 2026-10-20)

Status: designed and built 2026-10-20; see 14.8 for what was built and the simulation results.
Motivation: users (e.g. an ultrasound study with 3 operators × 20
participants × 3 scans per operator) have **replicates inside each
subject × item cell**. `.reli_prepare()` currently stops ("aggregate
replicate measurements first", `R/reli_engine.R:114`), and the legacy
`reli_stats()` silently folded the extra rows into the error with wrong df
(its model had no id × item term; now an error, see NEWS). Aggregating is valid only when the
score used in practice is also a mean of r replicates, and it cannot
separate intra-rater from inter-rater error, which is usually the question.

### 14.1 Model

Balanced design: n subjects (p), k items (i: raters, devices), r
replicates nested in each p × i cell, design r:(p × i) in G-theory
notation:

y_pir = μ + p + i + (pi) + e_pir,  all effects random, independent,
variances σ²_p, σ²_i, σ²_pi, σ²_e.

lme4: `y ~ 1 + (1 | id) + (1 | item) + (1 | id:item)`.

Two-way ANOVA with replication (unrestricted, random-items
parameterization, as lme4 and McGraw & Wong's Case 2 use):

| Source | df | E[MS] |
|---|---|---|
| Subjects (MS_p) | n − 1 | σ²_e + r σ²_pi + k r σ²_p |
| Items (MS_i) | k − 1 | σ²_e + r σ²_pi + n r σ²_i |
| Subjects × items (MS_pi) | (n − 1)(k − 1) | σ²_e + r σ²_pi |
| Within cells (MS_w) | n k (r − 1) | σ²_e |

ANOVA estimates: σ²_e = MS_w; σ²_pi = (MS_pi − MS_w)/r;
σ²_p = (MS_p − MS_pi)/(k r); σ²_i = (MS_i − MS_pi)/(n r).

**Consistency forms with fixed items.** As in the current functions, the
consistency estimands simply leave σ²_i out of the denominator; the
estimators are the same (McGraw & Wong: the fixed/random distinction
changes interpretation, not computation). Under the *restricted* mixed
model E[MS_p] would lack the r σ²_pi term; the design follows the
unrestricted model (lme4, current engine). Document this.

**What r = 1 reduces to.** With r = 1, σ²_pi and σ²_e are confounded and
MS_pi is the current MS_E, so every estimand below collapses to an
existing `icc_test()` / `reli_err_test()` form. The new function requires
r ≥ 2 in at least some cells and otherwise points to those functions.

**Nested vs crossed replicates.** The model assumes replicates are
exchangeable within a cell (scan 1 vs scan 2 means nothing). If the
replicate index is a shared occasion (every operator measures everyone on
day 1 and day 2), the design is p × i × o, fully crossed; treating it as
nested puts σ²_o and σ²_io into σ²_e (error looks larger, intra-rater ICC
lower). Out of scope for v1; document the distinction and how to check it
(fit both in lme4 and compare σ²_o).

### 14.2 Estimands

Measurement error (all can be given on `scale = "raw"`, `"log"`
(sympercents) or `"mean"`, as in `reli_err_test()`):

| Name | Definition | Question |
|---|---|---|
| SEM_rep (repeatability, intra-rater) | √σ²_e | Same rater, same subject, rescanned |
| SEM_C (inter-rater, consistency) | √(σ²_pi + σ²_e) | Different rater, rater bias handled separately |
| SEM_A (inter-rater, agreement) | √(σ²_i + σ²_pi + σ²_e) | Different rater, scores used interchangeably |

In mean squares: SEM_C² = (MS_pi + (r − 1) MS_w)/r;
SEM_A² = (MS_i + (n − 1) MS_pi + n (r − 1) MS_w)/(n r). Both are
**nonnegative combinations** of mean squares (good for Satterthwaite and
Graybill-Wang intervals). Gauge R&R names: SEM_rep² = repeatability,
σ²_i + σ²_pi = reproducibility.

Reliability coefficients, for a planned score that is the mean of k′
items (raters) and r′ replicates per item (D-study; defaults k′ = r′ = 1):

- Relative (consistency) coefficient
  Eρ² = σ²_p / (σ²_p + σ²_pi/k′ + σ²_e/(k′r′)).
- Absolute (agreement) coefficient
  Φ = σ²_p / (σ²_p + σ²_i/k′ + σ²_pi/k′ + σ²_e/(k′r′)).
- Intra-rater ICC (same rater, two replicates)
  ρ_intra = (σ²_p + σ²_pi) / (σ²_p + σ²_pi + σ²_e), pooled over raters
  (assumes equal σ²_e across raters; `check()` should show per-rater
  within-cell SDs).

With k′ = r′ = 1, Eρ² and Φ are the single-measurement ICC(C,1) and
ICC(A,1) with the replicate noise correctly placed. 

**Exact special case (verify algebraically in phase G1).** When r′ = r
(the planned score averages as many replicates as the study had), σ²_pi +
σ²_e/r = MS_pi/r, so Eρ² = (F − 1)/(F − 1 + k/k′) with F = MS_p/MS_pi: a
monotone function of one F ratio, so it has an **exact** F interval and
test for every k′ (it is ICC(C,·) on the cell means, Spearman-Brown
projected). For r′ ≠ r (the common practical case, r′ = 1: one scan) three
mean squares are involved and no exact interval exists. Similarly
ρ_intra = (MS_p + (k − 1) MS_pi − k MS_w) / (MS_p + (k − 1) MS_pi +
k (r − 1) MS_w), not exact.

MDCs follow by passing the matching SEM to `mdc_test()`: same rater,
z√2·SEM_rep (or /√r′ for means of r′ scans); two raters drawn from the
pool, z√2·SEM_A. (`mdc_test()` currently takes a `reli_err_test()` SEM;
accept the new SEM results too.)

Negative component estimates: ANOVA estimates are reported unconstrained
in the component table (REML truncates at 0); for coefficients and SEMs,
decide in G1 whether to truncate at 0 (current rule: a negative ICC gives
SEE = 0, SEP = SD). **Open question for the user.**

### 14.3 API (proposed; decide before G1)

Option A (recommended): a new `htest`-style function, one estimand per
call, consistent with the suite:

```r
gstudy_test(x = NULL, data = NULL, measure = NULL, item = NULL, id = NULL,
            stat = c("ICC", "SEM"),
            form = c("intra", "consistency", "agreement"),
            k_avg = 1, r_avg = 1,          # D-study: score = mean of k′ items × r′ repeats
            scale = c("raw", "log", "mean"),   # SEMs only
            method = c("anova", "reml"),
            conf.level = 0.95, null.value = NULL,
            alternative = c("two.sided", "greater", "less"),
            ci_method = c("auto", ...),       # analytic choice, decided by 14.5
            boot = FALSE, boot_ci = c("auto", "perc", "bca", "stud", "basic"),
            replicates = 1999, se_method = c("jackknife", "influence", "normal"))
```

- Returns `c("reli_htest", "htest")`; `summary()` adds the variance
  component table (with Satterthwaite/χ² intervals) and the share of each
  component, which is the most informative output for raters × scans
  studies. `plot()` gains a variance-component bar; `check()` per-rater
  within-cell SDs and Mandel-type additivity on cell means.
- Input: formula `values ~ item | id` or long data, with duplicate
  `(id, item)` rows **allowed** (they are the repeats). Wide data is not
  supported (no natural layout). No repeat-index column in v1 (exchangeable
  repeats); an optional `occasion` column could later enable the crossed
  p × i × o design.
- **Naming clash:** `replicates` already means bootstrap replicates in
  every reliability function, so the measurements are called **repeats**
  in arguments and docs (`r_avg`, "repeat measurements"). `repeat` itself
  is reserved in R.
- `icc_test()` / `reli_err_test()` keep stopping on duplicates; the error
  message and the vignette point to the new function.

Option B: an argument on `icc_test()`/`reli_err_test()` (e.g.
`repeats = "nested"`) that switches the engine. Fewer functions, but the
estimand list (intra, inter-consistency, inter-agreement, D-study) does
not map onto `model`/`type`/`unit`, and the arguments would interact
awkwardly. Option C: a fit object (`reli_components()`) plus a D-study
method returning several coefficients at once; flexible but breaks the
one-estimate `htest` pattern used everywhere else.

### 14.4 Interval candidates

| Estimand | Candidates (first = expected default) |
|---|---|
| SEM_rep | Exact χ²(nk(r − 1)) on MS_w |
| SEM_C, SEM_A | Graybill-Wang MLS for nonnegative combinations; Satterthwaite χ² (current approach for the agreement SEM) |
| Eρ² with r′ = r | Exact F on MS_p/MS_pi, transformed |
| Eρ² with r′ ≠ r, ρ_intra | MLS for ratios of variance components (Burdick & Graybill, balanced designs); Satterthwaite-F (pool the numerator mean squares into one with Satterthwaite df, then an F ratio, the McGraw & Wong Type A device) |
| Φ (any k′, r′) | Satterthwaite-F generalizing McGraw & Wong's ICC(A,1) interval; MLS (Cappelleri & Ting 2003 style) |
| All | Logit-Wald with jackknife SE (coefficients), log-Wald (SEMs): fallback for REML |
| All, `boot = TRUE` | Subject resampling (each drawn subject keeps all cells and repeats, so repeat variability is carried within subject), plus the hybrid χ²(k − 1)/(k − 1) multiplier on MS_i for statistics containing σ²_i; `perc`, `bca`, `stud` (jackknife SE) |

Tests: exact F where the interval is exact; otherwise by inverting the
chosen interval (documented as approximate), as now.

Unbalanced data (unequal repeats per cell, missing cells): REML only (the
ANOVA estimators lose their closed forms). Analytic intervals use the
balanced formulas with observed-data df and a harmonic-mean r̃ and a
warning (the current REML device), or the bootstrap (`perc` default for
REML, as now). To be checked in the simulation, not assumed.

### 14.5 Simulation plan and acceptance criteria (set before running)

**Grid.** n ∈ {10, 30}; k ∈ {2, 3, 5}; r ∈ {2, 3}; data normal or
non-normal (chi-square(3) for p, i and pi effects, t(5) errors, as in
phase 3). Variance mixes, σ²_p = 1 throughout:

| Mix | σ²_i | σ²_pi | σ²_e | Character |
|---|---|---|---|---|
| R (repeat-dominated) | 0.05 | 0.05 | 0.30 | intra-rater error is the main error |
| I (interaction-dominated) | 0.05 | 0.30 | 0.05 | raters rank subjects differently |
| B (rater-bias-dominated) | 0.30 | 0.05 | 0.10 | the few-items problem for Φ, SEM_A |
| L (low reliability) | 0.10 | 0.50 | 0.50 | coefficients near 0.5 |

Estimands: SEM_rep, SEM_C, SEM_A; Eρ² (k′ = 1, r′ = 1), Eρ² (k′ = 1,
r′ = r, exact check), Φ (k′ = 1, r′ = 1), Φ (k′ = k, r′ = r), ρ_intra.

Replications: analytic methods 2000 sims per cell (MC SE ≈ 0.005);
bootstrap 300 sims × 499 replicates (MC SE ≈ 0.013), on a reduced grid
(k ∈ {2, 3}, r = 2) if run time demands. Scripts in `reli_sims/`
(`gstudy_*.R`), parallel with progress logging, tables recorded in
Section 9 under the G phases.

**Pre-set criteria.**

1. *Correctness (no simulation).* ANOVA components equal lme4 REML on
   balanced data with all components positive (tolerance 1e-6); with
   r′ = r, Eρ² and its interval equal `icc_test()` (consistency, `unit`
   matched via Spearman-Brown) on the cell means exactly; with r = 1
   input, the function stops and names the existing function; one
   published worked example reproduced (a Brennan (2001) r:(p × i)
   synthetic data set or a gauge R&R example from Burdick et al.; locate
   in G1); and the cross-package checks in 14.7 pass.
2. *Exact intervals.* SEM_rep and Eρ² (r′ = r): coverage .94–.96 in every
   normal-data cell (they are exact; this checks the code).
3. *Analytic default per non-exact estimand.* Choose MLS over
   Satterthwaite if its mean coverage over normal cells is ≥ the
   Satterthwaite mean − 0.005 and its minimum at n = 30 is ≥ .93;
   otherwise Satterthwaite. The analytic default ships for an estimand
   only if every normal-data cell at n = 30 has coverage ≥ .92, except
   mix-B cells with k = 2 (σ²_i on 1 df), which are reported and
   documented rather than required; if the criterion fails, the docs
   recommend `boot = TRUE` for that estimand.
4. *Bootstrap.* For estimands containing σ²_i, hybrid ≥ subject-only −
   0.02 in every cell (as phase 3). `boot_ci = "auto"` picks, per estimand
   family, the type with the highest mean coverage over non-normal cells,
   provided no n = 30 cell falls below .88; `stud` must also beat `perc`
   by ≥ 0.01 on average to be preferred (it costs a jackknife per
   replicate).
5. *Unbalanced / REML (phase G4).* With 20% of repeats MCAR (no cell left
   empty) and with unequal r per cell, the REML analytic intervals cover
   ≥ .91 at n = 30 with normal data; otherwise REML defaults to the
   bootstrap and the analytic interval is not offered.
6. *Size of tests.* For any analytic test shipped, Type I error at
   α = .05 within .035–.065 (2000 sims, normal data, n = 30).
7. *Bias report (not a gate).* Mean bias of each point estimate,
   recorded, to decide the truncation rule (14.2).

### 14.6 Phases

- **G1:** engine support for repeats (`.reli_prepare()` mode that allows
  duplicates, `.reli_fit()` four-source ANOVA and lme4 formula), estimands,
  exact intervals, correctness checks (criterion 1). API decision (14.3)
  and truncation decision first.
- **G2:** analytic intervals (MLS and Satterthwaite), coverage study,
  criteria 2, 3, 6.
- **G3:** bootstrap (hybrid, `stud` with jackknife SE), criterion 4.
- **G4:** REML for unbalanced data, criterion 5.
- **G5:** methods (`summary()` component table, plot, check), `mdc_test()`
  accepting the new SEMs, vignette section "Repeat measurements" (nested
  vs crossed repeats, what averaging changes, intra- vs inter-rater error,
  a D-study example: more raters or more scans), error message pointer,
  NEWS. Jamovi later, if at all.

Out of scope for v1: the crossed p × i × o design (occasions as a facet),
more than one item facet, unequal error variance by rater, and power /
precision functions for G coefficients (simulation-based, later).

### 14.7 Existing R packages: validation and citation

Surveyed 2026-10-20 (CRAN `available.packages()` plus the packages' own
manuals; none of these packages was installed or run, so function names
and arguments below are from documentation and must be confirmed in G1). **None of them provides confidence intervals for G coefficients
or for the inter-rater SEMs of the r:(p × i) design by MLS, Satterthwaite or
a nonparametric bootstrap**, so the interval work in 14.4–14.5 is not
duplicated anywhere in R. Several do fit the model, so they serve as
references for point estimates, and some go further than v1 will, so the
docs should point users to them (as `agreement_analysis.Rmd` does for
BivRegBLS).

| Package (version checked) | What it does relevant here | Use for validation | Point users to it for |
|---|---|---|---|
| **VCA** 1.5.2 (Schuetzenmeister & Dufey; CRAN) | ANOVA (Searle) and REML variance components for arbitrary crossed/nested random designs; CIs for each component, total variance, SD and CV (`VCAinference(ci.method = "sas"` or `"satterthwaite")`); CLSI EP05 precision | Mean squares, df and components of the four-source ANOVA; the exact χ² CI for σ²_e (SEM_rep); total-variance CI; Satterthwaite df of components; ANOVA estimates with unbalanced data (its ANOVA handles imbalance, ours will use REML) | Laboratory precision designs and deeper nesting (runs within days within sites) |
| **rptR** 0.9.23 (Stoffel, Nakagawa & Schielzeth; CRAN) | Repeatability (ICC) and variance components from lme4 with several grouping factors (`grname = c("id", "item", "id:item")`), parametric-bootstrap CIs, permutation tests; Gaussian, Poisson, binary, proportion data | REML components (balanced and unbalanced); ratio estimates for ρ_intra and the single-measurement coefficients; its parametric-bootstrap intervals as an informal comparator (a different method, so not expected to agree exactly) | Non-Gaussian outcomes (counts, binary), and adjusted repeatability with fixed effects |
| **SixSigma** 0.11.1 (`ss.rr()`; CRAN) | Gauge R&R for parts × operators × replicates, crossed or nested: repeatability, reproducibility, %study variation, %tolerance, number of distinct categories; no CIs | Components of the same design under gauge R&R names. **Set `alphaLim = 1`**: by default the interaction is pooled into repeatability when not significant, which changes the estimands | Industrial measurement-system analysis (%tolerance, ndc) |
| **gtheoryr** 0.2.0 (Tyagi; CRAN, first released 2026-03, so new) | Balanced fully crossed G-studies (p × i, p × i × r, p × i × o, more facets), items-within-person nesting, D-studies with G and Φ, design grids, cost optimisation, error budgets, parametric-bootstrap percentile CIs. **Does not fit repeats nested in p × i cells** (r:(p × i)); negative components stop by default | D-study formulas: with r = 1, our relative and absolute coefficients for k′ items should equal `dstudy_pxi()`; the crossed p × i × o fit as a check on the nested-vs-crossed documentation | Multi-facet crossed designs (raters × occasions), D-study design optimisation and costs |
| **gtheory** 0.1.2 (Moore) | lme4-based G- and D-studies for balanced or unbalanced, crossed or nested designs (Brennan 2001 examples). **Archived from CRAN 2025-03-24** (maintainer email undeliverable); still installable from the CRAN archive / r-universe | Point estimates and D-study coefficients for r:(p × i) directly, including a Brennan example (one-off check, versions recorded) | Not recommended to users (archived); mention only if it returns to CRAN |
| **cccrm** 3.0.6 (Carrasco et al.; CRAN; already used for `ccc_test()` checks) | CCC from variance components with replicates (`ccc_vc(..., int = TRUE)`) | For k = 2, the replicate CCC is close to Φ(k′ = 1, r′ = 1) but treats the methods as fixed (a sum of squared method differences, not σ²_i), so it is a near-check to explain, not an equality. Confirm in G1 | Already cited for the CCC |
| **lme4** (Imports) | The model itself | REML fit in `method = "reml"` | — |

**How to use them (follow the existing convention in
`tests/testthat/test-ccc_test.R`):** compute reference values once in a
script under `reli_sims/` (`gstudy_crosscheck.R`), record the package
versions and calls in comments, and hard-code the numbers in
`tests/testthat/test-gstudy_test.R`. Do **not** add any of these packages
to Suggests for the checks (gtheory cannot be, being archived).
Planned checks, added to criterion 1:

- Balanced simulated data and one published example: mean squares and the
  four ANOVA components equal VCA's `anovaVCA()` (tolerance 1e-8);
  REML components equal VCA's `remlVCA()` and rptR (1e-5); the σ²_e χ²
  interval equals VCA's (1e-6).
- Repeatability and reproducibility equal `SixSigma::ss.rr(alphaLim = 1)`.
- With r = 1: relative and absolute D-study coefficients equal
  `gtheoryr::dstudy_pxi()` for k′ = 1 and k′ = k.
- One-off: Brennan (2001) r:(p × i) example through archived gtheory,
  if its data set covers that design.
- Informal: our analytic and bootstrap intervals beside rptR's and
  gtheoryr's parametric-bootstrap intervals on the same data; record the
  comparison, no pass/fail.

**Docs (phase G5):** in the vignette section and the Rd, a short
"Other software" paragraph naming VCA (deeper nesting, lab precision),
rptR (non-Gaussian data), gtheoryr (crossed multi-facet designs and
D-study optimisation) and SixSigma (gauge R&R summaries), and saying
what `gstudy_test()` adds (intervals and tests for the coefficients and
the intra-/inter-rater SEMs, and the link to `mdc_test()`). Re-check
versions and CRAN status at release, since gtheoryr is new and gtheory
has already been archived once.

### 14.8 As built (2026-10-20): decisions and results

**Decisions (user, 2026-10-20):** Option A, `gstudy_test()`; estimates
**unconstrained** (coefficients from the mean squares, as `icc_test()`;
negative components kept in the component table, shown as 0% share; an
SEM whose error variance is negative, possible only with r′ > r, is
reported as 0 with a warning); all phases G1–G5 in one pass.

**Code.** `R/gstudy_engine.R` (prep with repeats, four-source ANOVA and
lme4 fit, estimands as mean-square weights, MLS (Graybill-Wang / Ting et
al. 1990, without the G*/H* terms) and Satterthwaite-F test inversion,
jackknife and delta SEs, hybrid bootstrap step, component table);
`R/gstudy_test.R`; methods in `R/methods.reli_htest.R` (`summary()` prints
the component table, `plot(type = "components")`, `check()` with Bartlett's
test of equal repeat error across items); `mdc_test()` accepts gstudy SEMs
(averaging set in `gstudy_test()`); the duplicate-cell error of
`.reli_prep_from_long()` points to `gstudy_test()`. `.reli_boot()` and
`.reli_resample()` gained a `rebuild` argument. Tests:
`tests/testthat/test-gstudy_test.R`.

Deviations from 14.3: `se_method` is `"jackknife"` or `"normal"` (no
influence SE for this design); `scale` applies to SEMs only; the logit-Wald
REML fallback was not needed (criterion 5 passed with the analytic MLS).
Labels: `ICC(intra)`, `Erho2`, `Phi`, `SEM(intra)`, `SEM(C)`, `SEM(A)`.
REML uses r̃ = harmonic mean of repeats per observed cell, kc = cells per
subject, nc = cells per item, df_W = N − C, df_PI = C − n − k + 1.

**Criterion 1 (correctness), all pass** (`reli_sims/gstudy_crosscheck.R`,
values hard-coded in the tests; VCA 1.5.2, rptR 0.9.23, SixSigma 0.11.1,
gtheoryr 0.1.0 (CRAN binary; not 0.2.0 as surveyed), in a scratch library):
mean squares, df and components equal `VCA::anovaVCA` (1e-7); σ²_e χ²
interval equals `VCAinference` two-sided (1e-6); REML equals rptR/lme4
(1e-6, unbalanced) and `VCA::remlVCA` (3e-6 relative, its own optimiser);
repeatability/reproducibility equal `SixSigma::ss.rr(alphaLim = 1)`;
Eρ² and Φ with r′ = r equal `gtheoryr::dstudy_pxi()` on the cell means for
k′ = 1 and k′ = k; Eρ² (r′ = r) estimate, interval and F test equal
`icc_test()` on the cell means (1e-8), and the consistency SEM (r′ = r)
equals `reli_err_test()`. The Ting bound is exactly 0 at the F boundary
for two mean squares (checks the formula). rptR does not accept `id:items`
beside `id`; an explicit cell factor gives the same model. No Brennan
(2001) r:(p × i) data set was available, and gtheory (archived) was not
run. Informal: rptR's parametric-bootstrap interval for (σ²_p + σ²_pi)/total
on the check data was close to ours.

**Phase G2: analytic coverage** (`reli_sims/gstudy_coverage.R`; 96 cells,
2000 sims each, MC SE ≈ 0.005; 95% two-sided). Normal data:

| Estimand | Method | Mean | Min | Min, n = 30 |
|---|---|---|---|---|
| SEM_rep | exact | .951 | .940 | .941 |
| Eρ² (k′=1, r′=r) | exact | .951 | .942 | .944 |
| SEM_C | MLS / Satt. | .950 / .947 | .940 / .930 | .940 / .936 |
| SEM_A | MLS / Satt. | .951 / .913 | .939 / .667 | .939 / .667 |
| Eρ² (1,1) | MLS / Satt. | .951 / .949 | .942 / .939 | .942 / .939 |
| Φ (1,1) | MLS / Satt. | .962 / .928 | .947 / .722 | .948 / .722 |
| Φ (k,r) | MLS / Satt. | .962 / .928 | .944 / .728 | .949 / .728 |
| ρ_intra | MLS / Satt. | .951 / .944 | .939 / .928 | .939 / .935 |

- Criterion 2 passes (exact .940–.951). Criterion 3: MLS meets both
  conditions for every estimand, including the mix-B, k = 2 cells (no
  exemption needed) → **`ci_method = "auto"` = MLS for all**. Satterthwaite
  fails badly for the agreement statistics when rater bias dominates (mix
  B: .67–.74 at k = 2, .78–.82 at k = 3, misses on one side); it is kept as
  an option. MLS is slightly conservative for Φ (mean .962).
- Criterion 6 (size): tests invert the intervals, so two-sided size at .05
  is 1 − coverage: .038–.061 for MLS and exact at n = 30.
- Non-normal data (χ²₃ effects, t₅ errors): all analytic intervals
  under-cover, mean .83–.90, min .78–.86 (MLS) and down to .59
  (Satterthwaite SEM_A); the docs point to `boot = TRUE`.
- Criterion 7 (bias, not a gate): unconstrained estimates have small
  negative relative bias, −0.6% to −4.9% at n = 10 and −0.3% to −1.8% at
  n = 30 (SEM_A up to −9% in single cells); no case for truncation.

**Phase G4: REML, unbalanced** (`reli_sims/gstudy_reml.R`; n = 30, normal,
1000 sims per cell; 20% of repeats MCAR with no empty cell, k ∈ {2, 3},
r ∈ {2, 3}; or 1–3 repeats per cell, k ∈ {2, 3}). MLS and exact: min .937,
mean .949–.969 → **criterion 5 passes; REML keeps the analytic MLS
interval** (with the warning). Satterthwaite: SEM_A and Φ down to .71–.84
in mix B, as with balanced data.

**Phase G3: bootstrap** (`reli_sims/gstudy_boot.R`; n ∈ {10, 30},
k ∈ {2, 3}, r = 2, 4 mixes, normal and non-normal; 300 sims × 499
replicates, MC SE ≈ 0.013). Mean coverage (min at n = 30), non-normal:

| Estimand | perc | BCa | stud | basic |
|---|---|---|---|---|
| SEM_rep | .848 (.857) | .866 (.873) | **.922 (.917)** | .857 (.850) |
| SEM_C | .813 (.803) | .845 (.860) | **.928 (.927)** | .849 (.843) |
| SEM_A | .838 (.597) | **.889 (.740)** | .872 (.690) | .796 (.473) |
| Eρ² (1,1) | .847 (.840) | .842 (.820) | **.917 (.873)** | .776 (.797) |
| Φ (1,1) | .869 (.827) | .850 (.767) | **.914 (.847)** | .711 (.530) |
| ρ_intra | .846 (.850) | .851 (.843) | **.925 (.910)** | .809 (.807) |

- Criterion 4: hybrid ≥ subject-only in every cell (minimum difference
  +0.007 perc, +0.007 BCa; mean +0.14 / +0.15). Subject-only fell to
  .15–.22 for SEM_A in mix B. Hybrid kept.
- `boot_ci = "auto"`: highest mean non-normal coverage → `stud` for all but
  SEM_A, `bca` for SEM_A (stud beats perc by ≥ .05 everywhere, well over the
  .01 required). The .88 floor at n = 30 holds for SEM_rep, SEM_C and
  ρ_intra only; Eρ² stud .873 (one cell), and the agreement statistics fail
  it with every type (mix B, k ≤ 3, worst .69–.85): no bootstrap rescues
  σ²_i on 1–2 df. Documented, as for the agreement SEM of
  `reli_err_test()`. REML keeps `perc` (stud needs n refits per replicate).
- Normal data: stud .926–.946 mean; analytic (exact/MLS) intervals better,
  so the analytic interval stays the default and `boot = TRUE` is
  recommended for non-normal data.
- Comparison with analytic MLS under non-normal data: bootstrap stud gains
  +.06 to +.09 for the intra and consistency statistics and +.03 for Φ;
  for SEM_A, MLS (.899) and BCa (.889) are similar.

## 15. Crossed occasions: repeats that are shared across raters (built 2026-10-20)

Status: designed and built 2026-10-20; decisions and results in 15.9.
Decisions needed before phase C1 are marked **Decide**.

### 15.1 Motivation and naming

`gstudy_test()` (Section 14) assumes the repeats are exchangeable within a
subject × item cell (design r:(p × i)). Often they are not: every rater
measures every subject on day 1 and again on day 2, so "repeat 2" is a
shared **occasion**. MethComp's `BA.est(linked = TRUE)` models this with an
item × replicate term; we do not. Fitting the nested model to crossed data
puts the occasion effects (σ²_o, σ²_io, σ²_po) into the repeat error: the
intra-rater SEM looks larger, and it cannot say whether the error is a
day-to-day shift (systematic, fixable) or noise.

**Decide: the function name.** "gstudy" is broader than what the function
does (one item facet plus repeats; G theory covers arbitrary designs, and
gtheoryr and gtheory use the word). The function is unreleased, so a rename
costs nothing now. Candidates (recommendation first):

| Name | For | Against |
|---|---|---|
| `rater_reli_test()` | Says what users have (raters or devices measuring subjects repeatedly) and fits the `reli_*` family | `icc_test()` also does inter-rater reliability |
| `intra_inter_test()` | Names the key output (intra- vs inter-rater error) | Unusual form; less obviously a reliability function |
| `reli_reps_test()` | Matches the package's "reps" vocabulary (`agree_reps()`, `data_type = "reps"`) | "reps" sits awkwardly beside `replicates` (bootstrap); does not say raters |
| `rr_test()` | Gauge R&R (repeatability and reproducibility), the industrial name for exactly this | Opaque outside metrology |

### 15.2 Model

p × i × o, all random: n subjects, k items, m occasions, r ≥ 1
measurements per p × i × o cell (r = 1 is the usual case):

y_pior = μ + p + i + o + (pi) + (po) + (io) + (pio) + e_pior.

With r = 1, σ²_pio and σ²_e are confounded (reported together as the
residual). Balanced ANOVA expected mean squares follow the all-random rule
E[MS_S] = σ²_e + Σ_{T ⊇ S} c_T σ²_T, with c_T = r × (product of the levels
of the factors not in T); for example
E[MS_p] = σ²_e + r σ²_pio + kr σ²_po + mr σ²_pi + kmr σ²_p.
Components are E⁻¹ · MS (unconstrained, as in Section 14). σ²_p, σ²_i and
σ²_o are signed combinations of four mean squares (quasi-F situations), so
the Ting et al. MLS for sign-unrestricted combinations, already in the
engine, is the natural interval; no new theory is needed.

lme4 (REML, unbalanced):
`y ~ 1 + (1|id) + (1|item) + (1|occ) + (1|id:item) + (1|id:occ) + (1|item:occ)`,
plus `(1|id:item:occ)` when r ≥ 2.

### 15.3 Engine refactor (prerequisite)

Generalise `R/gstudy_engine.R` from the hard-coded four-source table to a
**design object**: the facets, their levels, r, and the object of
measurement (p). From it:

- the EMS coefficient matrix by the rule above, so components =
  solve(EMS) %*% MS for any balanced, fully crossed random design with
  optional replication. The nested r:(p × i) design is the special case
  {p, i} with replication, so every Section 14 result must be reproduced
  exactly (regression tests against the current hard-coded version);
- balanced ANOVA mean squares from marginal means (closed form, no `aov()`);
- estimand weights by Brennan's (2001) general rules, given which facets
  are generalised over (random, with D-study sizes n′) and which are fixed
  (part of the universe score):
  - universe score τ: effects containing p and only fixed facets, each
    divided by the D-study sizes of its fixed facets;
  - relative error δ: effects containing p and at least one random facet,
    divided by the product of the D-study sizes of its facets other than p;
  - absolute error Δ: δ plus the effects without p that contain a random
    facet, divided likewise;
  - coefficient τ / (τ + δ) (consistency) or τ / (τ + Δ) (agreement); SEM
    √δ or √Δ.

Everything downstream (MLS and Satterthwaite inversion, jackknife, hybrid
bootstrap, component table) already works on MS weights and carries over.

### 15.4 Estimands

D-study sizes k′ (raters, `k_avg`) and m′ (occasions, new `m_avg`).

| Question | Fixed | Random | Consistency error | Agreement error adds |
|---|---|---|---|---|
| Intra-rater (same rater, another occasion) | i (k′ = 1) | o | (σ²_po + σ²_res)/m′ | (σ²_o + σ²_io)/m′ |
| Inter-rater (another rater, same occasion) | o (m′ = 1) | i | (σ²_pi + σ²_res)/k′ | (σ²_i + σ²_io)/k′ |
| Overall (any rater, any occasion) | none | i, o | σ²_pi/k′ + σ²_po/m′ + σ²_res/(k′m′) | σ²_i/k′ + σ²_o/m′ + σ²_io/(k′m′) |

σ²_res = σ²_pio + σ²_e. Universe scores: σ²_p + σ²_pi (intra), σ²_p + σ²_po
(inter, occasion fixed), σ²_p (overall). The overall row is Brennan's
p × i × o Eρ² and Φ. With r ≥ 2, a pure repeatability SEM (σ²_e: same rater,
same occasion) is also available, as in Section 14.

Notes:

- In the nested design "intra" had one form (exchangeable repeats have no
  systematic effect). With occasions, consistency vs agreement matters for
  intra too: agreement counts a day-2 shift (σ²_o) and rater-specific drift
  (σ²_io) as error; consistency sets them aside, to be reported separately,
  as Hopkins and Weir do for trials.
- The inter-rater row at a *fixed* occasion is what most inter-rater
  studies report; the overall row is what a clinic using any rater on any
  day experiences.

**Decide: the API.** The current `form = c("intra", "consistency",
"agreement")` mixes two things: *which measurement varies* and *whether
systematic effects count as error*. Recommended (before release):

```r
<name>(x, data, measure, item, id, occasion = NULL,
       stat = c("ICC", "SEM"),
       facet = c("intra", "inter", "overall"),   # which measurement varies
       type = c("consistency", "agreement"),     # as in icc_test()
       k_avg = 1, r_avg = 1, m_avg = 1, ...)     # the rest unchanged
```

- `occasion = NULL`: nested design, as now. `facet = "overall"` is then
  the same as `"inter"`, and `type` makes no difference for `"intra"`. The
  current `form = "intra"`, `"consistency"`, `"agreement"` become
  `facet = "intra"`, `facet = "inter", type = "consistency"` and
  `facet = "inter", type = "agreement"`.
- `occasion = "day"`: crossed design; every facet/type combination is
  defined.
- Alternative: keep `form` and add `occasion_effects = c("error",
  "exclude")`. Fewer changes, but two arguments that both mean "what counts
  as error".

### 15.5 Nested or crossed: helping users choose

- Docs and vignette: the decision rule ("is repeat 2 the same event for
  every rater? Then it is an occasion").
- With `occasion` given, `summary()` reports the occasion components' share
  (σ²_o + σ²_io + σ²_po), and the result keeps what the nested model would
  have given (the intra SEM with occasions folded into repeat error), so
  the cost of ignoring the design is visible.
- `check()`: occasion means by rater (the io interaction) and each
  subject's change between occasions against their mean.
- No formal "nested vs crossed" test: with r = 1 per occasion the nested
  model just pools σ²_o, σ²_io and σ²_po into the error; the intervals of
  those components (excluding 0 or not) answer the practical question.

### 15.6 Intervals and bootstrap

- Exact cases are rare (every coefficient involves a quasi-F); the engine
  detects them generically (weights on at most 2 MS for a ratio, 1 MS for an
  SEM). MLS (test inversion for coefficients) is the expected default and
  Satterthwaite the alternative, decided by simulation as before.
- Hybrid bootstrap generalised: resampling subjects captures the variation
  of every MS involving p; each MS without p (MS_i, MS_o, MS_io) is
  multiplied by an independent χ²_df/df draw in each replicate. This
  reduces to the current rule (MS_I only) for the nested design. **Check by
  simulation** whether MS_io's draw should replace or scale its resampled
  value (it is a function of item × occasion means, which subject
  resampling also moves).
- Jackknife SE: leave-one-subject-out mean squares by downdating, as now.
- REML with unbalanced data: reconstructing mean squares with observed-data
  df is less obvious with three crossed facets. Plan: try the analogue
  (harmonic means of cell counts per margin; observed-data df by
  subtraction) and gate it by criterion C5. If it fails, REML gives point
  estimates and `boot = TRUE` only (no analytic interval).

### 15.7 Simulation plan and acceptance criteria (set before running)

Grid: n ∈ {10, 30}; k ∈ {2, 3}; m ∈ {2, 3}; r = 1 (and r = 2 on a reduced
grid); normal and non-normal (χ²₃ effects, t₅ errors). σ²_p = 1. Mixes:

| Mix | σ²_i | σ²_o | σ²_pi | σ²_po | σ²_io | σ²_res | Character |
|---|---|---|---|---|---|---|---|
| N (noise) | .05 | .05 | .05 | .05 | .02 | .30 | residual dominates |
| D (day shift) | .05 | .30 | .05 | .05 | .05 | .10 | occasion main effect: the few-levels problem for intra agreement |
| R (rater bias) | .30 | .05 | .05 | .05 | .02 | .10 | as Section 14 mix B |
| X (interactions) | .05 | .05 | .25 | .25 | .10 | .10 | raters and days rank subjects differently |

Estimands: intra C/A, inter C/A, overall Eρ²/Φ (k′ = m′ = 1, and k′ = k,
m′ = m), and their SEMs; plus the nested model's intra SEM fitted to the
same crossed data (the bias of ignoring occasions; a report, not a gate).

Criteria (as Section 14.5):

- **C1 correctness (no simulation):** the refactored engine reproduces
  every Section 14 result exactly; crossed components equal
  `VCA::anovaVCA(y ~ id*item*occ)` (1e-7) and lme4 REML (1e-5); overall
  Eρ² and Φ equal gtheoryr's p × i × o G- and D-study (gtheoryr 0.2.0 is now
  on CRAN; confirm the function names) and, if obtainable, a Brennan (2001)
  p × i × o example; MethComp `BA.est(linked = TRUE)` components compared
  informally (methods are fixed there, so not expected to be equal).
- **C2 exact intervals** (if any): .94–.96 in every normal cell.
- **C3 analytic default:** MLS over Satterthwaite if its mean normal
  coverage ≥ Satterthwaite's − .005 and its minimum at n = 30 ≥ .93. Ships
  if every normal n = 30 cell ≥ .92, except agreement forms whose
  systematic facet has 2 levels and dominates (mixes D and R with m = 2 or
  k = 2), which are reported and documented.
- **C4 bootstrap:** generalised hybrid ≥ subject-only − .02 in every cell;
  `boot_ci = "auto"` by highest mean non-normal coverage, with the .88 floor
  at n = 30; `stud` preferred only if ≥ perc + .01.
- **C5 REML, unbalanced:** 20% of cells MCAR, n = 30, normal: analytic
  ≥ .91, else bootstrap only for REML.
- **C6 size:** analytic tests .035–.065 at n = 30, normal.
- **C7 bias report:** including the nested-model intra SEM on crossed data.

Replications: analytic 2000 per cell; bootstrap 300 × 499 on k, m ∈ {2, 3},
r = 1. Scripts `reli_sims/occasion_*.R`.

### 15.8 Phases

- **C0:** decisions on the name (15.1) and API (15.4); rename
  `gstudy_test()` with its files, labels, docs and tests in one commit
  before C1 (no change in behaviour).
- **C1:** engine refactor to a design object (15.3), regression tests
  against Section 14; crossed fit, estimands, correctness checks (C1).
- **C2:** analytic coverage (C2, C3, C6).
- **C3:** generalised hybrid bootstrap (C4).
- **C4:** REML with unbalanced data (C5).
- **C5:** methods (occasion components in `summary()`, `check()` panels),
  `mdc_test()` (the intra agreement SEM gives the test-retest MDC for one
  rater), vignette (a worked day-1/day-2 example), NEWS.

Out of scope: rater-specific (heteroscedastic) error variances, MethComp's
other strength (possible later via `nlme::varIdent`, with bootstrap-only
inference); more facets; power and precision functions.

### 15.9 As built (2026-10-20): decisions and results

**Decisions (user, 2026-10-20):** name `reli_facet_test()`, with the
documentation saying the name is experimental and may change; implement the
crossed design now. The `facet` / `type` API of 15.4 was adopted (the
function was unreleased, so `form` was dropped rather than deprecated):
`form = "intra"`, `"consistency"`, `"agreement"` became `facet = "intra"`,
`facet = "inter", type = "consistency"`, `facet = "inter", type =
"agreement"`. Labels: `ICC(intra)` / `SEM(intra)` (nested intra),
`ICC(facet,C|A)` / `SEM(facet,C|A)`, `Erho2` / `Phi` (overall coefficients).
Restrictions: `k_avg = 1` for intra; `m_avg = 1` for inter and without
`occasion`; `r_avg = 1` for crossed data with one measurement per cell;
`facet = "overall"` without `occasion` is `"inter"`.

**Code.** `R/reli_facet_engine.R` replaces `R/gstudy_engine.R`: the design
(nested or crossed, with or without repeats) is data; effects are named by
their facets; EMS from the all-random rule (observed cells per level
combination for REML); ANOVA mean squares by Möbius inclusion-exclusion of
marginal means; estimand weights by Brennan's rules (15.3); observed-data
df by subtraction; jackknife by recomputing the ANOVA without each subject;
generalised hybrid step and jackknife hold for every mean square without
subjects in the estimand (`.facet_nonsubject()`). `R/reli_facet_test.R`
replaces `R/gstudy_test.R`. `.reli_prepare()` gained `occasion`. With
`occasion` and `facet = "intra"`, the result carries `nested`, the repeat
model's estimate, and `summary()` reports it. `check()` adds the occasion
means by item. `boot_ci = "auto"` uses BCa for any SEM that counts
effects without subjects (generalising "agreement SEM").

**C1 correctness, all pass.** The refactored engine reproduced all 244
recorded `gstudy_test()` results (every form, statistic, D-study size, CI
method and scale; REML unbalanced; bootstrap with the same seed) to 6e-11.
Crossed: mean squares, df and components equal `VCA::anovaVCA` (1e-7) with
one measurement per cell and with repeats (three-way interaction and error
separated); REML equals `VCA::remlVCA` (balanced and 15 empty cells,
1e-5); overall relative and absolute error variances, Eρ² and Φ equal
`gtheoryr::dstudy_pxio()` (gtheoryr 0.2.0, CRAN source) for (k′, m′) =
(1, 1), (3, 3), (2, 1). Script `reli_sims/occasion_crosscheck.R`; values
hard-coded in `tests/testthat/test-reli_facet_test.R`. MethComp was not
compared (fixed methods; informal only, not done).

A finding: with occasions, the intra-rater *agreement* SEM has exactly the
same point estimate as the repeat model's intra SEM (the within-cell sum of
squares splits into the occasion terms), but a far wider interval; the
consistency SEM is smaller. See C7.

**C2/C3/C6: analytic coverage** (`reli_sims/occasion_coverage.R`; n ∈
{10, 30}, k ∈ {2, 3}, m ∈ {2, 3}, r = 1, mixes N, D, R, X, normal and
non-normal, 2000 sims per cell). No estimand in this grid is exact. Normal
data, MLS (Satterthwaite):

| Estimand | Mean | Min | Min, n = 30 | Max, n = 30 |
|---|---|---|---|---|
| ICC intra C | .954 (.947) | .942 (.928) | .942 | |
| ICC intra A | .968 (.927) | .955 (.789) | .956 | .974 |
| ICC inter C | .954 (.947) | .943 (.933) | .943 | |
| ICC inter A | .968 (.922) | .954 (.754) | .954 | .972 |
| Eρ² (1,1) / (k,m) | .955 / .957 | .942 / .943 | .942 / .943 | |
| Φ (1,1) / (k,m) | .969 / .971 (.926 / .922) | .961 / .936 (.842 / .809) | .961 / .936 | .979 / .988 |
| SEM intra C / A | .951 / .953 (.951 / .906) | .934 / .938 (.939 / .745) | .934 / .941 | |
| SEM inter C / A | .950 / .951 (.950 / .899) | .939 / .930 (.936 / .692) | .939 / .935 | |
| SEM overall C / A | .950 / .952 (.954 / .899) | .938 / .928 (.945 / .784) | .944 / .936 | |

- C3 passes for every estimand without exemptions → `ci_method = "auto"`
  stays MLS. Satterthwaite fails again for every agreement statistic
  (mixes D and R).
- **C6 not met, on the conservative side:** the MLS agreement
  *coefficients* (intra A, inter A, Φ) cover up to .97–.99 at n = 30, so
  their tests have size as low as .01–.035 (less power), not .035–.065.
  Nothing is liberal. Likely contributors: the omitted Ting G*/H* terms
  for several same-signed mean squares, and the ratio inversion. Kept (MLS
  conservative beats Satterthwaite liberal); documented. **Open:** try the
  G*/H* terms, or a different ratio interval, for the agreement
  coefficients.
- Non-normal: MLS .85–.94 mean (agreement statistics .91–.94, consistency
  .85–.89); Satterthwaite lower.
- **C7 (ignoring occasions):** the repeat model's intra SEM interval
  covered the true intra-agreement SEM .675 on average (min .24) and the
  intra-consistency SEM .50 (min .04), the latter with +30% bias; its intra
  ICC covered .84 / .76. Treating shared occasions as repeats is not a
  harmless simplification.
- Bias (unconstrained): −5% to +1% for coefficients and SEMs at n = 10,
  within ±2.4% on average at n = 30.

**C5: REML, unbalanced** (`reli_sims/occasion_reml.R`; n = 30, normal,
20% of cells MCAR, 1000 sims, 16 cells). MLS: mean .951–.969, min .922
(SEM overall A, mix X) → **passes; REML keeps the analytic MLS interval.**
Satterthwaite down to .75–.79 for agreement statistics in mixes D and R.

**C3 bootstrap (C4 criterion):**
(`reli_sims/occasion_boot.R`; n ∈ {10, 30}, k ∈ {2, 3}, m = 2, r = 1,
4 mixes, normal and non-normal, 300 sims × 499 replicates; mean squares in
closed form, estimand coefficients from `.facet_ms_coef()`, jackknife SE
checked equal to `.facet_se_z()`). Mean coverage, non-normal (min at
n = 30):

| Estimand | perc | BCa | stud | MLS (analytic, for comparison) |
|---|---|---|---|---|
| ICC intra C | .871 (.860) | .860 (.830) | **.917 (.903)** | .848 |
| ICC intra A | .904 (.847) | .874 (.800) | **.917 (.860)** | .914 |
| ICC inter A | .895 (.820) | .869 (.767) | **.919 (.837)** | .916 |
| Φ (1,1) | .898 (.880) | .876 (.847) | **.920 (.900)** | .921 |
| SEM intra C | .796 (.823) | .834 (.863) | **.926 (.907)** | .882 |
| SEM intra A | .841 (.663) | **.911 (.797)** | .864 (.743) | .923 |
| SEM overall A | .833 (.683) | **.913 (.823)** | .862 (.763) | .929 |

(The MLS column is from the analytic grid, whose cells differ: m ∈ {2, 3}.)

- C4: the generalised hybrid (MS_i, MS_o and MS_io scaled) beats subject
  resampling in every cell: minimum +0.013 (BCa), +0.037 (perc); mean +0.20
  to +0.22; subject-only fell to .14 for the intra agreement SEM. Scaling
  MS_io (rather than replacing it) is adequate; the open check of 15.6 is
  closed.
- `boot_ci = "auto"` unchanged: `stud`, except `bca` for SEMs that count
  effects without subjects (agreement SEMs), which the generalised rule
  already selects.
- The .88 floor fails for agreement statistics when the 2-day occasion
  effect dominates (mix D: .62–.87) and with rater bias (mix R), for every
  type: σ²_o on 1 df. Documented, as for few items.
- With occasions, the analytic MLS intervals are as good as or better than
  the bootstrap for the agreement statistics even with non-normal data;
  the bootstrap helps for the consistency statistics (+.04 to +.07).
