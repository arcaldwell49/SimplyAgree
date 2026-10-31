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

- `cbls_reg()`: see `cbls_reg_roadmap.md`.
