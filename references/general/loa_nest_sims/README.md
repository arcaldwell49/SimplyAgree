# agreement_limit(data_type = "nest"): coverage of the LoA bounds

Status (2026-10-31): run; **the bounds were liberal**. Fixed on branch
`fix-loa-nest-bounds` (the "Proposed fix" below, now applied).

## Question

`calc_loa_nest()` (R/agreement_limit.R) splits the variance of a single
difference for the confidence bounds of the limits into `between_variance`
(s_b^2, from `lmer`) on n - 1 df and `(1 - 1/m_h) within_variance` on N - n
df, and uses `s_b^2 / n` as the variance of the bias. The quantity that has
n - 1 df, and whose 1/n is the variance of the bias, is the variance of the
subject mean differences, `s_b^2 + s_w^2 / m_h` (Zou, 2011; the older
`agree_nest()` uses it as `d_var`). The current pieces add up to
`s_b^2 + (1 - 1/m_h) s_w^2`, not to the total, so `s_w^2 / m_h` gets no
uncertainty. Does it matter?

## Design

`sim_loa_nest.R`. Differences d_ij = u_i + e_ij, bias 0, SD of a single
difference 1, between/within variance ratio 0.1, 1, 10; n = 10, 20, 40
subjects; 2 or 5 pairs per subject, or 1 to 5 at random (unbalanced);
2000 data sets per cell. The bounds are computed from one `lmer` fit both
ways ("current" reproduces `agreement_limit()` exactly, checked for both
`loa_calc` methods). Coverage of the one-sided 95% bound of each limit
(`bound_type = "iu"`), averaged over the lower and upper limit; Monte Carlo
SE about 0.005.

## Results (`summary.csv`)

| between/within | current, MOVER | Zou, MOVER | current, BA | Zou, BA |
|---|---|---|---|---|
| 0.1 | 0.846-0.911 | 0.949-0.974 | 0.789-0.866 | 0.910-0.945 |
| 1   | 0.878-0.920 | 0.942-0.963 | 0.811-0.887 | 0.891-0.932 |
| 10  | 0.923-0.941 | 0.937-0.946 | 0.851-0.914 | 0.865-0.918 |

- The current MOVER bounds (the default `loa_calc`) are liberal, most when
  the within-subject variance dominates and pairs per subject vary
  (0.846 with 40 subjects, ratio 0.1, 1-5 pairs). More subjects do not help:
  the error is in the decomposition, not small-sample.
- With Zou's decomposition, MOVER covers 0.937-0.974 (slightly conservative
  when within dominates, slightly liberal when between dominates).
- The Bland-Altman approximation (`loa_calc = "blandaltman"`) is liberal
  either way; the fix improves it (0.789-0.914 to 0.865-0.945).

## Proposed fix

In `calc_loa_nest()`, with `v1 = between_variance + within_variance / mh`:

- `loa_var`: `v1 * infl / n_sub` for the bias, and `v1^2 / (n_sub - 1)` in
  place of `between_variance^2 / (n_sub - 1)`;
- MOVER: `move_u_1` on `v1`; `LME` with `v1 * infl / n_sub`;
- vignette "Nested" calculation steps (`s_b^2` to `s_b^2 + s_w^2 / m_h` in
  u_1, LME, and S_LoA^2);
- NEWS entry (bug fix: the bounds were too narrow), and a test that the
  bounds match Zou's formula / `agree_nest()` without `prop_bias`.

The same substitution exists in `agree_nest()` with `prop_bias = TRUE`
(`d_var` is the lmer between-subject variance there), a superseded function.
