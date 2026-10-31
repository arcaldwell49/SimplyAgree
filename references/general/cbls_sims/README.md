# cbls_reg(): coverage of the intervals and size of the joint test

Status (2026-10-31): both studies run; all criteria met (study 2: with the
right variance model).

## Question

Do the 95% confidence intervals (intercept, slope, line), prediction
intervals (XY and Bland-Altman plots), CBLS slope interval, and the joint
test of `cbls_reg()` hold their nominal levels with known variances and
with balanced or unbalanced replicates?

Criteria (set before running): coverage 0.935-0.965 and joint test size
0.035-0.065 (about 3 Monte Carlo SEs with 2000 data sets).

## Design

`sim_cbls.R`. True values t ~ N(20, 5^2); x = t + e_x, y = a + b t + e_y,
a = 1, b = 1.1 (coverage) or a = 0, b = 1 (joint test size); SD of x errors
1, var_x / var_y = 0.5 or 2; n = 10, 20, 60 subjects; one measurement each
with `var_x`, `var_y` given ("known"), 2 or 3 replicates, or 1-4 at random;
2000 data sets per cell. Prediction intervals: a new subject measured once
by each method. The line CI at x = 12, 20, 28.

## Results (`summary.csv`)

| design | intercept | slope | line (3 points) | PI, XY | PI, MD | CBLS slope | joint test size |
|---|---|---|---|---|---|---|---|
| known  | 0.937-0.942 | 0.932-0.945 | 0.934-0.960 | 0.950-0.958 | 0.950-0.956 | 0.934-0.946 | 0.045-0.060 |
| 2 reps | 0.942-0.952 | 0.942-0.953 | 0.934-0.965 | 0.935-0.953 | 0.935-0.953 | 0.940-0.953 | 0.050-0.057 |
| 3 reps | 0.946-0.952 | 0.941-0.953 | 0.941-0.957 | 0.943-0.962 | 0.943-0.962 | 0.944-0.957 | 0.040-0.062 |
| 1-4    | 0.941-0.957 | 0.942-0.957 | 0.942-0.960 | 0.944-0.957 | 0.944-0.957 | 0.941-0.959 | 0.047-0.057 |

All fits converged; none failed. The intervals are close to nominal
throughout. With known variances and one measurement each, the intercept
and slope intervals are slightly liberal (about 0.94): `s^2` is estimated
on n - 2 df, although the error variances are known. The joint test
(Wald, F(2, n - 2), without BivRegBLS's Hotelling factor) holds its size.

Study 2 below covers error variances that change with the level.

## Study 2: error variances that change with the level

`sim_cbls_vf.R`. True values t ~ U(10, 100); x = t (1 + e_x),
y = (a + b t)(1 + e_y): proportional error, CV 5% (x) and 8% (y); a = 2,
b = 1.05 (a = 0, b = 1 for the joint test size). n = 20, 60; fits: the true
variance functions given with one measurement each ("known"), or estimated
from 2, 3, or 1-4 replicates with `var_model = "proportional"`, `"power"`,
or (misspecified) `"constant"`. Prediction intervals for new subjects at a
low (10-30) and a high (80-100) level. 1000 data sets per cell (Monte
Carlo SE about 0.007). Results in `summary_vf.csv`.

| fit | intercept, slope, line | PI low | PI high | PI (MD) | joint test size |
|---|---|---|---|---|---|
| known        | 0.933-0.950 | 0.944-0.946 | 0.936-0.952 | 0.947 | 0.053-0.055 |
| proportional | 0.933-0.960 | 0.948-0.965 | 0.944-0.956 | 0.948-0.955 | 0.041-0.056 |
| power        | 0.945-0.968 | 0.933-0.951 | 0.940-0.958 | 0.935-0.953 | 0.056-0.064 |
| constant     | 0.918-0.995 | 0.999-1.000 | 0.809-0.831 | 0.904-0.915 | 0.072-0.085 |

With the right variance model (known, or estimated as proportional or
power), all intervals are close to nominal and the joint test holds its
size; all fits converged. Ignoring the heteroscedasticity (constant model)
gives prediction intervals far too wide at low levels and too narrow at high
levels, and an inflated joint test size.

## Study 3: equation error

Item-specific deviations of the true values from the line (method-by-item
or matrix effects) make the dispersion `s^2 > 1`. BLS/CBLS and BivRegBLS
model only measurement error, so their prediction intervals are too narrow
and the error ratio (from replicates) is wrong. `cbls_reg(equation_error =
TRUE)` (default) estimates a common-tau equation error (var_x + tau^2,
var_y + b^2 tau^2, as `method_vc(tau = "common")`) and refits.

`sim_cbls_eq.R`: t ~ N(20, 5^2); x = t + e_x (SD 0.6); y = 1 + 1.1 t +
delta + e_y (SD 0.9), delta ~ N(0, sd_eq^2) per subject (all in y);
sd_eq = 0, 0.5, 1.5; n = 20, 60; known variances (one measurement) or 2-3
replicates; 1000 data sets per cell (`summary_eq.csv`).

| sd_eq | slope CI | PI, equation_error = TRUE | PI, FALSE (BivRegBLS) | dispersion test rejects |
|---|---|---|---|---|
| 0   | 0.942-0.958 | 0.949-0.963 | 0.940-0.958 | 0.041-0.059 (size) |
| 0.5 | 0.934-0.955 | 0.941-0.959 | 0.913-0.939 | 0.15-0.76 |
| 1.5 | 0.861-0.938 | 0.941-0.957 | 0.764-0.808 | 0.92-1.00 |

`sim_cbls_eq_side.R`: sd_eq = 1.5, with the same total deviation variance
placed in y only, in x only, or equally in both (common tau true);
`summary_eq_side.csv`.

| deviations in | slope CI | PI (XY) | PI (MD) | PI without equation error |
|---|---|---|---|---|
| both (common tau) | 0.942-0.953 | 0.936-0.951 | 0.935-0.951 | 0.770-0.801 |
| y only | 0.879-0.938 | 0.946-0.951 | 0.948-0.951 | 0.764-0.797 |
| x only | 0.834-0.914 | 0.943-0.962 | 0.942-0.962 | 0.785-0.807 |

- The prediction intervals hold (0.94-0.96) whenever equation error is
  modeled, wherever the deviations are; without it they cover 0.76-0.81.
- The dispersion test holds its size and detects equation error.
- The slope is unbiased when the deviations are split as assumed (common
  tau). When they belong to one method only, the slope is biased (worse
  with more subjects), with or without the correction (BLS alone: 0.890-0.916
  with all deviations in y, n = 60). With two methods the split cannot be
  estimated; this is the identifiability problem of Deming regression with
  error ratios from replicates (Carstensen, 2010). With three or more
  methods, `method_vc()` / `method_convert(bias = "scaled")` can estimate
  method-specific tau.

## Study 4: tolerance limits around the line (experimental)

`sim_cbls_tl.R` (`summary_tl.csv`). t ~ N(20, 5^2); x = t + c_x + e_x
(SD 0.6), y = 1 + 1.1 (t + c_y) + e_y (SD 0.9); common-tau equation error
c ~ N(0, tau^2), tau = 0 or 0.7; known variances (one measurement each) or 2-3
replicates; n = 10, 20, 60; 1000 data sets per cell. For a new pair at x0
(12, 20, 28), y0 - (a + b x0) ~ N(0, sigma^2), so the content of limits is
exact. 95% content (`pred_level`) and confidence (`tol_level`).

Francq & Govaerts (2016) give tolerance intervals only for a constant bias
(horizontal lines; `tolerance_limit()`), so the construction is our own. It
took five versions; the first four, for the record:

1. Howe-type `z u_s / s sqrt(s^2 + se^2)` with a MOVER bound over the error
   pieces (as `tolerance_limit(bound_type = "joint")`): joint confidence 0.73
   with known variances (the line's uncertainty only enters on average),
   0.89 with replicates and equation error.
2. Joint = `level` quantile of |N(c, u_s^2)| with c a t bound for the line
   and u_s the SD bound, each at sqrt(tol_level); whole spread on n - 2 df
   when tau-hat > 0: 0.95 with known variances, but 0.998-1.000 with
   replicates.
3. As 2, pieces always: no change for replicates.
4. As 3, bounds at tol_level (no split), Wallis's factor when tau-hat > 0 on
   the whole single-measurement spread: replicates 0.97-0.999 (the
   single-measurement error variances are known from the replicates; only
   the spread of the means is uncertain).

Final version (5): without equation error, c (t bound, n - 2 df) and u_s
(MOVER over the error pieces, exact when known) at `tol_level`; with
equation error, the spread of the subject means about the line (measurement
error of the means + equation error) and the variance of the line share a
chi-square bound on n - 2 df, and the single-measurement error variances are
taken as known (Wallis's textbook factor with one measurement per subject).

| design | tau | joint confidence | iu upper / lower | mean content of PI |
|---|---|---|---|---|
| known | 0   | 0.950-0.962 | 0.952-0.966 | 0.955-0.956 |
| known | 0.7 | 0.875 (n = 10), 0.936-0.946 | 0.912-0.950 | 0.930-0.946 |
| 2-3 reps | 0   | 0.967-0.992 | 0.958-0.987 | 0.953-0.960 |
| 2-3 reps | 0.7 | 0.931-0.958 | 0.933-0.952 | 0.941-0.949 |

Close to nominal with known variances, and with replicates and equation
error. Conservative with replicates and no equation error: by chance
tau-hat > 0 in about half the data sets, which adds uncertainty. Liberal
(0.875) only with 10 subjects, known variances, and equation error, where
tau-hat is sometimes 0. Kept experimental.
