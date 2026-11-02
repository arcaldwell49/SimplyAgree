# bls_reg(): coverage of the intervals and size of the joint test

Status (2026-10-31; studies 5-6 and a rerun of all 2026-11-02): all criteria
met (study 2: with the right variance model; study 3: when the equation
error is split as assumed).

**Standard errors (2026-11-02).** All studies were rerun after the default
standard errors changed to `se_method = "delta"`: an errors-in-variables
sandwich (Fuller's variance with equal weights) plus a delta-method term
for the estimated error variances and correlation. The tables below are
the rerun; values with the earlier SEs (`se_method = "bivregbls"`, those of
BivRegBLS) are given where they differ. Study 6 shows why: the BivRegBLS SEs
are too small when the error of x is not small relative to the spread of
the true values, down to 0.71 coverage. In studies 1-4 the true values are
widely spread (reliability of x about 0.96 or more), so the change is small:
the coefficient CIs moved up by at most 0.02, and so did the prediction and tolerance
intervals (which also use the variance of the line).

## Question

Do the 95% confidence intervals (intercept, slope, line), prediction
intervals (XY and Bland-Altman plots), CBLS slope interval, and the joint
test of `bls_reg()` hold their nominal levels with known variances and
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
| known  | 0.945-0.950 | 0.944-0.950 | 0.940-0.960 | 0.957-0.966 | 0.958-0.964 | 0.943-0.952 | 0.042-0.057 |
| 2 reps | 0.945-0.954 | 0.948-0.956 | 0.934-0.966 | 0.943-0.968 | 0.942-0.966 | 0.947-0.957 | 0.047-0.056 |
| 3 reps | 0.949-0.955 | 0.942-0.954 | 0.942-0.959 | 0.946-0.966 | 0.945-0.966 | 0.945-0.958 | 0.038-0.060 |
| 1-4    | 0.946-0.959 | 0.948-0.961 | 0.942-0.961 | 0.950-0.963 | 0.950-0.963 | 0.943-0.961 | 0.044-0.054 |

All fits converged; none failed. The intervals are close to nominal
throughout. With the BivRegBLS SEs, the intercept and slope intervals
were slightly liberal with known variances and one measurement each (0.932-0.945);
the errors-in-variables term brings them to 0.944-0.950. The joint test
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
| known        | 0.938-0.958 | 0.962-0.964 | 0.940-0.954 | 0.953-0.958 | 0.045-0.052 |
| proportional | 0.934-0.967 | 0.956-0.975 | 0.946-0.956 | 0.952-0.963 | 0.036-0.054 |
| power        | 0.945-0.975 | 0.956-0.964 | 0.940-0.960 | 0.951-0.961 | 0.051-0.054 |
| constant     | 0.921-0.995 | 0.999-1.000 | 0.818-0.846 | 0.909-0.924 | 0.072-0.084 |

With the right variance model (known, or estimated as proportional or
power), all intervals are close to nominal and the joint test holds its
size; all fits converged. Ignoring the heteroscedasticity (constant model)
gives prediction intervals far too wide at low levels and too narrow at high
levels, and an inflated joint test size.

## Study 3: equation error

Item-specific deviations of the true values from the line (method-by-item
or matrix effects) make the dispersion `s^2 > 1`. BLS/CBLS and BivRegBLS
model only measurement error, so their prediction intervals are too narrow
and the error ratio (from replicates) is wrong. `bls_reg(equation_error =
TRUE)` (default) estimates a common-tau equation error (var_x + tau^2,
var_y + b^2 tau^2, as `method_vc(tau = "common")`) and refits.

`sim_cbls_eq.R`: t ~ N(20, 5^2); x = t + e_x (SD 0.6); y = 1 + 1.1 t +
delta + e_y (SD 0.9), delta ~ N(0, sd_eq^2) per subject (all in y);
sd_eq = 0, 0.5, 1.5; n = 20, 60; known variances (one measurement) or 2-3
replicates; 1000 data sets per cell (`summary_eq.csv`).

| sd_eq | slope CI | PI, equation_error = TRUE | PI, FALSE (BivRegBLS) | dispersion test rejects |
|---|---|---|---|---|
| 0   | 0.943-0.958 | 0.949-0.963 | 0.940-0.959 | 0.041-0.059 (size) |
| 0.5 | 0.936-0.958 | 0.941-0.961 | 0.913-0.939 | 0.15-0.76 |
| 1.5 | 0.867-0.944 | 0.940-0.960 | 0.761-0.809 | 0.92-1.00 |

`sim_cbls_eq_side.R`: sd_eq = 1.5, with the same total deviation variance
placed in y only, in x only, or equally in both (common tau true);
`summary_eq_side.csv`.

| deviations in | slope CI | PI (XY) | PI (MD) | PI without equation error |
|---|---|---|---|---|
| both (common tau) | 0.951-0.959 | 0.936-0.951 | 0.935-0.951 | 0.766-0.802 |
| y only | 0.892-0.947 | 0.946-0.951 | 0.948-0.952 | 0.765-0.804 |
| x only | 0.852-0.926 | 0.944-0.962 | 0.942-0.962 | 0.785-0.810 |

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

## Study 5: linked replicates (`replicates = "linked"`, 2026-11-02)

`sim_cbls_linked.R`, `summary_linked.csv`. Both methods measure each item
on the same occasions; the occasion effect a ~ N(0, omega^2) is shared:
x = t + a + c_x + e_x (SD 0.6), y = 1 + 1.1 (t + a + c_y) + e_y (SD 0.9),
t ~ N(20, 5^2), c ~ N(0, tau^2) (equation error). 15 or 40 items, 2 or 4
occasions, omega 0.5 or 2, tau 0 or 0.5; 1000 data sets per cell. The
linked fit is compared with the exchangeable fit (`replicates` left at its
default) on the same data. The prediction interval is for a new item
measured once by both methods on one occasion.

Criteria (set before running): coverage 0.93-0.97 for the slope and
intercept CIs and the prediction intervals.

| omega | tau | slope CI | intercept CI | PI (xy / md) | slope CI, exchangeable | PI, exchangeable | PI width, linked / exchangeable |
|---|---|---|---|---|---|---|---|
| 0.5 | 0   | 0.951-0.956 | 0.948-0.958 | 0.950-0.957 | 0.952-0.956 | 0.976-0.984 | 0.84-0.89 |
| 0.5 | 0.5 | 0.949-0.955 | 0.948-0.957 | 0.946-0.959 | 0.949-0.954 | 0.961-0.972 | 0.90-0.94 |
| 2   | 0   | 0.944-0.960 (0.930-0.941) | 0.936-0.960 (0.924-0.946) | 0.950-0.969 | 0.944-0.962 | 1.000 | 0.35-0.39 |
| 2   | 0.5 | 0.949-0.962 (0.913-0.942) | 0.948-0.956 (0.920-0.947) | 0.948-0.953 | 0.950-0.967 | 1.000 | 0.42-0.45 |

Values in parentheses: the first run, with `se_method = "bivregbls"`. The
estimated error correlation was unbiased (0.31-0.34 and 0.88-0.89, true
0.33 and 0.89).

1. **Prediction intervals: criterion met** (0.946-0.969). Ignoring the
   linking (the exchangeable fit) counts the occasion effects of both
   methods as independent errors: its intervals covered 0.96-1.00 and were
   up to 2.9 times as wide.
2. **Slope and intercept CIs: criterion met with the delta-method SEs**
   (0.936-0.962). With the BivRegBLS SEs they were slightly liberal with a
   large occasion effect (omega = 2, about 3 times the larger error SD):
   0.913-0.947. In the worst cell (40 items, 2 occasions, equation error)
   the slope was unbiased (mean 1.097 with 400 data sets) but its SD
   (0.039) was 13% larger than the mean BivRegBLS SE (0.034). Oracle fits
   (error parameters known, no equation error) separated two causes. The
   BivRegBLS SE was about 10% too small even with the parameters known
   (oracle SD 0.0271, SE 0.0245), because R uses the observed means, whose
   spread includes the error of x; Fuller's variance (the
   errors-in-variables sandwich) gave 0.0267. And estimating the variances
   and correlation added variance (7.7e-5), 87% of which the delta-method
   term recovers (6.7e-5). With a large occasion effect, the residual
   variance `var_y + b^2 var_x - 2 b cov` is a small difference of large
   estimated terms, so both matter more.
## Study 6: standard errors and the reliability of x (2026-11-02)

`sim_cbls_se.R`, `summary_se.csv`. Both SE methods on the same fits, as
the reliability of x falls: t ~ N(20, t_sd^2) with t_sd = 1.5, 3, 6;
x = t + e_x (SD 1), y = 1 + 1.1 t + e_y (SD 1.2); n = 15 or 40; one
measurement each with known variances, 3 exchangeable replicates, or 3
linked occasions with a shared occasion effect (SD 2). No equation error
(`equation_error = FALSE`); 1000 data sets per cell.

Criteria (set before running): coverage 0.93-0.97 for the slope,
intercept, and CBLS slope CIs.

| design | t_sd | slope / intercept / CBLS slope, delta | same, bivregbls | SE delta / bivregbls |
|---|---|---|---|---|
| known  | 1.5 | 0.959-0.970 | 0.869-0.897 | 1.38-1.51 |
| known  | 3   | 0.939-0.956 | 0.922-0.935 | 1.09-1.11 |
| known  | 6   | 0.951-0.962 | 0.948-0.958 | 1.02-1.03 |
| reps   | 1.5 | 0.952-0.964 | 0.923-0.939 | 1.14-1.16 |
| reps   | 3   | 0.936-0.954 | 0.931-0.946 | 1.03-1.04 |
| reps   | 6   | 0.934-0.950 | 0.934-0.948 | 1.01 |
| linked | 1.5 | 0.969-0.976 | 0.707-0.734 | 2.23-5.72 |
| linked | 3   | 0.938-0.965 | 0.859-0.906 | 1.24-1.40 |
| linked | 6   | 0.948-0.959 | 0.937-0.949 | 1.06-1.07 |

1. **Delta-method SEs: criterion met** (0.934-0.976), slightly
   conservative (0.97-0.98) only for linked occasions with the lowest
   reliability, where the occasion effect is larger than the spread of
   the true values.
2. **BivRegBLS SEs: too small at low reliability**, down to 0.71 (linked)
   and 0.87 (one measurement each). They are fine (0.93-0.96) when the
   true values are widely spread (t_sd = 6), as in studies 1-4.
3. Remaining limit: the uncertainty in the estimated equation error
   (tau^2) is not included; with a large equation error placed in one
   method only, the slope is biased anyway (study 3).
