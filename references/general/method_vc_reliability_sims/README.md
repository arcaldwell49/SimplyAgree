# method_vc(replicates = "linked"): coverage of the within-item reliability

Status (2026-11-02): **run**. "average" and common-SD intervals (logit):
0.93 to 0.96 in every cell, criterion met. Per-method intervals: met when
omega is comparable to the residual SDs and with three methods; liberal
(down to 0.86) for the method with the smaller SD with two methods and a
large omega. The logit interval is used; the F interval was no better.

## Question

With linked replicates, `method_vc()` reports the within-item reliability
R = omega^2 / (omega^2 + sigma_m^2) per method and, with method-specific
SDs, R = omega^2 / (omega^2 + mean sigma_m^2) ("average", equal to the ICC
of item-centred data; trending_agreement_plan.md, step 0). Do its 95%
intervals cover?

Two intervals were compared (`mc_vc_reliability(ci = )`):

- "logit": delta method for log(omega^2 / sigma^2) with the covariance of
  the estimated components (`vc_cov`), back-transformed;
- "F": omega^2 / sigma^2 divided by F quantiles on the components' df (as
  for a one-way ANOVA ICC), ignoring their covariance.

Criteria (set before running): coverage 0.93 to 0.97 in every cell for the
reported interval; report which interval is used.

## Design

`sim_reliability.R`. Data from `sim_methods(replicates = "linked")` (unit
slope: both methods follow the within-item changes), tau = 0.3, biases 0
and 0.5 (-0.3).

| cells | methods | sigma | items | occasions | omega |
|---|---|---|---|---|---|
| 1-8 | 2 | 0.25, 0.25 (also fitted with `sigma = "common"`) | 20, 50 | 3, 6 | 0.3, 1 |
| 9-16 | 2 | 0.25, 0.6 | 20, 50 | 3, 6 | 0.3, 1 |
| 17-18 | 3 | 0.25, 0.4, 0.6 | 20 | 3 | 0.3, 1 |

1000 data sets per cell. Methods A, B (C) have the SDs in that order, so A
has the smaller SD.

## How to run

```
Rscript references/general/method_vc_reliability_sims/sim_reliability.R
```

Environment variables `MVREL_NSIM`, `MVREL_CORES`, `MVREL_CELLS`. Finished
cells go to `raw/` (not committed); `summary.csv` (one row per cell, fit,
reliability and interval) is rebuilt at the end. About 15 minutes on 14
workers.

## Results

Run 2026-11-02 (R 4.4.1, Windows, 14 workers). Coverage of the logit
interval (F in parentheses), among data sets with a two-sided interval:

| cell | items | occ. | omega | sigma | A | B | C | average | common |
|---|---|---|---|---|---|---|---|---|---|
| 1 | 20 | 3 | 0.3 | equal | 0.976 (0.994) | 0.976 (0.989) | | 0.958 (0.949) | 0.960 (0.948) |
| 2 | 50 | 3 | 0.3 | equal | 0.964 (0.975) | 0.966 (0.967) | | 0.943 (0.926) | 0.946 (0.926) |
| 3 | 20 | 6 | 0.3 | equal | 0.964 (0.966) | 0.960 (0.967) | | 0.947 (0.928) | 0.947 (0.928) |
| 4 | 50 | 6 | 0.3 | equal | 0.954 (0.946) | 0.949 (0.940) | | 0.953 (0.932) | 0.953 (0.931) |
| 5 | 20 | 3 | 1 | equal | 0.921 (0.912) | 0.883 (0.860) | | 0.930 (0.937) | 0.936 (0.937) |
| 6 | 50 | 3 | 1 | equal | 0.935 (0.926) | 0.920 (0.907) | | 0.942 (0.941) | 0.944 (0.940) |
| 7 | 20 | 6 | 1 | equal | 0.943 (0.938) | 0.902 (0.890) | | 0.958 (0.957) | 0.960 (0.958) |
| 8 | 50 | 6 | 1 | equal | 0.949 (0.937) | 0.932 (0.923) | | 0.955 (0.955) | 0.953 (0.951) |
| 9 | 20 | 3 | 0.3 | 0.25, 0.6 | 1.000 (0.994) | 0.956 (0.963) | | 0.946 (0.929) | |
| 10 | 50 | 3 | 0.3 | 0.25, 0.6 | 0.995 (0.999) | 0.955 (0.957) | | 0.956 (0.941) | |
| 11 | 20 | 6 | 0.3 | 0.25, 0.6 | 0.999 (1.000) | 0.947 (0.954) | | 0.947 (0.933) | |
| 12 | 50 | 6 | 0.3 | 0.25, 0.6 | 0.978 (0.958) | 0.952 (0.958) | | 0.955 (0.938) | |
| 13 | 20 | 3 | 1 | 0.25, 0.6 | 0.861 (0.770) | 0.971 (0.980) | | 0.960 (0.959) | |
| 14 | 50 | 3 | 1 | 0.25, 0.6 | 0.887 (0.828) | 0.955 (0.977) | | 0.946 (0.940) | |
| 15 | 20 | 6 | 1 | 0.25, 0.6 | 0.892 (0.827) | 0.950 (0.967) | | 0.944 (0.938) | |
| 16 | 50 | 6 | 1 | 0.25, 0.6 | 0.920 (0.882) | 0.969 (0.982) | | 0.949 (0.948) | |
| 17 | 20 | 3 | 0.3 | 0.25, 0.4, 0.6 | 0.994 (0.995) | 0.967 (0.979) | 0.957 (0.962) | 0.964 (0.957) | |
| 18 | 20 | 3 | 1 | 0.25, 0.4, 0.6 | 0.952 (0.936) | 0.953 (0.964) | 0.931 (0.938) | 0.931 (0.930) | |

1. **"average" and common SD: criterion met** with the logit interval
   (0.930 to 0.964); the F interval is slightly liberal with omega = 0.3
   (0.926 to 0.958). The point estimates are unbiased (mean within 0.012
   of the truth).
2. **Per method: depends on identification.** With two methods the
   within-item data give three moments (two variances and a covariance)
   for three unknowns (omega, sigma_A, sigma_B), so the split of the
   residual variance is weakly identified when omega is large. With omega
   four times the smaller SD, the reliability of that method (true value
   0.94) is often estimated at 1 (an SD at zero: 7% to 30% of data sets
   in cells 5-7 and 13-16, 0.5% in cell 8), and its interval misses low
   (coverage 0.86 to 0.95). With omega comparable to the SDs, the
   intervals are conservative (up to 1.00) for the method with the smaller
   SD. With three methods: 0.93 to 0.99. The docs recommend the "average"
   row when omega is large relative to the SDs with two methods.
3. **Residual SD at zero.** In data where both methods track perfectly, an
   SD at zero is common (up to 30% with two methods, 7% with three in
   cell 18), so `method_vc()` notes it in `print()` rather than warning
   (the warning first planned for a non-unit within-item slope would be a
   false alarm in these designs).
4. **No interval** (apVar unavailable) in up to 7% of data sets (cells
   5-8); those are left out of the coverage.
