# tdi(): coverage of the bounds

Status (2026-10-31): run; results below decide the default (`method = "gpq"`).

## Question

Do the one-sided 95% upper bounds for the total deviation index (and the
lower bounds for the coverage probability) from `tdi()` cover the true
values at the nominal rate, for each `method` ("gpq", "delta", "tolerance"),
for independent, replicated, and nested data, and for each class of input
(`agreement_limit()`, `tolerance_limit()`, `method_vc()`)?

Criteria (set before running): a method is acceptable as the default if its
coverage is at least about 0.935 in every setting (2 Monte Carlo SEs below
0.95 with 1000 replicates) and not grossly conservative (well below 0.99)
where the bias is not zero.

## Design

`sim_tdi.R` (run from the package root; `TDI_NS`, `TDI_NR` set the number of
data sets; `TDI_SKIP_SIMPLE=true` reruns only part 2).

1. **Simple** (`summary_simple.csv`): D ~ N(mu, 1); n = 10, 20, 50;
   mu = 0, 0.5, 1.5; p = 0.8, 0.9, 0.95; 2000 data sets per cell. The engine
   is called with the inputs `agreement_limit(data_type = "simple")` gives.
2. **Replicated / nested** (`summary_reps.csv`): n = 10, 20, 40 subjects,
   3 replicates, mu = 0 or 1, p = 0.9, SD of a single difference 1
   (tau = 0.4, sigma_x = 0.5); 1000 data sets per cell.
   - `reps`: true value constant within subject;
     `agreement_limit(data_type = "reps")`.
   - `nest`: the true value changes between occasions (SD 0.7), shared by
     both methods; `agreement_limit(data_type = "nest")`.
   - `tol_id`: the nest data, `tolerance_limit(id = "id")`.
   - `mvc`: the nest data, `method_vc(replicates = "linked")`.

The CP bound is the dual of the TDI bound (it is the largest p whose TDI
bound is below delta), so with delta = the true TDI its coverage equals the
coverage of the TDI bound. The `cp_delta` column of the first run used an
earlier logit-scale bound and is superseded.

## Results

See `summary_simple.csv`, `summary_reps.csv` and the notes below.

Monte Carlo SE about 0.005 (part 1) and 0.007 (part 2).

**Simple data** (27 cells): gpq 0.943-0.980, delta 0.925-0.957, tolerance
0.945-0.989. gpq is conservative without bias (0.97-0.98) and close to
nominal with bias 1.5 SD; delta is lowest with large bias and p >= 0.9; the
Escaramis tolerance bound (exact noncentral t here) tracks gpq closely.

**Replicated / nested data**, coverage of the TDI bound:

| design | gpq, total variance on Satterthwaite df (first run) | gpq, by component (final) | delta | tolerance, max abs. joint TL (first run) | tolerance, Escaramis (final) |
|---|---|---|---|---|---|
| reps   | 0.913-0.965 | 0.956-0.987 | 0.920-0.948 | 0.979-1.000 | 0.931-0.987 |
| nest   | 0.936-0.978 | 0.953-0.986 | 0.922-0.960 | 0.983-1.000 | 0.937-0.984 |
| tol_id | 0.936-0.971 | 0.958-0.988 | 0.911-0.960 | 0.990-1.000 | 0.941-0.988 |
| mvc    | 0.948-0.976 | (unchanged method) 0.942-0.985 | 0.936-0.968 | 0.986-0.999 | 0.940-0.990 |

The lowest values of the first gpq run were with bias 1 SD and 10-20
subjects: drawing the bias independently of the total variance ignores that
the SE of the bias depends on one component (the variance of a subject
mean). Drawing each component on its own df and scaling the bias draw by the
first component (exact pivots for these models) fixed it.

The first "tolerance" bound, max(|L|, |U|) of a p-content two-sided
tolerance interval, was valid but conservative (up to 1.00 with bias) and
not what Escaramis et al. describe. The final version follows their paper
(and TDIagree's code): a one-sided tolerance bound for the quantile q at
which |mu| + z_q sigma is the TDI. With replicates it uses the MOVER bound
of tolerance_limit(bound_type = "iu") and dips to about 0.93 with bias and
10-20 subjects. TDIagree counts measurements (n = 2 x subjects) rather than
differences, which makes its bound narrower; tests/testthat/test-tdi.R
checks that the same formula reproduces TDIagree's bound with its inputs.

**Decision:** `"gpq"` (by component where the components are known) stays
the default; it meets the criteria everywhere. `"delta"` is kept as an
analytic alternative, documented as somewhat liberal; `"tolerance"` as the
Escaramis et al. (2010) bound, documented as close to gpq for independent
data and somewhat liberal with replicates, bias, and few subjects.

**method_vc failures:** with 10 items, 4-5% of fits had a variance component
at zero, for which `method_vc()` gives no approximate covariance and no
bounds (`n_failed`); `tdi()` then returns the estimate with `NA` bounds and a
warning. These fits are excluded from the coverage above.

`summary_simple.csv` and `summary_reps.csv` hold the final run.
