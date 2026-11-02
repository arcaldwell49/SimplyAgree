# linked_reps_test(): size and power

Status (2026-11-01): **run** (R 4.4.1, Windows, 14 workers, about 1.5
hours). Size and power criteria met; `sd_perm` is 0.6% to 3.9% below
`sd_exch`, more than the 2% criterion in three cells (see Results).
Rerun 2026-11-02 after `method_vc()` started refitting without variance
components at zero: the rejection rates are identical in every cell and the
mean estimates differ only from the fifth digit (`summary.csv` holds the
rerun), so the omega estimates now set to exactly zero did not make the
test more conservative.

## Question

`linked_reps_test()` permutes the replicate labels within each item and
method of a `method_vc(replicates = "linked")` fit and compares the
observed `omega` (SD of the item-by-replicate effects) with the `omega` of
the permuted fits; p = (1 + #{omega_perm >= omega}) / (n_perm + 1).

1. Size: with no occasion effect (omega = 0), does it reject at about the
   nominal rate? Many fits put omega at (or near) zero, which ties with the
   permuted fits and should make the test conservative rather than liberal.
2. Power: how often does it detect omega = 0.2 and 0.4 (with tau = 0.3 and
   sigma = 0.4) for 15 or 30 items and 2 or 3 replicates?
3. Do the permuted limits behave as described in the vignette, i.e. is the
   mean SD of the differences over the permuted fits (`sd_perm`) close to
   that of an exchangeable fit (`sd_exch`) and, with omega = 0, close to
   the linked fit (`sd_linked`)?

Criteria (set before running):

- Size at alpha = 0.05 at most 0.065 (about 2 Monte Carlo SEs above 0.05
  with 1000 data sets); conservative is acceptable, but report it.
- Power: no fixed criterion. Report it, and add a sentence to the
  `linked_reps_test()` docs and the Part 1 vignette on how many items and
  replicates are needed to detect an omega comparable to sigma.
- `sd_perm` within about 2% of `sd_exch` on average in every cell.

If the size is above 0.065, look first at ties near zero (the comparison
uses a tolerance of 1e-12 relative to omega) and at fits that fail.

## Design

`sim_linked_reps.R`. Data from `sim_methods(replicates = "linked")`:
method biases 0, 0.5 (and -0.3), tau = 0.3, sigma = 0.4 for every method,
omega in {0, 0.2, 0.4}.

| cells | n_items | n_reps | methods |
|---|---|---|---|
| 1-12 | 15, 30 | 2, 3 | 2 |
| 13-15 | 30 | 3 | 3 |

1000 data sets per cell with omega = 0 and 300 with omega > 0; 49
permutations per test (p-values in steps of 0.02, so p <= 0.05 means at most
one permuted omega as large as the observed one).

## How to run

On the other machine, from the package root on branch `MethComp` (with
SimplyAgree's dependencies and `pkgload` installed):

```
Rscript references/general/linked_reps_sims/sim_linked_reps.R
```

Environment variables: `LRT_NSIM_NULL`, `LRT_NSIM_ALT`, `LRT_NPERM`,
`LRT_CORES` (default: all cores but 2), `LRT_CELLS` (e.g. `"1,13"` to run
some cells only). Each finished cell is saved to `raw/cell_XX.rds` (not
committed; see `.gitignore`), and finished cells are skipped on a restart.
`summary.csv` is rebuilt from all saved cells at the end. A quick check
before the full run:

```
LRT_NSIM_NULL=4 LRT_NSIM_ALT=4 LRT_NPERM=5 LRT_CELLS=1,15 Rscript references/general/linked_reps_sims/sim_linked_reps.R
```

(then delete `raw/` and `summary.csv`).

Time: about 0.25 s per model fit on the development machine, so about 10 s
per data set (51 fits) and 22 core-hours for the whole study; about 1.5 to 2
hours on 14 cores.

## Results

Run on 2026-11-01 (R 4.4.1, Windows, 14 workers; cells took 2 to 18
minutes, about 1.5 hours in all). Every data set and every permuted fit
was fitted (`n_ok` = `n_sim` in every cell). Full results in
`summary.csv`; means over the data sets below.

| cell | omega | items | reps | methods | reject 0.05 (MC SE) | reject 0.10 | omega_hat | omega_perm | sd_linked | sd_perm | sd_exch | perm / exch - 1 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | 0.0 | 15 | 2 | 2 | 0.039 (0.006) | 0.096 | 0.080 | 0.084 | 0.698 | 0.698 | 0.710 | -1.7% |
| 2 | 0.2 | 15 | 2 | 2 | 0.123 (0.019) | 0.263 | 0.176 | 0.095 | 0.693 | 0.711 | 0.727 | -2.1% |
| 3 | 0.4 | 15 | 2 | 2 | 0.473 (0.029) | 0.690 | 0.371 | 0.140 | 0.702 | 0.784 | 0.816 | -3.9% |
| 4 | 0.0 | 30 | 2 | 2 | 0.039 (0.006) | 0.090 | 0.070 | 0.070 | 0.698 | 0.698 | 0.706 | -1.2% |
| 5 | 0.2 | 30 | 2 | 2 | 0.187 (0.022) | 0.400 | 0.171 | 0.080 | 0.712 | 0.730 | 0.740 | -1.4% |
| 6 | 0.4 | 30 | 2 | 2 | 0.823 (0.022) | 0.930 | 0.390 | 0.121 | 0.701 | 0.791 | 0.814 | -2.8% |
| 7 | 0.0 | 15 | 3 | 2 | 0.034 (0.006) | 0.102 | 0.068 | 0.070 | 0.691 | 0.690 | 0.702 | -1.6% |
| 8 | 0.2 | 15 | 3 | 2 | 0.270 (0.026) | 0.427 | 0.184 | 0.079 | 0.694 | 0.722 | 0.736 | -1.9% |
| 9 | 0.4 | 15 | 3 | 2 | 0.827 (0.022) | 0.957 | 0.397 | 0.109 | 0.706 | 0.828 | 0.852 | -2.8% |
| 10 | 0.0 | 30 | 3 | 2 | 0.043 (0.006) | 0.108 | 0.060 | 0.059 | 0.698 | 0.698 | 0.706 | -1.1% |
| 11 | 0.2 | 30 | 3 | 2 | 0.397 (0.028) | 0.593 | 0.188 | 0.066 | 0.700 | 0.728 | 0.738 | -1.3% |
| 12 | 0.4 | 30 | 3 | 2 | 0.990 (0.006) | 0.997 | 0.397 | 0.090 | 0.703 | 0.829 | 0.844 | -1.8% |
| 13 | 0.0 | 30 | 3 | 3 | 0.043 (0.006) | 0.113 | 0.047 | 0.044 | 0.700 | 0.701 | 0.705 | -0.6% |
| 14 | 0.2 | 30 | 3 | 3 | 0.683 (0.027) | 0.847 | 0.192 | 0.051 | 0.709 | 0.740 | 0.745 | -0.8% |
| 15 | 0.4 | 30 | 3 | 3 | 1.000 (0.000) | 1.000 | 0.397 | 0.067 | 0.706 | 0.835 | 0.844 | -1.0% |

1. **Size: criterion met.** With omega = 0 the rejection rate at 0.05 is
   0.034 to 0.043 (at most 0.065). This is not a sign of a conservative
   test: with 49 permutations the p-values are multiples of 0.02, so
   p <= 0.05 means p <= 0.04, and 0.04 is the attainable level. At 0.10,
   which is attainable exactly, the rates are 0.090 to 0.113. The test
   holds its level; the ties near zero did not make it liberal or
   noticeably conservative.
2. **Power** (reported; no criterion). With omega = sigma (0.4), two
   methods detect it about half the time with 15 items and 2 replicates
   (0.47), and about 80% of the time with 30 items and 2 replicates (0.82)
   or 15 items and 3 replicates (0.83); 30 items and 3 replicates detect it
   almost always (0.99). With omega = sigma / 2 (0.2), power is low with
   two methods (0.12 to 0.40) and moderate with three (0.68 for 30 items
   and 3 replicates). A third method adds a lot of power, since each
   occasion effect is then shared by more measurements. A non-significant
   result in a small study is therefore weak evidence that the replicates
   are exchangeable.
3. **Permuted limits: criterion not met in every cell.** With omega = 0,
   `sd_perm` equals `sd_linked` (within 0.1% on average), as described.
   `sd_perm` is always somewhat below `sd_exch`: by 0.6% to 1.7% with
   omega = 0 and by 0.8% to 3.9% with omega > 0, more than the 2%
   criterion in cells 3, 6 and 9 (omega = 0.4 with 2 methods and 2
   replicates or 15 items) and at the edge in cell 2 (2.1%). The permuted
   fits still put a small `omega` on chance alignments (`omega_perm`
   0.04 to 0.14), which takes a little of the occasion variance out of the
   differences; the gap shrinks with more items, replicates and methods.
   The vignette's "close to those of an analysis with exchangeable
   replicates" holds approximately; the docs now say the permuted limits
   are slightly narrower.

For reference, `sd_exch` behaves as expected: with exchangeable replicates
the residual absorbs omega^2 and the method-by-item variance loses
omega^2 / R, so the SD of the differences is about
sqrt(2 tau^2 + 2 sigma^2 + 2 omega^2 (1 - 1/R)), e.g. 0.812 (R = 2) and
0.845 (R = 3) for omega = 0.4, against 0.814 to 0.816 and 0.844 to 0.852
observed.
