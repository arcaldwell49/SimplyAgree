# linked_reps_test(): size and power

Status (2026-11-01): script written and smoke-tested (4 data sets per cell,
5 permutations); **not yet run**. To be run on a separate machine.

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

Not yet run. Commit `summary.csv`, fill in this section (table of
`reject_05` by cell, with the omega and sd columns), and update the status
line, `NEWS.md`, the `linked_reps_test()` docs and the Part 1 vignette with
the size and power.
