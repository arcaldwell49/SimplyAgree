# SimplyAgree - GitHub Copilot Instructions

## Package Context
R package for agreement and reliability analyses (Bland-Altman, ICC, CCC). Uses roxygen2 with markdown enabled.

## Code Style Requirements
- **Naming**: snake_case functions (`agree_test`, `reli_stats`), parameters with dots (`conf.level`, `agree.level`)
- **Style**: Tidyverse conventions (pipes, dplyr verbs)
- **Validation**: Use `match.arg()` for choice parameters, early `stop()` for invalid inputs
- **Returns**: Named lists via `structure(..., class = "classname")`
- **Match surrounding code** - consistency over personal preference

## For Issue Fixes
- Make **minimal, targeted changes** addressing only the specific problem
- **Do not refactor** unrelated code
- **Preserve function signatures** unless the issue explicitly requires API changes
- If fix requires a **new dependency**, flag for human review instead of adding it
- Check if similar patterns exist elsewhere in codebase and follow them

## For PR Reviews
Check for:
- [ ] Consistency with existing code style
- [ ] Functions returning test results use `htest` or `power.htest` class where appropriate
- [ ] Complete roxygen2 docs: `@param`, `@return`, `@examples`, `@export`
- [ ] Test coverage for new code paths
- [ ] Hardcoded values that should be parameters
- [ ] Edge cases: NA handling, zero-length inputs, type validation
- [ ] `NAMESPACE` changes match `@export`/`@importFrom` tags

## Limitations - Do Not Attempt
**Recommend human review instead for:**
- C++ code in `src/`
- GitHub Actions workflows (`.github/workflows/`)
- Adding/removing dependencies in `DESCRIPTION`
- Refactoring S3 method dispatch
- Changing exported function APIs (breaking changes)
- Changes spanning more than 2-3 files

## Testing Expectations
- Every fix needs a corresponding test case
- Framework: `testthat`
- Numerical tolerance: `expect_equal(..., tolerance = 1e-5)`
- Validate against published results when available
- Test file pattern: `tests/testthat/test-{function_name}.R`

## When Uncertain
- **Multiple approaches?** Describe options, don't pick arbitrarily
- **Backward compatibility risk?** Flag it explicitly
- **Complex fix?** Recommend human review
- **Unsure about statistical correctness?** Always flag for expert review
