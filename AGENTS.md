# spatialops agent workflow

## Project
`spatialops` is an experimental R package. Source is in `R/`; roxygen generates `man/` and `NAMESPACE`. `DESCRIPTION` defines metadata and dependencies. Examples, vignette, and CI live in `inst/examples/`, `vignettes/`, and `.github/workflows/`.

## Session workflow
1. Inspect `git status --short` and preserve existing work.
2. Read relevant `MEMORY.md` and `BACKLOG.md` entries.
3. For substantial or multi-file work, write a brief plan in `quality_reports/plans/`.
4. Implement, verify, update durable decisions in memory and pending tasks in backlog. Add a session log for a meaningful handoff.

See `.agents/WORKFLOW_QUICK_REF.md`. Use `spatial-ops` for work where sf or DuckDB Spatial semantics matter. `CLAUDE.md` is a thin adapter; this file is canonical.

## Package rules
- Treat exported signatures, return types, classes, CRS, units, and geometry behavior as API contracts. Inspect callers and examples before changing them.
- Keep reusable code in `R/`. Document exported functions with roxygen, including arguments, return value, edge cases, and runnable examples. Regenerate `man/` and `NAMESPACE` after documentation changes.
- Declare dependencies in `DESCRIPTION`; prefer explicit `pkg::fun()` calls. Avoid attaching packages inside package code.
- Add focused `testthat` tests for behavior changes, especially empty input, missing geometry, CRS mismatch, units, and class retention. Avoid live network in tests.
- Keep package data small and redistributable. Do not commit secrets, private data, or machine-specific paths.
- State planar versus geodesic distance and CRS transformations explicitly.
- Align README and vignette with the current API; record user-visible changes in NEWS when release notes exist.

## Verification
Run the narrowest meaningful test first. For public API or dependency changes, use `devtools::document()`, `devtools::test()`, and `devtools::check()` when available. Alternatives are `roxygen2::roxygenise()`, `testthat::test_local()`, and `R CMD check`. Report commands actually run and results. Do not claim checks passed from inspection.

Do not commit, push, or open a PR unless requested. Keep paths relative to this repository. On the mapped Windows drive, shell calls may need sandbox escalation; do not persist backing UNC paths.