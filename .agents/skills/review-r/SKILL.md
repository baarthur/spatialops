---
name: review-r
description: Review spatialops package code, API, documentation, tests, and checks without editing.
---

# Review R package code

Inspect the requested diff, callers, tests, documentation, and package metadata. Report concrete findings by severity with file and line, failure mode, and smallest credible fix. State checks actually run. This skill is read-only unless fixes are requested.

- Use [R Packages (2e)](https://r-pkgs.org/) for package structure, dependencies, documentation, testing, and `R CMD check`. Check that roxygen, `man/`, `NAMESPACE`, examples, `DESCRIPTION`, and tests agree.
- Use the [rOpenSci Dev Guide](https://devguide.ropensci.org/) as a quality review lens: clear scope, user documentation, reproducible tests, CI, and maintainability. Its submission policies apply only if the project seeks rOpenSci review.
- Use [Tidy design principles](https://design.tidyverse.org/) to assess API consistency: predictable names and outputs, useful defaults, and clear errors. Treat them as design guidance, not mandatory style.
- For spatial behavior, invoke `spatial-ops` when CRS, units, geometry engine, or DuckDB Spatial semantics materially affect correctness.