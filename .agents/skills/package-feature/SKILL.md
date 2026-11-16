---
name: package-feature
description: Build or change a spatialops feature through API design, implementation, documentation, tests, and package checks.
---

# Build a package feature

1. Inspect adjacent functions, callers, public examples, and `DESCRIPTION`. Define input and output classes, missing and empty behavior, units, CRS, and error behavior.
2. For a non-trivial change, write a short plan. Implement in `R/`, document with roxygen, and add focused tests. Update README or vignette when usage changes.
3. Regenerate documentation, run focused tests, and broaden to `R CMD check` for public API or dependency changes when feasible. Report what ran and what remains unverified.
4. Record durable decisions in `MEMORY.md` and pending work in `BACKLOG.md`.

Use [R Packages (2e)](https://r-pkgs.org/) for mechanics, the [rOpenSci Dev Guide](https://devguide.ropensci.org/) for quality criteria, and [Tidy design principles](https://design.tidyverse.org/) for API choices. Apply the spatial-ops skill when spatial semantics or DuckDB Spatial are central.