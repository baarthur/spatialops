# Restore MapHub API

- Restored `R/maphub_to_sf.R` from commit `5d025ddd5255606eeb7d2f25a0fc7af4e8c01ce0` at the author's request; destination remains undecided.
- Regenerated `NAMESPACE` and the three MapHub help pages with `devtools::document()`. Updated README, MEMORY, and BACKLOG to reflect the temporary retention.
- Focused smoke test: `maphub_to_sf()`, `maphub_tidy_lines()`, and `maphub_tidy_stations()` passed on small `sf` objects. No network was used.
- `devtools::test()` found no testing infrastructure. `devtools::check(args = "--no-manual")` with R 4.6.1, a valid Windows UTF-8 locale, and Positron Pandoc finished with 0 errors, 0 warnings, 0 notes; vignette rebuilt successfully.