# MapHub API cleanup

- Inspected the downloaded Mendeley Data v1 ZIP. Active uses: `maphub_to_sf`, `geom_bbox`, `theme_abnq`. `maphub_tidy_lines` and `maphub_tidy_stations` are commented out.
- Removed the three MapHub helpers from `R/`, `NAMESPACE`, generated `man/`, and current README examples. `tidyr` remains in `Imports` because `get_h3_grid` uses `crossing()`.
- Pre-removal commit `5d025ddd5255606eeb7d2f25a0fc7af4e8c01ce0` is present on `origin/dev`. This only addresses the original MapHub API call; full reproduction of the deposit has not been checked.
- Future work: migrate helpers to a personal package, verify the Mendeley workflow, and decide whether to publish a corrected dataset v2.
- Verification: 17 R files parsed. `R CMD check --no-manual .` stopped at DESCRIPTION with missing Author/Maintainer in this environment. `roxygen2`, `devtools`, and `testthat` are not installed, so generated files were updated manually.