# OSM API cleanup

- Author confirmed only `spatialops` and `mapchallenge-2023` appeared in their GitHub code search for the OSM wrappers; one local `shenanigans` folder also uses `get_osm_roads`.
- Removed `get_osm_roads`, `get_osm_rail`, `get_osm_postcodes` from code, exports, generated help, and removed `osmdata` from Imports.
- Updated vignette to use the included `metrofor` dataset instead of calling the removed OSM wrapper. The remaining neighborhood and park ZIP inputs are absent from the package and need a separate vignette review.
- `R/metrofor.R` and its help page still mention `osmdata` as historical data provenance; this is not a runtime dependency.
- Verification: 16 R files parsed; `git diff --check` passed. `R CMD check --no-manual .` stopped at DESCRIPTION because this environment reports missing Author and Maintainer. `roxygen2`, `devtools`, and `testthat` were unavailable, so generated files were updated manually.
- Follow-up: recovered `data-raw/metrofor.R` from history. It already used `osmdata::opq()`, `add_osm_feature()`, and `osmdata_sf()` directly, never `get_osm_rail()`. The restored version moves `usethis::use_data()` after object creation, keeps the historical query and filters, and is excepted from the `data-raw/*` ignore rule. Parsed successfully; live OSM query and `.rda` regeneration were not run.
