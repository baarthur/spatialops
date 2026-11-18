# Remove redundant ZIP reader

- Remove `shp_extract_read()` and its generated help/export/import.
- Use `sf::st_read()` with GDAL `/vsizip/` in the vignette and document the two required sample ZIPs.
- Put sample ZIPs under `inst/extdata/` when provided so the vignette can find them in an installed package.
- Verify parse and references now; test actual ZIP reading and vignette after the ZIPs and sf are available.