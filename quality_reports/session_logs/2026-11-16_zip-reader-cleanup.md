# ZIP reader cleanup

- Removed `shp_extract_read()` from R code, exports, generated help, and README. Removed `utils::unzip` import.
- Vignette now reads Fortaleza neighborhood and park ZIPs using `sf::st_read()` and GDAL `/vsizip/`, expecting them under `inst/extdata/` as `Bairros_de_Fortaleza.zip` and `Pracas_de_Fortaleza.zip`.
- Historical source control contained `Bairros_de_Fortaleza.zip` and `Praças_de_Fortaleza.zip`; the vignette uses an ASCII filename for the latter. Obtain and review current files before including them in the package.
- Verification: 15 R files parsed and `git diff --check` passed. `sf` is not installed here and the ZIPs are absent, so actual ZIP reading and vignette rendering remain unverified.
- User supplied both ZIPs. `Praças_de_Fortaleza.zip` was renamed to `Pracas_de_Fortaleza.zip` to match the vignette. Each ZIP contains one SHP and its DBF/SHX/PRJ sidecars. DBF headers include the attributes used by the vignette (`id`, `Nome`, area, `nome`). No R installation here has `sf` or `knitr`; live read and rendering remain untested. Check data redistribution terms before release.
