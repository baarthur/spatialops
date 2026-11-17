# spatialops 0.2.0

This release narrows the package's focus to spatial operations.
The `maphub` and `abnq` helpers remain for now and are expected to move to another package in a future release.

## Breaking changes

* Removed OSM wrappers, RAIS functions, and quantile regression helpers, as they fall outside the package's scope.
* Removed `shp_extract_read()` and `get_h3_grid()`, which have been superseded.

## Internal changes

* Replaced magrittr pipes with the native R pipe (`|>`).