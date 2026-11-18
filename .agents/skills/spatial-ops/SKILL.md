---
name: spatial-ops
description: Design, implement, or review spatial operations in sf and DuckDB Spatial, especially CRS, distance, geometry conversion, and large-data execution.
---

# Spatial operations with sf and DuckDB Spatial

Choose the engine according to data size, available functions, and the required output. Keep small in-memory operations in `sf` when clear; consider DuckDB Spatial for filtering, joins, or distance computation on data too large to collect comfortably. Do not add a DuckDB dependency to `spatialops` merely because this skill exists.

Before computing, specify input CRS, axis order, geometry type and validity, distance model, units, and expected output class. Check how `sf` uses s2 for geographic coordinates and planar GEOS for projected coordinates. In DuckDB Spatial, `ST_Distance` is planar; use a suitable spheroid function or transform to an appropriate projected CRS for metric distances. Check `ST_Transform` axis-order behavior for the installed DuckDB version, including `always_xy` or `geometry_always_xy`.

When crossing R and SQL, make geometry serialization and CRS assignment explicit. Do not assume a WKB geometry carries the intended CRS through every conversion. Push selective work into SQL, avoid collecting a full cross join, and inspect the query plan when scale matters. Verify a representative small subset against `sf`, including a boundary case, and state the tolerance and units. Test empty and missing geometries when the function supports them.

Primary references: [sf measurement semantics](https://r-spatial.github.io/sf/reference/geos_measures.html), [sf spherical geometry](https://r-spatial.github.io/sf/articles/sf7.html), and [DuckDB Spatial functions](https://duckdb.org/docs/stable/core_extensions/spatial/functions). Check installed-version documentation before relying on version-specific behavior.