#' Get a tidy H3 grid in a sf dataframe
#' @description
#' More than a wrapper around `h3jsr::polygon_to_cells()`, this function automates the process
#'  of getting an hexagonal grid for polygons, e.g. cities. It is particularly useful when retrieving
#'  a grid for intersecting polygons, as it automates the cropping process (optional) to avoid
#'  duplicates. Based on {aopdata}[https://github.com/ipeaGIT/acesso_oport/].
#'
#' @param shp A `sf` object of type `POLYGON` or `MULTIPOLYGON`
#' @param res Desired H3 resolution, defaults to 9.
#' @param crop Should the polygons be cropped to the original polygon? If yes, `sf::st_intersection`
#'  will be used. Defaults to `TRUE`
#' @param buffer In some polygons, border areas may not be included if they do not cover an hexagon's
#'  centroid. Creating a border increases the probability of all borders being selected; 
#'  defaults to `TRUE`
#' @param buffer_size Allows selecting a custom buffer distance (in meters), when `buffer = TRUE`.
#'  If left empty, defaults to 300 when `crop = TRUE` and 15 when `crop = FALSE`. To use a unit other
#'  than metrics, pass as `buffer_size = units::as_units(x, "unit)`.
#' @param keep_crs Should the original coordinate reference system (`crs`) be preserved? Defaults to
#'  `TRUE`; otherwise, will return an object with `crs = 4326` (WGS84).
#'
#' @import sf
#' @importFrom dplyr pull relocate
#' @importFrom h3jsr polygon_to_cells cell_to_polygon
#' @importFrom magrittr %>%
#' @importFrom tidyr crossing
#' @importFrom units as_units
#'
#' @export
#'
#' @returns An object with classes `sf`, `tbl_df`, `tbl`, and `data.frame`
#' 
#' @details
#' `h3jsr` recommends passing polygons in WGS84 coordinates, `shp` is automatically converted
#'  to that format if not already in WGS84. Since WGS84's Buffer size is passed in meters since 
#'  it is the default unit fot WGS84.
#' 
#' @example inst/examples/get_h3_grid.R



# function ------------------------------------------------------------------------------------

get_h3_grid <- function(shp, res = 9, crop = TRUE, buffer = TRUE, buffer_size = NULL, keep_crs = TRUE) {
  
  buffer_size <- if(is.null(buffer_size)) {
    if(crop) units::as_units(300,"m") else units::as_units(15,"m")
  } else buffer_size
  
  crs_old <- if(keep_crs) st_crs(shp)
  
  shp <- shp %>% 
    {if(st_crs(.) != 4326) st_transform(., crs = 4326)} 
  
  shp %>% 
    {if(buffer) st_buffer(., buffer_size) else .} %>% 
    polygon_to_cells(res = res, simple = FALSE) %>% 
    pull(h3_addresses) %>% 
    unlist() %>%
    cell_to_polygon(simple = FALSE) %>% 
    crossing(st_drop_geometry(shp)) %>% 
    st_as_sf() %>% 
    relocate(geometry, .after = everything()) %>% 
    {if(crop) st_intersection(., shp) else .} %>% 
    {if(keep_crs) st_transform(., crs = crs_old) else .}
}

