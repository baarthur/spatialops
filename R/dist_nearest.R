#' Distance to the nearest feature
#'
#' @description
#' For each spatial object in x, calculate the distance to the nearest feature in y.
#'
#' @param x A `sf` object to calculate distances from.
#' @param y A `sf` object to retrieve the closest one to each observation in `x`
#' @param name An optional argument to set the new column's name, otherwise it is set to "distance".
#' @param id_feature Specifies if an additional column with `y`'s row id should be inserted. Defaults to `FALSE.`
#' @param geometry Specifies the name of the geometry column, if different from the default, to reorder variables.
#' @param drop_units Should units be included? Default: `FALSE`
#'
#' @importFrom sf st_nearest_feature st_as_sf st_transform st_crs
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr left_join mutate bind_cols
#' @importFrom units drop_units
#'
#' @export
#'
#' @details
#' `dist_nearest` First finds the closest feature in `y` to each feature in `x`, calculating then
#'    pairwise distances faster than e.g. using a for loop and then minimizing distances or
#'    calculating distance matrices.
#'
#' @returns Original `x` with an additional column containing the distance to the closest  `y` feature.
#'
#' @example inst/examples/dist_nearest.R

dist_nearest <- function(x, y, name, id_feature = F, geometry = geometry, drop_units = T) {

  closest <- st_nearest_feature(x, y) %>%
    tibble(id_closest = `.`)

  distance <- closest %>%
    left_join(rowid_to_column(y, "id_closest")) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(y))

  if(missing(name)) {
    x <- x %>%
      mutate(distance = st_distance(distance, ., by_element = TRUE), .before = geometry)
  } else {
    x <- x %>%
      mutate({{name}} := st_distance(distance, ., by_element = TRUE), .before = geometry)
  }

  if(drop_units) {
    x <- x %>% drop_units()
  }

  if(id_feature) {
    x <- x %>% bind_cols(closest)
  }

  return(x)
}
