#' Distance to the nearest feature
#' @param x A `sf` object to calculate distances from.
#' @param y A `sf` object to retrieve the closest one to each observation in `x`
#' @param name description An optional argument to set the new column's name, otherwise it is set to "distance".
#' @param id_feature Specifies if an additional column with `y`'s row id should be inserted. Defaults to FALSE.
#' @param geometry Specifies the name of the geometry column, if different from the default, to reorder variables.
#' @importFrom sf st_nearest_feature st_as_sf st_transform st_crs
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr left_join mutate bind_cols
#' @importFrom units drop_units
#' @export
#' @returns Original `x` with an additional column containing the distance to the closest  `y` feature.
#' @details
#' For each spatial object in x, calculate the distance to the nearest feature in y.
#' @example inst/examples/dist_nearest.R

dist_nearest <- function(x, y, name, id_feature = F, geometry = geometry) {
  closest <- st_nearest_feature(x, y) %>%
    tibble(id_closest = `.`)

  distance <- closest %>%
    left_join(rowid_to_column(y, "id_closest")) %>%
    st_as_sf() %>%
    st_transform(crs = st_crs(y))

  if(missing(name)) {
    x <- x %>%
      mutate(distance = st_distance(distance, ., by_element = TRUE), .before = geometry) %>%
      drop_units()
  } else {
    x <- x %>%
      mutate({{name}} := st_distance(distance, ., by_element = TRUE), .before = geometry) %>%
      drop_units()
  }

  if(id_feature == F) {
    return(x)
  } else {
    x <- x %>%
      bind_cols(closest)

    return(x)
  }
}
