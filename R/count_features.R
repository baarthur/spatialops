#' Count features in a given space
#' @param x `{sf}` Counting reference.
#' @param y Feature to be counted.
#' @param radius Radius to count features within, when `x` is a POINT.
#' @param predicate Spatial operation to be performed before counting. Default: `st_intersects`
#' @param name Optional. Argument to set the new column's nazme, otherwise it is set to "n_features".
#' @returns Original `x` with an additional column containing the number of `y` features that
#' intersect with observation in `x`.
#' @import sf
#' @importFrom tibble tibble
#' @importFrom dplyr rename bind_cols
#' @export
#' @examples
#' \dontrun{
#' df <- osmdata::getbb(place_name = "times square, new york") %>%
#' osmdata::opq() %>%
#' osmdata::add_osm_feature(key = "amenity", value = "restaurant") %>%
#' osmdata::osmdata_sf()
#'
#' df <- df$osm_points %>%
#' dplyr::select(name, amenity, cuisine)
#'
#' count_features(df[1,], df[2:17,], 200, sf::st_intersects)
#' }

count_features <- function(x, y, radius, predicate = st_intersects, name) {
  if(missing(radius)){
    df <- predicate(x, y)
  } else {
    df <- st_buffer(x, radius) %>%
      predicate(y)
  }

  df <- df %>%
    lengths() %>%
    tibble()

  if(missing(name)) {
    df <- df %>%
      rename("n_features" := .)
  } else {
    df <- df %>%
      rename({{name}} := .)
  }

  df <- df %>%
    bind_cols(x) %>%
    st_as_sf()

  return(df)
}
