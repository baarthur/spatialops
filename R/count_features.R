#' Count features in a given space
#' @param x Counting reference.
#' @param y Feature to be counted.
#' @param radius Radius to count features within, when `x` is a `POINT`.
#' @param predicate Spatial operation to be performed before counting. Default: `st_intersects`
#' @param name Optional. Argument to set the new column's name, otherwise it is set to "n_features".
#' @returns Original `x` with an additional column containing the number of `y` features that
#' intersect with observation in `x`.
#' @importFrom sf st_buffer st_as_sf
#' @importFrom tibble tibble
#' @importFrom dplyr rename bind_cols
#' @export
#' @example inst/examples/count_features.R

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
      rename("n_features" := `.`)
  } else {
    df <- df %>%
      rename({{name}} := `.`)
  }

  df <- df %>%
    bind_cols(x) %>%
    st_as_sf()

  return(df)
}
