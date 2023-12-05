#' Make a geom_sf layer with a fixed scale
#'
#' @description
#' A wrapper around `ggplot2::geom_sf`, this function generates fixed scale maps, which enhances
#'  comparison between different plots.
#'
#' @param data An object with classes `sf` and `data.frame`. Won't inherit data from previous layer
#'  since to make the scale, it is necessary to calculate a buffer around the centroid.
#' @param dist Buffer distance, see `sf::st_buffer`
#' @param col_ref `<data-masking>` Column in `data` with the filtering reference.
#'  Optional, provide only if desired output is a subset of original data.
#' @param ref Filtering reference to be passed on `col_ref`. Either a single value or a vector.
#' @param ... Other arguments to pass to \code{ggplot2::geom_sf()}.
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom rlang quo_is_null enquo
#' @importFrom sf st_union st_centroid st_buffer st_bbox
#'
#' @export
#'
#' @returns A `ggplot` object
#'
#' @example inst/examples/geom_sf_toscale.R

geom_sf_toscale <- function(data, dist, col_ref = NULL, ref = NULL, ...) {

  data <- if(!quo_is_null(enquo(col_ref)) & !is.null(ref)) {
    data %>% filter({{col_ref}} %in% ref)
  } else {data}

  bbox <- data %>% st_union() %>% st_centroid() %>% st_buffer(dist) %>% st_bbox()

  list(
    geom_sf(data = data, ...),
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))
  )
}

# tester <- function(data, col_ref = NULL, ref = NULL) {
#
#   if(!rlang::quo_is_null(enquo(col_ref)) & !is.null(ref)) {
#     data %>% filter({{col_ref}} %in% ref)
#   } else print("null")
#
# }


