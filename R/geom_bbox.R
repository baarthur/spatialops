#' Limit a ggplot map
#'
#' @description
#' This function creates a bounding box from a `sf` object and apply its limits to `coord_sf`.
#'
#' @param data An object with classes `sf` and `data.frame`.
#'
#' @importFrom sf st_bbox
#' @importFrom ggplot2 coord_sf
#'
#' @export
#'
#' @returns A `ggplot` object
#'
#' @example inst/examples/geom_bbox.R


geom_bbox <- function(data) {
  bbox <- data %>% st_bbox
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))
}

