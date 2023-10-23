#' Extract and read compacted shapefiles
#' @description
#' Extract shapefiles from a compacted folder and read them using `sf::st_read`.
#'
#' @param path Path of the compacted folder, including file name and extension.
#' @inheritParams sf::st_read
#' @inheritDotParams sf::st_read
#'
#' @importFrom utils unzip
#'
#' @export
#'
#' @returns A `sf` object

shp_extract_read <- function(path, dsn = "shp", ...) {

  temp <- tempfile()
  unzip(path, exdir = temp, junkpaths = TRUE)
  listfiles <- list.files(temp, pattern = paste0("\\.", dsn, "$"), full.names = TRUE)

  shp <- sf::st_read(dsn = listfiles, ...)

  unlink(temp, recursive = TRUE)

  return(shp)
}
