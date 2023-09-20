#' Extract and read compacted shapefiles
#' @description
#' Extract shapefiles from a compacted folder and read them using `sf::st_read`.
#' @param path Path of the compacted folder, including file name and extension.
#' @param opt.st WIP!!!
#' @export
#' @returns A `sf` object

shp_extract_read <- function(path, dsn, crs, opt.st) {
  temp <- tempfile()
  unzip(path, exdir = temp, junkpaths = TRUE)
  listfiles <- list.files(temp, pattern = shape, full.names = TRUE)
  if(missing(opt.st)){
    shp <- sf::st_read(dsn = listfiles, crs = crs)
  } else{
    shp <- sf::st_read(dsn = listfiles, crs = crs, options = opt.st)
  }
  unlink(temp, recursive = TRUE)
  return(shp)
}
