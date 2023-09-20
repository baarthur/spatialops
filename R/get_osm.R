#' Download osm roads
#' @description
#' A convenient wrapper for osm features to get roads in `sf` format.
#' @param bbox A bounding box for the search area.
#' @import osmdata
#' @export
#' @returns A `list` for each kind of geometry retrieved by osmdata.
#' @details
#' Returns values that meet `key` = "highway" and `value` = "motorway", "primary", "secondary",
#' "tertiary", "residential", "living_street" and "pedestrian". For more info, please check
#' https://wiki.openstreetmap.org/wiki/Key:highway.

get_osm_roads <- function(bbox) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "highway",
      value = c("motorway", "primary", "secondary", "tertiary", "residential", "living_street", "pedestrian")
    ) %>%
    osmdata_sf()
  return(query)
}



#' A convenient wrapper for osm features to get postcodes in `sf` format
#' @param bbox A bounding box for the search area.
#' @import osmdata
#' @export
#' @returns A `list` for each kind of geometry retrieved by osmdata.
#' @details
#' Returns values that meet `key` = "addr:postcode". For more info, please check
#' https://wiki.openstreetmap.org/wiki/Key:highway.

get_osm_zips <- function(bbox) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "addr:postcode"
    ) %>%
    osmdata_sf()

  return(query)
}
