#' Download osm roads
#' @description
#' A convenient wrapper for osm features to get roads easily convertible to `sf` format.
#' @param bbox A bounding box for the search area.
#' @import osmdata
#' @export
#' @returns A `list` for each kind of geometry retrieved by osmdata.
#' @details
#' Returns values that meet `key` = "highway" and `value` = "motorway", "primary", "secondary",
#' "tertiary", "residential", "living_street" and "pedestrian". For more info, please check
#' ["Key:highway"](https://wiki.openstreetmap.org/wiki/Key:highway) page at OSM Wiki.

get_osm_roads <- function(bbox) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "highway",
      value = c("motorway", "primary", "secondary", "tertiary", "residential", "living_street", "pedestrian")
    ) %>%
    osmdata_sf()
  return(query)
}



#' Download osm postcodes
#' @description
#' A convenient wrapper for osm features to get postcodes easily convertible to `sf` format
#' @param bbox A bounding box for the search area.
#' @import osmdata
#' @export
#' @returns A `list` for each kind of geometry retrieved by osmdata.
#' @details
#' Returns values that meet `key` = "addr:postcode". For more info, please check
#' ["Key:railway"](https://wiki.openstreetmap.org/wiki/Key:railway) page at OSM Wiki.

get_osm_zips <- function(bbox) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "addr:postcode"
    ) %>%
    osmdata_sf()

  return(query)
}



#' Download osm rail transit
#' @description
#' A convenient wrapper for osm features to get (rail) transit lines and stations
#' easily convertible to `sf` format
#' @param bbox A bounding box for the search area.
#' @import osmdata
#' @export
#' @returns Object with classes `list`, `osmdata`, and `osmdata_sf`
#' for each kind of geometry retrieved by osmdata.
#' @details
#' Returns values that meet `key` = "railway" and `value` = "rail", "light_rail", "subway", "tram",
#' "service", "construction", "usage", "station", and "tram_stop". For more info, please check
#' ["Key:addr:*"](https://wiki.openstreetmap.org/wiki/Key:addr:*) page at OSM Wiki

get_osm_rail <- function(bbox) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "railway",
      value = c("rail", "light_rail", "subway", "tram", "service", "construction", "usage",
                "station", "tram_stop")
    ) %>%
    osmdata_sf()

  return(query)
}
