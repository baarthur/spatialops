#' Download osm data
#'
#' @description
#' A convenient wrapper for osm features to get some features I need easily convertible to `sf` format.
#'
#' @param bbox A bounding box for the search area.
#' @param use Should all geometries be kept? Defaults to all, but can also be one of
#'  "points", "lines", "multilines", "multipolygons". Use this when sure about what geometry you
#'  want to retrieve (e.g. when getting postcodes, you most surely want `use = "points"`).
#'
#' @import osmdata
#'
#' @returns If `use = ("all")`, a `list` for each kind of geometry retrieved by `{osmdata}`. Else, a
#'  `simple feature` dataframe for the selected geometry type instead of a list with all geometries.
#'
#' @seealso [osmdata::add_osm_feature()]



#' @rdname get_osm_
#' @name get_osm_roads
#' @export
#'
#' @details
#' Returns values that meet `key` = "highway" and `value` = "motorway", "primary", "secondary",
#' "tertiary", "residential", "living_street" and "pedestrian". For more info, please check
#' ["Key:highway"](https://wiki.openstreetmap.org/wiki/Key:highway) page at OSM Wiki.
#'
#' @examples
#' \dontrun{
#' # Get roads in Brazil's smallest city
#' geobr::read_municipality(code_muni = 3157336) %>%
#' sf::st_bbox() %>%
#' get_osm_roads()
#' }

get_osm_roads <- function(bbox, use = c("all", "points", "lines", "multilines", "multipolygons")) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "highway",
      value = c("motorway", "primary", "secondary", "tertiary", "residential", "living_street", "pedestrian")
    ) %>%
    osmdata_sf()

  if(use != "all") {
    query <- query[[paste0("osm_",use)]]
  }

  return(query)
}


#' @rdname get_osm_
#' @name get_osm_postcodes
#' @export
#'
#' @details
#' Returns values that meet `key` = "addr:postcode". For more info, please check
#' ["Key:railway"](https://wiki.openstreetmap.org/wiki/Key:railway) page at OSM Wiki.
#'
#' @examples
#' \dontrun{
#' # Get postcodes in Brazil's smallest city
#' geobr::read_municipality(code_muni = 3157336) %>%
#' sf::st_bbox() %>%
#' get_osm_postcodes()
#' }


get_osm_postcodes <- function(bbox, use = c("all", "points", "lines", "multilines", "multipolygons")) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "addr:postcode"
    ) %>%
    osmdata_sf()

  if(use != "all") {
    query <- query[[paste0("osm_",use)]]
  }

  return(query)
}



#' @rdname get_osm_
#' @name get_osm_rail
#' @export
#'
#' @details
#' Returns values that meet `key` = "railway" and `value` = "rail", "light_rail", "subway", "tram",
#' "service", "construction", "usage", "station", and "tram_stop". For more info, please check
#' ["Key:addr:*"](https://wiki.openstreetmap.org/wiki/Key:addr:*) page at OSM Wiki
#'
#' @examples
#' \dontrun{
#' # Get railways in Brazil's smallest city
#' geobr::read_municipality(code_muni = 3157336) %>%
#' sf::st_bbox() %>%
#' get_osm_rail()
#' }


get_osm_rail <- function(bbox, use = c("all", "points", "lines", "multilines", "multipolygons")) {
  query <- opq(bbox, timeout = 50) %>%
    add_osm_feature(
      key = "railway",
      value = c("rail", "light_rail", "subway", "tram", "service", "construction", "usage",
                "station", "tram_stop")
    ) %>%
    osmdata_sf()

  if(use != "all") {
    query <- query[[paste0("osm_",use)]]
  }

  return(query)
}
