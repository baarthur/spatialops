#' Metrofor - Fortaleza Rapid Transit
#'
#' Shapefile with Metrofor's lines and stations. Data obtained via package `{osmdata}`.
#'
#' @format ## `fortaleza`
#' A simple feature collection with 189 features and 5 fields
#' \describe{
#'   \item{Geometry type}{GEOMETRY}
#'   \item{Dimension}{XY}
#'   \item{Bounding box}{xmin: -38.64377 ymin: -3.889588 xmax: -38.47097 ymax: -3.709044}
#'   \item{Geodetic CRS}{SIRGAS 2000}
#'   \item{osm_id}{Unique OSM identifier (character)}
#'   \item{name}{Feature name in OSM (character)}
#'   \item{railway}{Either "station" or, for lines, "light_rail", "rail", or "subway" (character)}
#'   \item{start_date}{Station opening date (character)}
#'   \item{station}{Identifies if the feature is a station (character)}
#'   \item{geometry}{Feature geometry (sfc_GEOMETRY)}
#' }
#'
#' @source <https://mapas.fortaleza.ce.gov.br/>

"metrofor"
