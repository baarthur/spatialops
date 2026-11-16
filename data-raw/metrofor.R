# Rebuild the metrofor package dataset from OpenStreetMap.
# Requires geobr, osmdata, sf, dplyr, and usethis.
# OSM is live data: rerunning this script may not reproduce the committed .rda.

fortaleza <- geobr::read_municipality(code_muni = 2304400)

railway_query <- osmdata::opq(sf::st_bbox(fortaleza), timeout = 55) |>
  osmdata::add_osm_feature(
    key = "railway",
    value = c(
      "rail", "light_rail", "subway", "tram", "service",
      "construction", "usage", "station", "tram_stop"
    )
  ) |>
  osmdata::osmdata_sf()

stations <- railway_query$osm_points |>
  sf::st_transform(crs = sf::st_crs(fortaleza)) |>
  dplyr::filter(public_transport == "station") |>
  dplyr::select(osm_id, name, railway, start_date, station)

lines <- railway_query$osm_lines |>
  sf::st_transform(crs = sf::st_crs(fortaleza)) |>
  dplyr::filter(
    (is.na(operator) | operator != "FTL") &
      !(service %in% c("yard", "spur")) &
      railway != "construction"
  ) |>
  dplyr::select(osm_id, name, railway, start_date = opening_date, station)

metrofor <- dplyr::bind_rows(lines, stations) |>
  dplyr::arrange(railway)

usethis::use_data(metrofor, overwrite = TRUE)