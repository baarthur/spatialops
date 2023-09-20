## code to prepare `metrofor` dataset goes here

usethis::use_data(metrofor, overwrite = TRUE)

# tidyverse
library(dplyr)
library(ggplot2)

# geocomputation
library(sf)
library(osmdata)


fortaleza <- geobr::read_municipality(code_muni = 2304400)

#rmfor <- geobr::read_metro_area() %>% filter(name_metro == "RM Fortaleza")

query_for <- fortaleza %>%
  st_bbox() %>%
  opq(timeout = 55) %>%
  add_osm_feature(
    key = "railway",
    value = c("rail", "light_rail", "subway", "tram", "service", "construction", "usage",
              "station", "tram_stop")
  ) %>%
  osmdata_sf()

metrofor <- query_for$osm_points %>%
  st_transform(crs = st_crs(fortaleza)) %>%
  filter(public_transport == "station") %>%
  select(osm_id, name, railway, start_date, station)

metrofor <- query_for$osm_lines %>%
  st_transform(crs = st_crs(fortaleza)) %>%
  filter((is.na(operator)| operator != "FTL") & !(service %in% c("yard", "spur")) & railway != "construction") %>%
  select(osm_id, name, railway, start_date = opening_date, station) %>%
  bind_rows(metrofor) %>%
  arrange(railway)

remove(fortaleza, metrofor)

#bbox <- st_union(metrofor, fortaleza) %>% st_bbox()

# ggplot() +
#   geom_sf(
#     data = rmfor,
#     fill = "grey95"
#   ) +
#   geom_sf(
#     data = metrofor %>% filter(railway != "station")
#   ) +
#   geom_sf(
#     data = metrofor %>% filter(railway == "station"),
#     aes(color = factor(station))
#   ) +
#   coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)]) +
#   theme_void()
