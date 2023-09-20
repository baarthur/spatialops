# load data
data("fortaleza")
data("metrofor")

# example 1: count features inside polygon

## select only stations
metrofor <- metrofor %>%
  dplyr::filter(station %in% c("light_rail", "subway"))

## 1.1 count stations per neighborhood
count_features(fortaleza, metrofor) %>%
  dplyr::arrange(dplyr::desc(n_features))

## 1.2 give a name to the new column
count_features(fortaleza, metrofor, name = "n_stations") %>%
  dplyr::arrange(dplyr::desc(n_stations))


# example 2: count features inside a buffer

## get neighborhood centroids
fortaleza <- sf::st_centroid(fortaleza)

## 2.1 count stations in a 500m buffer from centroid
count_features(fortaleza, metrofor, radius = 500) %>%
  dplyr::arrange(dplyr::desc(n_features))

## 2.2 using another predicate than st_intersects
count_features(fortaleza, metrofor, radius = 500, predicate = sf::st_covers) %>%
  dplyr::arrange(dplyr::desc(n_features))
