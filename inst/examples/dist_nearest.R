data("fortaleza")
data("metrofor")

# select only stations
metrofor <- dplyr::filter(metrofor, railway == "station")

# closest station to each neighborhood
dist_nearest(fortaleza, metrofor)

# specifying a column name
dist_nearest(fortaleza, metrofor, name = dist_metro)

# binding `y`s id to `x`
dist_nearest(fortaleza, metrofor, id_feature = TRUE)
