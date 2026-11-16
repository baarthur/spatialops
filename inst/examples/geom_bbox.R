library(ggplot2)
library(dplyr)

data("fortaleza")

# plot the whole city, fill neighborhood "aldeota" in red, and zoom in to region "SER II"
ggplot(fortaleza) +
  geom_sf(fill = NA) +
  geom_sf(data = filter(fortaleza, name_neigh == "aldeota"), fill = "red") +
  geom_bbox(fortaleza |> filter(name_region == "SER II")) +
  theme_void()

