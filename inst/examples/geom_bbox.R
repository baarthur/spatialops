data("fortaleza")

# plot the whole city, fill neighborhood "aldeota" in red, and zoom in to region "SER II"
fortaleza %>%
  ggplot() +
  geom_sf(fill = NA) +
  geom_sf(data = . %>% filter(name_neigh == "aldeota"), fill = "red") +
  geom_bbox(fortaleza %>% filter(name_region == "SER II")) +
  theme_void()

