data("fortaleza")

# grid for only one polygon

## cutting grid edges (default)
fortaleza %>%
  filter(name_neigh == "centro") %>%
  get_h3_grid() %>%
  ggplot() +
  geom_sf()

## letting grid cross borders
fortaleza %>%
  filter(name_neigh == "centro") %>%
  get_h3_grid(crop = FALSE) %>%
  ggplot() +
  geom_sf()


# grid for multiple polygons using purrr::map()

## cutting edges
fortaleza %>%
  filter(name_region %in% c("SER III", "SER IV")) %>%
  pull(name_neigh) %>%
  map(
    \(x) fortaleza %>%
      filter(name_neigh == x) %>%
      get_h3_grid()
  ) %>%
  bind_rows() %>%
  ggplot() +
  geom_sf(aes(fill = name_region), alpha = 0.25, linewidth = 0.125) +
  theme_void()
