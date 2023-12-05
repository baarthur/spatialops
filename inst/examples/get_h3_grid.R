library(dplyr)

data("fortaleza")

# grid for only one polygon

## cutting grid edges (default)
fortaleza %>%
  filter(name_neigh == "centro") %>%
  get_h3_grid()

## letting grid cross borders
fortaleza %>%
  filter(name_neigh == "centro") %>%
  get_h3_grid(crop = FALSE)


# grid for multiple polygons using purrr::map()

library(purrr)

## cutting edges
fortaleza %>%
  filter(name_region %in% c("SER III", "SER IV")) %>%
  pull(name_neigh) %>%
  map(
    \(x) fortaleza %>%
      filter(name_neigh == x) %>%
      get_h3_grid()
  ) %>%
  bind_rows()
