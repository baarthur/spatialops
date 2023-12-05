data("fortaleza")

# entire data
ggplot() +
  geom_sf_toscale(fortaleza, 15000)

# filtering a few neighborhoods, all at once
ggplot() +
  geom_sf_toscale(fortaleza, 3000, name_neigh, c("centro", "aldeota"))

# filtering a few neighborhoods, one per plot
p <- fortaleza[1:2,] %>%
  pull(name_neigh) %>%
  map(
    \(x) ggplot() +
      geom_sf(data = fortaleza, fill = "white") +
      geom_sf_toscale(fortaleza, 2000, name_neigh, x) +
      labs(title = x)
  )

## plotting them together
cowplot::plot_grid(p[[1]], p[[2]])
