library(magrittr)

# Generate data
pts <- sf::st_sfc(
  sf::st_point(c(0,0)),
  sf::st_point(c(1,1)),
  sf::st_point(c(1,-1)),
  sf::st_point(c(-1,1)),
  sf::st_point(c(-1,-1))
) %>%
  sf::st_as_sf() %>%
  cbind(inaug = c(1970, 1980, 1990, 2000, 2010),
        status = c("Always treated", "Always treated", "Treated", "Treated", "Never treated")) %>%
  dplyr::rename(geometry = x)

df <- seq(0.25, 1.25, 0.25) %>%
  purrr::map(
    \(x) sf::st_buffer(pts, x) %>%
      dplyr::mutate(rad = x)
  ) %>%
  dplyr::bind_rows()


# Make ring sets
rings <- ring_sets(df, rad, inaug)


# Dataviz

## ggplot2
# rings %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(ggplot2::aes(fill = factor(rad)), color = NA, alpha = 0.5) +
#   ggplot2::scale_fill_viridis_d() +
#   ggplot2::theme_void()

## base plot
plot(rings$geometry, col = factor(rings$rad), border = "transparent", axes = F)
legend("topright", legend = unique(rings$rad), fill = factor(unique(rings$rad)))
