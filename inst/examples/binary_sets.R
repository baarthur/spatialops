
# Generate data
pts <- sf::st_sfc(
  sf::st_point(c(0,0)),
  sf::st_point(c(1,1)),
  sf::st_point(c(1,-1)),
  sf::st_point(c(-1,1)),
  sf::st_point(c(-1,-1))
) |>
  sf::st_as_sf() |>
  cbind(inaug = c(1970, 1980, 1990, 2000, 2010),
        status = c("Always treated", "Always treated", "Treated", "Treated", "Never treated")) |>
  dplyr::rename(geometry = x)

df <- pts |> sf::st_buffer(1.25)


# Make binary sets
catchment <- binary_sets(df, status, inaug, cat1_order = c("Treated", "Never treated"))


# Dataviz

## ggplot2
# ggplot2::ggplot(catchment) +
#   ggplot2::geom_sf(ggplot2::aes(fill = status), color = NA, alpha = 0.25) +
#   ggplot2::theme_void()

## base plot
plot(catchment$geometry, col = catchment$status, border = "transparent", axes = F)
legend("topright", legend = unique(catchment$status), fill = unique(catchment$status))
