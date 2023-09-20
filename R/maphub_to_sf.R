#' Widen description from MapHub
#' @description
#' Convert maphub geojson-like description info to tibble columns.
#' @param data A `sf` object imported from MapHub and read into R using `sf::st_read`.
#' @param values <data-masking> Column containing data to be converted into multiple variables.
#' Defaults to `description`.
#' @param key_sep Character string that separates a key from its value.
#' @param pair_sep Character string that separates key-value pairs.
#' @import tidyr
#' @import sf
#' @export
#' @returns A `sf` object.
#' @details
#' Returns a spatial dataset with separate columns for each key-value pair in the original description
#' column

maphub_to_sf <- function(data, values = description, key_sep = ": ", pair_sep = "\n") {
  data <- data %>%
    separate_longer_delim(
      {{values}},
      delim = pair_sep
    ) %>%
    separate_wider_delim(
      {{values}},
      delim = key_sep,
      names = c("key", "value")
    ) %>%
    pivot_wider(
      names_from = key,
      values_from = value
    ) %>%
    st_as_sf()
}



#' Tidy transit lines from Maphub.
#' @param data A `sf` object imported from MapHub and already converted using maphub_to_sf
#' @import dplyr
#' @import sf
#' @export
#' @returns A `sf` object.
#' @details
#' Very context-specific, columns created using maphub_to_sf() should match colum names here.

maphub_tidy_lines <- function(data) {
  data <- data %>%
    select(-title) %>%
    relocate(stroke:geometry, .after = everything()) %>%
    relocate(empresa:numero_linha, .before = everything()) %>%
    rename(color = stroke) %>%
    mutate(inaugura = as.numeric(inaugura)) %>%
    mutate(numero_linha = as.numeric(numero_linha)) %>%
    arrange(inaugura)
}



#' Tidy transit stations from Maphub.
#' @param data A `sf` object imported from MapHub and already converted using maphub_to_sf
#' @import dplyr
#' @import sf
#' @export
#' @returns A `sf` object.
#' @details
#' Very context-specific, columns created using maphub_to_sf() should match colum names here.

maphub_tidy_stations <- function(data) {
  data <- data %>%
    select(-title) %>%
    relocate(c(marker.color:geometry), .after = everything()) %>%
    relocate(nome_est, .after = numero_linha) %>%
    relocate(inaugura, .before = status) %>%
    rename(color = marker.color) %>%
    mutate(inaugura = as.numeric(inaugura)) %>%
    mutate(numero_linha = as.numeric(numero_linha)) %>%
    arrange(inaugura)
}
