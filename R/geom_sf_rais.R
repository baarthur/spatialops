#' RAIS maps using ggplot
#'
#' @description
#' An easy way to make maps with data from the Brazilian employer-employee dataset. This function
#'    requires an already spatialized dataset
#'
#' @param x Plot variable.
#' @param scale_theme Palette theme to be used in the plots, currently "distiller" and "viridis" are supported
#' @param palette Optional palette specification. For absolute values, defaults to "YlGnBu" when
#' `scale_theme` = "distiller" or "D" if `scale_theme` = "viridis". For relative values, defaults to "RdBu".
#' @param direction Palette's direction, defaults to 1.
#' @param data_format Specify if data is in absolute (default) or relative format.
#' @param data_level Specify if data represents a point value or growth between two periods.
#' @param data An object with classes `sf` and `data.frame` (or convertible to it, see
#'    [ggplot2::fortify()]). Optional; will inherit data from previous layers if provided.
#' @param auto.labels When `TRUE`, automatically generates title, subtitle, and caption based on `geo_name`,
#' `geo_level`, and `var_name`. If `FALSE`, specify `title`, `subtitle`, and `caption`.
#' @param geo_name Name of the geographic area being represented.
#' @param geo_level Name of the smallest geographic area in the data (e.g. state, neighborhood).
#' @param var_name Name of the plotted variable.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param faceted Defaults to `FALSE`, change to `TRUE` to use a faceted plot.
#' @param facet_var A variable to be passed into `ggplot2::vars()` when faceting.
#' @param faceted_labs Labels for the faceted subplots.
#'
#' @import ggplot2
#'
#' @export
#'
#' @returns A `ggplot` object
#'
#' @examples
#' \dontrun{
#' # 1 one year
#'
#' data("fortaleza")
#' data("rais_fortaleza")
#'
#' ## get geometry data into dataframe
#' rais_fortaleza <- rais_fortaleza %>%
#'   left_join(fortaleza) %>%
#'   sf::st_as_sf() %>%
#'   filter(year == 2019)
#'
#' ## 1.1 inheriting data from ggplot()
#' rais_fortaleza %>%
#'   ggplot() +
#'   geom_sf_rais(n_jobs, var_name = "Jobs", geo_name = "Fortaleza", geo_level = "neighborhood") +
#'   theme_void()
#'
#' ## 1.2 passing data explicitly
#' ggplot() +
#'   geom_sf_rais(n_jobs, var_name = "Jobs", geo_name = "Fortaleza", geo_level = "neighborhood",
#'                data = rais_fortaleza) +
#'   theme_void()
#' }
#'
#' \dontrun{
#' # 2 two years (faceted)
#'
#' data("fortaleza")
#' data("rais_fortaleza")
#'
#' ## get geometry data into dataframe
#' rais_fortaleza <- rais_fortaleza %>%
#'   left_join(fortaleza) %>%
#'   sf::st_as_sf()
#'
#' ## plot
#' ggplot() +
#'   geom_sf(data = fortaleza, color = NA) +
#'   geom_sf_rais(n_jobs, var_name = "Jobs", geo_name = "Fortaleza", geo_level = "neighborhood",
#'                data = rais_fortaleza,
#'                faceted = T, facet_var = year) +
#'   theme_void()
#' }

geom_sf_rais <- function(
    x,
    scale_theme = c("distiller", "viridis"),
    palette = NULL,
    direction = 1,
    data_format = c("absolute", "relative"),
    data_level = c("level", "growth"),
    data = NULL,
    auto.labels = TRUE,
    geo_name = NULL, geo_level = NULL, var_name = NULL,
    title = NULL, subtitle = NULL, caption = NULL,
    faceted = FALSE, facet_var = NULL, faceted_labs = NULL
) {

  scale_theme <- if (missing(scale_theme)) "distiller" else match.arg(scale_theme)
  data_format <- if (missing(data_format)) "absolute" else match.arg(data_format)
  data_level <- if (missing(data_level)) "level" else match.arg(data_level)

  scale_labs <- ifelse(data_format == "absolute", scales::label_comma(big.mark = " "), scales::percent)
  scale_name <- ifelse(data_format == "absolute", "Total", "Change (%)")

  labs_args <- if (auto.labels) {
    list(
      title = paste0(var_name, " in ", geo_name),
      subtitle = paste0(ifelse(data_format == "absolute", "Total", "Growth rate"),
                        ifelse(is.null(geo_level), "", paste0(" per ", geo_level))),
      caption = "Note: excludes public administration.\nSource: RAIS/MTE (2023)"
    )
  } else {list(title = title, subtitle = subtitle, caption = caption)}

  p <- ifelse(is.null(data), list(geom_sf(aes(fill = {{x}}, color = {{x}}))),
              list(geom_sf(data = data, aes(fill = {{x}}, color = {{x}}))))

  if (data_level == "level") {
    scale_fill <- scale_fill_distiller
    scale_color <- scale_color_distiller
    if (scale_theme == "distiller") {
      scale_args <- list(palette = ifelse(is.null(palette), "YlGnBu", palette), direction = direction,
                         labels = scale_labs, name = scale_name)
    } else {
      scale_fill <- scale_fill_viridis_c
      scale_color <- scale_color_viridis_c
      scale_args <- list(option = ifelse(is.null(palette), "D", palette), direction = direction,
                         labels = scale_labs, name = scale_name)
    }
  } else {
    scale_fill <- colorspace::scale_fill_continuous_divergingx
    scale_color <- colorspace::scale_color_continuous_divergingx
    scale_args <- list(palette = ifelse(is.null(palette), "RdBu", palette),
                       labels = scale_labs, name = scale_name)
  }

  p <- p %>%
    c(list(do.call(scale_fill, scale_args), do.call(scale_color, scale_args), do.call(ggplot2::labs, labs_args)))

  if (faceted) {
    if (is.null(faceted_labs)) {
      p <- p %>%
        c(list(facet_wrap(vars({{facet_var}}))))
    } else {
      p <- p %>%
        c(list(facet_wrap(vars({{facet_var}}), labeller = labeller({{facet_var}} := faceted_labs))))
    }
  }
  return(p)
}
