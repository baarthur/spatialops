#' ggplot in ABNT format
#' @description
#' `{abnquarto}` template for `ggplot` objects.
#' @param base_size Font baseline size. Defaults to 10pt.
#' @param base_family Font family. Defaults to "Times" (for Times New Roman).
#' @param base_line_size,base_rect_size Base stroke for lines and rectangles. Defaults to dividing
#' the font baseline size by 22 (`ggplot`'s default).
#' @import ggplot2
#' @export
#' @returns A `ggplot` object
#' @examples
#' \dontrun{
#' data(mtcars)
#' p <- mtcars %>%
#'   ggplot() +
#'   geom_point(
#'     aes(x = hp, y = mpg, color = factor(am))
#'     ) +
#'  labs(
#'     title = "The fast and the goulash",
#'     subtitle = "The statistical analysis!",
#'     caption = "Ye old dummy",
#'     tag = "Tag along",
#'     x = "Horsepower",
#'     y = "Miles per gallon"
#'     ) +
#'  scale_color_discrete(name = "I'm a legend") +
#'  facet_wrap(vars(gear))
#'
#'  p + theme_abnq()
#' }

theme_abnq <- function (base_size = 10, base_family = "Times", base_line_size = base_size/22,
                        base_rect_size = base_size/22)
{
  half_line <- base_size/2
  theme_linedraw(base_size = base_size, base_family = base_family,
                 base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      text = element_text(
        family = base_family,
        size = base_size,
        color = "black"
      ),

      strip.background = element_rect(
        fill = "grey95",
        color = NA
      ),

      strip.text = element_text(
        color = "black",
        size = base_size,
        margin = margin(2,0,2,0)
      ),

      strip.placement = "outside",

      plot.caption = element_text(hjust = 0)
    )
}



#' ggplot map in ABNT format
#' @description
#' `{abnquarto}` template for `ggplot`. Void version to use with maps.
#' @param base_size Font baseline size. Defaults to 10pt.
#' @param base_family Font family. Defaults to "Times" (for Times New Roman).
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' data(mtcars)
#' p <- mtcars %>%
#'   ggplot() +
#'   geom_point(
#'     aes(x = hp, y = mpg, color = factor(am))
#'     ) +
#'  labs(
#'     title = "The fast and the goulash",
#'     subtitle = "The statistical analysis!",
#'     caption = "Ye old dummy",
#'     tag = "Tag along",
#'     x = "Horsepower",
#'     y = "Miles per gallon"
#'     ) +
#'  scale_color_discrete(name = "I'm a legend") +
#'  facet_wrap(vars(gear))
#'
#'  p + theme_abnq_map()
#' }

theme_abnq_map <- function (base_size = 10, base_family = "Times")
{
  theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text = element_text(
        family = base_family,
        size = base_size,
        color = "black"
      ),

      strip.background = element_rect(
        fill = "grey95",
        color = NA
      ),

      strip.text = element_text(
        color = "black",
        size = base_size,
        margin = margin(2,0,2,0)
      ),

      strip.placement = "outside",

      plot.caption = element_text(hjust = 0)
    )
}
