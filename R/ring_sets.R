#' Ring sets
#' @description
#' Divide space in concentric rings and one additional category.
#' @param df A `sf` object.
#' @param cat1 <data-masking> A numeric vector. Ring order, increasing on the polygon size.
#'  E.g., the radius for buffers or the time limit for isochrones.
#' @param cat2 <data-masking> Second category to guide sets classification.
#' @export
#' @returns A `sf` object



# function ------------------------------------------------------------------------------------

ring_sets <- function(df, cat1, cat2) {
  levels <- sort(unique(dplyr::pull(df, {{cat1}})))

  rings <- rev(seq(levels)) %>%
    purrr::map(
      \(x) summarise_set(df, {{cat1}}, {{cat2}}, order = x)
    )

  rings <- purrr::map2(
    rings[-length(rings)], rings[-1],
    \(x, y) trim_set(y, x, {{cat1}})
  ) %>%
    dplyr::bind_rows(rings[length(rings)])
}
