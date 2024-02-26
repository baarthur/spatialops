#' Auxiliary functions

# summarise_set -------------------------------------------------------------------------------

#' Summarise sets
#' @description
#' For each leading category class, aggregate polygons according to a second category.
#' @param df A `sf` object.
#' @param cat1 <data-masking> Leading category to guide sets classification.
#' @param cat2 <data-masking> Second category to guide sets classification.
#' @param order Category's hierarchy in `cat1_order` (external).
#' @param cat1_order A vector with size 3 with cat1 class names in order of importance: the first
#'  one dominates the second if they overlap and so on. If `NULL`, the order in which they appear in data
#'  will be taken (or the `levels` order, if `cat1` is a `factor`).
#' @export
#' @returns A `sf` object
#' @details
#' This function is not intended do be used isolatedly, it is a part of binary_sets and trinary_sets.

summarise_set <- function(df, cat1, cat2, cat1_order = NULL, order) {

  geometry <- NULL
  sf::st_geometry(df) <- "geometry"

  cat1_order <- if(is.null(cat1_order)) {
    if(is.factor(dplyr::pull(df, {{cat1}}))) {
      levels(dplyr::pull(df, {{cat1}}))
    } else {
      unique(dplyr::pull(df, {{cat1}}))
    }
  } else cat1_order

  df %>%
    dplyr::filter({{cat1}} == cat1_order[order]) %>%
    sf::st_make_valid() %>%
    dplyr::group_by({{cat1}}, {{cat2}}) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    sf::st_difference() %>%
    sf::st_make_valid()
}


# trim_set ------------------------------------------------------------------------------------

#' Summarise sets
#' @description
#' Trim polygons according to a reference.
#' @param ref <data-masking> A `sf` object to use as reference.
#' @param set <data-masking> The `sf` object to be cropped.
#' @param cat1 <data-masking> Leading category to guide sets classification.
#' @param remove_ref Should the reference be discarded? Defaults to `TRUE`.
#' @export
#' @returns A `sf` object
#' @details
#' This function is not intended do be used isolatedly, it is a part of binary_sets and trinary_sets.

trim_set <- function(ref, set, cat1, remove_ref = TRUE) {
  df <- dplyr::bind_rows(ref, set) %>%
    dplyr::group_by({{cat1}}) %>%
    sf::st_make_valid() %>%
    sf::st_difference() %>%
    sf::st_make_valid()

  if(remove_ref) {
    df %>%
      dplyr::filter(!{{cat1}} %in% dplyr::pull(ref, {{cat1}}))
  } else
    df
}
