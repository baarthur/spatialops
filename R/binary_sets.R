#' Binary sets
#' @description
#' Divide space in two transitive sets and one additional category. This is a "lite" version
#'  of `trinary_sets`.
#' @param df A `sf` object.
#' @param cat1 <data-masking> Column containing the set classes. If more than two classes are
#'  present, any class different from those provided on `cat1_order` (or the first two if
#'  `cat1_order` is `NULL`) are ignored.
#' @param cat2 <data-masking> Second category to guide sets classification.
#' @param cat1_order A vector with size 2 with cat1 class names in order of importance: the first
#'  one dominates the second if they overlap. If `NULL`, the order in which they appear in data
#'  will be taken (or the `levels` order, if `cat1` is a `factor`).
#' @export
#' @returns A `sf` object
#' @details
#' Current implementation uses `dplyr::group_by`, which groups data in ascending order (numerical or
#' alphabetical). If `cat2` is `numeric`, smaller numbers dominate larger ones when
#' they overlap; similarly, if `cat2` is `character` or `factor`, the hierarchy will be alphabetical.



# function ------------------------------------------------------------------------------------

binary_sets <- function(df, cat1, cat2, cat1_order = NULL) {

  if(dplyr::n_distinct(dplyr::pull(df, {{cat1}})) < 2) {
    stop("'", deparse(substitute(cat1)), "' should have at least two classes, only one is present")
  } else {
    if(dplyr::n_distinct(dplyr::pull(df, {{cat1}})) > 2) {
      message("More than two categories present in '", deparse(substitute(cat1)),
              "'. Geometries in other categories will be ignored.")
    }
  }

  sf::st_geometry(df) <- "geometry"

  ## WIP - not implemented
  # df <- if(is.null(cat2_order)) {
  #   df
  # } else {
  #   if(cat2_order == "asc") {
  #     dplyr::arrange(df, {{cat2}})
  #   } else
  #     if(cat2_order == "desc") {
  #       dplyr::arrange(df, desc({{cat2}}))
  #     }
  # }

  set_1 <- summarise_set(df, {{cat1}}, {{cat2}}, cat1_order, 1)
  set_2 <- summarise_set(df, {{cat1}}, {{cat2}}, cat1_order, 2)

  trim_set(set_1, set_2, {{cat1}}, F)
}
