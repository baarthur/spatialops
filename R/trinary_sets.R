#' Trinary sets
#' @description
#' Divide space in three transitive sets and one additional category.
#' @param df A `sf` object.
#' @param cat1 <data-masking> Column containing the set classes. If more than three classes are
#'  present, any class different from those provided on `cat1_order` (or the first three if
#'  `cat1_order` is `NULL`) are ignored.
#' @param cat2 <data-masking> Second category to guide sets classification.
#' @param cat1_order A vector with size 3 with cat1 class names in order of importance: the first
#'  one dominates the second if they overlap and so on. If `NULL`, the order in which they appear in data
#'  will be taken (or the `levels` order, if `cat1` is a `factor`).
#' @export
#' @returns A `sf` object
#' @details
#' Current implementation uses `dplyr::group_by`, which groups data in ascending order (numerical or
#' alphabetical). If `cat2` is `numeric`, smaller numbers dominate larger ones when
#' they overlap; similarly, if `cat2` is `character` or `factor`, the hierarchy will be alphabetical.



# function ------------------------------------------------------------------------------------

trinary_sets <- function(df, cat1, cat2, cat1_order = NULL) {

  if(dplyr::n_distinct(dplyr::pull(df, {{cat1}})) < 3) {
    stop("'", deparse(substitute(cat1)), "' should have at least three classes, only one is present")
  } else {
    if(dplyr::n_distinct(dplyr::pull(df, {{cat1}})) > 3) {
      message("More than three categories present in '", deparse(substitute(cat1)),
              "'. Geometries in other categories will be ignored.")
    }
  }

  sf::st_geometry(df) <- "geometry"

  set_1 <- summarise_set(df, {{cat1}}, {{cat2}}, cat1_order, 1)
  set_2 <- summarise_set(df, {{cat1}}, {{cat2}}, cat1_order, 2)
  set_3 <- summarise_set(df, {{cat1}}, {{cat2}}, cat1_order, 3)

  set_2 <- trim_set(set_1, set_2, {{cat1}})
  set_3 <- trim_set(set_1, set_3, {{cat1}})

  dplyr::bind_rows(set_2, set_3) %>%
    trim_set(set_1, {{cat1}}, F) %>%
    return()

}


