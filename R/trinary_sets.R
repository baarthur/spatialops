#' Trinary sets
#' @description
#' Perform spatial operations in sets of three classes and an additional category.
#' @param df A `sf` object.
#' @param cat1 <data-masking> Column containing the object classes. Cannot contain more than three
#' distinct classes.
#' @param cat2 <data-masking> Second category to guide sets classification.
#' @param keep.overlap Should overlapping sets (i.e. entire sets and unions) be included? Defaults
#' to `FALSE`.
#' @export
#' @returns A `sf` object
#' @details
#' Returned data contains one geometry for each class (A', B', C', AB, AC, BC, ABC, and overlaps
#' when required) per additional category. Categories are renamed according to the order displayed
#' in the message after the code runs successfully.



trinary_sets <- function(df, cat1, cat2, keep.overlap = F) {

# ground rules --------------------------------------------------------------------------------

  ## get original categores
  cats <- df %>% dplyr::pull({{cat1}}) %>% unique()

  if(length(cats) != 3) break else

  ## replace them by ABC
  df <- df %>% dplyr::mutate(
    {{cat1}} := dplyr::case_match(
      {{cat1}},
      cats[1] ~ "A",
      cats[2] ~ "B",
      cats[3] ~ "C"
    )
  )

  ## we'll show this message in the end
  message_1 <- paste0("Groups were renamed as follow: ", cats[1], " ~ A, ", cats[2], " ~ B, ", cats[3], " ~ C.")

  cats <- df %>% dplyr::pull({{cat1}}) %>% unique()

  ## get category pairs to map them
  pairs <- tidyr::expand_grid(x = cats, y = cats, .name_repair = "unique") %>%
    dplyr::filter(x != y) %>%
    dplyr::slice(c(1:2, 4)) %>%
    as.matrix(nrow = 2) %>%
    split(1:nrow(.))

  ## classify base sets
  df <- df %>%
    dplyr::group_by({{cat1}}, {{cat2}}) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(class = "base", .after = {{cat1}})



# pairwise unions -----------------------------------------------------------------------------

  message("Making pairwise unions...")

  df <- pairs %>%
    purrr::map(
      \(pair)
      df %>%
        dplyr::filter({{cat1}} %in% pair) %>%
        dplyr::group_by({{cat2}}, class) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        sf::st_make_valid() %>%
        dplyr::mutate({{cat1}} := paste0(pair[1],"u",pair[2]), class = "union", .before = {{cat2}})
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(df)

  message("Done.")



# extract prime sets --------------------------------------------------------------------------
  ## prime sets are those that do not intersect with anything; e.g. A' = A - B\cupC

  ## union categories need to be apart to be combined with base ones
  cats2 <- df %>% dplyr::pull({{cat1}}) %>% unique() %>% setdiff(cats)

  pairs2 <- tidyr::expand_grid(x = cats, y = cats2, .name_repair = "unique") %>%
    dplyr::filter(stringr::str_detect(y, x) == F) %>%
    as.matrix(nrow = 2) %>%
    split(1:nrow(.))

  ## prime sets

  message("Getting prime sets...")

  df <- pairs2 %>%
    purrr::map(
      \(pair)
      df %>%
        dplyr::filter({{cat1}} %in% pair) %>%
        sf::st_difference() %>%
        sf::st_make_valid() %>%
        dplyr::filter({{cat1}} == pair[1]) %>%
        dplyr::mutate({{cat1}} := paste0(pair[1],1), class = "prime", .before = {{cat2}})
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(df)

  message("Done.")



# triple intersection -------------------------------------------------------------------------

  message("Getting triple intersection...")

  df <- df %>%
    dplyr::group_by({{cat2}}) %>%
    dplyr::filter({{cat1}} == cats[1]) %>%
    sf::st_intersection(df %>% dplyr::filter({{cat1}} == cats[2])) %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_intersection(df %>% dplyr::filter({{cat1}} == cats[3])) %>%
    sf::st_make_valid() %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    sf::st_collection_extract("POLYGON") %>%
    dplyr::group_by({{cat2}}) %>%
    dplyr::summarise(geometry = sf::st_union(geometry)) %>%
    sf::st_make_valid() %>%
    sf::st_difference() %>%
    dplyr::filter(!sf::st_is_empty(geometry)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_make_valid() %>%
    dplyr::mutate({{cat1}} := "ABC", class = "intersection", .before = {{cat2}}) %>%
    dplyr::bind_rows(df)

  message("Done.")



# pairwise intersections ----------------------------------------------------------------------

  message("Getting pairwise intersections...")

  ## first step: get the whole pairwise intersection

  message("Full XY intersection")

  df_aux <- pairs %>%
    purrr::map(
      \(pair)
      df %>%
        dplyr::group_by({{cat2}}) %>%
        dplyr::filter({{cat1}} == pair[1]) %>%
        sf::st_intersection(df %>% dplyr::filter({{cat1}} == pair[2])) %>%
        sf::st_make_valid() %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        dplyr::group_by({{cat2}}) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        sf::st_make_valid() %>%
        sf::st_difference() %>%
        sf::st_make_valid() %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        dplyr::mutate({{cat1}} := paste0(pair[1], pair[2]), class = "intersection", .before = {{cat2}})
    ) %>%
    dplyr::bind_rows()

  ## second step: remove triple intersection

  message("Removing triple intersection from XY")

  df <- pairs %>%
    purrr::map(
      \(pair)
      df_aux %>%
        dplyr::group_by({{cat2}}) %>%
        dplyr::filter({{cat1}} == paste0(pair[1], pair[2])) %>%
        sf::st_difference(df %>% dplyr::filter({{cat1}} == "ABC") %>% sf::st_union() %>% sf::st_make_valid()) %>%
        sf::st_make_valid() %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>%
        sf::st_collection_extract("POLYGON") %>%
        dplyr::group_by({{cat2}}) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        sf::st_make_valid() %>%
        sf::st_difference() %>%
        sf::st_make_valid() %>%
        dplyr::mutate({{cat1}} := paste0(pair[1], pair[2]), class = "intersection", .before = {{cat2}})
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::bind_rows(df)

  message("Done.")


# final ---------------------------------------------------------------------------------------

  message("Success!")
  message(message_1)
  if(keep.overlap) {
    return(df)
  } else {
    df %>%
      dplyr::filter(!(class %in% c("base", "union"))) %>%
      return()
  }

}
