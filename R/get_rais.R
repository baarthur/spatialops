#' Download RAIS data
#' @description
#' Get data from the official Brazilian employer-employee dataset (RAIS) using package `{basedosdados}`
#' @param variables A character vector containing the selected variables. To select all, use
#' variables = "*".
#' @param source Data source in BigQuery.
#' @param city IBGE 7-digit code of the city in single quotes, e.g. "id_municipio = '1234567'.
#' @param geo_filter Optional geographic filters for districts and neighborhoods, when available.
#' @param ano Selected year(s).
#' @param path Path, including file name and extension, to save results to.
#' @importFrom basedosdados download
#' @export
#' @returns a `.csv` file downloaded in the specified path
#' @details
#' A simple wrapper of the `download()` function to automate download for multiple years. `variables`,
#' `source`, `city`, `geo_filter`, and `ano` should all be written as part of an SQL query. To select
#' multiple cities at once, use format `city` = "id_municipio IN (city1, city2)". For more information
#' on the RAIS dataset available at `{basedosdados}`, please refer to
#' https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00?table=c3a5121e-f00d-41ff-b46f-bd26be8d4af3.
#' @examples
#' \dontrun{
#' # Getting RAIS data for Caete (MG)
#'
#' ## setting variables
#' variables <- "ano, quantidade_vinculos_ativos, cnae_2, indicador_atividade_ano"
#' source <- "`basedosdados.br_me_rais.microdados_estabelecimentos`"
#' city <- "id_municipio='3110004'"
#'
#' ## download
#' get_rais(
#'   variables = variables,
#'   source = source,
#'   city = city,
#'   ano = 2018,
#'   path = "data/csv/df_rais_caete.csv"
#'   )
#'}


get_rais <- function(variables, source, city, geo_filter, ano, path) {
  if(missing(geo_filter)) {
    query <- paste0(
      "SELECT ", variables,
      " FROM ", source,
      " WHERE", city,
      " AND ano=", ano
    )
  } else {
    query <- paste0(
        "SELECT ", variables,
        " FROM ", source,
        " WHERE ", city,
        " AND ", geo_filter,
        " AND ano=", ano
      )
  }
  download(
    query = query,
    path = path
  ) %>%
    return()
}

