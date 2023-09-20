#' RAIS Firms Data - Fortaleza, 2019
#'
#' A subset of the Brazilian employer-employee dataset (RAIS) for Fortaleza in 2019. Data obtained
#'    via package `{basedosdados}`.
#'
#' @format ## `fortaleza_rais`
#' A tibble with 28,453 rows and 3 columns:
#' \describe{
#'   \item{ano}{Year (double)}
#'   \item{quantidade_vinculos_ativos}{Number of employees per firm (double)}
#'   \item{cep}{Firm postcode (double)}
#'   \item{cod_bairro_rais}{Neighborhood code in the RAIS dataset (character)}
#'   \item{nome_bairro}{Neighborhood name, withouth capital letters os special characters}
#'   \item{cod_bairro}{Neighborhood code according to city counsil data (character)}
#' }
#'
#' @source <https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00?table=86b69f96-0bfe-45da-833b-6edc9a0af213>

"rais_fortaleza"