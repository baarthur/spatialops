#' RAIS Firms Data - Fortaleza, 2019
#'
#' A subset of the Brazilian employer-employee dataset (RAIS) for Fortaleza in 2019, grouped by
#'    neighborhood. Data obtained via package `{basedosdados}`.
#'
#' @format ## `fortaleza_rais`
#' A `data.frame` with 65 rows and 6 columns:
#' \describe{
#'   \item{year}{Year (double)}
#'   \item{name_neigh}{Neighborhood name, withouth capital letters os special characters}
#'   \item{code_neigh}{Neighborhood code according to city counsil data (character)}
#'   \item{n_jobd}{Number of employees in the neighborhood (integer)}
#'   \item{avg_wage}{Average wage in the neighborhood (double)}
#'   \item{avg_wage_h}{Average hourly wage in the neighborhood (diouble)}
#' }
#'
#' @seealso [basedosdados::download()], [basedosdados::read_sql()]
#' @source <https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00?table=86b69f96-0bfe-45da-833b-6edc9a0af213>

"rais_fortaleza"
