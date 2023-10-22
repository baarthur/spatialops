#' Unified Summary for Quantile Regression
#'
#' @description
#' A wrapper for `summary.rq()`. Display a quantile reregression's call in a tidy format,
#'    one column for each variable and the coefficient, confidence intervals, and tau in the rows.
#'    This makes it easy to make plots with confidence intervals for each coefficient.
#'
#' @param rqs A `rq` or `rqs` object from `{quantreg}`.
#' @param ... Arguments passed on to [quantreg::summary.rq()]
#'
#' @import quantreg
#' @importFrom dplyr mutate bind_rows
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @returns A `tibble`
#'
#' @seealso [quantreg::rq()]

coef_rqs <- function(rqs, ...) {
  summary <- summary(rqs, ...)

  summary %>%
    map(
      \(x)
      x$coefficients %>%
        t() %>%
        as_tibble(rownames = "value") %>%
        mutate(
          quant = x$tau
        )
    ) %>%
    bind_rows() %>%
    return()
}
