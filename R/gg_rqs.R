#' Quantile Regression Plots with CI
#'
#' @description
#' A easy way to plot quantile regression coefficient plots with confidence intervals.
#'
#' @param rqs A `rq` or `rqs` object from `{quantreg}`.
#' @param var `<data-masking>` Variable to be plotted
#' @param labels A list with the plot labels (optional)
#' @param ci Should confidence intervals be plotted? (Defaut: `TRUE`)
#' @param ci_color CI bands color (default: blue)
#' @param zero Should a horizontal line indicating the zero be plotted? (Defaut: `TRUE`)
#' @param zero_color Zero line color (default: red)
#' @inheritDotParams quantreg::summary.rq
#'
#' @import quantreg
#' @importFrom dplyr mutate bind_rows
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @import ggplot2
#'
#' @export
#'
#' @returns A `ggplot` object
#'

gg_rqs <- function(rqs, var, labels = NULL,
                   ci = TRUE, ci_color = "blue", zero = TRUE, zero_color = "red",
                   ...) {

  value <- quant <- NULL

  coef <- coef_rqs(rqs, ...)

  p <- ggplot() +
    geom_line(
      data = coef %>% filter(value == "coef"),
      aes(x = quant, y = {{var}})
    ) +
    geom_point(
      data = coef %>% filter(value == "coef"),
      aes(x = quant, y = {{var}})
    )

  if(ci) {
    p <- p +
      geom_line(
        data = coef %>% filter(value == "lower_bound"),
        aes(x = quant, y = {{var}}),
        linetype = "dashed",
        color = ci_color
      ) +
      geom_line(
        data = coef %>% filter(value == "upper_bound"),
        aes(x = quant, y = {{var}}),
        linetype = "dashed",
        color = ci_color
      )
  }

  if(zero) {
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed", color = zero_color)
  }

  if(!is.null(labs)) {
    p <- p + do.call(labs, labels)
  }

  return(p)
}
