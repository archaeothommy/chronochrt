#' Arranging the regions (sections) of a chronological chart
#'
#' This function ensures that the regions/sections of a chronological chart are arranged different from an alphabetal order.
#'
#' @param data A data set with chronological data.
#' @param region A character string with the column name of the regions/sections
#' @param order A character vector with right order of the region/section titles. Each title must be given only once.
#'
#' @return A tibble with chronological data ready-to-use for plotting with \code{\link[chronochrt]{plot_chronochrt}}.
#'
#' @export
#'
#' @examples

arrange_regions <- function(data, region, order)
{
  if (!exists(deparse(substitute(data)))) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (!exists(deparse(substitute(order)))) {
    stop("The object ", substitute(order) , " does not exist.")
  }

  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or tibble.")
  }

  if (!is.character(region)) {
    stop("Incompatible input format: ", substitute(region), " must be a character string.")
  }

  if (!is.character(order) && !is.vector(order)) {
    stop("Incompatible input format: ", substitute(order), " must be a vector of unique character strings.")
  }

  data$region <- dplyr::mutate(data, !!region := factor(!!(as.name(region)), levels = order))

  data
}


