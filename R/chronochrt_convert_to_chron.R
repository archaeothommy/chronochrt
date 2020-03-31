#' Prepare an existing data set for plotting
#'
#' Convert an already existing data set in a ready-to-plot data set.
#'
#' Additional columns useful for plotting might e.g. specify the x and y position of the names to place them at an arbitrary spot
#'
#' @param data A data frame or tibble
#' @param region A character string with the column name of the regions/sections in the plot.
#' @param name A character string with the column name of the names of the chronological units. Must be a character string.
#' @param start A character string with the column name of the start dates of the chronological units.
#' @param end A character string with the column name of the end date of the chronological units.
#' @param level A character string with the column name of the level of the chronological unit.
#' @param add  A character string with the column name of the columns which signals whether the chronological units within a geographical area should be drawn separately.
#' @param ... Additional columns to include in the data set and/or additional arguments passed to \code{\link[dplyr]{rename()}}.
#'
#' @return A tibble with chronological data ready-to-use for plotting with \code{\link[chronochrt]{plot_chronochrt}}.
#'
#' @export
#'
#' @examples

convert_to_chron <- function(data, region, name, start, end, level, add, ...)
{
  if (!exists(deparse(substitute(data)))) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or tibble.")
  }

  data <- dplyr::rename(data, region = (!!as.name(region)), name = (!!as.name(name)), start = (!!as.name(start)), end = (!!as.name(end)), level = (!!as.name(level)), add = (!!as.name(add)), ...)

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  data
}
