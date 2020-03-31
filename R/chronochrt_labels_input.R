#' Provide text labels for a chronological chart
#'
#' The function creates a tibble with text labels to be plotted in a chronological chart or adds them to an anlready existing tibble.
#'
#' If the input is in the same order like the arguments, the arguments does not need to be explicitly named. Values can be provided as one number or one character string, if they are the same for all other data. If not, they must be provided as vectors with equal lengths.
#' It is assumed that most of the labels will be located on the right side of each column. The \code{position} of a label defines it right most end to prevent it from running outside the plotting area. Vertically, it will be placed centered on the \code{year} given. Text in labels can be wrapped by inserting \code{"\n"} (without blanks).
#'
#' @param data An object to which labels should be added. Must not be provided if \code{new = FALSE}.
#' @param region A character string or character vector with the the titles of the sections the label(s) should be placed in.
#' @param year A number or a numeric vector with the year(s) at which the label should be placed (i.e. its vertical position).
#' @param position A number or a numeric vector with the horiontal position(s) of the label. See Details for explanation.
#' @param label A character string or character vector with the text of the label(s).
#' @param new Logical operator. If \code{TRUE}, a new data set will be created. If \code{FALSE}, the default, the input will be added to an existing data set.
#' @param ... Further arguments or columns to include in or additional arguments passed to \code{\link[tibble]{tibble()}} or \code{\link[tibble]{add_row()}}.
#'
#' @return A tibble with text labels ready-to-use for plotting with \code{\link[chronochrt]{plot_chronochrt}}.
#'
#' @export
#'
#' @examples new TREU/FALSE, Vector vs. single string.
#'

add_label_text <- function(data, region, year, position = 0.9, label, new = FALSE, ...)
{
  if (!exists(deparse(substitute(data))) && new == FALSE) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (new == TRUE) {data <- tibble::tibble(region, year, position, label, ...)}
  if (new == FALSE) {data <- tibble::add_row(data, region, year, position, label, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.character(data$label), is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, label), numeric (year, position).")
  }

  data
}

# add image labels --------------------------------------------------------


#' Provide image labels for a chronological chart
#'
#' The function creates a tibble with path of image labels to be plotted in a chronological chart or adds them to an anlready existing tibble.
#'
#' If the input is in the same order like the arguments, the arguments does not need to be explicitly named. Values can be provided as one number or one character string, if they are the same for all other data. If not, they must be provided as vectors with equal lengths.
#'
#' @param data An object to which labels should be added. Must not be provided if \code{new = FALSE}.
#' @param region A character string or character vector with the titles of the sections the label(s) should be placed in.
#' @param year A number or a numeric vector with the year(s) at which the label should be placed (i.e. its vertical position).
#' @param position A number or a numeric vector with the horiontal position(s) of the label. See Details for explanation.
#' @param image_path A character string or character vector with the file path(s) or weblinks to the image files.
#' @param new Logical operator. If \code{TRUE}, a new data set will be created. If \code{FALSE}, the default, the input will be added to an existing data set.
#' @param ... Further arguments or columns to include in or additional arguments passed to \code{\link[tibble]{tibble()}} or \code{\link[tibble]{add_row()}}.
#'
#' @return A tibble with image labels ready-to-use for plotting with \code{\link[chronochrt]{plot_chronochrt}}.
#'
#' @noRd
#'
#' @examples

add_label_image <- function (data, region, year, position = 0.9, image_path, new = FALSE, ...)
{
  if (!exists(deparse(substitute(data))) && new == FALSE) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (new == TRUE) {data <- tibble::tibble(region, year, position, image_path, ...)}
  if (new == FALSE) {data <- tibble::add_row(data, region, year, position, image_path, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region), numeric (year, position).")
  }

  data
}

