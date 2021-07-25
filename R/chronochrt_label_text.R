#' Provide text labels for a chronological chart
#'
#' The function creates a tibble with text labels to be plotted in a
#' chronological chart or adds them to an an already existing tibble.
#'
#' If the input is in the same order as the arguments, the arguments do not
#' need to be explicitly named. Values can be provided as one number or one
#' character string, if they are the same for all other data. If not, they must
#' be provided as vectors with equal lengths.
#' It is assumed that most of the labels will be located on the right side of
#' each column. The \code{position} of a label defines it right most end to
#' prevent it from running outside the plotting area. Vertically, it will be
#' placed centred on the \code{year} given. Text in labels can be wrapped by
#' inserting \code{\\n} (without blanks around it).
#'
#' @param data An object to which labels should be added. Must not be provided
#'   if \code{new = FALSE}.
#' @param region A character string or character vector with the the titles of
#'   the sections the label(s) should be placed in.
#' @param year A number or a numeric vector with the year(s) at which the label
#'   should be placed (i.e. its vertical position).
#' @param position A number or a numeric vector with the horizontal position(s)
#'   of the label. See Details for explanation.
#' @param label A character string or character vector with the text of the
#'   label(s).
#' @param new Logical operator. If \code{TRUE}, a new data set will be created.
#'   If \code{FALSE}, the default, the input will be added to an existing data
#'   set.
#' @param ... Further columns to include, or additional arguments
#'   passed to \code{\link[tibble]{tibble}} or
#'   \code{\link[tibble]{add_row}}.
#'
#' @return A tibble with text labels ready-to-use in
#'   \code{\link{plot_chronochrt}}.
#'
#' @export
#'
#' @examples
#' # Create new label data set
#' labels <- add_label_text(region = "A",
#'                          year = -50,
#'                          position = 0.5,
#'                          label = "Flood",
#'                          new = TRUE)
#'
#' # Add labels to existing data set
#' labels <- add_label_text(data = labels,
#'                          region = "B",
#'                          year = 50,
#'                          position = 0.9,
#'                          label = "Earthquake",
#'                          new = FALSE)
#'
#' # They can be linked using the pipe operator \code{%>%}:
#' library(magrittr)
#'
#' labels <- add_label_text(region = "A",
#'                          year = -50,
#'                          position = 0.5,
#'                          label = "Flood",
#'                          new = TRUE) %>%
#'           add_label_text(region = "B",
#'                          year = 50,
#'                          position = 0.9,
#'                          label = "Earthquake")

add_label_text <- function(data, region, year, position = 0.9, label, new = FALSE, ...)
{
  if (new == FALSE) {
    data <- tibble::add_row(data, region, year, position, label, ...)
    } else {
      data <- tibble::tibble(region, year, position, label, ...)
    }

  if (!all(is.character(data$region), is.numeric(data$year), is.character(data$label), is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, label) or numeric (year, position).")
  }

  data
}
