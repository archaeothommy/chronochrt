#' Provide image labels for a chronological chart
#'
#' The function creates a tibble with the paths of the image labels to be
#' plotted in a chronological chart or adds them to an already existing tibble.
#'
#' If the input is in the same order as the arguments, the arguments do not need
#' to be explicitly named. Values can be provided as a number or character
#' string, if they are the same for all other data. If not, they must be
#' provided as vectors with equal lengths.
#'
#' @param data An object to which labels should be added. Must not be provided
#'   if \code{new = FALSE}.
#' @param region A character string or character vector with the titles of the
#'   sections the label(s) should be placed in.
#' @param year A number or a numeric vector with the year(s) at which the label
#'   should be placed (i.e. its vertical position).
#' @param position A number or a numeric vector with the horizontal position(s)
#'   of the label. See Details for explanation.
#' @param image_path A character string or character vector with the file
#'   path(s) or URL(s) to the image files.
#' @param new Logical operator. If \code{TRUE}, a new data set will be created.
#'   If \code{FALSE}, the default, the input will be added to an existing data
#'   set.
#' @param ... Further columns to include or additional arguments passed to
#'   \code{\link[tibble]{tibble}} or \code{\link[tibble]{add_row}}.
#'
#' @return A tibble with image labels ready-to-use for plotting with
#'   \code{\link{plot_chronochrt}}.
#'
#' @export
#' @examples
#' # Create new label data set
#' labels <- add_label_image(region = "A",
#'                           year = -50,
#'                           position = 0.5,
#'                           image_path = "https://www.r-project.org/logo/Rlogo.png",
#'                           new = TRUE)
#'
#' # Add labels to existing data set
#' labels <- add_label_image(data = labels,
#'                           region = "B",
#'                           year = 50,
#'                           position = 0.9,
#'                           image_path = "https://www.r-project.org/logo/Rlogo.png",
#'                           new = FALSE)
#'
#' # They can be linked using the pipe operator \code{%>%}:
#' library(magrittr)
#'
#' labels <- add_label_image(region = "A",
#'                           year = -50,
#'                           position = 0.5,
#'                           image_path = "https://www.r-project.org/logo/Rlogo.png",
#'                           new = TRUE) %>%
#'           add_label_image(region = "B",
#'                           year = 50,
#'                           position = 0.9,
#'                           image_path = "https://www.r-project.org/logo/Rlogo.png")

add_label_image <- function (data, region, year, position = 0.75, image_path,
                             new = FALSE, ...)
{
  if (new == FALSE) {
    data <- tibble::add_row(data, region, year, position, image_path, ...)
  } else {
    data <- tibble::tibble(region, year, position, image_path, ...)
  }

  if (!all(is.character(data$region),
           is.numeric(data$year),
           is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data
         must be strings (region), numeric (year, position).")
  }

  data
}
