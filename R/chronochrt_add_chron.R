#' Create or add chronological data
#'
#' This function either creates a data set with chronological data which can be
#' directly used for plotting or it adds chronological data to such a data set.
#'
#' If the input is in the same order like the arguments, the arguments do not
#' need to be explicitly named. Values can be provided as one number or one
#' character string, if they are the same for all other data. If not, they must
#' be provided as vectors with equal lengths.
#'
#' \code{start} and \code{end} of neighbouring chronological units must be the same
#' to get a good plotting results, as well as the start date of e.g. the top
#' chronological unit and its respective oldest sub-units. Dates in BCE must
#' provided as negative data. Currently, only years can be handled (i.e. 2020
#' but not 20.10.2020).
#'
#' If \code{start} and \code{end} dates are not certain or the change between
#' chronological units is regarded a period, dates must be given as character
#' string in the format \code{"1000/2000"}. Consistency is required for matching
#' \code{start} and \code{end} dates to avoid unclean borders in the plot.
#'
#' The \code{level} indicates the position of the chronological unit.
#' \code{level = 1} denotes a top chronological unit (e.g. Ha), a sub-unit (e.g.
#' Ha B) is \code{level = 2}, a sub-sub-unit (e.g. Ha B1) \code{level = 3} etc.
#' The parameter \code{add} indicates whether the respective chronological
#' unit(s) should be plotted in the same or an additional column. This might be
#' useful if competing chronologies in one region exist (e.g. short and long
#' chronologies). See the vignette for a detailed explanation how the parameters
#' \code{level} and \code{add} work.
#'
#' Additional columns might be useful to e.g.  specify the x and y position of
#' the names of the chronological units to place them at an arbitrary spot.
#'
#' @param data A data set with chronological data. Must not be provided if
#'   \code{new_table = FALSE}.
#' @param region A character string or character vector with the title(s) of the
#'   section(s).
#' @param name A character string or character vector with the name(s) of the
#'   chronological unit(s).
#' @param start A number or a vector with the start date(s) of the chronological
#'   unit(s). Use negative values for BCE dates. See Details how to handle
#'   unsecure start dates.
#' @param end A number or a vector with the end date(s) of the chronological
#'   unit(s). Use negative values for BCE dates. See Details how to handle
#'   unsecure end dates.
#' @param level A whole number or numeric vector of whole numbers (i.e. 1, 2, 3,
#'   ...) with the level(s) of the chronological unit(s). The default is
#'   \code{1}, i.e. the top unit.
#' @param add  A logical value (\code{TRUE} or \code{FALSE}) or a logical vector
#'   signaling whether the chronological units within a geographical area should
#'   be drawn separately (\code{TRUE}) or not (\code{FLASE}, the default).
#' @param new_table Logical operator. If \code{TRUE}, a new data set will be
#'   created. If \code{FALSE}, the default, the input will be added to an
#'   existing data set.
#' @param ... Additional columns to include in the data set and/or additional
#'   arguments passed to \code{\link[tibble]{tibble()}} (if \code{new_table =
#'   TRUE}) or \code{\link[tibble]{add_row}} (if \code{new_table = FALSE}).
#' @param ... Further arguments or columns to include in or additional arguments
#'   passed to \code{\link[tibble]{tibble}} or
#'   \code{\link[tibble]{add_row}}.
#'
#' @return A tibble with chronological data ready-to-use for plotting with
#'   \code{\link{plot_chronochrt}}.
#'
#' @export
#'
#' @examples
#' # Create new data set
#'
#' chrons <- add_chron(region = c("A", "B"),
#'                     name = c("a", "a"),
#'                     start = -100,
#'                     end = c(200, 150),
#'                     level = c(1, 1),
#'                     add = FALSE,
#'                     new_table = TRUE)
#'
#' # Add chrons to an existing data set
#' chrons2 <- add_chron(data = chrons,
#'                      region = "A",
#'                      name = c("1", "2"),
#'                      start = c(-100, 100),
#'                      end = c(100, 200),
#'                      level = 2,
#'                      add = FALSE,
#'                      new_table = FALSE)
#'
#' # Include chrons with unclear start/end data
#' chrons <- add_chron(data = chrons,
#'                     region = "B",
#'                     name = c("1", "2"),
#'                     start = c(-100, "0/50"),
#'                     end = c("0/50", 150),
#'                     level = 2,
#'                     add = FALSE,
#'                     new_table = FALSE)
#'
#' # They can be linked using the pipe operator \code{%>%}:
#' library(magrittr)
#'
#' chrons <- add_chron(region = c("A", "B"),
#'                     name = c("a", "a"),
#'                     start = -100,
#'                     end = c(200, 150),
#'                     level = c(1, 1),
#'                     add = FALSE,
#'                     new_table = TRUE) %>%
#'           add_chron(region = "B",
#'                     name = c("1", "2"),
#'                     start = c(-100, "0/50"),
#'                     end = c("0/50", 150),
#'                     level = 2,
#'                     add = FALSE)
#'


add_chron <- function(data, region, name, start, end, level = 1,
                      add = FALSE, new_table = FALSE, ...)
{
  if (new_table == FALSE) {
    if (!missing(data)) {

      if (typeof(data$start) != typeof(start)) {
        data$start <- as.character(data$start)
        start <- as.character(start)
      }

      if (typeof(data$end) != typeof(end)) {
        data$end <- as.character(data$end)
        end <- as.character(end)
      }

      data <- tibble::add_row(data, region, name, start, end, level, add, ...)
      } else {
        stop("The argument `data` must be provided or `new_table` must be TRUE.")
        }
    } else {
      data <- tibble::tibble(region, name, start, end, level, add, ...)
    }

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  data
}
