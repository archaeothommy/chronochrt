#' Prepare an existing data set for plotting
#'
#' Convert an existing data set in a ready-to-plot data set.
#'
#' Additional columns if the data set are directly passed to the output.
#'
#' @param data A data frame or tibble
#' @param region The column name of the regions/sections in the plot.
#' @param name The column name of the names of the chronological units. Must be
#'   a character string.
#' @param start The column name of the start dates of the chronological units.
#' @param end The column name of the end date of the chronological units.
#' @param level The column name of the level of the chronological unit.
#' @param add  The column name of the columns which signals whether the
#'   chronological units within a geographical area should be drawn separately.
#'
#' @return A tibble with chronological data ready-to-use for plotting with
#'   \code{\link{plot_chronochrt}}.
#'
#' @export
#'
#' @examples
#' # Create example data set
#' data <- data.frame(Location = c("A", "B"),
#'                    Dynasty = c("a", "a"),
#'                    Begin = -100,
#'                    End = c(200, 150),
#'                    Subunit = 1,
#'                    Parallel = FALSE)
#'
#' # Convert to chronological data set
#' chrons <- convert_to_chron(data = data,
#'                            region = Location,
#'                            name = Dynasty,
#'                            start = Begin,
#'                            end = End,
#'                            level = Subunit,
#'                            add = Parallel)

convert_to_chron <- function(data, region, name, start, end, level, add)
{
  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or
         tibble.")
  }

  pos <- tidyselect::eval_rename(rlang::expr(c(region = {{region}},
                                               name = {{name}},
                                               start = {{start}},
                                               end = {{end}},
                                               level = {{level}},
                                               add = {{add}})),
                                 data)
  names(data)[pos] <- names(pos)

  if (!all(is.character(data$region),
           is.character(data$name),
           is.numeric(data$start) | is.character(data$start),
           is.numeric(data$end) | is.character(data$end),
           is.numeric(data$level), is.logical(data$add)
           )
      ) {
    stop("One or more columns of the data set contain incompatible data. Data
         must be strings (region, name), numbers (start, end), whole numbers
         (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop(
      "Wrong input format: level must contain only whole numbers (1, 2, 3, ...)"
      )
  }

  data
}
