#' Import data for Chronological chart
#'
#' The function imports and converts chronological information from tabular data
#' saved as \code{.csv}, \code{.xlsx} and \code{.xls}, and any other kind of
#' delimited file format into a ready-to-use data set for plotting with
#' ChronochRt. It automatically selects the appropriate import function from the
#' file extension and the argument \code{delim}. To import excel files, the
#' package \pkg{readxl} must be installed.
#'
#' Additional columns in the import file will be imported as they are. Among
#' these might be e.g. columns specifying the x and y position of the names to
#' place them at an arbitrary spot.
#'
#' @param path A character string with either the path or a URL to the file to
#'   be imported.
#' @param region Character string (case sensitive) with the column name of the
#'   region/section, default to \code{"region"}.
#' @param name Character string (case sensitive) with the column name of the
#'   chronological units' names, default to \code{"name"}.
#' @param start Character string (case sensitive) with the column name of the
#'   chronological units' start dates, default to \code{"start"}.
#' @param end Character string (case sensitive) with the column name of the
#'   chronological units' end dates, default to \code{"end"}.
#' @param level Character string (case sensitive) with the column name of the
#'   chronological units' levels, default to \code{"level"}.
#' @param add Character string (case sensitive) with the column name of the
#'   information whether the chronological units within a region/section should
#'   be drawn separately or not, default to \code{"add"}.
#' @param delim A character string with the separator for tabular data. Use
#'   \code{delim = "\t"} for tab-separated data. Must be provided for all file
#'   types except \code{.xlsx} or \code{.xls}.
#' @param ... Additional arguments passed to the respective import functions.
#'   See their documentation for details: \itemize{ \item
#'   \code{\link[readxl]{read_excel}} for file formats \code{.xlsx} and
#'   \code{.xls}, \item \code{\link[readr]{read_csv}} for the file format
#'   \code{.csv}, \item \code{\link[readr]{read_delim}} for all other file
#'   formats. }
#'
#' @return A tibble containing the desired chronological information.
#'
#' @export

#' @examples
#' \dontrun{
#'
#' # Import of Excel files
#' chrons <- import_chron(path = "ex_urnfield_periods.xlsx",
#'                        region = "Region",
#'                        name = "Name",
#'                        start = "Start",
#'                        end = "End",
#'                        level = "Level",
#'                        add = "Add")
#'
#' # Import of delimited tabular data
#' chrons <- import_chron(path = "ex_urnfield_periods.csv",
#'                        region = "Region",
#'                        name = "Name",
#'                        start = "Start",
#'                        end = "End",
#'                        add = "Add",
#'                        delim = ",")
#'
#' chrons <- import_chron(path = "ex_urnfield_periods.txt",
#'                        region = "Region",
#'                        name = "Name",
#'                        start = "Start",
#'                        end = "End",
#'                        add = "Add",
#'                        delim = "\t")
#'
#' # Include additional parameters of the import function
#' chrons <- import_chron(path = "ex_urnfield_periods.xlsx",
#'                        region = "Region",
#'                        name = "Name",
#'                        start = "Start",
#'                        end = "End",
#'                        level = "Level",
#'                        add = "Add",
#'                        sheet = "data")
#' }

import_chron <- function(path, region = "region", name = "name",
                         start = "start", end = "end", level = "level",
                         add = "add", delim, ...) {

  if (!file.exists(path)) {
    stop("The file path is not correct or the file does not exist.")
  }

  ext <- strsplit(basename(path), split = "\\.")[[1]][-1] # extract file format

  if (missing(delim) && !(ext %in% c("xlsx", "xls"))) {
    stop("Missing argument: delim")
  }

  if (ext %in% c("xlsx", "xls")) {

    if (!requireNamespace("readxl")) {
      stop("Import of Excel files requires the package `readxl`. Please install it or choose another file format.")
    }

    data <- import_chron_excel(path = path, ...)
  } else {
    if (ext == "csv") {
      if (delim %in% c(",", ";")) {
        data <- import_chron_csv(path = path, delim = delim, ...)
      } else {
        stop("No valid separator for csv files: ", delim)
      }
    } else {
      data <- import_chron_delim(path = path, delim = delim, ...)
    }
  }

  data <- dplyr::rename(
    data, tidyselect::all_of(
      c(
        region = region,
        name = name,
        start = start,
        end = end,
        level = level,
        add = add
      )
    )
  )

  data$add <- as.logical(data$add)

  if (sum(!is.na(data$add)) != length(data$add)) {
    stop("Wrong input format: ", substitute(add),
         " contains empty cells or non-logical values. ")
  }

  if (!all(
    is.character(data$region),
    is.character(data$name),
    is.numeric(data$start) | is.character(data$start),
    is.numeric(data$end) | is.character(data$end),
    is.numeric(data$level), is.logical(data$add)
  )
  ) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: ", substitute(level),
         " must contain only whole numbers (1, 2, 3, ...).")
  }

  data
}
