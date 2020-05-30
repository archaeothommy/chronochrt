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
#' @param path A character string with either the path or a weblink to the file
#'   to be imported.
#' @param region,name,start,end,level,add Character string (case sensitive) with the column names of:
#'    \itemize{
#'    \item the region/sections,
#'    \item the names of the chronological units,
#'    \item their start dates,
#'    \item their end dates,
#'    \item their level,
#'    \item the information whether the chronological units within a
#'    region/section should be drawn separately or not.
#'    }
#' @param delim A character string with the separator for tabular data. Use
#'   \code{delim = "\t"} for tab-separated data. Must be provided for all file
#'   types except \code{.xlsx} or \code{.xls}.
#' @param ... Additional arguments passed to the respective import functions.
#'   See their documentation for details:
#'   \itemize{
#'     \item \code{\link[readxl]{read_excel}} for file formats \code{.xlsx}
#'     and \code{.xls},
#'   \item \code{\link[readr]{read_csv}} for the file format \code{.csv},
#'   \item \code{\link[readr]{read_delim}} for all other file formats.
#'   }
#'
#' @return A tibble containing the desired chronological information.
#'
#' @examples
#'
#' @export

# UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx", "Region", "Name", "Start", "End", "Level")

import_chron <- function(path, region, name, start, end, level, add, delim, ...)
{
  if (!file.exists(path)) {
    stop("The file path is not correct or the file does not exist.")
  }

  ext <- strsplit(basename(path), split = "\\.")[[1]][-1] # extract file format

  if (missing(delim) && !(ext %in% c("xlsx", "xls"))) {
    stop("Missing argument: delim")
  }

  if (ext %in% c("xlsx", "xls")) {

    if ("readxl" %in% utils::installed.packages() == FALSE) {
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

  pos <- tidyselect::eval_rename(rlang::expr(c(region = region, name = name, start = start, end = end, level = level, add = add)), data)
  names(data)[pos] <- names(pos)

  data$add <- as.logical(data$add)

  if (sum(!is.na(data$add)) != length(data$add)) {
    stop("Wrong input format: ", substitute(add), " contains empty cells or non-logical values. ")
  }

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: ", substitute(level), " must contain only whole numbers (1, 2, 3, ...).")
  }

  data
}
