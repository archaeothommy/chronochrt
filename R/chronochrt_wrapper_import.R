#' Import data for Chronological chart
#'
#' The function imports and converts chronological information from tabular data saved as \code{.csv}, \code{.xlsx} and \code{.xls}, and any other kind of delimited file format into a ready-to-use data set for plotting with ChronochRt. It automatically selects the appropriate import function from the file extension and the argument \code{delim}. To import excel files, the package \code{\link[readxl]} must be installed.
#'
#' Additional columns in the import file will be imported as they are. Among these might be e.g. columns specifying the x and y position of the names to place them at an arbitrary spot.
#'
#' @param path Either a path or a weblink to the file to be imported.
#' @param region A character string with the column name of the regions/sections in the plot.
#' @param name A character string with the column name of the names of the chronological units. Must be a character string.
#' @param start A character string with the column name of the start dates of the chronological units.
#' @param end A character string with the column name of the end date of the chronological units.
#' @param level A character string with the column name of the level of the chronological unit.
#' @param add  A character string with the column name of the columns which signals whether the chronological units within a geographical area should be drawn separately.
#' @param delim A character string with the separator for tabular data. USe \code{delim = "\t"} for tab-separated data. Must be provided for all file types except \code{.xlsx} or \code{.xls}.
#' @param ... Additional arguments passed to the respective import functions. See their documentation for details:
#' \itemize{
#'   \item \code{\link[readxl]{read_excel]}} for file formats \code{.xlsx} and \code{.xls}
#'   \iten \code{\link[readr]{read_csv}} for the file format \code{.csv}
#'   \item \code{\link[readr]{read_delim()}} for all other file formats.
#'   }
#'
#'
#' @return A tibble containing the desired chronological information.
#'
#' @examples
#'


#'@export

import_chron <- function(path, region, name, start, end, level, add, delim, ...)
{

  if (!file.exists(path)) {
    stop("The file path is not correct or the file does not exist.")
  }

  ext <- strsplit(basename(path), split = "\\.")[[1]][-1] # extract file format

  if (!missing(delim) && !(ext %in% c("xlsx", "xls"))) {
    stop("Missing argument: delim")
  }

  if (ext %in% c("xlsx", "xls")) {
    data <- import_chron_excel(path = path, region = region, name = name, start = start, end = end, level = level, add = add, ...)
  }

  if (ext = "csv" && delim %in% c(",", ";")) {
    data <- import_chron_csv(path = path, region = region, name = name, start = start, end = end, level = level, add = add, delim = delim, ...)
    } else {
    data <- import_chron_delim(path = path, region = region, name = name, start = start, end = end, level = level, add = add, delim = delim, ...)
    }

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  data
}
