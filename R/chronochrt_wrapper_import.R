#' ChronochRt import wrapper function
#'
#' This functions checks the file extension, automatically selects the import function and prepares the data for plotting with chronochRt.
#'
#' Import of excel files require the package \code{readxl}. All other file formats are sent to the resptective import function of the package \code{readr}.
#'
#' @author Chiara Girotto, \email{chiara.girotto@web.de}
#'
#' @param path The path of the file to be imported.
#' @param region Specifies the colum of the geographical area/Level 1 (overarching) headline.
#' @param name Specifies the colum of the name of the time period.
#' @param start Specifies the colum of the start date (can be negative).
#' @param end Specifies the colum of the end date (can be negative).
#' @param level Specifies the colum of the sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether data within "region" should be displayed as separate column. Defaults to FALSE.
#' @param ... additional arguments inherited from \code{read_delim()} and \code{read_table()}. See their documentation for details.
#'
#' @return A table containing the desired chronological information.
#'
#' @examples


#'@export

import_chron <- function(path, region, name, start, end, level, add = FALSE, delim, ...)
{

  ext <- strsplit(basename(data_path), split = "\\.")[[1]][-1] # extract file format

  if (ext %in% c("xlsx", "xls")) {
    import_chron_excel(path = path, region = region, name = name, start = start, end = end, level = level, add = add, ...)
  }

  if (ext = c("csv")) {
    import_chron_csv(path = path, region = region, name = name, start = start, end = end, level = level, add = add, delim = delim, ...)

  } else {
      import_chron_delim(path = path, region = region, name = name, start = start, end = end, level = level, add = add, delim = delim, ...)
    }

  #check columns formats
}


# implementation check_format like in add_chron

# Select the data path
# path <- "Example_data/ex_urnfield_periods.xlsx"
# data_path <- path
