#' Import chronological information from excel files
#'
#' This functions allows to import chronological information previously compiled in another programme ready to plot. Missing values will be substituted by "!".
#'
#' @author Thomas Rose, \email{thomas.rose@daad-alumni.de}
#'
#' @param path The path of the file to be imported.
#' @param region Specifies the colum of the geographical area/Level 1 (overarching) headline.
#' @param name Specifies the colum of the name of the time period.
#' @param start Specifies the colum of the start date (can be negative).
#' @param end Specifies the colum of the end date (can be negative).
#' @param level Specifies the colum of sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether data within "region" should be displayed as separate column. Defaults to FALSE.
#' @param ... Additional arguments inherited from \code{read_excel()}.
#'
#'
#' @return A table containing the desired chronological information.
#'
#' @examples
#' UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx", "Region", "Name", "Start", "End", "Level")

#'@export


import_chron_excel <- function(path, region, name, start, end, level, add = FALSE, ...)
{
  data <- readxl::read_excel(path = path, na = "!", ...) %>%
    dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)

  data
}


#' Import chronological information from .csv files
#'
#' This functions allows to import chronological information previously compiled in another programme ready to plot. Missing values will be substituted by "!".
#'
#' @author Thomas Rose, \email{thomas.rose@daad-alumni.de}
#'
#' @param path the path of the file to be imported.
#' @param region specifies the colum of the geografical area/Level 1 (overarching) headline.
#' @param name specifies the colum of the name of the time period.
#' @param start specifies the colum of the start date (can be negative).
#' @param end specifies the colum of the end date (can be negative).
#' @param level specifies the colum of sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether data within "region" should be displayed as separate column. Defaults to FALSE.
#' @param delim Single character used to separate fields within a record
#' @param ... Additional arguments inherited from \code{read_csv()}.
#'
#' @return A table containing the desired chronological information.
#'
#' @examples
#' UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx", "Region", "Name", "Start", "End", "Level")

#'@export


import_chron_csv <- function(path, region, name, start, end, level, add = FALSE,  delim, ...)
{
  if (!(delim %in% c(",", ";"))) {
    stop("csv file recognised, delim must be set either to ",","," or ",";",". Alternatively use another file format.")
  }

  if (delim = ",") {
    data <- readr::read_csv(file = path, na = "!", ...) %>%
      dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)
  }

  if (delim = ";") {
  data <- readr::read_csv2(file = path, na = "!", ...) %>%
    dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)
  }

  data
}


#' Import chronological information from other delimited files
#'
#' This functions allows to import chronological information previously compiled in another programme ready to plot. Missing values will be substituted by "!".
#'
#' @author Thomas Rose, \email{thomas.rose@daad-alumni.de}
#'
#' @param path the path of the file to be imported.
#' @param region specifies the colum of the geografical area/Level 1 (overarching) headline.
#' @param name specifies the colum of the name of the time period.
#' @param start specifies the colum of the start date (can be negative).
#' @param end specifies the colum of the end date (can be negative).
#' @param level specifies the colum of sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether data within "region" should be displayed as separate column. Defaults to FALSE.
#' @param delim Single character used to separate fields within a record
#' @param ... Additional arguments inherited from \code{read_delim()} and \code{read_table}.
#'
#' @return A table containing the desired chronological information.
#'
#' @examples
#' UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx", "Region", "Name", "Start", "End", "Level")

#'@export


import_chron_delim <- function(path, region, name, start, end, level, delim, add = FALSE, ...)
{
  if (missing(delim)) {
    stop("Argument delim is undefined.")
  }

  if (delim = "\t") {
    data <- readr::read_tsv(file = path, na = "!", ...) %>%
      dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)

  }

  if (delim = " ") {
    data <- readr::read_table2(file = path, na = "!", ...) %>%
      dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)

  } else {
    data <- readr::read_delim(file = path, delim = delim, na = "!", ...) %>%
      dplyr::rename(region = region, name = name, start = start, end = end, level = level, add = add)
  }

  data
}
