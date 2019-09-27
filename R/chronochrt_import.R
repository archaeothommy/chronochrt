#' Import chronological information from .xlsx files
#'
#' This functions allows to import chronological information previously compiled
#' in another programme. Missing values will be substituted by "!".
#'
#' @author Thomas Rose, \email{###@###}
#'
#' @param path the path of the file to be imported.
#' @param region specifies the colum of the geografical area/Level 1 (overarching) headline.
#' @param name specifies the colum of the name of the time period.
#' @param start specifies the colum of the start date (can be negative).
#' @param end specifies the colum of the end date (can be negative).
#' @param level specifies the colum of sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether conflicting data within "region" should be displayed simulatenously. Defaults to FALSE.
#'
#'
#' @return A table containing the desired chronological information.
#'
#' @examples
#' UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx",
"Region", "Name", "Start", "End", "Level")

#'@export


import_chron_xl <- function(path, region, name, start, end, level, add = FALSE, ...)
{
  data <- readxl::read_excel(path, na = "!", ...) %>%
    rename(region = region, name = name, start = start, end = end, level = level, add = add)
      # implementation check_format like in add_chron
  data
}


#' Import chronological information from .csv files
#'
#' This functions allows to import chronological information previously compiled in a comma seperated .csv file. Missing values will be substituted by "!".
#'
#' @author Thomas Rose, \email{###@###}
#'
#' @param path the path of the file to be imported.
#' @param region specifies the colum of the geografical area/Level 1 (overarching) headline.
#' @param name specifies the colum of the name of the time period.
#' @param start specifies the colum of the start date (can be negative).
#' @param end specifies the colum of the end date (can be negative).
#' @param level specifies the colum of sublevel.
#' @param add = TRUE or FALSE. A binary operator indicating whether conflicting data within "region" should be displayed simulatenously. Defaults to FALSE.
#'
#' @return A table containing the desired chronological information.
#'
#' @examples
#' UK_chronology <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx",
"Region", "Name", "Start", "End", "Level")

#'@export


import_chron_csv<- function(path, region, name, start, end, level, add = FALSE, ...)
{
  data <- readxl::read_csv(path, na = "!", ...) %>%
    rename(region = region, name = name, start = start, end = end, level = level, add = add)
    data
}
