#' Import chronological information from excel files
#'
#' This function imports and converts chronological information saved in
#' \code{.xls} or \code{.xslx} into a ready-to-plot data set. Missing values
#' will be substituted by "!".
#'
#' @param path A characer string with the path of the file to be imported.
#' @param ... Additional arguments inherited from \code{\link[readxl]{read_excel}}.
#'
#'
#' @return A tibble containing the chronological information.
#'
#' @export
#' @keywords internal

import_chron_excel <- function(path,  ...)
{
  data <- readxl::read_excel(path = path,  ...)

  data
}


#' Import chronological information from .csv files
#'
#' This function imports and converts chronological information saved in
#' \code{.csv} into a ready-to-plot data set. Missing values will be substituted
#' by "!".
#'
#' @param path the path of the file to be imported.
#' @param delim A character string with the separator
#' @param ... Additional arguments inherited from \code{\link[readr]{read_csv}}.
#'
#' @return A tibble containing the desired chronological information.
#'
#' @export
#' @keywords internal

import_chron_csv <- function(path, delim, ...)
{
  if (delim == ",") {data <- readr::read_csv(file = path, ...)}

  if (delim == ";") {data <- readr::read_csv2(file = path, ...)}

  data
}

#' Import chronological information from other delimited files
#'
#' This function imports and converts chronological information saved as tabular
#' data into a ready-to-plot data set. Missing values will be substituted by
#' "!".
#'
#' @param path the path of the file to be imported.
#' @param delim A character string with the separator
#' @param ... Additional arguments inherited from \code{\link[readr]{read_delim}} and
#'   \code{\link[readr]{read_table}}.
#'
#' @return A tibble containing the desired chronological information.
#'
#' @export
#' @keywords internal

import_chron_delim <- function(path, delim, ...)
{
  if (delim %in% c("\t", " ")) {
    if (delim == "\t") {data <- readr::read_tsv(file = path, ...)}
    if (delim == " ") {data <- readr::read_table2(file = path, ...)}
  } else {data <- readr::read_delim(file = path, delim = delim, ...)}

  data
}
