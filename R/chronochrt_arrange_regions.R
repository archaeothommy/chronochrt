#' Arranging the regions (sections) of a chronological chart
#'
#' This function ensures that the regions/sections of a chronological chart are
#' arranged in the desired order, not necessarily in an aphabethical one.
#'
#' @param data A data set with chronological data and a cloumn named "region".
#' @param order A character vector with desired order of the region/section
#'   titles. Each title must be given only once.
#'
#' @return A tibble with chronological data ready-to-use for plotting with
#'   \code{\link{plot_chronochrt}}.
#'
#' @export
#'
#' @examples
#' # Create example data set
#'
#' chrons <- add_chron(region = c("A", "B"),
#'                     name = c("a", "a"),
#'                     start = -100,
#'                     end = c(200, 150),
#'                     level = c(1, 1),
#'                     add = FALSE,
#'                     new_table = TRUE)
#'
#' # Arrange regions
#'
#' chrons <- arrange_regions(data = chrons, order = c("B", "A"))
#'
#'



arrange_regions <- function(data, order)
{
  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or tibble.")
  }

  if (!"region" %in% names(data)) {
    stop("Columns `region` does not exist in ", data, " .")
  }

  if (!is.character(order)) {
    if (!is.vector(order)) {
      stop("Incompatible input format: ", substitute(order), " must be a vector of unique character strings.")
    } else {stop("Incompatible input format: ", substitute(order), " is not a character vector.")
    }
  }

  data$region <- factor(data$region, levels = order)

  data
}


