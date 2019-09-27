#' ChronochRt import wrapper function
#'
#' This functions checks the file extension and automatically selects the xlsx or csv import.
#'
#' @author Chiara Girotto, \email{chiara.girotto@web.de}
#'
#' @param path the path of the file to be imported.
#'
#' @return A table containing the desired chronological information.
#'
#' @examples


#'@export



# Select the data path
# path <- "Example_data/ex_urnfield_periods.xlsx"
# data_path <- path

# Function to check the file extension
check_file_extension <- function (data_path) {
  ext <- strsplit(basename(data_path), split = "\\.")[[1]]
  return(ext[-1])
}

# Function to select the relevant import function
## no idea how to make that a pretty thing.
if (ext = c("xlsx")) {
  import_chron_xl ()
}
else if (ext = c("csv)"))
  import_chron_csv
}

## overarching import function
import_chron <- function(path, region, name, start, end, level, add = FALSE, ...)
{
  data <- readxl::read_excel(path, na = "!", ...) %>%
    rename(region = region, name = name, start = start, end = end, level = level, add = add)
  # implementation check_format like in add_chron
  data
}
