# Determine maximum x value of parallel chrons ----------------------------

#' Determine maximum x value of parallel chrons
#'
#' This function determines the maximum x value of parallel chrons
#'
#' @param start,end Start and end of the chrons.
#' @param xmax The upper x value of the chrons.
#'
#' @return A vector
#'
#' @keywords internal
#' @export

corr_xmax <- function(start, end, xmax)
{
  data <- xmax

  for(i in 1:length(start))
  {
    for(j in i:length(start))
    {
      if (start[i] + 1  >= start[j] && start[i] + 1 <= end[j]){
        data[i] <- max(xmax[i], xmax[j])
        data[j] <- max(xmax[i], xmax[j])
      }
    }
  }
  data
}


# Implementation with lubridate -------------------------------------------

# corr_xmax <- function(interval, xmax)
# {
#   data <- xmax
#
#   for(i in 1:length(interval))
#   {
#     for(j in i:length(interval))
#     {
#       if (lubridate::int_overlaps(interval[i], interval[j])){
#         data[i] <- max(xmax[i], xmax[j], data[i])
#         data[j] <- max(xmax[i], xmax[j], data[j])
#       }
#     }
#   }
#   data
# }
