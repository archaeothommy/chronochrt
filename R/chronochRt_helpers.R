# Counting subchrons ------------------------------------------------------

#' Calcuate number of subchrons
#'
#' This function is used to calculate for each chron the number of parallel chrons.
#'
#' @param left Left argument in evaluation
#' @param right Right argument in evaluation
#'
#' @return A vector
#'
#' @keywords internal
#' @export
#' @noRd

subchron_count <- function(left, right)
{
  data <- rep_len(0, length(left))

  for(i in 1:length(left))
  {
    for(j in 1:length(left))
    {
      if (left[i] >= left[j] & left[i] < right[j] & i != j){
        data[i] <- data[i]+1
      }
    }
  }
  data
}


# corrects center of a chron if there are missing subchrons ---------------

#' Correct the center of a chron
#'
#' This function corrects the calculated center of a chron
#'
#' @param center The center of the chron
#' @param width The width of the chron
#'
#' @return A vector
#'
#' @keywords internal
#' @export
#' @noRd

center_corr <- function(center, width)
{
  for(i in 1:length(center))
  {
    for(j in 1:length(center))
    {
      k <- center[j] - (width[j]/2) - center[i] - (width[i]/2);

      if (center[i] <= center[j] & k > 0 & k < 1 & k <= width[i]){
        center[j] <- center[j] - k/2;
      }
    }
  }
  center
}


# corrects width of a chron if there are missing subchrons ----------------

#' Correct the width of a chron
#'
#' This function corrects the calculated width of a chron
#'
#' @param center The center of the chron
#' @param width The width of the chron
#'
#' @return A vector
#'
#' @keywords internal
#' @export
#' @noRd

width_corr <- function(center, width)
{
  for(i in 1:length(center))
  {
    for(j in 1:length(center))
    {
      k <- center[j] - (width[j]/2) - center[i] - (width[i]/2);

      if (center[i] <= center[j] & k > 0 & k < 1 & k <= width[i]){
        width[j] <-  width[j] + k
      }
    }
  }
  width
}
