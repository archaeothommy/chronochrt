#' ChronochRt
#'
#' ChronochRt offers an easy way to draw chronological charts from tables. It
#' aims to provide an intuitive environment for anyone new to R.
#'
#' @section Features: \itemize{\item Slim structure of chronological datasets
#'   \item Import tabular data files and \item Import Excel files (requires the
#'   package \pkg{readxl} \item Possibility to display up to 2 chronological
#'   systems within the same region (e.g. long and short chronologies) \item
#'   Layout of the chronological chart optimised for easy readability and
#'   comprehensibility \item Years in BCE must be negative - that's all you need
#'   to care about for dates \item Handling of insecure dates \item Handling of
#'   gaps, e.g. abandonment phases of sites \item Optional text labels \item
#'   Optional image labels to e.g. display key finds or show typological
#'   developments \item Geoms for the chronological chart and image labels \item
#'   Export of the chronological chart in different file formats (raster and
#'   vector graphics) \item Easy customisation of the chronological chart \item
#'   Based on the \href{https://www.tidyverse.org/}{tidyverse}: Seamless
#'   integration in pipes, enhanced customisation with \pkg{ggplot2}}
#'
#' @section Getting started: \itemize{ \item
#'   \href{https://gitlab.com/archaeothommy/chronochrt/-/raw/master/inst/ChronochRt_Cheatsheet.pdf?inline=false}{Cheatsheet}
#'    \item Vignettes}
#'
"_PACKAGE"
#'
#'
#'
#' @importFrom rlang .data
#' @importFrom magick image_read
#' @importFrom dplyr mutate case_when across filter if_else arrange group_by
#' ungroup select rename left_join distinct bind_rows
#' @importFrom tidyr separate pivot_longer drop_na

NULL
