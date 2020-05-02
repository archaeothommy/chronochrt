#' The ChronochRt ggplot theme
#'
#' This is a theme to provide a ready-to-use layout of chronological charts. See
#' Examples how to alter its appearance with \code{\link[ggplot2]{theme}}.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base size for line elements
#' @param base_rect_size base size for rect elements
#'
#' @return A ggplot-theme
#' @export
#'
#' @examples

#  Hier auch was mit theme(), z. b. um Hintergrundfarbe zu ändern

theme_chronochrt <- function (base_size = 11, base_family = "", base_line_size = base_size / 22, base_rect_size = base_size / 22)
{
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size)
    ggplot2::`%+replace%`
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "grey90", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black"),
      panel.grid = ggplot2::element_line(colour = "grey50", linetype = "dotted"),
      panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
      panel.spacing = grid::unit(0, "lines"),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      plot.caption = ggplot2::element_text(vjust = 9, hjust = 1),
      complete = TRUE
    )
}

