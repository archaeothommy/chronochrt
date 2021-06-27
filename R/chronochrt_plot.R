#' Plot a chronological chart
#'
#' This function converts a chronological data set into a chronological chart.
#' It provides basic features for the export of the plot and for its
#' customisation.
#'
#' This function is wrapper around various functions for an quick and convenient
#' way to draw chronological charts. It therefore relies on the common data
#' structure of ChronochRt (see \code{vignette("ChronochRt")} for details). For
#' full customisation use the respective geoms to build your own plot.
#'
#' It is assumed that the majority of the text labels will be placed on the
#' right side of each column. Therefore they are right aligned to prevent them
#' from running outside the plotting area. Vertically, it will be placed
#' centred on the \code{year} given. Text in labels can be wrapped by inserting
#' \code{"\n"} (without blanks).
#'
#' @inheritParams geom_chronochRt
#' @param data A data set with chronological data.
#' @param labels_text A data set containing the text labels.
#' @param labels_image A data set containing the image labels.
#' @param axis_title A character string with the axis label of the vertical
#'   axis. Default is \code{"Years"}.
#' @param size_text Font size of the names of the chronological units in mm. All
#'   other text elements will be scaled accordingly. The default is \code{6} mm.
#' @param height_image The absolute height of the image labels in cm. The
#'   default is  \code{2} cm.
#' @param size_line Thickness of the line in mm. The default is \code{0.5} mm.
#' @param fill_chron Fill colour of the chronological units. The default is
#'   \code{"white"}. See the colour specification section of
#'   \code{\link[graphics]{par}} for how to specify colours in R.
#' @param color_chron Line (border) colour of the chronological units. The
#'   default is \code{"black"}. See the colour specification section of
#'   \code{\link[graphics]{par}} for how to specify colours in R.
#' @param color_label Colour of the text labels. The default is \code{"black"}.
#'   See the colour specification section of \code{\link[graphics]{par}} for
#'   how to specify colours in R.
#' @param line_break Line length of the section labels in characters. Text will
#'   be wrapped at the blank closest to the specified number of characters.
#'   Default is \code{10} characters.
#' @param filename A character string with the filename or path. If specified,
#'   the plot will be saved on the given location. The file format is
#'   automatically recognised from the file extension. The most common file
#'   types are supported, e.g. \code{.tiff}, \code{.png}, \code{.jpg},
#'   \code{.eps}, and \code{.pdf}. To export as \code{.svg} installation of the
#'   package \pkg{svglite} is required. See \code{\link[ggplot2]{ggsave}} for
#'   more details about the supported file formats.
#' @param plot_dim Dimensions of the plot as a vector in the format
#'   \code{c(width, height, units)}. Supported units are "cm", "mm", in". For
#'   example, \code{plot_dim = c(5,5,"cm")} will produce a plot of the
#'   dimensions 5 x 5 cm. If unspecified, the standard values of the respective
#'   graphic device are used.
#' @param background Optional specifications for the background of the
#'   chronological chart as vector in the format \code{c(background colour,
#'   linetype of grid lines)} to overwrite the default behaviour of
#'   \code{\link{theme_chronochrt}}. Any valid colour and linetype
#'   specifications are accepted, e.g. \code{c("grey90", "dotted")} (these are
#'   the default values of \code{\link{theme_chronochrt}}. See the sections
#'   "colour specification" and "line type specification" in \code{\link[graphics]{par}} for
#'   how to specify colours and line types in R.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggsave}} to
#'   enhance the saved plot like \code{dpi} to specify its resolution. See
#'   \code{\link[ggplot2]{ggsave}} for detailed information.
#'
#' @return
#'
#' A ggplot2 object with the chronological chart.
#'
#' @export
#'
#' @examples
#'
#' # Create Example data
#' chrons <- data.frame(region = c("A", "B", "B", "B", "A"),
#'                      name = c("a", "a", "1", "2", "b"),
#'                      start = c(-100, -100, -100, "0/50", "50_100"),
#'                      end = c("50_100", 150, "0/50", 150, 200),
#'                      level = c(1, 1, 2, 2, 1),
#'                      add = FALSE)
#'
#' # Plot with default options
#' plot_chronochrt(chrons)
#'
#' # Add labels
#' labels <- data.frame(region = "A",
#'                      year = -50,
#'                      position = 0.5,
#'                      label = "Event")
#'
#' images <- data.frame(region = "B",
#'                      year = 100,
#'                      position = 0.5,
#'                      image_path = "https://www.r-project.org/logo/Rlogo.png")
#'
#' plot_chronochrt(chrons, labels, images)
#'
#' # Customise plot
#' plot_chronochrt(chrons, axis_title = "BC/AD", year_lim = c(-50,100),
#'                 fill_chron = "black", color_chron = "white", size_line = 5)
#' plot_chronochrt(chrons, labels, images, color_label = "red", size_text = 5, height_image = 4)
#'
#' # Export plot
#' \dontrun{
#'   plot_chronochrt(chrons, filename = "chronology.jpg", plot_dim = c(10, 15, "cm"))
#'
#'   # with additional parameters
#'   plot_chronochrt(chrons, filename = "chronology.jpg", plot_dim = c(10, 15, "cm"), dpi = 300)
#'
#' }
#'
#' # Additional customisation with ggplot2
#' plot_chronochrt(chrons) + ggplot2::theme_bw()


plot_chronochrt <- function(data, labels_text = NULL, labels_image = NULL, year_lim = NULL, axis_title = "Years", minimal = FALSE, size_text = 6, height_image = 2, size_line = 0.5, fill_chron = "white", color_chron = "black", color_label = "black", line_break = 10, filename = NULL, plot_dim, background = NULL, ...)
{

  if (is.data.frame(data)) {

    if (!("region" %in% names(data))) {stop("Wrong input format: The column `region` in ", substitute(data), " does not exist.")}
    if (!("name" %in% names(data))) {stop("Wrong input format: The column `name` in ", substitute(data), " does not exist.")}
    if (!("start" %in% names(data))) {stop("Wrong input format: The column `start` in ", substitute(data), " does not exist.")}
    if (!("end" %in% names(data))) {stop("Wrong input format: The column `end` in ", substitute(data), " does not exist.")}
    if (!("level" %in% names(data))) {stop("Wrong input format: The column `level` in ", substitute(data), " does not exist.")}
    if (!("add" %in% names(data))) {stop("Wrong input format: The column `add` in ", substitute(data), " does not exist.")}

    if (!all(is.character(data$region) | is.factor(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
      stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
    }
    if (!all(round(data$level) == data$level)) {
      stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
    }
  } else {
    stop("Wrong input format: ", substitute(data) , " must be a data frame or tibble.")
  }

  if (!is.null(labels_text)) {
    if (is.data.frame(labels_text)) {
      if (!("region" %in% names(labels_text))) {stop("Wrong input format: The column `region` in ", substitute(labels_text), " does not exist.")}
      if (!("year" %in% names(labels_text))) {stop("Wrong input format: The column `year` in ", substitute(labels_text), " does not exist.")}
      if (!("label" %in% names(labels_text))) {stop("Wrong input format: The column `label` in ", substitute(labels_text), " does not exist.")}
      if (!("position" %in% names(labels_text))) {stop("Wrong input format: The column `position` in ", substitute(labels_text), " does not exist.")}

      if (!all(is.character(labels_text$region), is.numeric(labels_text$year), is.character(labels_text$label), is.numeric(labels_text$position))) {
        stop("One or more columns of the text label data contain incompatible data. Data must be strings (region, label) and numeric (year, position).")
      }
    } else {
      stop("Wrong input format: ", substitute(labels_text) , " must be a data frame or tibble.")
    }
  }

  if (!is.null(labels_image)) {
    if (is.data.frame(labels_image)) {
      if (!("image_path" %in% names(labels_image))) {stop("Wrong input format: The column `region` in ", substitute(labels_image), " does not exist.")}
      if (!("region" %in% names(labels_image))) {stop("Wrong input format: The column `region` in ", substitute(labels_image), " does not exist.")}
      if (!("year" %in% names(labels_image))) {stop("Wrong input format: The column `year` in ", substitute(labels_image), " does not exist.")}
      if (!("position" %in% names(labels_image))) {stop("Wrong input format: The column `position` in ", substitute(labels_image), " does not exist.")}

      if (!all(is.character(labels_image$region), is.numeric(labels_image$year), is.character(labels_image$image_path), is.numeric(labels_image$position))) {
        stop("One or more columns of the image label data contain incompatible data. Data must be strings (region, label, image_path) and numeric (year, position).")
      }
    } else {
      stop("Wrong input format: ", substitute(labels_image) , " must be a data frame or tibble.")
    }
  }

  if (!is.null(year_lim) && any(!is.numeric(year_lim), length(year_lim) != 2)) {stop("Wrong input format: ", year_lim, " must be a numberic vector of length 2.")}
  if (!is.character(axis_title)) {stop("Wrong input format: ", axis_title, " must be a character string.")}
  if (!is.numeric(size_text) || length(size_text) != 1) {stop("Wrong input format: ", size_text, " must be a number.")}
  if (!any(is.character(fill_chron), is.numeric(fill_chron)) || length(fill_chron) != 1) {fill_chron <- "white"}
  if (!any(is.character(color_chron), is.numeric(color_chron)) || length(color_chron) != 1) {color_chron <- "black"}
  if (!any(is.character(color_label), is.numeric(color_label)) || length(color_label) != 1) {color_label <- "black"}
  if (!is.numeric(height_image) || length(height_image) != 1) {stop("Wrong input format: ", height_image, " must be a number.")}
  if (!is.logical(minimal)) {stop("Wrong input format: ", minimal, " must be 'TRUE' or 'FALSE'.")}
  if (!is.numeric(size_line) || length(size_line) != 1) {stop("Wrong input format: ", size_line, " must be a number.")}
  if (!is.numeric(line_break) || length(line_break) != 1) {stop("Wrong input format: ", line_break, " must be a number.")}
  if (!is.null(filename)) {
    if (!is.character(filename)) {stop("Wrong input format: ", filename, " must be a character string.")}
    if (!dir.exists(dirname(filename))) {stop("The directory ", dirname(filename), " does not exist.")}
    if (!is.null(plot_dim)) {

      if (!plot_dim[3] %in% c("in", "cm", "mm")) {stop("This unit is not supported. Only the following units are support: mm, cm, in.")}

      width <- as.numeric(plot_dim[1])
      height <- as.numeric(plot_dim[2])
      units <- as.character(plot_dim[3])

    } else {

      width <- NA
      height <- NA
      units <- NULL
    }
  }
  if(!is.null(background)) {
    if (length(background) != 2) {stop("Wrong input format:", background, " is not a vector of length 2.")}
    bg_fill <- background[1]
    grid_linetype <- background[2]
  }

  plot <- ggplot2::ggplot() +
    geom_chronochRt(data = data, ggplot2::aes(region = .data$region, name = .data$name, start = .data$start, end = .data$end, level = .data$level, add = .data$add), minimal = minimal, year_lim = year_lim, size_text = size_text, fill = fill_chron, color = color_chron, size_line = size_line)

  if (!is.null(labels_text)) {
    plot <- plot + ggplot2::geom_text(data = labels_text, ggplot2::aes(y = .data$year, x = .data$position, label = .data$label, hjust = 1, vjust = 0.5), na.rm = TRUE, size = size_text*0.75, color = color_label)
  }

  if (!is.null(labels_image)) {
    plot <- plot + geom_chronochRtImage(data = labels_image, ggplot2::aes(image_path = .data$image_path, x = .data$position, y = .data$year, height = height_image))
  }

  plot <- plot +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = axis_title, expand = c(0,0)) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$region), scales = "free_x", space = "free_x", labeller = ggplot2::label_wrap_gen(width = line_break)) +
    theme_chronochrt() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = size_text*0.8*72.27/25.4),
                   axis.title = ggplot2::element_text(size = size_text*72.27/25.4, face="bold"),
                   strip.text.x = ggplot2::element_text(size = size_text*1.25*72.27/25.4, face="bold"))

  if(!is.null(background)) {
    plot <- plot +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_fill),
                     panel.grid = ggplot2::element_line(linetype = grid_linetype))
  }

  if(!is.null(filename)) {
    ggplot2::ggsave(plot = plot, filename = filename, width = width, height = height, units = units, ...)
  }

  plot
}
