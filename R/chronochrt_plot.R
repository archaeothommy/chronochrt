#' Plot a chronological charts
#'
#' This function converts a chronological data set into a chronological chart.
#' It provides basic features for the export of the plot and for its
#' customaisation.
#'
#' The plot will use \code{\link{theme_chronochrt}} by default.
#' Additional customisation of the plot can be done by adding additional layers
#' to the plot (see Examples). In this case \code{filename} must not be
#' specified or \code{\link[ggplot2]{ggsave}} must be called again at the end
#' to save the final plot.
#'
#' It is assumed that most of the text labels will be located on the right side
#' of each column. For this reason they are right aligned to prevent them from
#' running outside the plotting area. Vertically, it will be placed centered on
#' the \code{year} given. Text in labels can be wrapped by inserting \code{"\n"}
#' (without blanks).
#'
#' The following arguments will silently use the default values, if the input is
#' incompatible: \code{axis_title}, \code{font_size_chrons},
#' \code{font_size_labels}, \code{years_major}, \code{color_fill},
#' \code{color_line}, \code{size_line}, \code{line_break}.
#'
#' @param data A data set with chronological data.
#' @param labels_text A data set containing the text labels.
#' @param chron_name_x,chron_name_y Specifies the horizontal and vertical
#'   position of the chronological unit's names. They can be: \itemize{ \item a
#'   character string with the name of the respective column in \code{data},
#'   \item a number. Then all names will plot at this position, \item a numeric
#'   vector specifiying the position of each name; \item it must not be
#'   specified if a column with the name "chron_name_x" already exists in
#'   \code{data}; \item if not specified and not present in \code{data}, names
#'   are placed in the centers of the chonological units. }
#' @param chron_name_angle Specifies the angle of the chronological unit's
#'   names. It can be: \itemize{ \item a character string with the name of the
#'   respective column in \code{data}, \item a number. Then all names are placed
#'   in the same angle, \item a numeric vector specifiying the position of each
#'   name; \item it must not be specified if a column with the name
#'   "chron_name_angle" already exists in \code{data}; \item if not specified
#'   and not present in \code{data}, the default value \code{0} is used, i.e.
#'   horzontal. }
#' @param axis_title A character string with the axis label of the vertical
#'   axis. Default is \code{"Years"}.
#' @param years_major A number in years giving the interval axis labels should
#'   be drawn on the vertical axis. Default is \code{100}.
#' @param years_minor An optional number in years giving the interval additional
#'   lines should be drawn to ease orientation in the plot. If not defined they
#'   will be drawn in the middle between the axis labels. If they should not be
#'   plottet, set this argument to the same value like \code{years_major}.
#' @param filename A character string with the filename or path. If specified,
#'   the plot will be saved on the given location. The file format is
#'   automatically recognised from the file extension. The most common file
#'   types are supported, e.g. \code{.tiff}, \code{.png}, \code{.jpg},
#'   \code{.eps}, and \code{.pdf}. To export as \code{.svg} installation of the
#'   package \pkg{svglite} is required. See
#'   \code{\link[ggplot2]{ggsave}} for more details about the supported file
#'   formats.
#' @param plot_dim Dimensions of the plot as a vector in the format
#'   \code{c(width, height, units)}. Supported units are "cm", "mm", in". For
#'   example, \code{plot_dim = c(5,5,"cm")} will produce a plot of the
#'   dimensions 5 x 5 cm. If unspecified, the standard values of the respective
#'   graphic device are used.
#' @param font_size_chrons Font size of the names of the chronological units in
#'   mm. The default is \code{6} mm.
#' @param font_size_labels Font size of the text labels  in mm. The default is
#'   \code{4} mm.
#' @param line_break Line length of the section labels in characters. Text will
#'   be Wrapped at the blank closest to the specified number of characters.
#'   Default is \code{8} characters.
#' @param color_fill Fill colour of the chronological units. The default is
#'   \code{"white"}. See the color specification section of \code{par()} for how
#'   to specify colors in R.
#' @param color_line Line (border) colour of the chronological units. The
#'   default is \code{"black"}. See the color specification section of
#'   \code{par()} for how to specify colors in R.
#' @param size_line Thickness of the line in mm. The default is \code{0.5} mm.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggsave}} to
#'   enhance the saved plot like \code{dpi} to specify its resolution. See
#'   \code{\link[ggplot2]{ggsave}} for detailed information.
#'
#' @return A chronological chart
#'
#' @export
#'
#' @examples
#'

# Changing background, changing theme

plot_chronochrt <- function(data, labels_text,
                            chron_name_x, chron_name_y, chron_name_angle = 0,
                            axis_title = "Years",
                            years_major = 100, years_minor,
                            filename, plot_dim,
                            font_size_chrons = 6, font_size_labels = 4,
                            line_break = 8,
                            color_fill = "white", color_line = "black",
                            size_line = 0.5, ...) #labels_image = NULL, image_size = 0.2,
{
  if (!exists(deparse(substitute(data)))) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (!missing(labels_text) && !exists(deparse(substitute(labels_text)))) {
    stop("The object ", substitute(labels_text) , " does not exist.")
  }

  # if (!missing(labels_image) && !exists(deparse(substitute(labels_image)))) {
  #   stop("The object ", substitute(labels_image) , " does not exist.")
  # }

  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data) , " must be a data frame or tibble.")
  }

  if (!all(is.character(data$region) | is.factor(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  if (!missing(labels_text) && !all(is.character(labels_text$region), is.numeric(labels_text$year), is.character(labels_text$label), is.numeric(labels_text$position))) {
    stop("One or more columns of the text label data contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
  }

  #  if (!missing(labels_image) && !all(is.character(labels_image$region), is.numeric(labels_image$year), is.numeric(labels_image$position))) {
  #    stop("One or more columns of the image label data contain incompatible data. Data must be strings (region), numeric (year, position) or logical (add).")
  #  }


  if (!is.character(axis_title)) {stop("Wrong inout format: ", axis_title, "must be a character string.")}

  if (!is.numeric(font_size_chrons)) {font_size_chrons <- 6}

  if (!is.numeric(font_size_labels)) {font_size_labels <- 4}

  if (!is.numeric(years_major)) {years_major <- 100}

  if (!is.character(color_fill)) {color_fill <- "white"}

  if (!is.character(color_line)) {color_line <- "black"}

  if (!is.numeric(size_line)) {size_line <- 0.5}

  if (!is.numeric(line_break)) {line_break = 8}

  if (!missing(filename) && !is.character(filename)) {stop("Wrong input format: ", filename, "must be a string.")}

  if (!missing(filename) && !dir.exists(dirname(filename))) {stop("The directory ", dirname(filename), " does not exist.")}

  if (!missing(plot_dim)) {
    width <- as.numeric(plot_dim[1])
    height <- as.numeric(plot_dim[2])
    units <- as.character(plot_dim[3])
  }

  if (missing(plot_dim) && !missing(filename)) {
    width <- NA
    height <- NA
    units <- NULL
  }

  if (!(units %in% c("in", "cm", "mm"))) {stop("This unit is not supported. Only the following units are support: mm, cm, in.")}

  if(missing(years_minor) || !is.numeric(years_minor)) {years_minor <- years_major/2}

  if (!missing(chron_name_x)) {

    if (is.numeric(chron_name_x)) {
      dplyr::mutate(data, chron_name_x = chron_name_x)
      }

    if (is.character(chron_name_x) && length(chron_name_x) == 1) {

      if (chron_name_x %in% names(data)) {
        dplyr::rename(data, chron_name_x = !!(as.name(chron_name_x)))
        } else {
          stop("The column ", chron_name_x, " does not exist.")
          }
    }
  }

  if (!all(missing(chron_name_x), is.numeric(chron_name_x), is.character(chron_name_x))) {
    stop("Wrong input format: chron_name_x must be numeric or a character string.")
  }

  if (!missing(chron_name_y)) {

    if (is.numeric(chron_name_y)) {dplyr::mutate(data, chron_name_y = chron_name_y)}

    if (is.character(chron_name_y) && length(chron_name_y) == 1) {

      if (chron_name_y %in% names(data)) {
        dplyr::rename(data, chron_name_y = !!(as.name(chron_name_y)))
      } else {
        stop("The column ", chron_name_y, " does not exist.")
      }
    }
  }

  if (!all(missing(chron_name_y), is.numeric(chron_name_y), is.character(chron_name_y))) {
    stop("Wrong input format: chron_name_y must be numeric or a character string.")
  }

  if (is.numeric(chron_name_angle)) {dplyr::mutate(data, chron_name_angle = chron_name_angle)}

  if (is.character(chron_name_angle) && length(chron_name_angle) == 1) {
    if (chron_name_angle %in% names(data)) {
      dplyr::rename(data, chron_name_angle = !!(as.name(chron_name_angle)))
      } else {
        stop("The column ", chron_name_angle, " does not exist.")
      }
    }

  if (!is.numeric(chron_name_angle) && !is.character(chron_name_angle)) {
    stop("Wrong input format: chron_name_angle must be numeric or a character string.")
  }

  data <- data %>% # calculation of geometry
    dplyr::group_by(.data$region, .data$add) %>%
    tidyr::separate(.data$start, c("start", "start2"), sep = "/", fill = "right") %>%
    tidyr::separate(.data$end, c("end", "end2"), sep = "/", fill = "right") %>%
    dplyr::mutate_at(c("start", "start2", "end", "end2"), as.numeric) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::mutate(subchron = subchron_count(.data$start, .data$end)) %>%
    dplyr::mutate(col_tot = dplyr::case_when(.data$level > subchron ~ .data$level,
                                             .data$level == subchron ~ .data$level + 1,
                                             .data$level < subchron ~ .data$level + subchron)) %>%
    dplyr::mutate(x_center = (.data$level - 0.5) / .data$col_tot,
                  x_width = 1 / .data$col_tot,
                  y_center = (.data$start + .data$end) / 2,
                  y_width = .data$end - .data$start) %>%
    dplyr::mutate(x_center_corr = center_corr(.data$x_center, .data$x_width),
                  x_width_corr = width_corr(.data$x_center, .data$x_width)
    ) %>%
    dplyr::mutate(x_center = .data$x_center_corr,
                  x_width = .data$x_width_corr) %>%
    dplyr::select(-.data$x_center_corr, -.data$x_width_corr) %>%
    dplyr::mutate(x_center = replace(.data$x_center, .data$add == TRUE, .data$x_center + 1))

  if (missing(chron_name_x)) {dplyr::mutate(data, chron_name_x = data$x_center)}
  if (missing(chron_name_y)) {dplyr::mutate(data, chron_name_y = data$y_center)}

  plot <- ggplot2::ggplot(data) + # plot
    ggplot2::geom_tile(ggplot2::aes(x = .data$x_center, width = .data$x_width, y = .data$y_center, height = .data$y_width), fill = color_fill, color = color_line, linetype = "solid", size = size_line) +
    ggplot2::geom_text(ggplot2::aes(x = .data$chron_name_x, y = .data$chron_name_y, label = .data$name, angle = .data$chron_name_angle), size = font_size_chrons)

  if ("start2" %in% colnames(data)) {
    plot <- plot +
      ggplot2::geom_segment(data = data %>% dplyr::filter(!is.na(.data$start2)), ggplot2::aes(x = .data$x_center - .data$x_width/2, xend = .data$x_center + .data$x_width/2, y = .data$start, yend = .data$start), color = color_fill, linetype = "dashed", size = size_line) +
      ggplot2::geom_segment(data = data %>% dplyr::filter(!is.na(.data$start2)), ggplot2::aes(x = .data$x_center - .data$x_width/2, xend = .data$x_center + .data$x_width/2, y = .data$start2, yend = .data$start2), color = color_line, linetype = "dashed", size = size_line)
  }

  if ("end2" %in% colnames(data)) {
    plot <- plot +
      ggplot2::geom_segment(data = data %>% dplyr::filter(!is.na(.data$end2)), ggplot2::aes(x = .data$x_center - .data$x_width/2, xend = .data$x_center + .data$x_width/2, y = .data$end, yend = .data$end), color = color_fill, linetype = "dashed", size = size_line) +
      ggplot2::geom_segment(data = data %>% dplyr::filter(!is.na(.data$end2)), ggplot2::aes(x = .data$x_center - .data$x_width/2, xend = .data$x_center + .data$x_width/2, y = .data$end2, yend = .data$end2), color = color_line, linetype = "dashed", size = size_line)
  }

  if(!missing(labels_text)) {
    if (is.factor(data$region)) {
      labels_text <- dplyr::mutate(labels_text, region = factor(.data$region, levels = levels(data$region)))
    }

    plot <- plot +
      ggplot2::geom_text(data = labels_text, ggplot2::aes(y = .data$year, x = .data$position, label = .data$label, hjust = 1, vjust = 0.5), na.rm = TRUE, size = font_size_labels)
  }

  # if(!missing(labels_image)) {
  #   if (is.factor(data$region)) {
  #     labels_text <- mutate(labels_text, region = factor(.data$region, levels = levels(data$region)))
  #     }
  #
  #   plot <- plot +
  #     ggimage::geom_image(data = labels_image, ggplot2::aes(y = .data$year, x = .data$position, image = .data$image_path), na.rm = TRUE, size = image_size, asp = 4)
  #   }

  plot <- plot +
    ggplot2::scale_x_continuous(name = NULL, breaks = NULL, minor_breaks = NULL, expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = axis_title, breaks = round(seq(min(data$start), max(data$end), by = years_major),1), minor_breaks = round(seq(min(data$start), max(data$end), by = years_minor),1), expand = c(0,0)) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$region), scales = "free_x", space = "free_x", labeller = ggplot2::label_wrap_gen(width = line_break)) +
    theme_chronochrt() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = font_size_chrons*0.8*72.27/25.4),
          axis.title = ggplot2::element_text(size = font_size_chrons*72.27/25.4, face="bold"),
          strip.text.x = ggplot2::element_text(size = font_size_chrons*72.27/25.4, face="bold")) #+
  #labs(caption = "Citation of the release paper")

  if(!missing(filename)) {
    plot <- plot +
      ggplot2::ggsave(filename = filename, width = width, height = height, units = units, ...)
  }


  plot
}
