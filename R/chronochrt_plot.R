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
#' @param chron_name_align One of the two character strings: \code{"left"} or
#'   \code{"center"}, the default. They determine the alignment of the
#'   chronological unit's names relativ to its border. \itemize{ \item
#'   \code{"center"}: Text alignment of the names will be centered. The text
#'   will be placed in a larger distance from the left border of its
#'   chronological unit. \item \code{"left"}: The names will be left-aligned
#'   next to the left borders of their chronological unit.} In both cases, the
#'   horizontal position of the chronological units' names will be identical and
#'   their vertical position always the middle of their chronological units.
#' @param chron_name_x,chron_name_y Specifies the horizontal and vertical
#'   position of the chronological units' names. They can be: \itemize{ \item a
#'   character string with the name of the respective column in \code{data},
#'   \item a number. Then all names will plot at this position, \item a numeric
#'   vector specifiying the position of each name; \item it must not be
#'   specified if a column with the name "chron_name_x" already exists in
#'   \code{data}; \item if not specified and not present in \code{data}, names
#'   are placed in the centers of the chonological units. }
#' @param chron_name_angle Specifies the angle of the chronological unit's
#'   names. It can be: \itemize{ \item a character string with the name of the
#'   respective column in \code{data}, \item a number. Then all names are placed
#'   in the same angle, \item a numeric vector specifiying the angle of each
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
#'   plotted, set this argument to the same value like \code{years_major}.
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
#' @param background Optional specifications for the background of the
#'   chronological chart as vector in the format \code{c(background color,
#'   linetype of grid lines)} to overwrite the default behaviour of
#'   \code{\link{theme_chronochrt}}. Any valis color and linetype specifications
#'   are accepted, e.g. \code{c("grey90", "dotted")} (these are the default
#'   values of \code{\link{theme_chronochrt}}. See the sections "color
#'   specification" and "line type specification" in \code{par()} for how to
#'   specify colors and line types in R.
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
                            chron_name_align = "center",
                            chron_name_x, chron_name_y, chron_name_angle = 0,
                            axis_title = "Years",
                            years_major = 100, years_minor,
                            filename, plot_dim,
                            font_size_chrons = 6, font_size_labels = 4,
                            line_break = 8,
                            color_fill = "white", color_line = "black",
                            size_line = 0.5,
                            background, ...) #labels_image = NULL, image_size = 0.2,
{

  if (!is.data.frame(data)) {
      stop("Wrong input format: ", substitute(data) , " must be a data frame or tibble.")
    }

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

  if (!missing(labels_text)) {
    if (is.data.frame(labels_text)) {
      if (!("region" %in% names(labels_text))) {stop("Wrong input format: The column `region` in ", substitute(labels_text), " does not exist.")}
      if (!("year" %in% names(labels_text))) {stop("Wrong input format: The column `year` in ", substitute(labels_text), " does not exist.")}
      if (!("label" %in% names(labels_text))) {stop("Wrong input format: The column `label` in ", substitute(labels_text), " does not exist.")}
      if (!("position" %in% names(labels_text))) {stop("Wrong input format: The column `position` in ", substitute(labels_text), " does not exist.")}

      if (!all(is.character(labels_text$region), is.numeric(labels_text$year), is.character(labels_text$label), is.numeric(labels_text$position))) {
        stop("One or more columns of the text label data contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
      }
    } else {
      stop("Wrong input format: ", substitute(data) , " must be a data frame or tibble.")
    }
  }

  # if (!missing(labels_image)) {
  #   if (is.data.frame(labels_image)) {
  #     if (!("region" %in% names(labels_image))) {stop("Wrong input format: The column `region` in ", substitute(labels_text), " does not exist.")}
  #     if (!("year" %in% names(labels_image))) {stop("Wrong input format: The column `year` in ", substitute(labels_text), " does not exist.")}
  #     if (!("position" %in% names(labels_image))) {stop("Wrong input format: The column `position` in ", substitute(labels_text), " does not exist.")}
  #
  #     if (!all(is.character(labels_text$region), is.numeric(labels_text$year), is.character(labels_text$label), is.numeric(labels_text$position))) {
  #       stop("One or more columns of the image label data contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
  #     }
  #   } else {
  #     stop("Wrong input format: ", substitute(data) , " must be a data frame or tibble.")
  #   }
  # }

  if (!is.character(axis_title)) {stop("Wrong input format: ", axis_title, " must be a character string.")}

  if (!is.numeric(font_size_chrons) && length(font_size_chrons) != 1) {font_size_chrons <- 6}

  if (!is.numeric(font_size_labels)&& length(font_size_labels) != 1) {font_size_labels <- 4}

  if (!is.numeric(years_major)&& length(years_major) != 1) {years_major <- 100}

  if (!is.character(color_fill)&& length(color_fill) != 1) {color_fill <- "white"}

  if (!is.character(color_line)&& length(color_line) != 1) {color_line <- "black"}

  if (!is.numeric(size_line)&& length(size_line) != 1) {size_line <- 0.5}

  if (!is.numeric(line_break)&& length(line_break) != 1) {line_break = 8}

  if (!missing(filename)) {
    if (!is.character(filename)) {stop("Wrong input format: ", filename, " must be a character string.")}
    if (!dir.exists(dirname(filename))) {stop("The directory ", dirname(filename), " does not exist.")}
    if (missing(plot_dim)) {
      width <- NA
      height <- NA
      units <- NULL
    }
  }

  if (!missing(plot_dim)) {
    width <- as.numeric(plot_dim[1])
    height <- as.numeric(plot_dim[2])
    units <- as.character(plot_dim[3])
  }

  if (!missing(plot_dim) && !(units %in% c("in", "cm", "mm"))) {stop("This unit is not supported. Only the following units are support: mm, cm, in.")}

  if (missing(years_minor) || !is.numeric(years_minor)) {years_minor <- years_major/2}

  if(!missing(background)) {
    if (length(background) != 2) {stop("Wrong input format:", background, " is not a vector of length 2.")}
    bg_fill <- background[1]
    grid_linetype <- background[2]
  }

  if (!(chron_name_align %in% c("left", "center"))) {stop("Wrong input format: ", chron_name_align, " must be 'left' or 'center'.")}

   if (!missing(chron_name_x)) {

    if (is.numeric(chron_name_x)) {
      data <- dplyr::mutate(data, chron_name_x = chron_name_x)
      }

    if (is.character(chron_name_x) && length(chron_name_x) == 1) {

      if (chron_name_x %in% names(data)) {
        data <- dplyr::rename(data, chron_name_x = !!(as.name(chron_name_x)))
        } else {
          stop("The column ", chron_name_x, " does not exist.")
          }
    } else {
      stop("Wrong input format: chron_name_x must be numeric or a character string.")
    }
  }

  if (!missing(chron_name_y)) {

    if (is.numeric(chron_name_y)) {
      data <- dplyr::mutate(data, chron_name_y = chron_name_y)
    }

    if (is.character(chron_name_y) && length(chron_name_y) == 1) {

      if (chron_name_y %in% names(data)) {
        data <- dplyr::rename(data, chron_name_y = !!(as.name(chron_name_y)))
      } else {
        stop("The column ", chron_name_y, " does not exist.")
      }
    } else {
      stop("Wrong input format: chron_name_y must be numeric or a character string.")
    }
  }

   if (is.numeric(chron_name_angle)) {data <- dplyr::mutate(data, chron_name_angle = chron_name_angle)}

   if (is.character(chron_name_angle)) {
     if (length(chron_name_angle) == 1) {
       if (chron_name_angle %in% names(data)) {
         data <- dplyr::rename(data, chron_name_angle = !!(as.name(chron_name_angle)))
         } else {
           stop("The column ", chron_name_angle, " does not exist.")
         }
       } else {
         stop("Wrong input format: chron_name_angle must be a character string.")
       }
   }

   if (!is.numeric(chron_name_angle) && !is.character(chron_name_angle)) {
     stop("Wrong input format: chron_name_angle must be numeric or a character string.")
   }

  data <- data %>% # calculation of geometry, commented lines for implementation with lubridate
    tidyr::separate(.data$start, c("start", "start2"), sep = "/", fill = "right") %>%
    tidyr::separate(.data$end, c("end", "end2"), sep = "/", fill = "right") %>%
    dplyr::mutate_at(c("start", "start2", "end", "end2"), as.numeric) %>%
#    dplyr::mutate(across(starts_with("start") | starts_with("end"), as.numeric)) %>%

    #dplyr::mutate(start = lubridate::ymd("0000-01-01") + lubridate::years(.data$start),
    #              start2 = lubridate::ymd("0000-01-01") + lubridate::years(.data$start2),
    #              end = lubridate::ymd("0000-01-01") + lubridate::years(.data$end),
    #              end2 = lubridate::ymd("0000-01-01") + lubridate::years(.data$end2)) %>%
    #dplyr::mutate(interval = lubridate::interval(.data$start + lubridate::days(1), .data$end - lubridate::days(1))) %>%

    dplyr::group_by(.data$region, .data$add) %>%
    dplyr::mutate(xmin = (.data$level - 1) / max(.data$level),
                  xmax = .data$level / max(.data$level)) %>%
    dplyr::mutate(x_name = .data$xmin + ((.data$xmax - .data$xmin) / 2),
                  y_name = .data$start + ((.data$end - .data$start) / 2)) %>%

    #dplyr::mutate(xmax_uncorr = corr_xmax(.data$interval, .data$xmax)) %>%

    dplyr::mutate(xmax_uncorr = corr_xmax(.data$start, .data$end, .data$xmax)) %>%
    dplyr::mutate(xmax = dplyr::if_else(.data$xmax == .data$xmax_uncorr, 1, .data$xmax)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$xmax_uncorr) %>%
    dplyr::mutate(xmax = dplyr::if_else(.data$add == TRUE, .data$xmax + 1, .data$xmax),
                  xmin = dplyr::if_else(.data$add == TRUE, .data$xmin + 1, .data$xmin),
                  x_name = dplyr::if_else(.data$add == TRUE, .data$x_name + 1, .data$x_name),
                  y_name = dplyr::if_else(.data$add == TRUE, .data$y_name + 1, .data$y_name))

  if (chron_name_align == "left") {data <- dplyr::mutate(data, x_name = .data$xmin + 0.01)}

  if (missing(chron_name_x)) {data <- dplyr::mutate(data, chron_name_x = .data$x_name)}
  if (missing(chron_name_y)) {data <- dplyr::mutate(data, chron_name_y = .data$y_name)}

  plot <- ggplot2::ggplot(data) + # plot
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$start, ymax = .data$end), fill = color_fill, color = color_line, linetype = "solid", size = size_line) +
    ggplot2::geom_text(ggplot2::aes(x = .data$chron_name_x, y = .data$chron_name_y, label = .data$name, angle = .data$chron_name_angle), size = font_size_chrons, hjust = chron_name_align)

  if (sum(!is.na(data$start2)) > 0 || sum(!is.na(data$end2)) > 0) {

    data_unsec <- data %>%
      dplyr::filter(!is.na(.data$start2) | !is.na(.data$end2)) %>%
      dplyr::mutate(start = ifelse(is.na(.data$start2), NA, .data$start),
                    end = ifelse(is.na(.data$end2), NA, .data$end)) %>%
      tidyr::pivot_longer(cols = c("start", "end"), names_to = "value", values_to = "unsec") %>%
      tidyr::pivot_longer(cols = c("start2", "end2"), names_to = "value2", values_to = "unsec2") %>%
      dplyr::filter(!is.na(.data$unsec) | !is.na(.data$unsec2)) %>%
      dplyr::group_by(.data$region, .data$add, .data$unsec)%>%
      dplyr::mutate(xmin = min(.data$xmin),
                    xmax = max(.data$xmax)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$region, .data$add, .data$unsec2)%>%
      dplyr::mutate(xmin = min(.data$xmin),
                    xmax = max(.data$xmax)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$xmin, .data$xmax, .data$unsec, .data$unsec2) %>%
      unique()

    plot <- plot +
      ggplot2::geom_segment(data = data_unsec %>%
                              dplyr::select(-.data$unsec2) %>%
                              tidyr::drop_na() %>%
                              unique(),
                            ggplot2::aes(x = .data$xmin, xend = .data$xmax, y = .data$unsec, yend = .data$unsec), color = color_fill, linetype = "dashed", size = size_line) +
      ggplot2::geom_segment(data = data_unsec %>%
                              dplyr::select(-.data$unsec) %>%
                              tidyr::drop_na() %>%
                              unique(),
                            ggplot2::aes(x = .data$xmin, xend = .data$xmax, y = .data$unsec2, yend = .data$unsec2), color = color_line, linetype = "dashed", size = size_line)
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

  if(!missing(background)) {
    plot <- plot +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_fill),
                     panel.grid = ggplot2::element_line(linetype = grid_linetype))
  }



  if(!missing(filename)) {
    plot <- plot +
      ggplot2::ggsave(filename = filename, width = width, height = height, units = units, ...)
  }


  plot
}
