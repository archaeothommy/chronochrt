# all text labels are right-aligned to the given x co-ordinate to avoid running out of bounds
# all image labels are scaled to uniform height
# fontsize input is in mm

# aspect ratio of images must be corrected

#' Title
#'
#' @param data
#' @param axis_title
#' @param labels_text
#' @param font_size_chrons
#' @param font_size_labels
#' @param years_major
#' @param years_minor
#' @param path
#' @param width
#' @param height
#' @param units
#' @param chron_title_x
#' @param chron_title_y
#' @param line_break
#' @param color_fill
#' @param color_line
#' @param size_line
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' @importFrom magrittr %>%

plot_chronochrt <- function(data, axis_title = "Years", labels_text, font_size_chrons = 6, font_size_labels = 4, years_major = 100, years_minor, path = NULL, width, height, units, chron_title_x, chron_title_y, line_break = 8, color_fill = "white", color_line = "black", size_line = 0.5, ...) #labels_image = NULL, image_size = 0.2,
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

  if (!is.character(axis_title)) {
    stop("Wrong inout format: ", axis_title, "must be a character string.")
  }

  if (!is.numeric(font_size_chrons)) {
    font_size_chrons <- 6
  }

  if (!is.numeric(font_size_labels)) {
    font_size_labels <- 4
  }

  if (!is.numeric(years_major)) {
    years_major <- 100
  }

  if (!is_character(color_fill)) {
    color_fill <- "white"
  }

  if (!is.character(color_line)) {
    color_line <- "black"
  }

  if (!is.numeric(size_line)) {
    size_line <- 0.5
  }

  if (!is.numeric(line_break)) {
    line_break = 8
  }

  if (!missing(path) && !is.character(path)) {
    stop("Wrong inout format: ", path, "must be a string.")
  }

  if (!missing(path) && !dir.exists(dirname(path))) {
    stop("The directory ", getwd(), "/", dirname(path), " does not exist.")
  }

  if (!missing(width) && !is.numeric(width)) {
    width <- NA
  }

  if (!missing(height) && !is.numeric(height)) {
    height <- NA
  }

  if (!missing(units)) {
    if (!is.character(units)) {
      stop("Units must be a string. The following units are support: mm, cm, in.")
    }

    if (!(units %in% c("in", "cm", "mm"))) {
      stop("This unit is not supported. Only the following units are support: mm, cm, in.")
    }
  }

  if(missing(years_minor) || !is.numeric(years_minor)) {
    years_minor <- years_major/2
  }

  data <- data %>% # calculation of geometry
    dplyr::group_by(region, add) %>%
    tidyr::separate(start, c("start", "start2"), sep = "/", fill = "right") %>%
    tidyr::separate(end, c("end", "end2"), sep = "/", fill = "right") %>%
    dplyr::mutate_at(c("start", "start2", "end", "end2"), as.numeric) %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::mutate(subchron = subchron_count(start, end)) %>%
    dplyr::mutate(col_tot = dplyr::case_when(level > subchron ~ level,
                                             level == subchron ~ level + 1,
                                             level < subchron ~ level + subchron)) %>%
    dplyr::mutate(x_center = (level - 0.5) / col_tot,
                  x_width = 1 / col_tot,
                  y_center = (start + end) / 2,
                  y_width = end - start) %>%
    dplyr::mutate(x_center_corr = center_corr(x_center, x_width),
                  x_width_corr = width_corr(x_center, x_width)
    ) %>%
    dplyr::mutate(x_center = x_center_corr,
                  x_width = x_width_corr) %>%
    dplyr::select(-x_center_corr, -x_width_corr) %>%
    dplyr::mutate(x_center = replace(x_center, add == TRUE, x_center + 1))

  if (missing(chron_title_x)) {chron_title_x <- data$x_center} # if name of chrons should be placed different than in their center
  if (missing(chron_title_y)) {chron_title_y <- data$y_center}


  plot <- ggplot2::ggplot(data) + # plot
    ggplot2::geom_tile(aes(x = x_center, width = x_width, y = y_center, height = y_width), fill = color_fill, color = color_line, linetype = "solid", size = size_line) +
    ggplot2::geom_text(aes(x = chron_title_x, y = chron_title_y, label = name), size = font_size_chrons)

  if ("start2" %in% colnames(data)) {
    plot <- plot +
      ggplot2::geom_segment(data = data %>% filter(!is.na(start2)), aes(x = x_center - x_width/2, xend = x_center + x_width/2, y = start, yend = start), color = color_fill, linetype = "dashed", size = size_line) +
      ggplot2::geom_segment(data = data %>% filter(!is.na(start2)), aes(x = x_center - x_width/2, xend = x_center + x_width/2, y = start2, yend = start2), color = color_line, linetype = "dashed", size = size_line)
  }

  if ("end2" %in% colnames(data)) {
    plot <- plot +
      ggplot2::geom_segment(data = data %>% filter(!is.na(end2)), aes(x = x_center - x_width/2, xend = x_center + x_width/2, y = end, yend = end), color = color_fill, linetype = "dashed", size = size_line) +
      ggplot2::geom_segment(data = data %>% filter(!is.na(end2)), aes(x = x_center - x_width/2, xend = x_center + x_width/2, y = end2, yend = end2), color = color_line, linetype = "dashed", size = size_line)
  }

  if(!missing(labels_text)) {
    if (is.factor(data$region)) {
      labels_text <- mutate(labels_text, region = factor(region, levels = levels(data$region)))
    }

    plot <- plot +
      ggplot2::geom_text(data = labels_text, aes(y=year, x = position, label = label, hjust = 1, vjust = 0.5), na.rm = TRUE, size = font_size_labels)
  }

  # if(!missing(labels_image)) {
  #   if (is.factor(data$region)) {
  #     labels_text <- mutate(labels_text, region = factor(region, levels = levels(data$region)))
  #     }
  #
  #   plot <- plot +
  #     ggimage::geom_image(data = labels_image, aes(y=year, x = position, image = image_path), na.rm = TRUE, size = image_size, asp = 4)
  #   }

  plot <- plot +
    ggplot2::scale_x_continuous(name = NULL, breaks = NULL, minor_breaks = NULL, expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = axis_title, breaks = round(seq(min(data$start), max(data$end), by = years_major),1), minor_breaks = round(seq(min(data$start), max(data$end), by = years_minor),1), expand = c(0,0)) +
    ggplot2::facet_grid(cols = vars(region), scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = line_break)) +
    theme_chronochrt() +
    theme(axis.text=element_text(size = font_size_chrons*0.8*72.27/25.4),
          axis.title=element_text(size = font_size_chrons*72.27/25.4, face="bold"),
          strip.text.x = element_text(size = font_size_chrons*72.27/25.4, face="bold")) #+
  #labs(caption = "Citation of the release paper")

  if(!missing(path)) {
    plot <- plot +
      ggplot2::ggsave(filename = path, width = width, height = height, units = units, ...)
  }


  plot
}

