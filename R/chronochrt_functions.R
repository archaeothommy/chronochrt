library(tidyverse)
library(readxl)
library(ggimage) # needed only to plot image labels

# example/playground ---------------------------
z <- tibble(test = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C", "B", "B", "C", "C", "C", "C", "A", "A", "A"),
            test2 = c("Test 1", "Test 2b", "Test 2", "Test 2a", "Test 6a", "Test 6b", "Test 6c", "Test 6d", "Test 3", "Test 1", "Test 2", "Test 3", "Test 3", "Test 4", "Test 5", "Test 1", "Test 2", "Test 4", "Test 5", "Test 6", "Test 2a1", "Test 2a2"),
            test3 = c(-2500, "-750/-700", -1500, -1500, -400, -350, -150, 100, -200, -3000, -2000, -1500, -750, -700, -150, -2000, -1500, 0, 150, -400, -1500, -1000),
            test4 = c(-1500, "-200/-150", -200, -750, -350, -250, 100, 300, 500, -2000, -1500, -700, 0, -150, 100, -1500, -750, 150, 300, 300, -1000, -750),
            test5 = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
            test6 = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))

z <- convert_to_chron(z, region = "test", name = "test2", start = "test3", end = "test4", level = "test5", add = "test6") %>%
  add_chron(., c("D"), c("Test 1", "Test 2", "Test 3", "Test 3a", "Test 3b", "Test 4"), c(100, 200, 300, 300, 400, 500), c(200, 300, 500, 400, 500, 600), c(1, 1, 1, 2, 2, 1))



plot_chronochrt(z)

xy <- add_chron(xy,
                c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C", "B", "B", "C", "C", "C", "C", "A", "A", "A"),
                c("Test 1", "Test 2b", "Test 2", "Test 2a", "Test 6a", "Test 6b", "Test 6c", "Test 6d", "Test 3", "Test 1", "Test 2", "Test 3", "Test 3", "Test 4", "Test 5", "Test 1", "Test 2", "Test 4", "Test 5", "Test 6", "Test 2a1", "Test 2a2"),
                c(-2500, "-750/-700", -1500, -1500, -400, -350, -150, 100, -200, -3000, -2000, -1500, -750, -700, -150, -2000, -1500, 0, 150, -400, -1500, -1000),
                c(-1500, "-200/-150", -200, -750, -350, -250, 100, 300, 500, -2000, -1500, -700, 0, -150, 100, -1500, -750, 150, 300, 300, -1000, -750),
                c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                new_table = TRUE) %>%
    add_chron(., c("D"), c("Test 1", "Test 2", "Test 3", "Test 3a", "Test 3b", "Test 4"), c(100, 200, 300, 300, 400, 500), c(200, 300, 500, 400, 500, 600), c(1, 1, 1, 2, 2, 1))


order <- c("D", "B", "A", "C")

xy <- arrange_regions(xy, region = "region", order = order)

labels <- add_label_text(labels,
                         c("A", "B"),
                         c(-900, 0),
                         c(1.5, 0.7),
                         c("LAB\nEL1", "LABEL2"),
                         add = FALSE) %>%
  add_label_text(., c("C", "A"), c(-500, -1000), 0.9, c("#", "LABEL4"))

 images <- add_label_image(images,
                         c("D", "B"),
                         c(250, 250),
                         0.5,
                         c("G:/Vignette Erz.png", "https://www.r-project.org/logo/Rlogo.png"),
                         add = FALSE) %>%
   add_label_image(., c("B", "A"), c(-500, -2250), 0.8, c("https://www.r-project.org/logo/Rlogo.png", "G:/Projekte/Cu-Isotope_Schmelz-Fraktionierung_FRA_DBM/Schmelzversuche_Mayen/Logos/dfg_logo_englisch_blau_en.jpg"))

plot_chronochrt(z, axis_title = "Jahr", labels_text = zzz, years_major = 200.5, years_minor = 115, color_fill = "yellow", color_line = "lightgreen", size_line = 2, height = 21, width = 29.7, units = "cm", path = "Test_ugly.jpg") #labels_image = images, image_size = c(.5,.2,.2,.25),

# Converting to chron -----------------------------------------------------
convert_to_chron <- function(data, region, name, start, end, level, add, ...)
{
  if (!exists(deparse(substitute(data)))) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or tibble.")
  }

  data <- rename(data, region = (!!as.name(region)), name = (!!as.name(name)), start = (!!as.name(start)), end = (!!as.name(end)), level = (!!as.name(level)), add = (!!as.name(add)), ...)

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  data
}

# Arrange regions ---------------------------------------------------------

arrange_regions <- function(data, region, order)
{
  if (!exists(deparse(substitute(data)))) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (!exists(deparse(substitute(order)))) {
    stop("The object ", substitute(order) , " does not exist.")
  }

  if (!is.data.frame(data)) {
    stop("Wrong input format: ", substitute(data), " must be a data frame or tibble.")
  }

  if (!is.character(region)) {
    stop("Incompatible input format: ", substitute(region), " must be a character string.")
  }

  if (!is.character(order) && !is.vector(order)) {
    stop("Incompatible input format: ", substitute(order), " must be a vector of unique character strings.")
  }

  data <- mutate(data, !!region := factor(!!(as.name(region)), levels = order))

  data
}



# Make new chronological unit ---------------------------------------------

add_chron <- function(data, region, name, start, end, level = 1, add = FALSE, new_table = FALSE, ...)
{
  if (!exists(deparse(substitute(data))) && new_table == FALSE) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (new_table == TRUE) {data <- tibble::tibble(region, name, start, end, level, add, ...)}
  if (new_table == FALSE) {data <- dplyr::add_row(data, region , name, start, end, level, add, ...)}

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start) | is.character(data$start), is.numeric(data$end) | is.character(data$end), is.numeric(data$level), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(round(data$level) == data$level)) {
    stop("Wrong input format: level must contain only whole numbers (1, 2, 3, ...)")
  }

  data
}

# Label input  ------------------------------------------------------------

  # add text labels

add_label_text <- function(data, region, year, position, label, add = FALSE, ...)
{
  if (!exists(deparse(substitute(data))) && add == TRUE) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (add == FALSE) {data <- tibble::tibble(region, year, position, label, ...)}
  if (add == TRUE) {data <- dplyr::add_row(data, region, year, position, label, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.character(data$label), is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
  }

  data
}

  # add image labels

add_label_image <- function (data, region, year, position = 0.9, image_path, add = FALSE, ...)
{
  if (!exists(deparse(substitute(data))) && add == TRUE) {
    stop("The object ", substitute(data) , " does not exist.")
  }

  if (add == FALSE) {data <- tibble::tibble(region, year, position, image_path, ...)}
  if (add == TRUE) {data <- dplyr::add_row(data, region, year, position, image_path, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.numeric(data$position))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region), numeric (year, position) or logical (add).")
  }

  data
}

# Plot chart --------------------------------------------------------------------

  # all text labels are right-aligned to the given x co-ordinate to avoid running out of bounds
  # all image labels are scaled to uniform height
  # fontsize input is in mm

# aspect ratio of images must be corrected

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


# ChronochRt theme --------------------------------------------------------
    # based on theme_grey, basic theme in ggplot2
theme_chronochrt <- function (base_size = 11, base_family = "", base_line_size = base_size / 22, base_rect_size = base_size / 22)
  {
    theme_grey(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size
    ) %+replace%
      theme(
        panel.background = element_rect(fill = "grey85", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.grid = element_line(colour = "grey50"),
        panel.grid.minor = element_line(size = rel(0.5)),
        panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white", colour = NA),
        plot.caption = element_text(vjust = 9, hjust = 1),
        complete = TRUE
      )
  }


# Helper functions ---------------------------------------------------------

subchron_count <- function(left, right) # counts subchrons
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

center_corr <- function(center, width) # corrects center if there are missing subunits in other subchrons
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

width_corr <- function(center, width) # corrects width if there are missing subunits in other subchrons
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
