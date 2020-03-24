library(tidyverse)
library(readxl)
library(ggimage) # needed only to plot image labels

# example/playground ---------------------------
xy <- add_chron(xy,
                c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C", "B", "B", "C", "C", "C", "C", "A", "A", "A"),
                c("Test 1", "Test 2b", "Test 2", "Test 2a", "Test 6a", "Test 6b", "Test 6c", "Test 6d", "Test 3", "Test 1", "Test 2", "Test 3", "Test 3", "Test 4", "Test 5", "Test 1", "Test 2", "Test 4", "Test 5", "Test 6", "Test 2a1", "Test 2a2"),
                c(-2500, -750, -1500, -1500, -400, -350, -150, 100, -200, -3000, -2000, -1500, -750, -700, -150, -2000, -1500, 0, 150, -400, -1500, -1000),
                c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, -2000, -1500, -700, 0, -150, 100, -1500, -750, 150, 300, 300, -1000, -750),
                c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                new_table = TRUE) %>%
    add_chron(., c("D"), c("Test 1", "Test 2", "Test 3", "Test 3a", "Test 3b", "Test 4"), c(100, 200, 300, 300, 400, 500), c(200, 300, 500, 400, 500, 600), c(1, 1, 1, 2, 2, 1))

labels <- add_text_label(labels,
                         c("A", "B"),
                         c(-900, 0),
                         0.7,
                         c("LAB \n EL1", "LABEL2"),
                         add = FALSE) %>%
  add_text_label(., c("C", "A"), c(-500, -1000), 0.9, c("#", "LABEL4"))

# images <- add_image_label(images,
#                         c("A", "B"),
 #                        c(-2250, 250),
  #                       0.5,
   #                      c("C:/Dokumente/Forschung/Vignette Erz.png", "https://www.r-project.org/logo/Rlogo.png"),
    #                     add = FALSE) %>%
  # add_image_label(., c("B", "D"), c(-500, 250), 0.8, c("https://www.r-project.org/logo/Rlogo.png", "C:/Dokumente/Forschung/Projekte/Cu-Isotope_Schmelz-Fraktionierung_FRA_DBM/Schmelzversuche_Mayen/Logos/dfg_logo_englisch_blau_en.jpg"))

plot_chronochrt(xy, year = "Jahr", labels_text = labels, years_major = 250, breaks_minor = 5, path = "Test.jpg", height = 29.7, width = 21, units = "cm")

#

tibble::tribble()

# Make new chronological unit ---------------------------------------------

  # makes a tibble or add a row to a tibble with the columns region, name, start, end, kind,
  # allows input of single values or vectors
  # works only with the given naming scheme, for other ones use regular dplyr-functions (add_row)

  # to implement: telling error messages
  # could probably be enhanced with missing-function to avoid input of data argument "new_table" analogous to plot-switches

add_chron <- function(data, region, name, start, end, level = 1, add = FALSE, new_table = FALSE, ...)
  {

  if (new_table == TRUE) {data <- tibble::tibble(region, name, start, end, level, add, ...)}
  if (new_table == FALSE) {data <- dplyr::add_row(data, region, name, start, end, level, add, ...)}

  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start), is.numeric(data$end), round(data$level) == data$level, is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  data
}

# Label input  ------------------------------------------------------------

  # add text labels

    # works analogous to add_chron
    # could probably enhanced with missing-function to avoid input of data argument "new_table" analogous to plot-switches

add_text_label <- function(data, region, year, position, label, add = TRUE, ...)
{
  if (add == FALSE) {data <- tibble::tibble(region, year, position, label, ...)}
  if (add == TRUE) {data <- dplyr::add_row(data, region, year, position, label, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.character(data$label), is.numeric(data$year), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
  }

  data
}

  # add image labels

    #works analogous to add_text_label

add_image_label <- function (data, region, year, position = 0.9, image_path, add = TRUE, ...)
{
  #if (!is.na(check_format())) {stop(check_format())} + file.exists
  if (add == FALSE) {data <- tibble::tibble(region, year, position, image_path, ...)}
  if (add == TRUE) {data <- dplyr::add_row(data, region, year, position, image_path, ...)}

  if (!all(is.character(data$region), is.numeric(data$year), is.numeric(data$position), is.logical(data$add))) {
    stop("One or more columns of the data set contain incompatible data. Data must be strings (region), numeric (year, position) or logical (add).")
  }

  if (!file.exists(data$image_path)) {
    stop("The path(s) of one or more files are not correct or the files do not exist.")
  }

  data
}

# Plot chart --------------------------------------------------------------------

  # all text labels are right-aligned to the given x co-ordinate to avoid running out of bounds
  # all image labels are scaled to uniform height
  # fontsize input is in mm (?)

# aspect ratio of images must be corrected

plot_chronochrt <- function(data, year = "years", labels_text = NULL, labels_image = NULL, font_size_chrons = 6, font_size_labels = 4, years_major = 100, breaks_minor = 1, path = NULL, width, height, units, chron_title_x = NULL, chron_title_y = NULL, line_break = 8, ...)
{
  if (!all(is.character(data$region), is.character(data$name), is.numeric(data$start), is.numeric(data$end), round(data$level) == data$level, is.logical(data$add))) {
    stop("One or more columns of the chronological data incompatible data. Data must be strings (region, name), numbers (start, end), whole numbers (level), and logical (add).")
  }

  if (!all(is.character(labels_text$region), is.numeric(labels_text$year), is.character(labels_text$label), is.numeric(labels_text$position), is.logical(labels_text$add))) {
    stop("One or more columns of the text label data contain incompatible data. Data must be strings (region, label), numeric (year, position), or logical (add).")
  }

  if (!all(is.character(labels_image$region), is.numeric(labels_image$year), is.numeric(labels_image$position), is.logical(labels_image$add))) {
    stop("One or more columns of the image label data contain incompatible data. Data must be strings (region), numeric (year, position) or logical (add).")
  }

  if (!file.exists(labels_image$image_path)) {
    stop("The path(s) of one or more image labels are not correct or the files do not exist.")
  }

  data <- data %>% # calculation of geometry
    dplyr::group_by(region, add) %>%
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
    dplyr::mutate(x_center = replace(x_center, add == TRUE, x_center + 1)) #%>%
  #   ungroup() %>%
  #   mutate(region = as.factor(region))

  if (missing(chron_title_x)) {chron_title_x <- data$x_center} # if name of chrons should be placed different than in their center
  if (missing(chron_title_y)) {chron_title_y <- data$y_center}

  plot <- ggplot2::ggplot(data) + # plot
    ggplot2::geom_tile(aes(x = x_center, width = x_width, y = y_center, height = y_width), fill = "white", color = "black", linetype = "solid", size = 0.5) +
    ggplot2::geom_text(aes(x = chron_title_x, y = chron_title_y, label = name), size = font_size_chrons) +

    if(!missing(labels_text)) {
      ggplot2::geom_text(data = labels_text, aes(y=year, x = position, label = label, hjust = 1, vjust = 1), na.rm = TRUE, size = font_size_labels)
    }

  plot <- plot +
    if(!missing(labels_image)) {
      ggplot2::geom_image(data = labels_image, aes(y=year, x = position, image = image_path), na.rm = TRUE, size = 0.2, asp = 1)
    }

  plot <- plot +
    ggplot2::scale_x_continuous(name = "", breaks = NULL, minor_breaks = NULL, expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = year, breaks = round(seq(min(data$start), max(data$end), by = years_major),1), expand = c(0,0)) +
    ggplot2::facet_grid(cols = vars(region), scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = line_break)) +
    theme_chronochrt() +
    ggplot2::theme(axis.text=element_text(size = (font_size_chrons*0.8*72.27/25.4)),
                   axis.title=element_text(size = font_size_chrons*72.27/25.4, face="bold"),
                   strip.text.x = element_text(size = font_size_chrons*72.27/25.4, face="bold")) #+
#    labs(caption = "Citation of the release paper")

    plot <- plot +
    if(!missing(path)) {
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
        panel.background = element_rect(fill = "grey85",
                                        colour = NA),
        panel.border = element_rect(fill = NA,
                                    colour = "black"),
        panel.grid = element_line(colour = "grey50"),
        panel.grid.minor = element_line(size = rel(0.5)),
        panel.spacing = unit(0, "lines"),
        strip.background = element_rect(fill = "white",
                                        colour = "black"),

        legend.key = element_rect(fill = "white",
                                  colour = NA),
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
