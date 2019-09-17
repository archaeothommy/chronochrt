library(tidyverse)
library(readxl)
library(imager)
# example/playground
xy <- add_chron(xy,
                c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C", "B", "B", "C", "C", "C", "C", "A", "A", "A"),
                c("Test 1", "Test 2b", "Test 2", "Test 2a", "Test 6a", "Test 6b", "Test 6c", "Test 6d", "Test 3", "Test 1", "Test 2", "Test 3", "Test 3", "Test 4", "Test 5", "Test 1", "Test 2", "Test 4", "Test 5", "Test 6", "Test 2a1", "Test 2a2"),
                c(-2500, -750, -1500, -1500, -400, -350, -150, 100, -200, -3000, -2000, -1500, -750, -700, -150, -2000, -1500, 0, 150, -400, -1500, -1000),
                c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, -2000, -1500, -700, 0, -150, 100, -1500, -750, 150, 300, 300, -1000, -750),
                c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                new_table = TRUE) %>%
    add_chron(., c("D"), c("Test 1", "Test 2", "Test 3", "Test 3a", "Test 3b", "Test 4"), c(100, 200, 300, 300, 400, 500), c(200, 300, 500, 400, 500, 600), c(1, 1, 1, 2, 2, 1))

plot_chronochrt(xy, years_major = 250)

# Make new chronological unit ---------------------------------------------

  # makes a tibble or add a row to a tibble with the columns region, unit, start, end, kind,
  # allows input of single values or vectors
  # works only with the given naming scheme, for other ones use regular dplyr-functions (add_row)

  # to implement: telling error messages

add_chron <- function(data, region, unit, start, end, level = 1, add = FALSE, new_table = FALSE, ...)
  {
  #if (!is.na(check_format())) {stop(check_format())}
  if (new_table == TRUE) {data <- tibble(region, unit, start, end, level, add, ...)}
  if (new_table == FALSE) {data <- add_row(data, region, unit, start, end, level, add, ...)}
  data
}

# Import existing dataset -------------------------------------------------

  # makes a tibble out of a file, the relevant columns must be identified and will be renamed

  # implementation needed: guessing of format from file name and choosing of right import method to get more flexibility

import_chron <- function(path, region, unit, start, end, level, add = FALSE, ...)
  {
   data <- read_excel(path, ...) %>%
    rename(region = region, unit= unit, start = start, kind = kind, level = level, add = add)
   # implementation check_format like in add_chron
  data
}

# Label input  ------------------------------------------------------------

  # add text labels

    # works analogous to add_chron

  add_label_text <- function (region, year, annotation, ...)
  {
    #if (!is.na(check_format())) {stop(check_format())}
    labels <- tibble(region, year, annotation, ...)

    labels
  }

  # add image labels

    #functions aimed: input of image,
  # determining input for geom_raster (x, y min/max) out of image size, prevent distortion
  # storing in list so that all images can be plotted with one command
  # shrink images to size given

  add_image <- function (region, path, year,
  {
    labels_image <-

  }


  # for bitmaps/files:
  annotation_raster(readPNG("C:/Dokumente/Forschung/Vignette Erz.png"), 0.25, 0.5, -300, 300)




# Plot chart --------------------------------------------------------------------

  # to implement: differentiation between e. g. Test 2b and Test 2a1 (last subunits and subunits with missing column)
  # current: all last subunits affected

  # in plot: switches/input for: labels, breaks auf y-Achse,

plot_chronochrt <- function(data, year, labels_text, years_major = 100, breaks_minor = 1, labels_image)
  {
  data <- data %>% # calculation of geometry
    group_by(region, add) %>%
    mutate(subchron = subchron_count(start, end)) %>%
    mutate(col_tot = case_when(level > subchron ~ level,
                               level == subchron ~ level +1,
                               level < subchron ~ level + subchron)) %>%
    mutate(x_center = (level-0.5)/col_tot, # column_pos lowered by one to keep counting of position starting at 1, i. e. coherent to the natural counting habit and coherent mit counting for column_tot
           x_width = 1/col_tot,
           y_center = (start+end)/2,
           y_width = end - start) %>%
    mutate_at(., .vars = c(x_center, x_width), .funs = width_corr(x_center, x_width)) %>%
   # mutate(x_width = if_else(level > subchron & subchron != 0, true = 1/col_tot + 1/(2*(col_tot+1)), false = x_width),
   #       x_center = if_else(level > subchron & subchron != 0, true = (level-0.5)/col_tot - 1/(4*(col_tot+1)), false = x_center)) %>%
    mutate(x_center = replace(x_center, add == TRUE, x_center + 1))

  plot <- ggplot(data) + # plot
    geom_tile(aes(x = x_center, width = x_width, y = y_center, height = y_width), fill = "white", color = "black", linetype = "solid") +
    geom_text(aes(x = x_center, y = y_center, label = unit)) +
    scale_x_continuous(name = "", breaks = NULL, minor_breaks = NULL, expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = round(seq(min(data$start), max(data$end), by = years_major),1)) +
    facet_grid(cols = vars(region), scales = "free_x", space = "free_x") +
    theme_chronochrt()

   # geom_text(data = labels_text, aes(y=year, x = ###, label = annotation, hjust = 0, vjust = 0.5), na.rm = TRUE, size = 2)


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
        complete = TRUE
      )
  }

# Check formats -----------------------------------------------------------

  # evaluation to an expression NA if everything is fine and errormessage if something is wrong
check_format <- function()
  {
    #zu implementierende checks:
    #stopifnot(is.character(region, annotation), is.numeric(start, end , year), level = is numeric + ganzzahlig , add = boolean
    # inkl. Ausgabe vernünftiger Fehlermeldungen

}


# Helper function ---------------------------------------------------------

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


width_corr <- function(x_center, x_width) # corrects width if there are missing subunits in other subchrons
  {
    data <- rep_len(0, length(x_center))

    for(i in 1:length(x_center))
    {
      for(j in 1:length(x_center))
      {
        k <- x_center[i] + (x_width[i]/2) - x_center[j] + (x_width[j]/2),

        if (x_center[i] < x_center[j] & k != 0 & k < x_width[i]){
          x_center[j] <- x_center[j] - k/2;
          x_width[j] <-  x_width[j] + k
          }
      }
    }
    data
  }
