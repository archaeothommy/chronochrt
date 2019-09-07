library(tidyverse)
library(readxl)

# example/playground
xy <- add_chron(xy,
                c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C", "B", "B", "C", "C", "C", "C", "A", "A", "A"),
                c("Test 1", "Test 2b", "Test 2", "Test 2a", "Test 6a", "Test 6b", "Test 6c", "Test 6d", "Test 3", "Test 1", "Test 2", "Test 3", "Test 3", "Test 4", "Test 5", "Test 1", "Test 2", "Test 4", "Test 5", "Test 6", "Test 2a1", "Test 2a2"),
                c(-2500, -750, -1500, -1500, -400, -350, -150, 100, -200, -3000, -2000, -1500, -750, -700, -150, -2000, -1500, 0, 150, -400, -1500, -1000),
                c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, -2000, -1500, -700, 0, -150, 100, -1500, -750, 150, 300, 300, -1000, -750),
                c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                new_table = TRUE) %>%
    add_chron(., c("D"), c("Test 1", "Test 2", "Test 3", "Test 3a", "Test 3b", "Test 4"), c(100, 200, 300, 300, 400, 500), c(200, 300, 500, 400, 500, 600), c(1, 1, 1, 2, 2, 1)) %>%

  group_by(region, add) %>%
  mutate(subchron = subchron_count(start, end)) %>%
  mutate(col_tot = case_when(level > subchron ~ level,
                             level == subchron ~ level +1,
                             level < subchron ~ level + subchron)) %>%
  mutate(x_center = (2*(level-1)+1)/(2*col_tot), # column_pos lowered by one to keep counting of position starting at 1, i. e. coherent to the natural counting habit and coherent mit counting for column_tot
         x_width = 1/col_tot,
         y_center = (start+end)/2,
         y_width = end - start) %>%
  mutate(x_center = replace(x_center, add == TRUE, x_center + 1))
 # Problem weiterhin: Test 2b: wie implementieren?

plot_chronochart(xy)

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



# Plot chart --------------------------------------------------------------------

  # store plot information in hidden variable
plot_chronochart <- function(data)
  {
  data <- data %>%
    group_by(region, add) %>%
    mutate(subchron = subchron_count(start, end)) %>%
    mutate(col_tot = case_when(level > subchron ~ level,
                               level == subchron ~ level +1,
                               level < subchron ~ level + subchron)) %>%
    mutate(x_center = (2*(level-1)+1)/(2*col_tot), # column_pos lowered by one to keep counting of position starting at 1, i. e. coherent to the natural counting habit and coherent mit counting for column_tot
           x_width = 1/col_tot,
           y_center = (start+end)/2,
           y_width = end - start) %>%
    mutate(x_center = replace(x_center, add == TRUE, x_center + 1))

  plot <- ggplot(data) +
    geom_tile(aes(x = x_center, width = x_width, y = y_center, height = y_width), fill = "white", color = "black", linetype = "solid") +
    geom_text(aes(x = x_center, y = y_center, label = unit)) +
    scale_x_continuous(name = "", breaks = NULL, minor_breaks = NULL) +
    scale_y_continuous(name = "", breaks = round(seq(min(data$start), max(data$end), by = 250),1), minor_breaks = NULL) +
    facet_grid(cols = vars(region), scales = "free_x", space = "free_x") +
    theme_bw() +
    theme(panel.spacing = unit(0, "lines"), panel.background = element_rect(fill = "grey"))

  plot
}

# Plot labels -------------------------------------------------------------




# ChronochRt theme --------------------------------------------------------

# structure copied from theme_grey, the basic theme in ggplot2
theme_chronochrt <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
          base_rect_size = base_size/22)
{
  half_line <- base_size / 2
  theme(
    line = element_line(
      colour = "black",
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = "white",
      colour = "black",
      size = base_rect_size,
      linetype = 1
    ),
    text = element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = rel(0.8),
                             colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8 *
                                                 half_line /
                                                 2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.8 *
                                                     half_line /
                                                     2), vjust = 0),
    axis.text.y = element_text(margin = margin(r = 0.8 *
                                                 half_line /
                                                 2), hjust = 1),
    axis.text.y.right = element_text(margin = margin(l = 0.8 *
                                                       half_line /
                                                       2), hjust = 0),
    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(margin = margin(t = half_line / 2),
                                vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = half_line / 2),
                                    vjust = 0),
    axis.title.y = element_text(
      angle = 90,
      margin = margin(r = half_line /
                        2),
      vjust = 1
    ),
    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line /
                        2),
      vjust = 0
    ),
    legend.background = element_rect(colour = NA),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = margin(half_line,
                           half_line, half_line, half_line),
    legend.key = element_rect(fill = "grey95",
                              colour = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(0, 0,
                               0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),
    panel.background = element_rect(fill = "grey92",
                                    colour = NA),
    panel.border = element_blank(),
    panel.grid = element_line(colour = "white"),
    panel.grid.minor = element_line(size = rel(0.5)),
    panel.spacing = unit(half_line,
                         "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "grey85",
                                    colour = NA),
    strip.text = element_text(
      colour = "grey10",
      size = rel(0.8),
      margin = margin(0.8 * half_line,
                      0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = unit(half_line / 2,
                                 "pt"),
    strip.switch.pad.wrap = unit(half_line / 2,
                                 "pt"),
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(
      size = rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.subtitle = element_text(
      hjust = 0,
      vjust = 1,
      margin = margin(b = half_line)
    ),
    plot.caption = element_text(
      size = rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = margin(t = half_line)
    ),
    plot.tag = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = margin(half_line,
                         half_line, half_line, half_line), complete = TRUE)
}

# Alternative: based on theme_bw (native in ggplot2) ======================

function (base_size = 11, base_family = "", base_line_size = base_size/22,
          base_rect_size = base_size/22)
{

  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      panel.background = element_rect(fill = "white",
                                      colour = NA),
      panel.border = element_rect(fill = NA,
                                  colour = "grey20"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(size = rel(0.5)),
      strip.background = element_rect(fill = "grey85",
                                      colour = "grey20"),
      legend.key = element_rect(fill = "white",
                                colour = NA), complete = TRUE)
}


# Check formats -----------------------------------------------------------

  # evaluation to an expression NA if everything is fine and errormessage if something is wrong
check_format <- funtion()
  {
    #zu implementierende checks:
    #stopifnot(is.character(region), is.numeric(start), is.numeric(end), level = is numeric + ganzzahlig , add = boolean
    # inkl. Ausgabe vernünftiger Fehlermeldungen

}


# Helper function ---------------------------------------------------------

subchron_count <- function(left, right) #
{
  data <- rep_len(0, length(left))

  for(i in 1:(length(left)))
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
