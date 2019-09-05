library(tidyverse)
library(readxl)


# Make new chronological unit ---------------------------------------------



# Import existing dataset -------------------------------------------------

data_chronochart <- read_excel("test.xlsx") %>%
  mutate(x_center = (2*(column_pos-1)+1)/(2*column_tot), # column_pos lowered by one to keep counting of position starting at 1, i. e. coherent to the natural counting habit and coherent mit counting for column_tot
         x_width = 1/column_tot,
         y_center = (Start+End)/2,
         y_width = End - Start)

# # Label input  ------------------------------------------------------------



# Plot chart --------------------------------------------------------------------

plot_chronochart <- ggplot(data_chronochart)
plot_chronochart +
  geom_tile(aes(x = x_center, width = 1/column_tot, y = y_center, height = y_width), fill = "white", color = "black", linetype = "solid") +
  geom_text(aes(x = x_center, y = y_center, label = Epoch)) +
  scale_x_continuous(name = "", breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(name = "Year", breaks = round(seq(min(data_chronochart$Start), max(data_chronochart$End), by = 250),1), minor_breaks = NULL) +
  facet_grid(cols = vars(Area)) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"))


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

