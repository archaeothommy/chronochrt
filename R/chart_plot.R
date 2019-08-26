require(tidyverse)
require(readxl)
require(ggthemes)

data_chronochart <- read_excel("test.xlsx") %>%
  mutate(x_center = (2*(column_pos-1)+1)/(2*column_tot), # column_pos lowered by one to keep counting of position starting at 1, i. e. coherent to the natural counting habit and coherent mit counting for column_tot
         x_width = 1/column_tot, 
         y_center = (Start+End)/2, 
         y_width = End - Start)

plot_chronochart <- ggplot(data_chronochart)
plot_chronochart + 
  geom_tile(aes(x = x_center, width = 1/column_tot, y = y_center, height = y_width), fill = "white", color = "black", linetype = "solid") + 
  geom_text(aes(x = x_center, y = y_center, label = Epoch)) +
  scale_x_continuous(name = "", breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(name = "Year", breaks = round(seq(min(data_chronochart$Start), max(data_chronochart$End), by = 250),1), minor_breaks = NULL) +
  facet_grid(cols = vars(Area)) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"))


