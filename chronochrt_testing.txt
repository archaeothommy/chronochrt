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






