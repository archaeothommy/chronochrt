setwd("C:/Users/Dell/Desktop/chronochrt")
data <- read_excel("Example_data/ex_urnfield_periods.xlsx")
data <- rename(region = data$Region,
               name = data$Name,
               start = data$Start,
               end = data$End,
               level = data$Level
)
# ich habe keine Ahnung warum nicht mal das funktioniert.

# vielleicht weil du das package für read_Excel nicht geladen hast?

# London data
# import dataset
# generate labels: 2 general ones, one specific. if not possible, just the great plague one
#   1559 - Coronation of Elizabeth I
#   12.04.1665 - The "Great Plague of London" begins [in plague deaths]
#   1666 - Great Fire of London


  library(tidyverse)
  library(readxl)
  library(ggimage) # es ist noch kein package, du musst die benötigen also noch selbst laden

atlantis <- add_chron(xy,
                  region = "Atlantis",
                  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                  start = c(-2500, -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                  end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
                  new_table = TRUE) %>%
    add_chron(., "Sargassosee",
              c("Nereide I", "Nereide II", "Aquaman", "a", "b", "Arthur Curry"),
              c(100, 200, 300, 300, 400, 500),
              c(200, 300, 500, 400, 500, 600),
              c(1, 1, 1, 2, 2, 1))

labels_atlantis <- add_text_label(labels_atlantis,
                           region = c("Atlantis", "Sargassosee"),
                           year = c(-1650, 250),
                           position = c(1, 0.99),
                           annotation = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                           add = FALSE) %>%
  add_text_label(., "Atlantis", 275, 2, "starkes Erdbeben ")

images_atlantis <- add_image_label(images_atlantis,
                          region = c("Atlantis", "Sargassosee"),
                          year = c(-2250, 250),
                          position = 0.2,
                          image_path = "https://www.r-project.org/logo/Rlogo.png",
                          add = FALSE)

plot_chronochrt(atlantis, year = "Jahr",
                labels_text = labels_atlantis,
                labels_image = images_atlantis,
                years_major = 250,
                font_size_chrons = 4,
                font_size_labels = 3,
                path = "Example_data/Atlantis.jpg",
                height = 15,
                width = 21,
                units = "cm")


# urnfield culture example

UK_Chronologie <- import_chron(path = "Example_data/ex_urnfield_periods.xlsx",
                               "Region", "Name", "Start", "End", "Level")

images <- add_image_label(images, region = "Süddeutschland (Reinecke 1911)",
                          year = -730, position = 0.85,
                          image_path = "Example_data/Fibel_HaD3_Süddeutschland.jpg",
                          add = FALSE)

plot_chronochrt(UK_Chronologie,
                year = "Jahre", years_major = 50,
                labels_image = images,
                font_size_chrons = 4,
                path = "UK-Chronologie.png",
                width = 15, height = 10, units = "cm")

# London example

London_Friedhöfe <- import_chron(path = "Example_data/ex_London_cem.xlsx", "Region", "Name", "Start", "End", "Level")

London_labels <- add_text_label(labels,
                         region = "urban",
                         year = c(1559, 1666),
                         annotation = c("1559: Coronation of Elizabeth I ", "1666: Great Fire of London"),
                         position = 1.98,
                         add =  FALSE) %>%
  add_text_label(., region = "low socio- economic status",
                 year = 1665,
                 annotation = "12.04.1665:\n The \"Great Plague of London\"\n begins",
                 position = 1.98)

plot_chronochrt(data = London_Friedhöfe, year = "Years", labels_text = London_labels, font_size_chrons = 3, font_size_labels = 2, years_major = 25, path = "London-Friedhöfe.png", width = 20, height = 10, units = "cm")

