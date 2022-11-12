library(tibble)
library(ggplot2)
library(vdiffr)

# reference data sets

test_reference <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
  end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_reference2 <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2", "Gaia"),
  start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000", 100),
  end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500", 0),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3, 1),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))

test_reference3 <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2", "Gaia"),
  start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000", 100),
  end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500", "0/100"),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3, 1),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))

test_labels_reference <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c(-1650, 250),
  position = c(1, 0.99),
  label = c("Krieg mit Ägypten ", "Geburt von Aquaman"))

test_labels_reference2 <- tibble(
  region = c("Atlantis", "Sargassosee", "Atlantis"),
  year = c(-1650, 250, 275),
  position = c(1, 0.99, 2),
  label = c("Krieg mit Ägypten ", "Geburt von Aquaman", "starkes Erdbeben"))

test_images_reference <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c(-1650, 250),
  position = c(1, 0.99),
  image_path = "https://www.r-project.org/logo/Rlogo.png")

test_images_reference2 <- tibble(
  region = c("Atlantis", "Sargassosee", "Atlantis"),
  year = c(-1650, 250, 275),
  position = c(1, 0.99, 2),
  image_path = "https://www.r-project.org/logo/Rlogo.png")

test_arranged_reference <- test_reference

test_arranged_reference$region <- factor(test_reference$region, levels = c("Atlantis"))

test_plot_reference <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
  end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )#,
  #x_label = 0.5,
  #y_label = -1500,
  #angle_label = 90)

# for checking convert_to_chron()

test <- tibble(Area = "Atlantis",
                    Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                    Begin = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                    End = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                    Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                    Switch = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err_Input <- list (Area = "Atlantis",
                        Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                        Begin = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                        End = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                        Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                        Switch = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err2_Format <- tibble(Area = "Atlantis",
                                Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                Begin = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                                End = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                                Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                Switch = c("FALSE", "XXX", "N/A", FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err3_Level <- tibble(Area = "Atlantis",
                               Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                               Begin = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                               End = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                               Subunit = c(1.5, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                               Switch = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_labels_err1 <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c(-1650, 250),
  position = c(1, "0.99"),
  label = c("Krieg mit Ägypten ", "Geburt von Aquaman"))

test_labels_err2 <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c("-1650", 250),
  position = c(1, 0.99),
  label = c("Krieg mit Ägypten ", "Geburt von Aquaman"))

test_images_err1 <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c(-1650, 250),
  position = c(1, "0.99"),
  image_path = "https://www.r-project.org/logo/Rlogo.png")

test_images_err2 <- tibble(
  region = c("Atlantis", "Sargassosee"),
  year = c("-1650", 250),
  position = c(1, 0.99),
  image_path = "https://www.r-project.org/logo/Rlogo.png")

test_images_err3 <- tibble(
  area = c("Atlantis", "Sargassosee"),
  year = c("-1650", 250),
  position = c(1, 0.99),
  image_path = "https://www.r-project.org/logo/Rlogo.png")

test_arranged_err <- tibble(
  Area = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
  end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_arranged_err2 <- as.list(test_reference)

# Test plotting

test_plot_err_format <- tibble(region = "Atlantis",
                                       name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                       start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                                       end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                                       level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                       add = c("XX", "AA", "FALSE", FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_plot_err_level <- tibble(region = "Atlantis",
                                      name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                      start = c("-2500/-2000", "-750_-500", -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, "-1100/-1000"),
                                      end = c(-1500, -200, -200, "-750_-500", -350, -250, 100, 300, 500, 300, "-1100/-1000", "-750_-500"),
                                      level = c(1.5, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                      add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

# for Tests not run in RCMD check (because it references to its own package)

p1 <- plot_chronochrt(test_reference, axis_title = "Jahre")

p2 <- plot_chronochrt(data = test_plot_reference, labels_text = test_labels_reference, labels_image = test_images_reference, axis_title = "BC/AD", year_lim = c(-1500, 100), filename = file.path(tempdir(), "Test_that.jpg"), height_image = 5, plot_dim = c(3, 3, "mm"),
                      line_break = 10, fill_chron = "red", color_chron = "green", size_line = 5, background = c("white", "dashed"), dpi = 1200, minimal = TRUE, color_label = "orange", size_text = 5)
print(p2)
