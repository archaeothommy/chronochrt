# reference data sets

test_reference <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
  end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_reference2 <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2", "Gaia"),
  start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000, 100),
  end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750, 0),
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

test_arranged_reference <- test_reference

test_arranged_reference$region <- factor(test_reference$region, levels = c("Atlantis"))

test_plot_reference <- tibble(
  region = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
  end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  x_label = 0.5,
  y_label = -1500,
  angle_label = 90)


# for checking convert_to_chron()

test <- tibble(Area = "Atlantis",
                    Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                    Begin = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                    End = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                    Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                    Switch = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err_Input <- list (Area = "Atlantis",
                        Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                        Begin = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                        End = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                        Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                        Switch = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err2_Format <- tibble(Area = "Atlantis",
                                Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                Begin = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                                End = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                                Subunit = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                Switch = c("FALSE", "XXX", "N/A", FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_err3_Level <- tibble(Area = "Atlantis",
                               Title = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                               Begin = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                               End = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
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


test_arranged_err <- tibble(
  Area = "Atlantis",
  name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
  start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
  end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
  level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
  add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_arranged_err2 <- as.list(test_reference)

# Test plotting

test_plot_err_format <- tibble(region = "Atlantis",
                                       name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                       start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                                       end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                                       level = c(1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                       add = c("XX", "AA", "FALSE", FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

test_plot_err_level <- tibble(region = "Atlantis",
                                      name = c("Atlas", "II", "Poseidon", "I", "a", "b", "c", "d", "Zeus", "Thanos", "a1", "a2"),
                                      start = c("-2500/-2000", -750, -1500, -1500, -400, -350, -150, 100, -200, -400, -1500, -1000),
                                      end = c(-1500, -200, -200, -750, -350, -250, 100, 300, 500, 300, -1000, -750),
                                      level = c(1.5, 2, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3),
                                      add = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))

# for Tests not run in RCMD check (because it references to its own package)

p <- plot_chronochrt(test_reference)

p1 <- plot_chronochrt(test_reference, axis_title = "Jahre")

p2 <- plot_chronochrt(data = test_plot_reference, labels_text = test_labels_reference, chron_name_align = "left", chron_name_x = "x_label", chron_name_y = "y_label", chron_name_angle = "angle_label", axis_title = "BC/AD", years_major = 250, years_minor = 200, filename = "Test_that.jpg", plot_dim = c(3, 3, "mm"), font_size_chrons = 4, font_size_labels = 2, line_break = 10, color_fill = "red", color_line = "green", size_line = 5, background = c("white", "dashed"), dpi = 1200)
print(p2)
