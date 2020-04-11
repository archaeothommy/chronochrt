test_that("Creating and adding text labels", {
  expect_equal(object = add_label_text(region = c("Atlantis", "Sargassosee"),
                                       year = c(-1650, 250),
                                       position = c(1, 0.99),
                                       label = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                                       new = TRUE),
               expected = test_labels_reference)
  expect_equal(object = add_label_text(test_labels_reference,
                                       region = "Atlantis",
                                       year = 275,
                                       position = 2,
                                       label = "starkes Erdbeben",
                                       new = FALSE),
               expected = test_labels_reference2)
  expect_equal(object = add_label_text(data = not_existent,
                                       region = c("Atlantis", "Sargassosee"),
                                       year = c(-1650, 250),
                                       position = c(1, 0.99),
                                       label = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                                       new = TRUE),
               expected = test_labels_reference)
  expect_error(object = add_label_text(data = not_existent,
                                       region = c("Atlantis", "Sargassosee"),
                                       year = c(-1650, 250),
                                       position = c(1, 0.99),
                                       label = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                                       new = FALSE),
               regexp = "The object *")
  expect_error(object = add_label_text(region = c("Atlantis", "Sargassosee"),
                                       year = c("-1650", 250),
                                       position = c(1, 0.99),
                                       label = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                                       new = TRUE),
               regexp = "One or more *")
  expect_error(object = add_label_text(region = c("Atlantis", "Sargassosee"),
                                       year = c("-1650", 250),
                                       position = c("1", 0.99),
                                       label = c("Krieg mit Ägypten ", "Geburt von Aquaman"),
                                       new = TRUE),
               regexp = "One or more *")
})
