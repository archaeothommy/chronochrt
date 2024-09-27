test_that("Creating and adding image labels", {

  expect_equal(
    object = add_label_image(
      region = c("Atlantis", "Sargassosee"),
      year = c(-1650, 250),
      position = c(1, 0.99),
      image_path = "https://www.r-project.org/logo/Rlogo.png",
      new = TRUE
    ),
    expected = test_images_reference
  )
  expect_equal(
    object = add_label_image(
      test_images_reference,
      region = "Atlantis",
      year = 275,
      position = 2,
      image_path = "https://www.r-project.org/logo/Rlogo.png",
      new = FALSE
    ),
    expected = test_images_reference2
  )
  expect_equal(
    object = add_label_image(
      data = not_existent,
      region = c("Atlantis", "Sargassosee"),
      year = c(-1650, 250),
      position = c(1, 0.99),
      image_path = "https://www.r-project.org/logo/Rlogo.png",
      new = TRUE
    ),
    expected = test_images_reference
  )
  expect_error(
    object = add_label_image(
      data = not_existent,
      region = c("Atlantis", "Sargassosee"),
      year = c(-1650, 250),
      position = c(1, 0.99),
      image_path = "https://www.r-project.org/logo/Rlogo.png",
      new = FALSE
    ),
    regexp = "object *"
  )
  expect_error(
    object = add_label_image(
      region = c("Atlantis", "Sargassosee"),
      year = c("-1650", 250),
      position = c(1, 0.99),
      image_path = "https://www.r-project.org/logo/Rlogo.png",
      new = TRUE
    ),
    regexp = "One or more *"
  )
  expect_error(
    object = add_label_image(
      region = c("Atlantis", "Sargassosee"),
      year = c("-1650", 250),
      position = c("1", 0.99),
      image_path =  "https://www.r-project.org/logo/Rlogo.png",
      new = TRUE
    ),
    regexp = "One or more *"
  )
}
)
