test_that("Plotting", {

  expect_identical(object = p2$plot_env$fill_chron,
                   expected = "red")
  expect_identical(object = p2$plot_env$color_chron,
                   expected = "green")
  expect_identical(object = p2$plot_env$color_label,
                   expected = "orange")
  expect_identical(object = p2$plot_env$minimal,
                   expected = TRUE)
  expect_identical(object = p1$plot_env$axis_title,
                   expected = "Jahre")
  expect_equal(object = file.exists(file.path(tempdir(), "Test_that.jpg")),
               expected = file.exists(file.path(tempdir(), "Test_that.jpg")))
  expect_error(object = plot_chronochrt(not_existent),
               regexp = "object*")
  expect_error(object = plot_chronochrt(test_reference, labels_text = not_existent),
               regexp = "object*")
  expect_error(object = plot_chronochrt(test_reference, labels_text = 2),
               regexp = "* must be a data frame or tibble.")
  expect_error(object = plot_chronochrt(test_reference, labels_image = not_existent),
               regexp = "object*")
  expect_error(object = plot_chronochrt(test_reference, labels_image = 2),
               regexp = "* must be a data frame or tibble.")
  expect_error(object = plot_chronochrt(test_err_Input),
               regexp = "* must be a data frame or tibble*")
  expect_error(object = plot_chronochrt(test_plot_err_level),
               regexp = "Wrong input format: level *")
  expect_error(object = plot_chronochrt(test_plot_err_format),
               regexp = "One or more columns*")
  expect_error(object = plot_chronochrt(test_reference, labels_text = test_labels_err1),
               regexp = "One or more columns of the text label *")
  expect_error(object = plot_chronochrt(test_reference, labels_text = test_labels_err2),
               regexp = "One or more columns of the text label *")
  expect_error(object = plot_chronochrt(test_reference, labels_text = test_err2_Format),
               regexp = "Wrong input format: The column *")
  expect_error(object = plot_chronochrt(test_reference, labels_image = test_images_err1),
               regexp = "One or more columns of the image label *")
  expect_error(object = plot_chronochrt(test_reference, labels_image = test_images_err2),
               regexp = "One or more columns of the image label *")
  expect_error(object = plot_chronochrt(test_reference, labels_image = test_images_err3),
               regexp = "Wrong input format: The column *")
  expect_error(object = plot_chronochrt(test_reference,  axis_title = 23),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, filename = 23),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, filename = "NOT-EXISTENT/plot.pdf"),
               regexp = "The directory*")
  expect_error(object = plot_chronochrt(test_reference, filename = "test.jpg", plot_dim = c(3, 3, "m")),
               regexp = "This unit is not supported. *")
  expect_error(object = plot_chronochrt(test_reference, background = c(1, 2, 3)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, year_lim = "right"),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, year_lim = 33),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, year_lim = c(33, 1, 100)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, height_image = "not_existent"),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, height_image = c(1, 2)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, line_break =  "not_existent"),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, line_break =  c(1, 2)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, size_line = "not_existent"),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, size_line = c(1, 2)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, size_text = "not_existent"),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, size_text = c(1, 2)),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, minimal = 1),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test),
               regexp = "Wrong input format: The column*")
})
