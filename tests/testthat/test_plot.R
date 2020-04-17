test_that("Plotting", {

  expect_identical(object = p2$plot_env$color_fill,
                   expected = "red")
  expect_identical(object = p2$plot_env$color_line,
                   expected = "green")
  expect_identical(object = p$plot_env$axis_title,
                   expected = "Years")
  expect_identical(object = p1$plot_env$axis_title,
                   expected = "Jahre")
  expect_equal(object = file.exists("Test_that.jpg"),
               expected = file.exists("Test_that.jpg"))
  expect_error(object = plot_chronochrt(not_existent),
               regexp = "object*")
  expect_error(object = plot_chronochrt(test_reference, labels_text = not_existent),
               regexp = "object*")
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
  expect_error(object = plot_chronochrt(test_reference,  axis_title = 23),
               regexp = "Wrong inout format: *")
  expect_error(object = plot_chronochrt(test_reference, filename = 23),
               regexp = "Wrong input format: *")
  expect_error(object = plot_chronochrt(test_reference, filename = "NOT-EXISTENT/plot.pdf"),
               regexp = "The directory*")
  expect_error(object = plot_chronochrt(test_reference, plot_dim = c(3, 3, "m")),
               regexp = "This unit is not supported. *")
  expect_error(object = plot_chronochrt(test_reference, chron_name_x = "not_existent"),
               regexp = "The column *")
  expect_error(object = plot_chronochrt(test_reference, chron_name_y = "not_existent"),
               regexp = "The column *")
  expect_error(object = plot_chronochrt(test_reference, chron_name_angle = "not_existent"),
               regexp = "The column *")
  expect_error(object = plot_chronochrt(test_reference, chron_name_x = c("not_existent", "not_here")),
               regexp = "Wrong input format: chron_*")
  expect_error(object = plot_chronochrt(test_reference, chron_name_y = c("not_existent", "not_here")),
               regexp = "Wrong input format: chron_*")
  expect_error(object = plot_chronochrt(test_reference, chron_name_angle = c("not_existent", "not_here")),
               regexp = "Wrong input format: chron_*")
  expect_error(object = plot_chronochrt(test),
               regexp = "Wrong input format: The column*")
  })
