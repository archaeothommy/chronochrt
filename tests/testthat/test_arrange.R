test_that("Arrange regions", {

  expect_equal(object = arrange_regions(test_reference, order = c("Atlantis")),
               expected = test_arranged_reference)
  expect_error(object = arrange_regions(not_existent, order = c("Atlantis")),
               regexp = "object*")
  expect_error(object = arrange_regions(test_arranged_err2, order = c("Atlantis")),
               regexp = "Wrong input format: *")
  expect_error(object = arrange_regions(test_arranged_err, order = c("Atlantis")),
               regexp = "Columns `region` does *")
  expect_error(object = arrange_regions(test_reference, order = test_reference),
               regexp = "Incompatible input format: *")
  expect_error(object = arrange_regions(test_reference, order = 23),
               regexp = "Incompatible input format: *")
  expect_equal(object = arrange_regions(test_reference, order = "Atlantis"),
               expected = test_arranged_reference)
})
