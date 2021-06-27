test_that("Test geom chronochrt", {

  geom_test <- ggplot(test_reference) + geom_chronochRt(aes(name = name, region = region, level = level, start = start, end = end, add = add))
  data <- layer_data(geom_test)

  expect_equal(object = max(data$ymax),
               expected = 500)
  expect_equal(object = max(data$xmax),
               expected = 2)
  expect_equal(object = data$x[data$name == "Thanos"],
               expected = 1.25)
  expect_equal(object = data$boundary_start[data$start2 == -2000][1],
               expected = "unsec")
})

# Visual tests ------------------------------------------------------------
test_that("geom_Chronochrt", {
  expect_doppelganger("chronochrt",
                      ggplot(test_reference) + geom_chronochRt(aes(name = name, region = region, level = level, start = start, end = end, add = add))
  )
})
