# Visual tests ------------------------------------------------------------
test_that("geom_image", {
  expect_doppelganger("image",
                      ggplot(test_images_reference) + geom_ChronochRtImage(aes(y = year, x = position, image_path = image_path))
  )
})
