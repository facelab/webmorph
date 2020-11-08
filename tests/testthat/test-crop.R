test_that("works", {
  path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
  temlist <- read_tem(path)
  ctems <- crop(temlist, 100, 200)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 100)
  expect_equal(ctems[[1]]$height, 200)
  expect_equal(info$width, 100)
  expect_equal(info$height, 200)
  expect_equal(temlist[[1]]$points, ctems[[1]]$points)

  ctems <- crop(temlist, 100, 200, 300, 400)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 100)
  expect_equal(ctems[[1]]$height, 200)
  expect_equal(info$width, 100)
  expect_equal(info$height, 200)
  orig_pt <- temlist[[1]]$points[, 1]
  new_pt <- ctems[[1]]$points[, 1]
  expect_equal(orig_pt, new_pt + c(300, 400))
})
