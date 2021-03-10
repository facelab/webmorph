path <- system.file("extdata/composite", package = "webmorphR")
stimlist <- read_stim(path, "f_multi")

test_that("works", {
  # no offsets
  ctems <- crop(stimlist, 500, 600)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 500)
  expect_equal(ctems[[1]]$height, 600)
  expect_equal(info$width, 500)
  expect_equal(info$height, 600)
  expect_equal(stimlist[[1]]$points - c(425, 375), ctems[[1]]$points)


  # squash
  ctems <- crop(stimlist, 500, 600, squash = TRUE)
  #plot(ctems, img.plot = TRUE, pt.shape="index")
  expect_equal(ctems[[1]]$points[2,140], 0)
  expect_equal(ctems[[1]]$points[1,118], 0)
  expect_equal(ctems[[1]]$points[1,123], 500-1)
  expect_equal(ctems[[1]]$points[2,146], 600-1)

  # with offsets
  ctems <- crop(stimlist, 100, 200, 300, 400)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 100)
  expect_equal(ctems[[1]]$height, 200)
  expect_equal(info$width, 100)
  expect_equal(info$height, 200)
  orig_pt <- stimlist[[1]]$points[, 1]
  new_pt <- ctems[[1]]$points[, 1]
  expect_equal(orig_pt, new_pt + c(300, 400))

  # percents, no height
  ctems <- crop(stimlist, .5)
  info <- magick::image_info(ctems[[1]]$img)

  expect_equal(ctems[[1]]$width, 675)
  expect_equal(ctems[[1]]$height, 1350)
  expect_equal(info$width, 675)
  expect_equal(info$height, 1350)
  expect_equal(stimlist[[1]]$points - c(337.5, 0), ctems[[1]]$points)
})

test_that("different crops", {
  stimlist <- faces("composite")

  w <- seq(.1, 1, .1)
  ctems <- crop(stimlist, w, w)
  expect_equivalent(sapply(ctems, `[[`, "width"), 1350*w)
  expect_equivalent(sapply(ctems, `[[`, "height"), 1350*w)
})
