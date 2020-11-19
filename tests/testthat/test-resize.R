path <- system.file("extdata/composite", package = "webmorph")
stimlist <- read_stim(path, "f_multi")

test_that("error", {
  expect_error(resize(),
               'argument "stimlist" is missing, with no default',
               fixed = TRUE)

  expect_error(resize(list("a")),
               'stimlist needs to be a webmorph_list',
               fixed = TRUE)

  expect_error(resize(stimlist, -2),
               "width must be a positive number",
               fixed = TRUE)

  expect_error(resize(stimlist, 1, -2),
               "height must be a positive number",
               fixed = TRUE)
})

test_that("basic", {
  no_change <- resize(stimlist)
  expect_equal(no_change, stimlist)

  # %, no height
  r <- resize(stimlist, .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimlist[[1]]$width, r[[1]]$width*2)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % no width
  r <- resize(stimlist, height = .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimlist[[1]]$width, r[[1]]$width*2)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % different height/width
  r <- resize(stimlist, width = 0.25, height = .50)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(stimlist[[1]]$width/4), r[[1]]$width)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels, no height
  r <- resize(stimlist, 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimlist[[1]]$width, r[[1]]$width*2)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels no width
  r <- resize(stimlist, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(stimlist[[1]]$width, r[[1]]$width*2)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels different height/width
  r <- resize(stimlist, width = 1350/4, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(stimlist[[1]]$width/4), r[[1]]$width)
  expect_equal(stimlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(stimlist[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(stimlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])
})
