path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
temlist <- read_tem(path)

test_that("error", {
  expect_error(resize(),
               'argument "temlist" is missing, with no default',
               fixed = TRUE)

  expect_error(resize(list("a")),
               'The argument temlist needs to be a temlist',
               fixed = TRUE)

  expect_error(resize(temlist, -2),
               "width must be a positive number",
               fixed = TRUE)

  expect_error(resize(temlist, 1, -2),
               "height must be a positive number",
               fixed = TRUE)
})

test_that("basic", {
  no_change <- resize(temlist)
  expect_equal(no_change, temlist)

  # %, no height
  r <- resize(temlist, .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % no width
  r <- resize(temlist, height = .5)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # % different height/width
  r <- resize(temlist, width = 0.25, height = .50)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(temlist[[1]]$width/4), r[[1]]$width)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels, no height
  r <- resize(temlist, 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels no width
  r <- resize(temlist, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               2*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])

  # pixels different height/width
  r <- resize(temlist, width = 1350/4, height = 1350/2)
  imginfo <- magick::image_info(r[[1]]$img)
  expect_equal(round(temlist[[1]]$width/4), r[[1]]$width)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
  expect_equal(imginfo$width, r[[1]]$width)
  expect_equal(imginfo$height, r[[1]]$height)
  expect_equal(temlist[[1]]$points[1, 1],
               4*r[[1]]$points[1, 1])
  expect_equal(temlist[[1]]$points[2, 1],
               2*r[[1]]$points[2, 1])
})
