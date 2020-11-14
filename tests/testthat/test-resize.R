path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
temlist <- read_tem(path)

test_that("error", {
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

  r <- resize(temlist, .5)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)

  r <- resize(temlist, height = .5)
  expect_equal(temlist[[1]]$width, r[[1]]$width*2)
  expect_equal(temlist[[1]]$height, r[[1]]$height*2)
})
