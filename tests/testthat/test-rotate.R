test_that("works", {
  path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
  temlist <- read_tem(path)
  rtems <- rotate(temlist, 45)

  expect_equal(rtems[[1]]$width, 1912)
  expect_equal(rtems[[1]]$height, 1912)


  # no images
  temlist <- read_tem(path, images = FALSE)
  rtems <- rotate(temlist, 45)

  expect_equal(rtems[[1]]$width, 1918)
  expect_equal(rtems[[1]]$height, 1918)
})
