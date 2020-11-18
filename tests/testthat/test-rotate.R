path <- system.file("extdata/test/f_multi.tem", package = "webmorph")

test_that("works", {
  # 45 degrees with image
  temlist <- read_tem(path)
  rtems <- rotate(temlist, 45)

  expect_equal(rtems[[1]]$width, 480)
  expect_equal(rtems[[1]]$height, 480)
  expect_equal(rtems[[1]]$points[, 1],
               c(232.4870, 209.6828),
               tolerance = 0.001)


  # no images, so estimates centre of image
  temlist <- read_tem(path, images = FALSE)
  rtems <- rotate(temlist, 45)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(233.2321, 208.6849),
               tolerance = 0.001)

  # negative rotation, no images
  rtems <- rotate(temlist, -45)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(208.6849, 246.2205),
               tolerance = 0.001)

  # > 360 rotation, images
  temlist <- read_tem(path)
  rtems <- rotate(temlist, 360+90)

  expect_equal(rtems[[1]]$width, 338)
  expect_equal(rtems[[1]]$height, 338)
  expect_equal(rtems[[1]]$points[, 1],
               c(185.125, 142.250),
               tolerance = 0.001)
  #plot(rtems, image = T)
})
