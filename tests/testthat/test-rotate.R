path <- system.file("extdata/test", package = "webmorphR")

test_that("works", {
  # 45 degrees with image
  stimlist <- read_stim(path)
  rtems <- rotate(stimlist, 45)

  expect_equal(rtems[[1]]$width, 480)
  expect_equal(rtems[[1]]$height, 480)
  expect_equal(rtems[[1]]$points[, 1],
               c(232.4870, 209.6828),
               tolerance = 0.001)


  # no images, so estimates centre of image
  stimlist <- read_stim(path, "tem$")
  rtems <- rotate(stimlist, 45)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(233.2321, 208.6849),
               tolerance = 0.001)

  # negative rotation, no images
  rtems <- rotate(stimlist, -45)

  expect_equal(rtems[[1]]$width, 479)
  expect_equal(rtems[[1]]$height, 479)
  expect_equal(rtems[[1]]$points[, 1],
               c(208.6849, 246.2205),
               tolerance = 0.001)

  # > 360 rotation, images
  stimlist <- read_stim(path)
  rtems <- rotate(stimlist, 360+90)

  expect_equal(rtems[[1]]$width, 338)
  expect_equal(rtems[[1]]$height, 338)
  expect_equal(rtems[[1]]$points[, 1],
               c(185.125, 142.250),
               tolerance = 0.001)
  #plot(rtems, image = T)
})
