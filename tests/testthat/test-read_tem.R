test_that("demo", {
  path <- system.file("extdata/composite", package = "webmorph")
  tems <- read_tem(path, images = FALSE)

  expect_equal(length(tems), 10)
})

