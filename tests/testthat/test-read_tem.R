test_that("demo", {
  path <- system.file("extdata/composite", package = "webmorph")
  temlist <- read_tem(path, images = FALSE)

  expect_equal(length(temlist), 10)

  path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
  tems <- read_tem(path, images = FALSE)
  expect_equal(class(temlist$f_multi)[[1]], "webmorph_tem")
})

