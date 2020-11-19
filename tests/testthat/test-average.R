test_that("works", {
  path <- system.file("extdata/composite", package = "webmorph")
  stimlist <- read_stim(path, images = FALSE)

  x <- average(stimlist)
  expect_equal(class(x)[[1]], "webmorph_stim")
  expect_equal(x$name, "average")
  expect_equal(dim(x$points), c(2, 189))
})
