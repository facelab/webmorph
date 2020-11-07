test_that("works", {
  path <- system.file("extdata/composite", package = "webmorph")
  temlist <- read_tem(path, images = FALSE)

  x <- average(temlist)
  expect_equal(class(x)[[1]], "webmorph_tem")
  expect_equal(x$name, "average")
  expect_equal(dim(x$points), c(2, 189))
})
