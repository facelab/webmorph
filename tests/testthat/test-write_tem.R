test_that("works", {
  path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
  temlist <- read_tem(path)

  dir <- paste(tempdir(), "ftest", sep = "/")
  write_tem(temlist, dir, prefix = "test")

  expect_equal(list.files(dir), c("testf_multi.jpg", "testf_multi.tem"))

  unlink(dir, recursive = TRUE)
})
