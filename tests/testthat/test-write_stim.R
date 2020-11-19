test_that("works", {
  path <- system.file("extdata/composite", package = "webmorph")
  stimlist <- read_stim(path, "f_multi")

  dir <- paste(tempdir(), "ftest", sep = "/")
  write_stim(stimlist, dir, prefix = "test")

  expect_equal(list.files(dir), c("testf_multi.jpg", "testf_multi.tem"))

  unlink(dir, recursive = TRUE)
})
