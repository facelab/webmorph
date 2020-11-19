path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
stimlist <- read_stim(path)

test_that("works", {
  nt <- pt_delete(stimlist, 0:9, 20:188)
  expect_equal(stimlist[[1]]$points[, 11:20], nt[[1]]$points)
})
