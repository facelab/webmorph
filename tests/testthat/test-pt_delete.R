path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
temlist <- read_tem(path)

test_that("works", {
  nt <- pt_delete(temlist, 0:9, 20:188)
  expect_equal(temlist[[1]]$points[, 11:20], nt[[1]]$points)
})
