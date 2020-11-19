path <- system.file("extdata/composite/", package = "webmorph")
stimlist <- read_stim(path)

test_that("works", {
  tpath <- tempfile(fileext = ".tps")
  tps <- write_tps(stimlist, tpath)
  g_array <- geomorph::readland.tps(tpath, specID = "ID")

  w_array <- tems_to_array(stimlist)

  expect_equivalent(dim(g_array), dim(w_array))
  expect_equal(dim(w_array), c(189, 2, 10))

})
