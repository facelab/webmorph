path <- system.file("extdata/composite/", package = "webmorph")
temlist <- read_tem(path)

test_that("works", {
  tpath <- tempfile(fileext = ".tps")
  tps <- write_tps(temlist, tpath)
  g_array <- geomorph::readland.tps(tpath, specID = "ID")

  w_array <- tems_to_array(temlist)

  expect_equivalent(dim(g_array), dim(w_array))
  expect_equal(dim(w_array), c(189, 2, 10))

})
