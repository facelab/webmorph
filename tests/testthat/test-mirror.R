path <- system.file("extdata/london", package = "webmorph")
stimlist <- read_stim(path, "001_03")

test_that("no map", {
  m <- mirror(stimlist)
  m2 <- mirror(m)
  expect_equal(m[[1]]$points[2,], m2[[1]]$points[2,])
  expect_false(all(m[[1]]$points[1,] == m2[[1]]$points[1,]))
  expect_equal(stimlist, m2)
  # left-eye vs right-eye: > for m, < for m2
  expect_true(m[[1]]$points[1, 1] > m[[1]]$points[1, 2])
  expect_true(m2[[1]]$points[1, 1] < m2[[1]]$points[1,2])

  m <- mirror(stimlist, axis = "horizontal")
  m2 <- mirror(m, axis = "horizontal")
  expect_equal(m[[1]]$points[1,], m2[[1]]$points[1,])
  expect_false(all(m[[1]]$points[2,] == m2[[1]]$points[2,]))
  expect_equal(stimlist, m2)
  # chin vs forehead: < for m, > for m2
  expect_true(m[[1]]$points[2, 130] < m[[1]]$points[2, 140])
  expect_true(m2[[1]]$points[2, 130] > m2[[1]]$points[2, 140])
})

test_that("map", {
  m <- mirror(stimlist, frl_sym())
  m2 <- mirror(m, frl_sym())
  expect_false(all(m[[1]]$points[1,] == m2[[1]]$points[1,]))
  expect_false(all(m[[1]]$points[2,] == m2[[1]]$points[2,]))
  expect_equal(stimlist, m2)
  # left-eye < right-eye for both
  expect_true(m[[1]]$points[1, 1] < m[[1]]$points[1, 2])
  expect_true(m2[[1]]$points[1, 1] < m2[[1]]$points[1,2])
})
