test_that("c", {
  path <- system.file("extdata/composite/", package = "webmorph")
  a <- read_stim(path, "multi")
  b <- read_stim(path, "african")

  # temlists
  x <- c(a, b)
  expect_equal(length(x), 4)
  expect_equal(names(x), c("f_multi", "m_multi", "f_african", "m_african"))

  # individual tems
  x <- c(a[[1]], b[[1]], a[[2]])
  expect_equal(length(x), 3)
  expect_equal(names(x), c("f_multi", "f_african", "m_multi"))

  # mixed tems and temlists
  x <- c(a, b[[1]])
  expect_equal(length(x), 3)
  expect_equal(names(x), c("f_multi", "m_multi", "f_african"))
})

test_that("print", {
  a <- faces("test")
  x <- capture.output(print(a))
  op <- c("* f_multi: 189 points, 44 lines, 338 x 338 JPEG",
          "* m_multi: 189 points, 44 lines, 338 x 338 JPEG")
  expect_equal(x, op)

  x <- capture.output(print(a[[1]]))
  expect_equal(x, "189 points, 44 lines, 338 x 338 JPEG")
})

test_that("rep", {
  a <- faces("test")

  x <- rep(a[[1]], 3)
  expect_equal(length(x), 3)
  expect_equal(names(x), rep("f_multi", 3))

  x <- rep(a, 3)
  expect_equal(length(x), 6)
  expect_equal(names(x), rep(c("f_multi", "m_multi"), 3))

  x <- rep(a, times = 3)
  expect_equal(length(x), 6)
  expect_equal(names(x), rep(c("f_multi", "m_multi"), times = 3))
})

