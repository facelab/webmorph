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


test_that("rename", {
  a <- faces()
  b <- rename(a, prefix = "a_")
  expect_equal(names(b)[1], "a_f_african")

  b <- rename(a, suffix = "_c")
  expect_equal(names(b)[2], "f_easian_c")

  b <- rename(a, prefix = "d_", suffix = "_e")
  expect_equal(names(b)[3], "d_f_multi_e")

  b <- rename(a, new_names = paste0("face", 1:10))
  expect_equal(names(b)[4], "face4")

  b <- rename(a, pattern = "f_", replacement = "w-") %>%
    rename(pattern = "m_", replacement = "m-")
  expect_equal(names(b)[5:6], c("w-white", "m-african"))
})
