path <- system.file("extdata/test", package = "webmorph")
stim_list <- read_stim(path)
stim <- stim_list[[1]]

test_that("works", {
  # valid webmorph_list
  x <- assert_webmorph(stim_list)
  expect_equal(x, stim_list)

  # valid webmorph_stim
  x <- assert_webmorph(stim)
  expect_equal(class(x), "webmorph_list")
  expect_equal(names(x), "f_multi")

  # valid webmorph_list with no class
  x <- assert_webmorph(unclass(stim_list))
  expect_equal(x, stim_list)

  # valid webmorph_list with no names
  x <- assert_webmorph(unname(stim_list))
  expect_equal(x, stim_list)

  # valid webmorph_stim with no class
  x <- assert_webmorph(unclass(stim))
  expect_equal(class(x), "webmorph_list")
  expect_equal(names(x), "f_multi")
})

test_that("errors", {
  expect_error(assert_webmorph())
  expect_error(assert_webmorph(c()))
  expect_error(assert_webmorph(list()))
  expect_error(assert_webmorph(list("a")))
})
