stimlist <- faces("test")
stim <- stimlist[[1]]

test_that("works", {
  # valid webmorph_list
  x <- assert_webmorph(stimlist)
  expect_equal(x, stimlist)

  # valid webmorph_stim
  x <- assert_webmorph(stim)
  expect_equal(class(x), "webmorph_list")
  expect_equal(names(x), "f_multi")

  # valid webmorph_list with no class
  x <- assert_webmorph(unclass(stimlist))
  expect_equal(x, stimlist)

  # valid webmorph_list with no names
  x <- assert_webmorph(unname(stimlist))
  expect_equal(x, stimlist)

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

test_that("reload img", {
  stim$img <- NULL
  stim2 <- webmorph:::assert_webmorph(stim)
  expect_equal(class(stim2[[1]]$img), "magick-image")
})
