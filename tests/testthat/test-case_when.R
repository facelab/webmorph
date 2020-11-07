test_that("works", {
  x <- case_when_jit(F ~ "A", T ~ "B")
  expect_equal(x, "B")

  x <- case_when_jit(F ~ "A", T ~ 1)
  expect_equal(x, 1)

  x <- case_when_jit(F ~ "A", F ~ "B")
  expect_true(is.null(x))


  x <- case_when_jit(F ~ "A", F ~ 1L, F ~ F, T ~ 1.1, T ~ "B")
  expect_equal(x, 1.1)

  x <- case_when_jit(F ~ "A", T ~ 1.1, bad_val ~ "B")
  expect_equal(x, 1.1)

  a <- list(b = 2)
  x <- case_when_jit(!is.null(a$a) ~ a$a, TRUE ~ a$b)
  expect_equal(x, a$b)
})

test_that("%||%", {
  x <- NULL %||% 2 %||% 1
  expect_equal(x, 2)

})
