test_that("one", {
  x <- faces("test")[[1]]

  p <- plot(x, border.width = 20, border.colour= "red")

  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$coordinates$limits$x, c(0, x$width))
  expect_equal(p$coordinates$limits$y, c(0, x$height))
})

test_that("two", {
  x <- faces("test")

  p <- plot(x, hjust = -0.1,
            label_colour = "dodgerblue",
            label_size = 10,
            label_fontface = "plain",
            label_fontfamily = "Fira Code",
            border.width = 10)

  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("tem", {
  x <- faces("test")[1]
  p.tem <- plot(x, pt.plot = TRUE)
  expect_equal(class(p.tem), c("gg", "ggplot"))

  p.lines <- plot(x, line.plot = TRUE)
  expect_equal(class(p.lines), c("gg", "ggplot"))
})
