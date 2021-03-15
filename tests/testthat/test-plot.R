test_that("one", {
  x <- faces("test")[[1]]

  p <- plot(x, border.width = 20, border.colour= "red")

  expect_equal(class(p), c("gg", "ggplot"))
  expect_equal(p$coordinates$limits$x, c(-20, x$width+20))
  expect_equal(p$coordinates$limits$y, c(-20, x$height+20))
})

test_that("two", {
  x <- faces("test")

  p <- plot(x,
            label.align = "left",
            label.x = .05,
            label.colour = "dodgerblue",
            label.fontface = "plain",
            label.fontfamily = "Fira Code")
  p

  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("tem", {
  x <- faces("test")[1]
  p.tem <- plot(x, pt.plot = TRUE)
  expect_equal(class(p.tem), c("gg", "ggplot"))

  p.lines <- plot(x, line.plot = TRUE)
  expect_equal(class(p.lines), c("gg", "ggplot"))
})

# labels ----
test_that("labels", {
  skip("Needs visual inspection")

  x <- faces("test")

  # top left
  plot(x, label.position = "top left", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0, vjust = 1, label.x = 0.05, label.y = 0.95)

  # top centre
  plot(x, label.colour = "red")
  plot(x, label.position = "top center", label.colour = "blue")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0.5, vjust = 1, label.x = 0.5, label.y = 0.95)

  # top right
  plot(x, label.position = "top right", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 1, vjust = 1, label.x = 0.95, label.y = 0.95)

  # middle left
  plot(x, label.position = "center left", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0, vjust = 0.5, label.x = 0.05, label.y = 0.5)

  # middle centre
  plot(x, label.position = "centre centre", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0.5, vjust = 0.5, label.x = 0.5, label.y = 0.5)

  # middle right
  plot(x, label.position = "middle right", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 1, vjust = 0.5, label.x = 0.95, label.y = 0.5)

  # bottom left
  plot(x, label.position = "bottom left", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0, vjust = 0, label.x = 0.05, label.y = 0.05)

  # bottom centre
  plot(x, label.position = "bottom middle", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 0.5, vjust = 0, label.x = 0.5, label.y = 0.05)

  # bottom right
  plot(x, label.position = "bottom right", label.colour = "red")
  plot(x, label.position = "z z", label.colour = "purple",
       hjust = 1, vjust = 0, label.x = 0.95, label.y = 0.05)

  plot(x, label.colour = "red")
  plot(x, label.size = 5)
  plot(x, label.size = 7)
  plot(x, label.size = 9, label.alpha = 1)
  plot(x, label.size = 9, label.alpha = 0.5)
  plot(x, label.size = 9, label.alpha = 0.25)
  plot(x, label.position = "center centre", label.color = "red", label.angle = 0)
  plot(x, label.position = "center centre", label.color = "orange", label.angle = 45)
  plot(x, label.position = "center centre", label.color = "yellow", label.angle = 90)
  plot(x, label.position = "center centre", label.color = "green", label.angle = 135)
  plot(x, label.position = "center centre", label.color = "blue", label.angle = 180)
})
