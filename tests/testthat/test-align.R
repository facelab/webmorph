path <- system.file("extdata/test", package = "webmorphR")
stimlist <- read_stim(path)

test_that("works", {

  f2 <- align(stimlist, x1 = 100, y1 = 150, x2 = 200, y2 = 250,
              width = 300, height = 400)

  expect_equal(f2[[1]]$points[, 1], c(100, 150))
  expect_equal(f2[[1]]$points[, 2], c(200, 250))
  expect_equal(f2[[2]]$points[, 1], c(100, 150))
  expect_equal(f2[[2]]$points[, 2], c(200, 250))
  expect_equal(f2[[1]]$width, 300)
  expect_equal(f2[[1]]$height, 400)
  expect_equal(f2[[2]]$width, 300)
  expect_equal(f2[[2]]$height, 400)
})


test_that("procrustes", {
  grps <- c("composite", "test", "london")
  grps <- "test"
  lapply(grps , function(grp) {
    data <- faces(grp) %>% tems_to_array()

    expect_silent(g <- procrustes_align(data))
    expect_silent(p0 <- procrustes_align(data, 0))
    expect_silent(p1 <- procrustes_align(data, 90))
    expect_silent(p2 <- procrustes_align(data, 180))
    expect_silent(p3 <- procrustes_align(data, 270))

    # plot(g[,1,1], g[,2,1])
    # plot(p0[,1,1], p0[,2,1])
    # plot(p1[,1,1], p1[,2,1])
    # plot(p2[,1,1], p2[,2,1])
    # plot(p3[,1,1], p3[,2,1])

    expect_equal(g, p0)
    expect_equivalent(p1, geomorph::rotate.coords(p0, "rotateC"))
    expect_equivalent(p2, geomorph::rotate.coords(p1, "rotateC"))
    expect_equivalent(p3, geomorph::rotate.coords(p2, "rotateC"))
  })
})

test_that("procrustes align", {
  stimlist <- faces("lisa")
  pr <- align(stimlist, pt1 = 63, pt2 = 81,
              procrustes = TRUE)

  # eye points all around the same place (low SD)
  p <- sapply(pr, function(x) {
    x$points[1:2, c(63, 81)] %>% as.vector()
  }) %>% t() %>%
    as.data.frame() %>%
    dplyr::summarise_all(sd)

  expect_true(p[[1]] < 5)
  expect_true(p[[2]] < 5)
  expect_true(p[[3]] < 5)
  expect_true(p[[4]] < 5)
})
