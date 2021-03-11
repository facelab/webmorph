email <- Sys.getenv("WEBMORPH_EMAIL")
password <- Sys.getenv("WEBMORPH_PASSWORD")

test_that("errors and warning", {
  skip_on_cran()

  expect_message(login(email, password),
                 "Logged in as user 1", fixed = TRUE)
  expect_message(logout(), "Logged out", fixed = TRUE)

  expect_error(login(email, "bad_password"),
               "The password is incorrect.", fixed = TRUE)
  expect_error(login("nope", "bad_password"),
               "The username is incorrect.", fixed = TRUE)

  yntbli <- "You need to be logged in to do this."
  expect_warning(logout(), yntbli, fixed = TRUE)
  expect_error(projListGet(), yntbli, fixed = TRUE)
  expect_error(projSet(1), yntbli, fixed = TRUE)
})

test_that("projects", {
  skip_on_cran()

  id <- login()
  expect_equal(id, 1)

  plist <- projListGet()
  expect_equal(plist$id[[1]], 1)
  expect_equal(plist$name[[1]], "Lisa DeBruine")

  p <- projSet(plist$id[[1]])
  expect_equal(p$project_id, 1)
  expect_equal(p$perm, "all")

  dir <- dirLoad(84877, "composites")

  expect_equal(dir[[1]], "84877/composites/_citation.txt")
  temp <- tempdir()
  newpaths <- fileDownload(dir, temp)

  expect_equal(length(newpaths), 21)
  expect_equal(readLines(newpaths[[1]])[1], "DeBruine, L. M. (2016). ")
  unlink(temp)
})

test_that("makeAvg", {
  skip_on_cran()

  login()
  dir <- dirLoad(84877, "lisa")

  f <- dir[grepl("lisa.\\.jpg", dir)]

  tex <- makeAvg(f, "test/tex")
  notex <- makeAvg(f, "test/notex", texture = FALSE)

  expect_true(file.exists("test/tex.jpg"))
  expect_true(file.exists("test/tex.tem"))
  expect_true(file.exists("test/notex.jpg"))
  expect_true(file.exists("test/notex.tem"))

  unlink("test", recursive = TRUE)
})

# getProjectID ----
test_that("getProjectID", {
  expect_error(getProjectID("abc"))
  expect_error(getProjectID(c("1/a", "2/b")))

  id <- getProjectID(c("1/a", "1/b", "1/c"))
  expect_equal(id, 1)
})

# fileUpload ----
test_that("fileUpload", {
  skip_on_cran()

  dirLoad(84877, "lisa") %>%
    fileDownload("test")

  f <- list.files("test", "lisa", full.names = T)
  u <- fileUpload(f, "84877/newtest/")

  u_exp <- sub("test/", "84877/newtest/", f)
  names(u_exp) <- f
  expect_equal(u, u_exp)

  u_check <- dirLoad(84877, "newtest")
  expect_equal(u_check, unname(u))

  d <- fileDelete(files = u[1:4])
  d_check <- dirLoad(84877, "newtest")
  expect_equal(d_check, unname(u[5:8]))

  expect_true(dirDelete("84877/newtest/"))
  expect_error(dirLoad(84877, "newtest"))

  unlink("test", recursive = TRUE)
})
