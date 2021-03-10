email <- Sys.getenv("WEBMORPH_EMAIL")
password <- Sys.getenv("WEBMORPH_PASSWORD")

test_that("errors and warning", {
  skip_on_cran()

  expect_message(login(email, password),
                 "Logged in as user 1", fixed = TRUE)
  expect_message(logout(), "Logged out", fixed = TRUE)

  expect_error(login(email, "bad_password"),
               "<li>The password is incorrect.</li>", fixed = TRUE)
  expect_error(login("nope", "bad_password"),
               "<li>The username is incorrect.</li>", fixed = TRUE)

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
