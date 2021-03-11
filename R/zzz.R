## set default options for webmorph_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.webmorph <- list(
    webmorph.connection = stdin(),
    webmorph.plot = TRUE,
    webmorph.verbose = TRUE
  )
  toset <- !(names(op.webmorph) %in% names(op))
  if(any(toset)) options(op.webmorph[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  paste(
    "\n************",
    "Welcome to webmorphR. For support and examples visit:",
    "https://facelab.github.io/webmorphR/",
    "************\n",
    sep = "\n"
  ) %>% packageStartupMessage()

  ## login if email and password are set
  if (Sys.getenv("WEBMORPH_EMAIL") != "" &&
      Sys.getenv("WEBMORPH_PASSWORD") != "") {
    login()
  }
}
