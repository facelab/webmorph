#' Webmorph Faces
#'
#' A convenience function to get a demo temlist
#'
#' @param dir the image directory in extdata to get images from
#' @param pattern defaults to all tem files
#' @param ... Other arguments to pass on to `read_tem()`
#'
#' @return webmorph_temlist
#' @export
#'
#' @examples
#' faces("test") %>% plot()
#'
faces <- function(dir = c("composite", "test", "london"),
                     pattern = "\\.tem$", ...) {
  dir <- match.arg(dir)
  path <- system.file(file.path("extdata", dir),
                      package = "webmorph")
  temlist <- read_tem(path, pattern, ...)

  invisible(temlist)
}
