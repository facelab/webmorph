#' Webmorph Faces
#'
#' A convenience function to get a demo stimlist
#'
#' @param dir the directory in extdata to get files from
#' @param pattern defaults to all files
#' @param ... Other arguments to pass on to `read_tem()`
#'
#' @return webmorph_list
#' @export
#'
#' @examples
#' faces("test") %>% plot()
#'
faces <- function(dir = c("composite", "test", "london", "lisa"),
                     pattern = NULL, ...) {
  dir <- match.arg(dir)
  path <- system.file(file.path("extdata", dir),
                      package = "webmorph")
  stimlist <- read_stim(path, pattern, ...)

  invisible(stimlist)
}
