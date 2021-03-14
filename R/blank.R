#' Make blank images
#'
#' @param n the number of images to return
#' @param width width of the images
#' @param height height of the images
#' @param color background colour of the images
#' @param names names of the images
#'
#' @return webmorph_list with labelled images
#' @export
#'
#' @examples
#' stimlist <- blank(5, 400, 100)
blank <- function(n = 1, width = 100, height = 100, color = "white", names = "img") {
  color <- rep_len(color, n)
  if (length(names) < n) names <- rep_len(names, n)
  names(color) <- paste0(names, 1:n)
  stimlist <- lapply(color, function(color) {
    stim <- list(
      img = magick::image_blank(width, height, color),
      desc = "Blank",
      width = width,
      height = height
    )
    class(stim) <- "webmorph_stim"
    stim
  })

  class(stimlist) <- "webmorph_list"

  stimlist
}
