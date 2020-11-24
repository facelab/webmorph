#' Mirror templates and images
#'
#' Use the function `frl_sym()` to get the sym_map for the standard webmorph template. If sym_map is omitted, images and templates will be fully reversed (e.g., if point 1 is the left eye in the original image, it will be the right eye in the mirrored image).
#'
#' @param stimlist list of class webmorph_list
#' @param sym_map list of corresponding template points
#' @param axis vertical or horizontal axis of mirroring
#'
#' @return webmorph_list with mirrored images and tems
#' @export
#'
#' @examples
#' o <- faces("london", "001_03")
#' m <- mirror(o, frl_sym())
#'
#' c(o, m) %>%
#'   plot(pt.plot = TRUE, labels = c("original", "mirrored"))
#'
mirror <- function(stimlist, sym_map = NULL, axis = "vertical") {
  stimlist <- assert_webmorph(stimlist)

  for (i in seq_along(stimlist)) {
    cx <- (stimlist[[i]]$width-1)/2
    cy <- (stimlist[[i]]$height-1)/2
    p <- stimlist[[i]]$points

    if (axis == "horizontal") {
      stimlist[[i]]$img <- magick::image_flip(stimlist[[i]]$img)
      # flip y points
      stimlist[[i]]$points <- (p - c(0, cy)) * c(1, -1) +c(0, cy)
    } else {
      stimlist[[i]]$img <- magick::image_flop(stimlist[[i]]$img)
      # flop x points
      stimlist[[i]]$points <- (p - c(cx, 0)) * c(-1, 1) +c(cx, 0)
    }

    # remember sym_map is 0-based
    n_pt <- ncol(stimlist[[i]]$points) - 1
    if (!is.null(sym_map) &&
        all(sym_map %in% 0:n_pt)) {
      stimlist[[i]]$points <- stimlist[[i]]$points[, sym_map+1]
    }
  }

  invisible(stimlist)
}

#' FRL Symmetry Map
#'
#' @return vector of symmetric matching points
#' @export
#'
#' @examples
#' frl_sym()
#'
frl_sym <- function() {
  # 0-based for compatibility with webmorph
  # keep consistent with frl_features()
  c(
    1, 0, 10, 17, 16, 15, 14, 13, 12, 11,
    2, 9, 8, 7, 6, 5, 4, 3, 27, 26, 25,
    24, 23, 22, 21, 20, 19, 18, 33, 32, 31,
    30, 29, 28, 43, 42, 41, 40, 39, 38, 37,
    36, 35, 34, 49, 48, 47, 46, 45, 44, 56,
    57, 58, 59, 60, 55, 50, 51, 52, 53, 54,
    66, 67, 68, 69, 70, 61, 62, 63, 64, 65,
    82, 81, 80, 79, 78, 77, 76, 75, 74, 73,
    72, 71, 86, 85, 84, 83, 93, 92, 91, 90,
    89, 88, 87, 98, 97, 96, 95, 94, 103, 102,
    101, 100, 99, 108, 107, 106, 105, 104, 112, 113,
    114, 109, 110, 111, 120, 121, 122, 123, 124, 115,
    116, 117, 118, 119, 133, 132, 131, 130, 129, 128,
    127, 126, 125, 144, 143, 142, 141, 140, 139, 138,
    137, 136, 135, 134, 157, 156, 155, 154, 153, 152,
    151, 150, 149, 148, 147, 146, 145, 161, 162, 163,
    158, 159, 160, 167, 168, 169, 164, 165, 166, 172,
    173, 170, 171, 174, 175, 178, 177, 176, 182, 181,
    180, 179, 184, 183, 187, 188, 185, 186
  )
}


