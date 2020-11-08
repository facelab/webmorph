#' Write tems and images to files
#'
#' @param temlist list of webmorph templates
#' @param dir Directory to save to
#' @param prefix Prefix to add to file names
#' @param suffix Suffix to add to file names
#'
#' @return list of saved paths
#' @export
#'
write_tem <- function(temlist, dir = ".", prefix = "", suffix = "") {
  # make dir if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  paths <- lapply(temlist, function(tem) {
    # save images
    info <- magick::image_info(tem$img)
    imgpath <- file.path(dir, paste0(prefix, tem$imgpath, suffix))
    magick::image_write(tem$img, path = imgpath)

    # save templates
    tem_txt <- list()
    tem_txt <- c(tem_txt, dim(tem$points)[[2]])
    pts <- apply(tem$points, 2, paste, collapse = "\t")
    tem_txt <- c(tem_txt, pts)

    # add lines
    tem_txt <- c(tem_txt, length(tem$lines))
    for (i in seq_along(tem$lines)) {
      tem_txt <- c(tem_txt, list(
        as.integer(tem$closed[[i]]),
        length(tem$lines[[i]]),
        paste(tem$lines[[i]], collapse = " ")
      ))
    }

    tem_txt <- paste(tem_txt, collapse = "\n")
    tempath <- gsub("\\..{0,4}$", ".tem", imgpath)
    write(tem_txt, tempath)

    list(tem = tempath,
         img = imgpath)
  })

  invisible(paths)
}
