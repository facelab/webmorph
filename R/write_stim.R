#' Write tems and images to files
#'
#' @param stimlist list of class webmorph_list
#' @param dir Directory to save to
#' @param prefix Prefix to add to file names
#'
#' @return list of saved paths
#' @export
#'
write_stim <- function(stimlist, dir = ".", prefix = "") {
  stimlist <- assert_webmorph(stimlist)

  # make dir if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  paths <- mapply(function(stim, name) {
    # save images
    # TODO: set save type
    if (!is.null(stim$img)) {
      ext <- "jpg"
      imgpath <- file.path(dir, paste0(prefix, name, ".", ext))
      magick::image_write(stim$img, path = imgpath)
    } else {
      imgpath <- NULL
    }

    # save templates
    if (!is.null(stim$points)) {
      tem_txt <- list()

      # add points
      tem_txt <- c(tem_txt, dim(stim$points)[[2]])
      pts <- apply(stim$points, 2, paste, collapse = "\t")
      tem_txt <- c(tem_txt, pts)

      # add lines
      if (!is.null(stim$lines)) {
        tem_txt <- c(tem_txt, length(stim$lines))
        for (i in seq_along(stim$lines)) {
          tem_txt <- c(tem_txt, list(
            as.integer(stim$closed[[i]]),
            length(stim$lines[[i]]),
            paste(stim$lines[[i]], collapse = " ")
          ))
        }
      }

      tem_txt <- paste(tem_txt, collapse = "\n")
      tempath <- file.path(dir, paste0(prefix, name, ".tem"))
      write(tem_txt, tempath)
    } else {
      tempath <- NULL
    }

    list(tem = tempath,
         img = imgpath)
  }, stimlist, names(stimlist) %||% seq_along(stimlist))

  invisible(paths)
}
