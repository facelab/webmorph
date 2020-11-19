#' Read stimuli
#'
#' @param path Path to directory containing image and/or template files (or a single file path)
#' @param pattern Pattern to use to search for files
#' @param ... further arguments to pass on to other functions, e.g., breaks argument for unique_names()
#'
#' @return a list of lists with class webmorph_tem
#'
#' @examples
#' path <- system.file("extdata/composite", package = "webmorph")
#' stimlist <- read_stim(path)
#' stimlist
#'
#' @export
#'
read_stim <- function (path, pattern = NULL, ...) {
  # get paths to temfiles ----
  if (dir.exists(path)) {
    files <- list.files(path, pattern, full.names = TRUE)
  } else if (file.exists(path)) {
    files <- path
  } else {
    stop(path, " is neither a directory nor a file")
  }

  i <- grepl("\\.(jpg|jpeg|gif|png|bmp)$",
             files, ignore.case = TRUE)
  imgfiles <- files[i]
  t <- grepl("\\.tem$", files, ignore.case = TRUE)
  temfiles <- files[t]


  # process tems ----
  temlist <- lapply(temfiles, function(temfile) {
    # read and clean  ----
    tem_txt <- readLines(temfile) %>%
      trimws() %>%
      `[`(. != "") %>% # get rid of blank lines
      `[`(substr(., 1, 1) != "#") # get rid of comments

    # process points ----
    npoints <- as.integer(tem_txt[[1]])
    points <- tem_txt[2:(npoints+1)] %>%
      strsplit("\t", fixed = TRUE) %>%
      sapply(as.numeric)

    # process lines ----
    nlines <- as.integer(tem_txt[[npoints+2]])
    x <- (npoints+3):length(tem_txt)
    line_rows <- tem_txt[x] %>%
      matrix(nrow = 3)

    lines <- line_rows[3, ] %>%
      strsplit("\\s+") %>%
      sapply(as.integer)

    # TODO: apply webmorph rules for open/closed lines
    closed <- line_rows[1, ] %>%
      as.integer() %>%
      as.logical()

    # create tem object ----
    tem <- list(
      tempath = temfile,
      points = points,
      lines = lines,
      closed = closed
    )

    class(tem) <- "webmorph_tem"
    tem
  })

  # load images ---
  imglist <- lapply(imgfiles, function(imgfile) {
      img <- list(
        img = magick::image_read(imgfile),
        imgpath = imgfile
      )
      # read attributes
      attr <- magick::image_attributes(img$img)
      wm_desc <- attr$value[attr$property == "exif:ImageDescription"]
      if (length(wm_desc) > 0) {
        img$desc <- jsonlite::fromJSON(wm_desc,
                                       simplifyDataFrame = FALSE,
                                       simplifyVector = TRUE)
      }
      # TODO: read embedded tem?

      img$img <- magick::image_strip(img$img)
      img_info <- magick::image_info(img$img)
      img$width <- img_info$width
      img$height <- img_info$height

      img
  })

  # join image and tem lists ----
  df_img <- data.frame(
    img_i = seq_along(imgfiles),
    path = tools::file_path_sans_ext(imgfiles)
  )

  df_tem <- data.frame(
    tem_i = seq_along(temfiles),
    path = tools::file_path_sans_ext(temfiles)
  )

  df_full <- dplyr::full_join(df_img, df_tem,by = "path")

  stimlist <- mapply(function(img_i, tem_i) {
    x <- c(imglist[[img_i]], temlist[[tem_i]])
    class(x) <- "webmorph_stim"
    x
  }, df_full$img_i, df_full$tem_i, SIMPLIFY = FALSE)

  # assign unique names ----
  args <- list(...)
  breaks <- ifelse(is.null(args$breaks), "/", args$breaks)
  names(stimlist) <- unique_names(df_full$path, breaks)

  class(stimlist) <- "webmorph_list"

  stimlist
}

