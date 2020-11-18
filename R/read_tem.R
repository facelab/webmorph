#' Read Webmorph template file
#'
#' @param path Path to directory containing template files (or a single template file)
#' @param pattern Pattern to use to search for files
#' @param images Whether to include images
#' @param ... further arguments to pass on to other functions, e.g., breaks argument for unique_names()
#'
#' @return a list of lists with class webmorph_tem
#'
#' @examples
#' path <- system.file("extdata/composite", package = "webmorph")
#' temlist <- read_tem(path)
#'
#' @export
#'
read_tem <- function (path, pattern = "\\.tem$",
                      images = TRUE, ...) {
  # get paths to temfiles ----
  if (dir.exists(path)) {
    temfiles <- list.files(path,
                           pattern=pattern,
                           full.names = TRUE)
  } else if (file.exists(path)) {
    temfiles <- path
  }

  img_ext <- "\\\\\\.(jpg|jpeg|gif|png|bmp)$"

  # process each temfile ----
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

    closed <- line_rows[1, ] %>%
      as.integer() %>%
      as.logical()

    # create tem object ----
    tem <- list(
      name = temfile,
      dirpath = dirname(temfile),
      tempath = basename(temfile),
      points = points,
      lines = lines,
      closed = closed
    )

    # find corresponding image ---
    if (images) {
      bname <- basename(temfile) %>%
        gsub("\\.tem$", img_ext, .)
      imgpath <- list.files(dirname(temfile),
                            bname,
                            ignore.case = TRUE)

      if (length(imgpath) > 0) {
        img <- file.path(dirname(temfile), imgpath[[1]]) %>%
          magick::image_read()

        tem$imgpath = imgpath[[1]]
        #tem$img_attr <- magick::image_attributes(img)
        tem$img <- magick::image_strip(img) # strip for rotation
        img_info <- magick::image_info(img)
        tem$width <- img_info$width
        tem$height <- img_info$height
      }
    }

    class(tem) <- "webmorph_tem"
    tem
  })

  args <- list(...)
  breaks <- ifelse(is.null(args$breaks),
                   "/", args$breaks)
  remove_ext <- ifelse(is.null(args$remove_ext),
                       TRUE, args$remove_ext)
  if (dir.exists(path)) {
    unames <- list.files(path, pattern=pattern)
  } else {
    unames <- path
  }
  unames <- unique_names(unames, breaks, remove_ext)

  temlist <- lapply(seq_along(temlist), function(i) {
    temlist[[i]]$name <- unames[[i]]
    temlist[[i]]
  })

  names(temlist) <- unames
  class(temlist) <- "webmorph_temlist"

  temlist
}

