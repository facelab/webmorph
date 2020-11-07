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
#' path <- system.file("extdata/demo", package = "webmorph")
#' tems <- read_tem(path)
#'
#' @export
#'
read_tem <- function (path, pattern = "*.tem",
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
  tems <- lapply(temfiles, function(temfile) {
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

    # create tem list ----
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
        tem$img <- file.path(dirname(temfile),
                         imgpath[[1]]) %>%
          magick::image_read()
        tem$imgpath = imgpath[[1]]
        img_info <- magick::image_info(tem$img)
        tem$width <- img_info$width
        tem$height <- img_info$height
      }
    }

    class(tem) <- c("webmorph_tem", "list")
    tem
  })

  args <- list(...)
  breaks <- ifelse(is.null(args$breaks),
                   "/", args$breaks)
  remove_ext <- ifelse(is.null(args$remove_ext),
                       TRUE, args$remove_ext)
  unames <- list.files(path, pattern=pattern) %>%
    unique_names(breaks, remove_ext)

  tems <- lapply(seq_along(tems), function(i) {
    tems[[i]]$name <- unames[[i]]
    tems[[i]]
  })

  names(tems) <- unames

  tems
}

#' Print webmorph templates
#'
#' @param x a list of class webmorph_tem
#' @param ... arguments passed to or from other methods
#'
#' @return prints the template info
#' @export
#'
print.webmorph_tem <- function(x, ...) {
  sprintf("%s [%i points, %i lines]",
          x$name,
          length(x$points),
          length(x$lines)
  ) %>% cat()

  invisible(x)
}


#' Plot webmorph template
#'
#' @param x webmorph_tem list
#' @param y omitted
#' @param ... Arguments to be passed to ggplot2 (width, height, pt.color, pt.size, pt.shape, bg.color, bg.fill)
#'
#' @return plot
#' @export
#'
plot.webmorph_tem <- function(x, y, ...) {

  points <- x$points %>%
    t() %>%
    as.data.frame()
  names(points) <- c("x", "y")

  # check args ----
  arg <- list(...)

  width <- arg$width %||% NULL
  if (isTRUE(arg$image)) width <- x$width %||% NULL
  xlim <- if (!is.null(width)) c(0, width)

  height <- arg$height %||% NULL
  if (isTRUE(arg$image)) height <- x$height %||% NULL
  ylim <- if (!is.null(height)) c(0, height)

  pt.size <-  arg$pt.size %||% arg$size %||% 1

  pt.shape <- arg$pt.shape %||% arg$shape %||% 3

  # point colour
  pt.color <- arg$pt.colour %||%
              arg$pt.color %||%
              arg$colour %||%
              arg$color %||%
              "#00AA00"
  points$color <- as.factor(pt.color)

  bg.color <- arg$bg.colour %||%
              arg$bg.color %||%
              "black"

  bg.fill <- arg$bg.fill %||% "transparent"

  # set up plot ----
  pt.aes <- ggplot2::aes(x, y, color = color)
  pborder <- ggplot2::element_rect(
    color = bg.color, fill = bg.fill)

  # plot setup ----
  g <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(xlim = xlim,
                         ylim = ylim) +
    ggplot2::scale_color_manual(values = levels(points$color)) +
    ggplot2::theme(legend.position = "none",
                   plot.background = pborder,
                   plot.margin = ggplot2::margin(0,0,0,0, "cm"))

  # add image ----
  if (isTRUE(arg$image)) {
    i <- grid::rasterGrob(x$img, interpolate = FALSE)
    g <- g + ggplot2::annotation_custom(
      i, 0, width, -height, 0)
  } else {

  }

  # add points ----
  g <- g +
    ggplot2::geom_point(data = points,
                        mapping = pt.aes,
                        size = pt.size,
                        shape = pt.shape)

  g
}


