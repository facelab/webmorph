#' Plot webmorph stimulus
#'
#' @param x webmorph_stim list
#' @param y omitted
#' @param ... Arguments controlling the plot characteristics (img.plot, pt.plot, line.plot, width, height, pt.color, pt.size, pt.shape, line.color, line.alpha, border.color, border.width)
#'
#' @return plot
#' @export
#'
#' @examples
#' # default values
#' plot(faces("test")[[1]],
#'      width = NULL,  # get from image or tem
#'      height = NULL, # get from image or tem
#'      img.plot = TRUE,
#'      pt.plot = FALSE,
#'      pt.color = "#00AA00",
#'      pt.size = 1,
#'      pt.shape = 3,
#'      pt.alpha = 1,
#'      line.plot = FALSE, # T, F, or "bezier"
#'      line.color = "#FFFF66",
#'      line.alpha = 0.5
#' )
#'
#' #custom settings
#' plot(faces("test")[[1]],
#'      img.plot = FALSE,
#'      pt.plot = TRUE,
#'      pt.color = "dodgerblue",
#'      pt.shape = 1,
#'      pt.alpha = 0.5,
#'      line.plot = TRUE,
#'      border.color = "black",
#'      border.width = 10 # in pixels
#' )
#'
plot.webmorph_stim <- function(x, y, ...) {

  # check args ----
  arg <- list(...)

  # visibility
  img.plot <- arg$img.plot %||% TRUE
  if (is.null(x$img)) img.plot <- FALSE
  pt.plot <- arg$pt.plot %||% FALSE
  line.plot <- arg$line.plot %||% FALSE

  # dimensions
  width <- arg$width %||% NULL
  if (isTRUE(img.plot)) width <-  x$width %||% NULL
  if (length(width)==1) {
    xlim <- c(0, width)
  } else if (length(width)==2){
    xlim <- width
  } else {
    xlim <- NULL
  }

  height <- arg$height %||% NULL
  if (isTRUE(img.plot)) height <-  x$height %||% NULL
  if (length(height)==1) {
    ylim <- c(0, height)
  } else if (length(height)==2){
    ylim <- height
  } else {
    ylim <- NULL
  }

  if (!is.null(x$points)) {
    points <- x$points %>%
      t() %>%
      as.data.frame()
    names(points) <- c("x", "y")
    points$y <- (height %||% 0) - points$y
    points$i <- 0:(nrow(points)-1)
  } else {
    # ignore any pt or line plotting
    pt.plot <- FALSE
    line.plot <- FALSE
  }

  pt.size <-  arg$pt.size %||% arg$size %||% 1
  pt.shape <- arg$pt.shape %||% arg$shape %||% 3
  pt.alpha <- arg$pt.alpha  %||% arg$alpha %||% 1
  font.size <- arg$font.size %||% 3
  line.alpha <- arg$line.alpha  %||% arg$alpha %||% 0.5

  # point colour
  pt.color <- arg$pt.colour %||%
    arg$pt.color %||%
    arg$colour %||%
    arg$color %||%
    "#00AA00"
  points$color <- as.factor(pt.color)

  # line colour
  line.color <- arg$line.colour %||%
    arg$line.color %||%
    arg$colour %||%
    arg$color %||%
    "#FFFF66"

  border.color <- arg$border.colour %||% arg$border.color %||% "transparent"

  # set up plot ----
  pt.aes <- ggplot2::aes(x, y, color = pt.color)
  text.aes <- ggplot2::aes(x, y, color = pt.color, label = i)
  pborder <- ggplot2::element_rect(color = border.color, fill = border.color)
  border <- ggplot2::expansion(add = arg$border.width %||% 0)

  # plot setup ----
  g <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    #ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(xlim = xlim,
                         ylim = ylim) +
    ggplot2::scale_color_manual(values = levels(points$color)) +
    ggplot2::theme(legend.position = "none",
                   panel.background = pborder,
                   plot.margin = ggplot2::margin(0,0,0,0, "pt")) +
    ggplot2::scale_x_continuous(expand = border) +
    ggplot2::scale_y_continuous(expand = border)

  # add image ----
  if (isTRUE(img.plot)) {
    i <- grid::rasterGrob(x$img, interpolate = FALSE)
    g <- g + ggplot2::annotation_custom(
      i, 0, width, 0, height)
  }

  # add label ----
  if (!is.null(arg$labels)) {
    label_x <- (arg$label_x %||% 0.5) * width
    label_y <- (arg$label_y %||% 0.95) * height
    label_color <- arg$label_colour %||% "black"
    label_size <- arg$label_size %||% 5

    g <- g + ggplot2::annotate("text", x = label_x, y = label_y,
                          size = label_size, colour = label_color,
                          label = arg$labels)
  }

  # add lines ----
  if (!isTRUE(line.plot)) {
    # don't plot lines
  } else if (line.plot == "bezier") {
    # bezier curves; still really buggy
    for (i in seq_along(x$lines)) {
      l <- x$lines[[i]]
      lpath <- points[(l+1), ]
      if (length(l) > 2) {
        for (i in 1:(length(l)-2)) {
          g <- g + ggforce::geom_bezier(
            data = getControlPoints(lpath, i),
            mapping = ggplot2::aes(x, y),
            # lets line.color be a vector of colours
            colour = line.color[[i%%length(line.color)+1]],
            alpha = line.alpha
          )
        }
      } else if (length(l) == 2) {
        g <- g + ggplot2::geom_path(
          data = lpath,
          mapping = ggplot2::aes(x, y),
          # lets line.color be a vector of colours
          colour = line.color[[i%%length(line.color)+1]],
          alpha = line.alpha
        )
      }
    }
  } else {
    # straight lines
    for (i in seq_along(x$lines)) {
      l <- x$lines[[i]]
      if (length(l) > 1) {
        lpath <- points[(l+1), ]
        g <- g + ggplot2::geom_path(
          data = lpath,
          mapping = ggplot2::aes(x, y),
          # lets line.color be a vector of colours
          colour = line.color[[i%%length(line.color)+1]],
          alpha = line.alpha
        )
      }
    }
  }

  # add points (over lines) ----
  if (!isTRUE(pt.plot)) {
    # don't plot points
  } else if (pt.shape == "index") {
    # plot index numbers
    g <- g + ggplot2::geom_text(
      data = points,
      mapping = text.aes,
      size = font.size
    )
  } else {
    # plot tem points
    g <- g + ggplot2::geom_point(
      data = points,
      mapping = pt.aes,
      size = pt.size,
      shape = pt.shape,
      alpha = pt.alpha
    )
  }

  g
}


#' Plot webmorph_list
#'
#' @param x webmorph_list
#' @param y omitted
#' @param ... Arguments to be passed to ggplot2 (width, height, pt.color, pt.size, pt.shape, bg.color, bg.fill) or cowplot::plot_grid (label_x, label_y, label_size, label_fontfamily, label_fontface, label_colour, hjust, vjust, nrow, ncol)
#'
#' @return plot
#' @export
#'
#' @examples
#' faces("test") %>% plot(pt.plot = TRUE)
#'
plot.webmorph_list <- function(x, y, ...) {
  arg <- list(...)

  if (!"border.width" %in% names(arg)) {
    arg$border.width = 10
  }

  # get all dots to x length
  dots <- lapply(arg, rep_len, length(x))
  labels <- rep_len(arg$labels %||% names(x) %||% NULL, length(x))

  plots <- lapply(seq_along(x), function(i) {
    subdots <- lapply(dots, `[[`, i)
    subdots$labels <- labels[i]
    do.call(plot, c(list(x = x[[i]]), subdots))
  })

  # n <- length(plots)
  # nrow <- arg$nrow %||% NULL
  # ncol <- arg$ncol %||% NULL
  # if (is.null(nrow) && is.null(ncol)) {
  #   nrow <- ceiling(sqrt(n))
  #   ncol <- ceiling(n / nrow)
  # } else if (is.null(ncol)) {
  #   ncol <- ceiling(n / nrow)
  # } else if (is.null(nrow)) {
  #   nrow <- ceiling(n / ncol)
  # }

  # w <- arg$fig.width %||% (x[[1]]$width * nrow)
  # h <- arg$fig.height %||% (x[[1]]$height * ncol)
  # bg.color <- arg$bg.color %||% "transparent"
  # fig <- magick::image_graph(w, h, bg.color)

  # pass only valid plot_grid arguments on
  valid_args <- args(cowplot::plot_grid) %>% as.list() %>% names()
  cpg_args <- arg[names(arg) %in% valid_args]
  cpg_args$plotlist <- plots
  cpg_args$labels = "" # no labels
  #cpg_args$labels <- arg$labels %||% names(x) %||% NULL
  do.call(cowplot::plot_grid, cpg_args)

  # dev.off()
  #
  # i <- grid::rasterGrob(fig, interpolate = FALSE)
  #
  # ggplot2::ggplot() +
  #   ggplot2::theme_void() +
  #   ggplot2::annotation_custom(i)
}


getControlPoints <- function(lpath, i = 1) {
  x0 <- lpath[i, ]$x
  y0 <- lpath[i, ]$y
  x1 <- lpath[i+1, ]$x
  y1 <- lpath[i+1, ]$y
  x2 <- lpath[i+2, ]$x
  y2 <- lpath[i+2, ]$y

  t = 0.3
  d01 = sqrt(`^`(x1 - x0, 2) + `^`(y1 - y0, 2))
  d12 = sqrt(`^`(x2 - x1, 2) + `^`(y2 - y1, 2))
  fa = t * d01 / (d01 + d12) # scaling factor for triangle Ta
  fb = t * d12 / (d01 + d12) # ditto for Tb, simplifies to fb=t-fa

  p1x = x1 - fa * (x2 - x0) # x2-x0 is the width of triangle T
  p1y = y1 - fa * (y2 - y0) # y2-y0 is the height of T
  p2x = x1 + fb * (x2 - x0)
  p2y = y1 + fb * (y2 - y0)

  data.frame(
    x = c(x0, p1x, p2x, x2),
    y = c(y0, p1y, p2y, y2)
  )
}

