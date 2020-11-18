#' Plot webmorph template
#'
#' @param x webmorph_tem list
#' @param y omitted
#' @param ... Arguments to be passed to ggplot2 (width, height, pt.color, pt.size, pt.shape, bg.color, bg.fill)
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
#'      line.alpha = 0.5,
#'      font.size = 3,
#'      bf.fill = "transparent",
#'      bg.color = "transparent"
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
#'      bg.fill = "grey70",
#'      bg.color = "black"
#' )
#'
plot.webmorph_tem <- function(x, y, ...) {

  points <- x$points %>%
    t() %>%
    as.data.frame()
  names(points) <- c("x", "y")
  points$i <- 0:(nrow(points)-1)

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

  bg.fill <- arg$bg.fill %||% "transparent"
  bg.color <- arg$bg.colour %||% arg$bg.color %||% bg.fill

  # set up plot ----
  pt.aes <- ggplot2::aes(x, y, color = pt.color)
  text.aes <- ggplot2::aes(x, y, color = pt.color, label = i)
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
  if (isTRUE(img.plot)) {
    i <- grid::rasterGrob(x$img, interpolate = FALSE)
    g <- g + ggplot2::annotation_custom(
      i, 0, width, -height, 0)
  }

  # add lines ----
  if (line.plot == "bezier") {
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
  } else if (!isTRUE(line.plot)) {
    # don't plot lines
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


#' Plot webmorph template list
#'
#' @param x webmorph_temlist list
#' @param y omitted
#' @param ... Arguments to be passed to ggplot2 (width, height, pt.color, pt.size, pt.shape, bg.color, bg.fill)
#'
#' @return plot
#' @export
#'
#' @examples
#' faces("test") %>% plot(pt.plot = TRUE)
#'
plot.webmorph_temlist <- function(x, y, ...) {
  # get all dots to x length
  dots <- list(...)

  if (length(x) > 1) {
    dots <- lapply(dots ,rep, length.out = length(x))
  }

  plots <- lapply(seq_along(x), function(i) {
    subdots <- lapply(dots, `[[`, i)
    do.call(plot, c(list(x = x[[i]]), subdots))
  })

  # check args ----
  arg <- list(...)

  nrow <- arg$nrow %||% NULL
  ncol <- arg$ncol %||% NULL
  labels <- arg$labels %||%
    sapply(x, `[[`, "name")

  cowplot::plot_grid(plotlist = plots,
                     nrow = nrow,
                     ncol = ncol,
                     labels = labels)
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

