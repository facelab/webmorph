#' Plot webmorph_list
#'
#' @param x webmorph_list
#' @param y omitted
#' @param ... Arguments to be passed to \code{\link{plot_fig}}
#'
#' @return plot
#' @export
#'
#' @examples
#' plot(faces("test"))
#'
plot.webmorph_list <- function(x, y, ...) {
  plot_fig(x, ...)
}

#' Plot webmorph_stim
#'
#' @param x webmorph_stim
#' @param y omitted
#' @param ... Arguments to be passed to \code{\link{plot_stim}}
#'
#' @return ggplot
#' @export
#'
#' @examples
#' plot(faces("test")[[1]])

plot.webmorph_stim <- function(x, y, ...) {
  plot_stim(x, ...)
}



#' Plot webmorph_list
#'
#' @param stimlist webmorph_list
#' @param width width of each image (in inches)
#' @param height width of each image (in inches)
#' @param filename path to save file to
#' @param units units for width, height, and margin
#' @param bc.color background colour of plot
#' @param nrow number of rows in the figure
#' @param ncol number of columns in the figure
#' @param ... Arguments to be passed to plot_stim
#'
#' @return plot
#' @export
#'
#' @examples
#' faces("test") %>% plot(pt.plot = TRUE)
#'
plot_fig <- function(stimlist, width = NA, height = NA,
                     filename = NULL,
                     units = c("in", "cm", "mm"),
                     bg.color = "white",
                     nrow = NULL, ncol = NULL,
                     ...) {
  stimlist <- assert_webmorph(stimlist)
  arg <- list(...)

  # get all dots to stimlist length
  arg$label.text <- arg$labels %||% names(stimlist) %||% NULL
  arg$labels <- NULL
  arg$units <- match.arg(units)
  dots <- lapply(arg, rep_len, length(stimlist))
  # rename if img.width and img.height are explicitly set
  dots$width <- dots$img.width
  dots$height <- dots$img.height

  plots <- lapply(seq_along(stimlist), function(i) {
    subdots <- lapply(dots, `[[`, i)
    # replace array items
    subdots$border.width <- arg$border.width
    do.call(plot_stim, c(list(stim = stimlist[[i]]), subdots))
  })

  n <- length(plots)
  if (is.null(nrow) && is.null(ncol)) {
    if (n < 6) {
      nrow <- 1
      ncol <- n
    } else {
      nrow <- ceiling(sqrt(n))
      ncol <- ceiling(n / nrow)
    }
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }

  # make plot grid ----
  margin <- (arg$margin %||% 0.1)/2
  p <- cowplot::plot_grid(plotlist = plots, nrow = nrow, ncol = ncol) +
    theme(plot.background = element_rect(fill = bg.color, colour = NA),
          plot.margin = ggplot2::margin(margin, margin, margin, margin, match.arg(units)))

  if (!is.na(width) || !is.na(height)) {
    filename <- filename %||% ifelse(
      isTRUE(getOption('knitr.in.progress')),
      knitr::fig_path("png"),
      tempfile(fileext = ".png")
    )
    ggplot2::ggsave(filename, p,
                    width = width,
                    height = height,
                    units = match.arg(units))
    knitr::include_graphics(filename)
  } else {
    p
  }
}


#' Get Control Points for Bezier Curves
#'
#' @param lpath line coordinates
#' @param i index
#'
#' @return data frame of 4 x and y coordinates
#' @keywords internal
#'
getControlPoints <- function(lpath, i = 1) {
  x0 <- lpath$x[i]
  y0 <- lpath$y[i]
  x1 <- lpath$x[i+1]
  y1 <- lpath$y[i+1]
  x2 <- lpath$x[i+2]
  y2 <- lpath$y[i+2]

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


#' Plot webmorph stimulus
#'
#' Arguments for plot characteristics can be:
#'
#' Points: pt.color, pt.size, pt.shape,
#' Lines: line.color, line.alpha,
#' Border: border.color, border.width,
#' Label: label.text, label.color, label.alpha,
#'        label.size, label.family, label.fontface,
#'        label.x, label.y, hjust, vjust, label.angle
#'
#' @param stim webmorph_stim
#' @param img.plot Whether to include the image in the plot
#' @param pt.plot Whether to include delineation points in the plot
#' @param line.plot Whether to include lines in the plot
#' @param width plot width
#' @param height plot height
#' @param label.position vertical (top, middle, bottom) and horizontal (left, center, right) position of the label
#' @param ... Arguments controlling the plot characteristics
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
plot_stim <- function(stim, img.plot = TRUE, pt.plot = FALSE, line.plot = FALSE,
                      width = NULL, height = NULL, label.position = "top center",
                      ...) {
  arg <- list(...)

  # visibility
  if (is.null(stim$img)) img.plot <- FALSE

  # dimensions ----
  if (isTRUE(img.plot)) width <- width %||% stim$width %||% NULL
  if (length(width)==1) {
    xlim <- c(0, width)
  } else if (length(width)==2){
    xlim <- width
  } else {
    xlim <- NULL
  }

  if (isTRUE(img.plot)) height <- height %||% stim$height %||% NULL
  if (length(height)==1) {
    ylim <- c(0, height)
  } else if (length(height)==2){
    ylim <- height
  } else {
    ylim <- NULL
  }

  # border.width ----
  bw <- (arg$border.width %||% 0) %>%
    sapply(function(b) {
      ifelse(b < 1, b * (stim$width %||% 200), b)
    })
  # deal with len != 4 (t, r, b , l)
  if (length(bw) == 3) bw[[4]] <- bw[[2]]
  bw <- rep_len(bw, 4)
  # add borders to limits
  if (!is.null(xlim)) xlim <- xlim + c(-bw[[4]], +bw[[2]])
  if (!is.null(ylim)) ylim <- ylim + c(-bw[[3]], +bw[[1]])

  if (!is.null(stim$points)) {
    points <- stim$points %>%
      t() %>%
      as.data.frame()
    names(points) <- c("x", "y")
    points$y <- (height %||% 0) - points$y
    points$i <- 0:(nrow(points)-1)

    # point colour
    pt.color <- arg$pt.colour %||%
      arg$pt.color %||%
      arg$colour %||%
      arg$color %||%
      "#00AA00"
    points$color <- as.factor(pt.color)

    color_values <- levels(points$color)
  } else {
    # ignore any pt or line plotting
    pt.plot <- FALSE
    line.plot <- FALSE
    color_values <- "black"
  }

  pt.size <-  arg$pt.size %||% arg$size %||% 1
  pt.shape <- arg$pt.shape %||% arg$shape %||% 3
  pt.alpha <- arg$pt.alpha  %||% arg$alpha %||% 1
  font.size <- arg$font.size %||% 3
  line.alpha <- arg$line.alpha  %||% arg$alpha %||% 0.5

  # line colour
  line.color <- arg$line.colour %||%
    arg$line.color %||%
    arg$colour %||%
    arg$color %||%
    "#FFFF66"

  border.color <- arg$border.colour %||% arg$border.color %||% "transparent"
  margin <- arg$margin %||% 0.1
  units <- arg$units %||% "in"

  # set up plot ----
  pt.aes <- ggplot2::aes(x, y, color = pt.color)
  text.aes <- ggplot2::aes(x, y, color = pt.color, label = i)
  pborder <- ggplot2::element_rect(color = border.color, fill = border.color)

  # plot setup ----
  g <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    #ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed(xlim = xlim,
                         ylim = ylim) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::theme(legend.position = "none",
                   panel.background = pborder,
                   plot.margin = ggplot2::margin(margin, margin, margin, margin, units)) +
    ggplot2::scale_x_continuous(expand = c(0,0,0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0,0,0))

  # add image ----
  if (isTRUE(img.plot)) {
    i <- grid::rasterGrob(stim$img, interpolate = FALSE)
    g <- g + ggplot2::annotation_custom(
      i, 0, width, 0, height)
  }

  # add label ----
  if (!is.null(arg$label.text)) {
    label.color <- arg$label.colour %||% arg$label.color %||%
                   arg$colour %||% arg$color %||%"black"
    label.size <- arg$label.size  %||% arg$size %||% 5
    label.alpha <- arg$label.alpha %||% arg$alpha %||% 1
    label.angle <- arg$label.angle %||% 0
    label.family <- arg$label.family %||% arg$family %||% "sans"
    label.fontface <- arg$label.fontface %||% arg$fontface %||% "plain"
    label.lineheight <- arg$label.lineheight %||% arg$lineheight %||% 1.2

    # label position
    lp <- strsplit(label.position, " ")[[1]]
    vjust <- switch(lp[1],
                    bottom = 0,
                    center = 0.5,
                    centre = 0.5,
                    middle = 0.5,
                    top = 1,
                    1)
    hjust <- switch(lp[2],
                    left = 0,
                    center = 0.5,
                    centre = 0.5,
                    middle = 0.5,
                    right = 1,
                    0.5)
    label.y <- switch(lp[1],
                    bottom = 0.05,
                    center = 0.5,
                    centre = 0.5,
                    middle = 0.5,
                    top = 0.95,
                    0.95)
    label.x <- switch(lp[2],
                    left = 0.05,
                    center = 0.5,
                    centre = 0.5,
                    middle = 0.5,
                    right = 0.95,
                    0.5)

    g <- g + ggplot2::annotate("text",
                               label = arg$label.text,
                               size = label.size,
                               colour = label.color,
                               alpha = label.alpha,
                               angle = label.angle,
                               family = label.family,
                               fontface = label.fontface,
                               lineheight = label.lineheight,
                               # explicit values override label.position
                               x = (arg$label.x %||% label.x) * width,
                               y = (arg$label.y %||% label.y) * height,
                               hjust = arg$hjust %||% hjust,
                               vjust = arg$vjust %||% vjust)
  }

  # add lines ----
  if (line.plot == "bezier") {
    # bezier curves; still really buggy
    for (i in seq_along(stim$lines)) {
      l <- stim$lines[[i]]
      lpath <- points[(l+1), ]
      if (length(l) > 2) {
        for (j in 1:(length(l)-2)) {
          g <- g + ggforce::geom_bezier(
            data = getControlPoints(lpath, j),
            mapping = ggplot2::aes(x, y),
            # lets line.color be a vector of colours
            colour = line.color[[j%%length(line.color)+1]],
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
  } else if (isTRUE(line.plot)) {
    # straight lines
    for (i in seq_along(stim$lines)) {
      l <- stim$lines[[i]]
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



