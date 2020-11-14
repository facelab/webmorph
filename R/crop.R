#' Crop images and templates
#'
#' Arguments width, height, x_off and y_off can bet set in pixels or proportions (between 0.0 and 2.0)
#'
#' @param temlist list of webmorph templates
#' @param width width of cropped image
#' @param height height of cropped image
#' @param x_off x-offset
#' @param y_off y-offset
#' @param fill background colour if cropping goes outside the original image
#'
#' @return temlist with cropped tems and/or images
#' @export
#'
#' @examples
#' path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
#' read_tem(path) %>%
#'   crop(.5, .5, .25, .25) %>%
#'   plot(image = TRUE)
#'
crop <- function(temlist, width = NULL, height = NULL,
                 x_off = 0, y_off = 0, fill = "white") {
  temlist <- check_temlist(temlist)

  for (i in seq_along(temlist)) {
    origw <- temlist[[i]]$width
    origh <- temlist[[i]]$height
    w <- width %||% origw
    h <- height %||% origh

    # handle percentages
    if (w <= 2) w <- w * origw
    if (h <= 2) h <- h * origh
    if (abs(x_off) <= 2) x_off <- x_off * origw
    if (abs(y_off) <= 2) y_off <- y_off * origh

    temlist[[i]]$width <- w
    temlist[[i]]$height <- h

    if (class(temlist[[i]]$img) == "magick-image") {
      # crop doesn't handle negative offsets well
      ga <- magick::geometry_area(
        width = min(w, origw),
        height = min(h, origh),
        x_off = max(0, x_off),
        y_off = max(0, y_off)
      )

      newimg <- magick::image_crop(
        image = temlist[[i]]$img,
        geometry = ga,
        gravity = "NorthWest"
      )

      # make background image with fill
      bg <- magick::image_blank(w, h, color = fill)
      offset <- magick::geometry_point(
        x = max(0, -x_off),
        y = max(0, -y_off)
      )

      temlist[[i]]$img <- magick::image_composite(
        image = bg,
        composite_image = newimg,
        offset = offset
      )
    }

    temlist[[i]]$points <- apply(temlist[[i]]$points, 2, function(pt) {
      pt - c(x_off, y_off)
    })
  }

  invisible(temlist)
}
