#' Rotate templates and images
#'
#' @param temlist list of webmorph templates
#' @param degrees degrees to rotate
#'
#' @return temlist with rotated tems and/or images
#' @export
#'
#' @examples
#' path <- system.file("extdata/composite/f_multi.tem", package = "webmorph")
#' read_tem(path) %>%
#'   rotate(45) %>%
#'   plot(image = TRUE)
#'
rotate <- function(temlist, degrees = 0) {
  temlist <- check_temlist(temlist)

  degrees <- degrees %% 360 %>%
    rep(length.out = length(temlist))
  radians <- degrees * (pi/180)

  for (i in seq_along(temlist)) {
    w <- temlist[[i]]$width
    h <- temlist[[i]]$height

    # rotate image ----
    if (class(temlist[[i]]$img) == "magick-image") {
      info <- magick::image_info(temlist[[i]]$img)
      xm1 = info$width/2
      ym1 = info$height/2
      temlist[[i]]$img <- magick::image_rotate(
        temlist[[i]]$img, degrees[i]
      ) %>%
        magick::image_repage()
      info <- magick::image_info(temlist[[i]]$img)
      xm2 = info$width/2
      ym2 = info$height/2
    } else if (!is.null(w) && !is.null(h)) {
      rotsize <- rotated_size(w, h, degrees[i])
      xm1 = w/2
      ym1 = h/2
      xm2 = rotsize$width/2
      ym2 = rotsize$height/2
    } else {
      # rotate around the centre of the points
      centre <- apply(temlist[[i]]$points, 1, mean)
      xm1 <- centre[[1]]
      ym1 <- centre[[2]]
      rotsize <- rotated_size(xm1*2, ym1*2, degrees[i])
      xm2 = rotsize$width/2
      ym2 = rotsize$height/2
    }

    temlist[[i]]$width = round(xm2*2)
    temlist[[i]]$height = round(ym2*2)

    # rotate points ----
    # Subtract original midpoints, rotate,
    # and add the new midpoints in the end again
    temlist[[i]]$points <- apply(temlist[[i]]$points, 2, function(pt) {
      crad <- cos(radians[i])
      srad <- sin(radians[i])
      x_offset <- pt[[1]] - xm1
      y_offset <- pt[[2]] - ym1
      xr = x_offset * crad - y_offset * srad + xm2
      yr = x_offset * srad + y_offset * crad + ym2

      c(xr, yr)
    })
  }

  invisible(temlist)
}

#' Image size after rotation
#'
#' @param width Width of the original image
#' @param height Height of the original image
#' @param degrees Rotation in degreed
#'
#' @return list of rotated width and height
#' @export
#'
#' @examples
#' rotated_size(100, 100, 45)
rotated_size <- function(width, height, degrees) {
  degrees <- degrees %% 180

  if (degrees < 0) {
    degrees <- 180 + degrees
  }
  if (degrees >= 90) {
    tmpw <- width
    width <- height
    height <- tmpw
    degrees <- degrees - 90
  }

  radians <- degrees * pi / 180;
  w <- (width * cos(radians)) + (height * sin(radians))
  h <- (width * sin(radians)) + (height * cos(radians))

  list(
    width = w,
    height = h
  )
}
