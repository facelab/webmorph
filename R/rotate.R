#' Rotate templates and images
#'
#' @param stimlist list of class webmorph_list
#' @param degrees degrees to rotate
#' @param fill background color
#' @param patch whether to use the patch function to set the background color
#'
#' @return webmorph_list with rotated tems and/or images
#'
#' @export
#'
#' @examples
#' faces("test") %>%
#'   rotate(45, fill = "dodgerblue") %>%
#'   plot(pt.plot = TRUE)
#'
#' faces("test") %>%
#'   rotate(45, patch = TRUE) %>%
#'   plot()
#'
rotate <- function(stimlist, degrees = 0,
                   fill = "none", patch = FALSE) {
  stimlist <- assert_webmorph(stimlist)

  degrees <- degrees %% 360 %>%
    rep(length.out = length(stimlist))
  radians <- degrees * (pi/180)

  suppressWarnings({
    fill <- rep(fill, length.out = length(stimlist))
    #patch <- rep(patch, length.out = length(stimlist))
  })

  for (i in seq_along(stimlist)) {
    w <- stimlist[[i]]$width
    h <- stimlist[[i]]$height

    # rotate image ----
    if (class(stimlist[[i]]$img) == "magick-image") {
      info <- magick::image_info(stimlist[[i]]$img)
      xm1 <- info$width/2
      ym1 <- info$height/2

      # set fill from patch
      if (isTRUE(patch)) {
        fill[i] <- patch(stimlist[[i]]$img)
      } else if (!isFALSE(patch)) {
        plist <- c(list(img = stimlist[[i]]$img), patch)
        fill[i] <- do.call("patch", plist)
      }

      stimlist[[i]]$img <- stimlist[[i]]$img %>%
        magick::image_background(color = fill[i]) %>%
        magick::image_rotate(degrees[i]) %>%
        magick::image_repage()
      info <- magick::image_info(stimlist[[i]]$img)
      xm2 <- info$width/2
      ym2 <- info$height/2
    } else if (!is.null(w) && !is.null(h)) {
      rotsize <- rotated_size(w, h, degrees[i])
      xm1 <- w/2
      ym1 <- h/2
      xm2 <- rotsize$width/2
      ym2 <- rotsize$height/2
    } else {
      # rotate around the centre of the points
      centre <- apply(stimlist[[i]]$points, 1, mean)
      xm1 <- centre[[1]]
      ym1 <- centre[[2]]
      rotsize <- rotated_size(xm1*2, ym1*2, degrees[i])
      xm2 = rotsize$width/2
      ym2 = rotsize$height/2
    }

    stimlist[[i]]$width = round(xm2*2)
    stimlist[[i]]$height = round(ym2*2)

    # rotate points ----
    # Subtract original midpoints, rotate,
    # and add the new midpoints in the end again
    stimlist[[i]]$points <- apply(stimlist[[i]]$points, 2, function(pt) {
      crad <- cos(radians[i])
      srad <- sin(radians[i])
      x_offset <- pt[[1]] - xm1
      y_offset <- pt[[2]] - ym1
      xr = x_offset * crad - y_offset * srad + xm2
      yr = x_offset * srad + y_offset * crad + ym2

      c(xr, yr)
    })
  }

  invisible(stimlist)
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
