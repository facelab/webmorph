#' Title
#'
#' @param temlist list of webmorph templates
#' @param width new width (in pixels or percent)
#' @param height new height (in pixels or percent)
#'
#' @return temlist with resized tems and/or images
#' @export
#'
resize <- function(temlist, width = NULL, height = NULL) {
  temlist <- check_temlist(temlist)

  if (is.null(width) && is.null(height)) {
    return(temlist)
  }
  if (!is.null(width) && width < 0) {
    stop("width must be a positive number")
  } else if (!is.null(height) && height < 0) {
    stop("height must be a positive number")
  }

  lapply(temlist, function(tem) {
    if (width <= 10) { # percentage
      w <-   width
    } else if (!is.null(width)) { # pixels
      w <- width/tem$width
    }

    if (is.null(height)) {
      h <- w
    } else if (height <= 10) { # percentage
      h <-  height
    } else { # pixels
      h <- height/tem$height
    }

    if (is.null(width)) w <- h

    tem$points <- tem$points * c(w, h)

    tem$width <- round(tem$width*w)
    tem$height <- round(tem$height*h)

    if (class(tem$img) == "magick-image") {
      tem$img <- magick::image_resize(
        tem$img,
        magick::geometry_size_percent(w*100, h*100)
      )
      info <- magick::image_info(tem$img)
      tem$width <- info$width
      tem$height <- info$height
    }

    tem
  })


}
