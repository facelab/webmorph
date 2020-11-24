#' Resize templates and images
#'
#' @param stimlist list of class webmorph_list
#' @param width new width (in pixels or percent)
#' @param height new height (in pixels or percent)
#'
#' @return webmorph_list with resized tems and/or images
#' @export
#'
#' @examples
#' faces("test") %>%
#'   resize(.5, .75) %>%
#'   plot(pt.plot = TRUE,
#'        pt.color = "black",
#'        pt.shape = 1)
#'
resize <- function(stimlist, width = NULL, height = NULL) {
  stimlist <- assert_webmorph(stimlist)

  if (is.null(width) && is.null(height)) {
    return(stimlist)
  }
  if (!is.null(width) && width < 0) {
    stop("width must be a positive number")
  } else if (!is.null(height) && height < 0) {
    stop("height must be a positive number")
  }

  suppressWarnings({
    width <- rep(width, length.out = length(stimlist))
    height <- rep(height, length.out = length(stimlist))
  })

  for (i in seq_along(stimlist)) {
    # express height and/or width as % and fill empty value

    if (is.null(width[i])) {
      # check height first
    } else if (width[i] <= 10) { # percentage
      w <-   width[i]
    } else if (!is.null(width[i])) { # pixels
      w <- width[i]/stimlist[[i]]$width
    }

    if (is.null(height[i])) {
      h <- w
    } else if (height[i] <= 10) { # percentage
      h <-  height[i]
    } else { # pixels
      h <- height[i]/stimlist[[i]]$height
    }

    if (is.null(width[i])) w <- h

    # resize template
    stimlist[[i]]$points <- stimlist[[i]]$points * c(w, h)

    # calculate new dimensions
    stimlist[[i]]$width <- round(stimlist[[i]]$width*w)
    stimlist[[i]]$height <- round(stimlist[[i]]$height*h)

    if (class(stimlist[[i]]$img) == "magick-image") {
      # resize image
      stimlist[[i]]$img <- magick::image_resize(
        stimlist[[i]]$img,
        magick::geometry_size_percent(w*100, h*100)
      )

      # make sure dimensions are consistent with image
      info <- magick::image_info(stimlist[[i]]$img)
      stimlist[[i]]$width <- info$width
      stimlist[[i]]$height <- info$height
    }
  }

  invisible(stimlist)
}
