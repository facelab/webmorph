#' Crop images and templates
#'
#' Remove or add margins to images and templates. Width, height, x_off and y_off can be set in pixels (> 2.0) or proportions (between 0.0 and 2.0). Cropping is anchored at the image center (or calculated template centroid if there is no image) unless x_off or y_off are set.
#'
#' Fill can be set to R color names (see `colors()`) or valid hex or rgb values. Patch can be set to TRUE (defaults to median color of top left 10-pixel square) or a list of arguments to the function `patch()` to set background from a patch of the image.
#'
#' @param temlist list of webmorph templates
#' @param width width of cropped image
#' @param height height of cropped image
#' @param x_off x-offset (NULL horizontally centers cropped image)
#' @param y_off y-offset (NULL vertically centers cropped image)
#' @param fill background color if cropping goes outside the original image
#' @param patch whether to use the patch function to set the background color
#' @param squash whether to move template points outside the image boundaries inside the image
#'
#' @return temlist with cropped tems and/or images
#' @export
#'
#' @examples
#' a <- faces("test")
#'
#' # crop to 60% width and 80% height (centered)
#' crop(a, width = .60, height = .80) %>% plot(pt.plot = TRUE)
#'
#' # crop to 40% width and 40% height (centered) and add border
#' crop(a, width = .4, height = .4) %>%
#'   crop(1.5, 1.5) %>%
#'   plot(pt.plot = TRUE)
#'
#' # crop to 40% width and 40% height (centered) and add border
#' # points are squashed to image edges
#' crop(a, width = .4, height = .4, squash = TRUE) %>%
#'   crop(1.5, 1.5) %>%
#'   plot(pt.plot = TRUE)
#'
#' # crop to upper right quadrant
#' crop(a, .5, .5, x_off = .5, y_off = 0) %>% plot(pt.plot = TRUE)
#'
#' # crop to larger size with custom fill
#' crop(a, 500, 500, fill = "hotpink") %>% plot()
#'
#' # take median color from a patch
#' crop(a, 1.2, 1.2, patch = c(100, 200, 1, 10)) %>% plot()
#'
crop <- function(temlist,
                 width = 1.0, height = 1.0,
                 x_off = NULL, y_off = NULL,
                 fill = "white", patch = FALSE,
                 squash = FALSE) {
  temlist <- check_temlist(temlist)

  suppressWarnings({
    width <- rep(width, length.out = length(temlist))
    height <- rep(height, length.out = length(temlist))
    x_off <- rep(x_off, length.out = length(temlist))
    y_off <- rep(y_off, length.out = length(temlist))
    fill <- rep(fill, length.out = length(temlist))
    #patch <- rep(patch, length.out = length(temlist))
  })

  for (i in seq_along(temlist)) {
    origw <- temlist[[i]]$width
    origh <- temlist[[i]]$height
    w <- width[i] %||% origw
    h <- height[i] %||% origh

    # handle percentages
    if (w <= 2) w <- w * origw
    if (h <= 2) h <- h * origh

    # null offsets split the remainder between orig and new dimensions
    if (is.null(x_off[i]) || is.na(x_off[i])) x_off[i] <- (origw - w)/2
    if (is.null(y_off[i]) || is.na(y_off[i])) y_off[i] <- (origh - h)/2

    # handle percentage offsets
    if (abs(x_off[i]) <= 2) x_off[i] <- x_off[i] * origw
    if (abs(y_off[i]) <= 2) y_off[i] <- y_off[i] * origh

    temlist[[i]]$width <- w
    temlist[[i]]$height <- h

    if (class(temlist[[i]]$img) == "magick-image") {
      # crop doesn't handle negative offsets well
      ga <- magick::geometry_area(
        width = min(w, origw),
        height = min(h, origh),
        x_off = max(0, x_off[i]),
        y_off = max(0, y_off[i])
      )

      newimg <- magick::image_crop(
        image = temlist[[i]]$img,
        geometry = ga,
        gravity = "NorthWest"
      )

      # set fill from patch
      if (isTRUE(patch)) {
        fill[i] <- patch(temlist[[i]]$img)
      } else if (!isFALSE(patch)) {
        plist <- c(list(img = temlist[[i]]$img), patch)
        fill[i] <- do.call("patch", plist)
      }

      # make background image with fill
      bg <- magick::image_blank(w, h, color = fill[i])
      offset <- magick::geometry_point(
        x = max(0, -x_off[i]),
        y = max(0, -y_off[i])
      )

      temlist[[i]]$img <- magick::image_composite(
        image = bg,
        composite_image = newimg,
        offset = offset
      )
    }

    temlist[[i]]$points <- apply(temlist[[i]]$points, 2, function(pt) {
      newpt <- pt - c(x_off[i], y_off[i])
      if (isTRUE(squash)) {
        # move points outside image boundaries
        newpt <- newpt %>%
          pmax(c(0, 0)) %>%
          pmin(c(w-1, h-1)) # subtract 1 for 0-vs 1-based origin
      }
      newpt
    })
  }

  invisible(temlist)
}
