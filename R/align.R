#' Align templates and images
#'
#' @param stimlist list of class webmorph_list
#' @param pt1 The first point to align (defaults to 0)
#' @param pt2 The second point to align (defaults to 1)
#' @param x1 The x-coordinate to align the first point to
#' @param y1 The y-coordinate to align the first point to
#' @param x2 The x-coordinate to align the second point to
#' @param y2 The y-coordinate to align the second point to
#' @param width The width of the aligned image
#' @param height The height of the aligned image
#' @param ref_img The reference image (by index or name) to get coordinates and dimensions from if NULL (defaults to first image)
#' @param fill background color if cropping goes outside the original image
#' @param patch whether to use the patch function to set the background color
#' @param squash whether to move template points outside the image boundaries inside the image
#' @param procrustes Whether to do a procrustes alignment
#'
#' @return webmorph_list with aligned tems and/or images
#' @export
#'
#' @examples
#' faces("test") %>%
#'   align(x1 = 100, y1 = 100, x2 = 200, y2 = 200,
#'         width = 300, height = 400) %>%
#'   plot()
#'
#'  faces("lisa")[1:2] %>%
#'    align(procrustes = TRUE) %>%
#'    plot()
align <- function(stimlist, pt1 = 0, pt2 = 1,
                  x1 = NULL, y1 = NULL, x2 = NULL, y2 = NULL,
                  width = NULL, height = NULL, ref_img = 1,
                  fill = "white", patch = FALSE, squash = FALSE,
                  procrustes = FALSE) {
  stimlist <- assert_webmorph(stimlist)

  # if values NULL, default to ref_img points
  ref_points <- stimlist[[ref_img]]$points
  x1 <- x1 %||% ref_points[1, pt1+1]
  y1 <- y1 %||% ref_points[2, pt1+1]
  x2 <- x2 %||% ref_points[1, pt2+1]
  y2 <- y2 %||% ref_points[2, pt2+1]
  width <- width %||% stimlist[[ref_img]]$width
  height <- height %||% stimlist[[ref_img]]$height

  if (pt1 == pt2) {
    # align points are the same, so no resize needed,
    # make sure that left and right align coordinates are the same
    x2 <- x1
    y2 <- y1
  }

  # procrustes align ----
  if (isTRUE(procrustes)) {
    # procrustes align coordinates
    data <- tems_to_array(stimlist)
    coords <- procrustes_align(data)

    # calculate size multiplier
    orig_avg <- apply(data, c(1, 2), mean)
    pro_avg <- apply(coords, c(1, 2), mean)

    oEyeWidth <- (orig_avg[pt1+1, ] - orig_avg[pt2+1, ])^2 %>%
      sum() %>% sqrt(.)
    pEyeWidth <- (pro_avg[pt1+1, ] - pro_avg[pt2+1, ])^2 %>%
      sum() %>% sqrt(.)
    mult = oEyeWidth/pEyeWidth

    # resize and convert to webmorph coordinates
    pts <- dim(coords)[[1]]
    for (i in 1:dim(coords)[[3]]) {
      coords[, , i] <- coords[, , i] *
        rep(c(mult, -mult), each = pts) +
        rep(c(width/2, height/2), each = pts)
    }

    # get align points for each image
    x1 <- coords[pt1+1, 1, ]
    x2 <- coords[pt2+1, 1, ]
    y1 <- coords[pt1+1, 2, ]
    y2 <- coords[pt2+1, 2, ]
  }

  suppressWarnings({
    x1 <- rep(x1, length.out = length(stimlist))
    y1 <- rep(y1, length.out = length(stimlist))
    x2 <- rep(x2, length.out = length(stimlist))
    y2 <- rep(y2, length.out = length(stimlist))
    fill <- rep(fill, length.out = length(stimlist))
    #patch <- rep(patch, length.out = length(stimlist))
  })


  # calculate rotation and resize parameters for each image
  rotate <- c()
  resize <- c()

  for (i in seq_along(stimlist)) {
    # calculate aligned rotation and inter-point width
    rotate_aligned <- ifelse(x1[i] - x2[i] == 0, pi/2,
                             atan((y1[i] - y2[i])/(x1[i] - x2[i])))
    aEyeWidth = sqrt(`^`(x1[i]-x2[i], 2) + `^`(y1[i]-y2[i], 2))


    o_pts <- stimlist[[i]]$points
    ox1 <- o_pts[1, pt1+1]
    oy1 <- o_pts[2, pt1+1]
    ox2 <- o_pts[1, pt2+1]
    oy2 <- o_pts[2, pt2+1]

    # calculate rotation
    rotate_orig <- ifelse(ox1 - ox2 == 0, pi/2,
                    atan((oy1 - oy2)/(ox1 - ox2)))
    rotate[i] <- -(rotate_orig - rotate_aligned) / (pi/180)

    # calculate resize needed
    oEyeWidth <- sqrt(`^`(ox1-ox2, 2) + `^`(oy1-oy2, 2))
    resize[i] <- ifelse(aEyeWidth == 0 || oEyeWidth == 0,
                        1, aEyeWidth / oEyeWidth)
  }

  stimlist <- stimlist %>%
    rotate(degrees = rotate, fill = fill, patch = patch) %>%
    resize(width = resize, height = resize)

  # recalculate eye position for cropping
  x_off <- c()
  y_off <- c()
  for (i in seq_along(stimlist)) {
    n_pts <- stimlist[[i]]$points
    newx1 <- n_pts[1, pt1+1]
    newy1 <- n_pts[2, pt1+1]
    x_off[i] = newx1 - x1[i]
    y_off[i] = newy1 - y1[i]
  }

  stimlist <- crop(
    stimlist, width = width, height = height,
    x_off = x_off, y_off = y_off,
    fill = fill, patch = patch, squash = squash
  )

  invisible(stimlist)
}

#' Procrustes align templates
#'
#' @param data Template points
#' @param rotate Whether to "guess" the rotation, or rotate 0, 90, 180, or 270 degrees
#'
#' @return coordinates
#' @export
#'
procrustes_align <- function(data, rotate = 0) {
  gpa <- geomorph::gpagen(data, PrinAxes = FALSE,
                          print.progress = FALSE)

  if (rotate != "guess") {
    rotate <- as.numeric(rotate)

    if (rotate == 90) {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateC")
    } else if (rotate == 180) {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateC") %>%
        geomorph::rotate.coords("rotateC")
    } else if (rotate == 270) {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateCC")
    } else { # rotate == 0 or anything else
      coords <- gpa$coords
    }
  } else {
    ### otherwise guess best rotation ----

    # calculate average face
    orig_avg <- apply(data, c(1, 2), mean)
    pro_avg <- apply(gpa$coords, c(1, 2), mean)

    all_pts <- expand.grid(x = 1:nrow(orig_avg),
                           y = 1:nrow(orig_avg)) %>%
      dplyr::filter(x != y) %>%
      dplyr::sample_n(min(100, nrow(.)))

    angles <- list(
      o = orig_avg,
      p0 = pro_avg,
      p1 = geomorph::rotate.coords(pro_avg, "rotateC"),
      p2 = geomorph::rotate.coords(pro_avg, "rotateC") %>%
        geomorph::rotate.coords("rotateC"),
      p3 = geomorph::rotate.coords(pro_avg, "rotateCC")
    ) %>%
      lapply(function(coords) {
        mapply(function(pt1, pt2) { angle_from_2pts(coords, pt1, pt2) },
                all_pts$x, all_pts$y)
      })

    dd <- data.frame(
      p0 = angles$p0 - angles$o,
      p1 = angles$p1 - angles$o,
      p2 = angles$p2 - angles$o,
      p3 = angles$p3 - angles$o
    ) %>%
      # take care of values near +-2pi (better way?)
      dplyr::mutate_all(function(x) {
        x <- ifelse(x > 2*pi, x - (2*pi), x)
        x <- ifelse(x > 0, x - (2*pi), x)
        x <- ifelse(x < -2*pi, x + (2*pi), x)
        x <- ifelse(x > 0, x + (2*pi), x)
        x
      }) %>%
      dplyr::summarise_all(mean)

    min_diff <- as.list(dd) %>% sapply(abs) %>% `[`(. == min(.)) %>% names()
    #message("rotation: ", min_diff)

    if (min_diff == "p1") {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateC")
    } else if (min_diff == "p2") {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateC") %>%
        geomorph::rotate.coords("rotateC")
    } else if (min_diff == "p3") {
      coords <- geomorph::rotate.coords(gpa$coords, "rotateCC")
    } else { # min_diff == "p0" or anything else
      coords <- gpa$coords
    }
  }

  dimnames(coords) <- dimnames(data)
  return(coords)
}



#' Get angle from 2 points
#'
#' @param coords The coordinate array
#' @param pt1 The first point
#' @param pt2 The second point
#'
#' @return double of angle in radians
#' @export
#'
angle_from_2pts <- function(coords, pt1 = 1, pt2 = 2) {
  x1 <- coords[[pt1,1]]
  x2 <- coords[[pt2,1]]
  y1 <- coords[[pt1,2]]
  y2 <- coords[[pt2,2]]

  atan2(y1 - y2, x1 - x2) %% (2*pi)
}
