#' Print webmorph_stim
#'
#' @param x a list of class webmorph_stim
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#'
print.webmorph_stim <- function(x, ...) {
  attr <- list()
  if ("points" %in% names(x)) {
    attr$points <- sprintf("%i points", ncol(x$points))
  }
  if ("lines" %in% names(x)) {
    attr$lines <- sprintf("%i lines", length(x$lines))
  }

  if ("img" %in% names(x)) {
    info <- tryCatch({
      magick::image_info(x$img)
    }, error = function(e) {
      x$img <- magick::image_read(x$imgpath)
      magick::image_info(x$img)
    })
    attr$img <- sprintf("%i x %i %s",
                        info$width,
                        info$height,
                        info$format)
  }

  paste(attr, collapse = ", ") %>% cat()

  invisible(x)
}

#' Print webmorph_list
#'
#' @param x a list of class webmorph_list
#' @param ... arguments passed to or from other methods
#'
#' @return prints summary info and returns x
#' @export
#'
print.webmorph_list <- function(x, ...) {
  mapply(function(xi, nm) {
    sprintf("* %s: ", nm) %>% cat()
    print(xi, ...)
    cat("\n")
  }, x, names(x) %||% seq_along(x)) # in case names are null

  invisible(x)
}

#' Repeat webmorph_stim in a list
#'
#' @param x A list of class webmorph_stim
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A webmorph_list
#' @export
#'
#' @examples
#' a <- faces()
#' rep(a[[1]], 3)
rep.webmorph_stim <- function (x, ...) {
  # turn into a list and handle below
  x <- assert_webmorph(x)
  rep.webmorph_list(x, ...)
}

#' Repeat stim in a list
#'
#' @param x A webmorph_list
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A webmorph_list
#' @export
#'
#' @examples
#' faces("test") %>%
#'   rep(3) %>%
#'   rotate(seq(10, 60, 10), fill = rainbow(6)) %>%
#'   plot()
rep.webmorph_list <- function(x, ...) {
  nm <- names(x)
  newnm <- rep(nm, ...)
  newx <- x[newnm]
  class(newx) <- "webmorph_list"
  newx
}

#' Combine webmorph_stim
#'
#' @param ... webmorph_stim to be concatenated
#'
#' @return webmorph_list
#' @export
#'
c.webmorph_stim <- function(...) {
  # turn into a temlist and handle below
  dots <- lapply(list(...), assert_webmorph)
  do.call("c", dots)
}


#' Combine webmorph_lists
#'
#' @param ... webmorph_lists to be concatenated
#'
#' @return webmorph_list
#' @export
#'
c.webmorph_list <- function(...) {
  dots <- lapply(list(...), assert_webmorph) %>%
    lapply(unclass) # prevent infinite recursion
  x <- do.call("c", dots)
  class(x) <- "webmorph_list"
  x
}


#' Extract webmorph_list elements
#'
#' @param x webmorph_list from which to extract elements
#' @param i indices to be selected
#'
#' @return webmorph_list
#' @export
#'
`[.webmorph_list` <- function(x, i) {
  x <- NextMethod()
  class(x) <- "webmorph_list"
  x
}

#' Replace webmorph_list element
#'
#' @param x webmorph_list from which to extract elements
#' @param i index to be replaced
#' @param value webmorph_stim element to replace with
#'
#' @return webmorph_list
#' @export
#'
`[[<-.webmorph_list` <- function(x, i, value) {
  stopifnot(is(value, "webmorph_stim"))
  NextMethod()
}



#' Get template bounds
#'
#' @param stimlist A webmorph_list
#' @param each Whether to calculate max and min for the full set (default) or each image separately
#'
#' @return A list of min and max x and y values
#' @export
#'
#' @examples
#' faces("london") %>% bounds()
#'
#' faces("test") %>% bounds(each = TRUE)
bounds <- function(stimlist, each = FALSE) {
  stimlist <- assert_webmorph(stimlist)

  if (isTRUE(each)) {
    # get separate bounds for each stimulus
    b <- sapply(stimlist, bounds) %>%
      t() %>%
      as.data.frame() %>%
      tidyr::unnest(cols = c("min_x", "max_x", "min_y", "max_y")) %>%
      as.data.frame()
    rownames(b) <- names(stimlist)
    return(b)
  }

  A <- tems_to_array(stimlist)
  x <- (A[, "X", ])
  y <- (A[, "Y", ]) * -1

  list(min_x = min(x),
       max_x = max(x),
       min_y = min(y),
       max_y = max(y))
}

#' WebmorphR Message
#'
#' @param ... arguments to pass to base::message()
#'
#' @return NULL
message <- function(...) {
  if (isTRUE(webmorph_options("verbose"))) {
    base::message(...)
  }
}
