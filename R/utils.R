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


#' Rename stimuli in a webmorph_list
#'
#' @param stim_list A webmorph_list
#' @param prefix String to prefix to each name
#' @param suffix String to append to each name
#' @param new_names Vector of new names - must be the same length as the webmorph_list
#' @param pattern Pattern for gsub
#' @param replacement Replacement for gsub
#' @param ... Additional arguments to pass on to `base::gsub()`
#'
#' @return a webmorph_list
#' @export
#'
#' @examples
#' faces() %>%
#'   rename(prefix = "new_") %>%
#'   names()
rename <- function(temlist, prefix = "", suffix = "", new_names = NULL,
                   pattern = NULL, replacement = NULL, ...) {
  temlist <- assert_webmorph(temlist)

  if (is.null(new_names)) {
    new_names <- names(temlist)
  } else if (length(new_names) != length(temlist)) {
    stop("The length of new_names must be equal to the length of temlist")
  }

  # search and replace strings
  if (!is.null(pattern) && !is.null(replacement)) {
    new_names <- gsub(pattern, replacement,
                      new_names, ...)
  }

  # add prefix and/or suffix
  new_names <- paste0(prefix, new_names, suffix)

  names(temlist) <- new_names
  for (i in seq_along(temlist)) {
    temlist[[i]]$name <- new_names[i]
  }

  invisible(temlist)

}
