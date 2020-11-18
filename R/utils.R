#' Print webmorph templates
#'
#' @param x a list of class webmorph_tem
#' @param ... arguments passed to or from other methods
#'
#' @return prints the template info
#' @export
#'
print.webmorph_tem <- function(x, ...) {
  sprintf("%s [%i points, %i lines]",
          x$name,
          ncol(x$points),
          length(x$lines)
  ) %>% cat()

  invisible(x)
}

#' Print webmorph template list
#'
#' @param x a list of class webmorph_temlist
#' @param ... arguments passed to or from other methods
#'
#' @return prints the template info
#' @export
#'
print.webmorph_temlist <- function(x, ...) {
  lapply(x, function(xi) {
    cat("* ")
    print(xi, ...)
    cat("\n")
  })

  invisible(x)
}

#' Repeat tems in a temlist
#'
#' @param x A single webmorph template
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A webmorph temlist
#' @export
#'
#' @examples
#' a <- faces()
#' rep(a[[1]], 3)
rep.webmorph_tem <- function (x, ...) {
  # turn into a temlist and handle below
  x <- check_temlist(x)
  rep.webmorph_temlist(x, ...)
}

#' Repeat items in a temlist
#'
#' @param x A webmorph temlist
#' @param ... Additional arguments to pass on to `base::rep()`
#'
#' @return A webmorph temlist
#' @export
#'
#' @examples
#' faces("test") %>%
#'   rep(3) %>%
#'   rotate(seq(10, 60, 10), fill = rainbow(6)) %>%
#'   plot()
rep.webmorph_temlist <- function(x, ...) {
  nm <- names(x)
  newnm <- rep(nm, ...)
  newx <- x[newnm]
  class(newx) <- "webmorph_temlist"
  newx
}

#' Combine templates
#'
#' @param ... webmorph templates to be concatenated
#'
#' @return webmorph template list
#' @export
#'
c.webmorph_tem <- function(...) {
  # turn into a temlist and handle below
  dots <- lapply(list(...), check_temlist)
  do.call("c", dots)
}


#' Combine template lists
#'
#' @param ... webmorph template lists to be concatenated
#'
#' @return webmoprh template list
#' @export
#'
c.webmorph_temlist <- function(...) {
  dots <- lapply(list(...), check_temlist) %>%
    lapply(unclass) # prevent infinite recursion
  x <- do.call("c", dots)
  class(x) <- "webmorph_temlist"
  x
}


#' Rename temlist
#'
#' @param temlist A webmorph temlist
#' @param prefix String to prefix to each name
#' @param suffix String to append to each name
#' @param new_names Vector of new names - must be the same length as the temlist
#' @param pattern Pattern for gsub
#' @param replacement Replacement for gsub
#' @param ... Additional arguments to pass on to `base::gsub()`
#'
#' @return a webmorph temlist
#' @export
#'
#' @examples
#' faces() %>%
#'   rename(prefix = "new_") %>%
#'   names()
rename <- function(temlist, prefix = "", suffix = "", new_names = NULL,
                   pattern = NULL, replacement = NULL, ...) {
  temlist <- check_temlist(temlist)

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
