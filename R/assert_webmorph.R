#' Check for webmorph_list
#'
#' Check if an object is a list of class webmorph_list. If class webmorph_stim, wrap in webmorph_list. If a properly structured list without the right class, add the right class.
#'
#' @param x The object
#' @param arg The name of the argument
#'
#' @return A webmorph_list
#'
assert_webmorph <- function(x) {
  # handle list without webmorph classes
  if (is.list(x) &&
      !"webmorph_list" %in% class(x) &&
      !"webmorph_stim" %in% class(x)) {

    # does x or the items in x have names consistent with a stim?
    is_stim <- c("points", "lines") %in% names(x) %>% all() ||
               c("img", "width", "height") %in% names(x) %>% all()
    is_wmlist <- sapply(x, function(xi) {
      c("points", "lines") %in% names(xi) %>% all() ||
        c("img", "width", "height") %in% names(xi) %>% all()
    }) %>% all()

    if (is_stim) {
      class(x) <- "webmorph_stim"
    } else if (is_wmlist) {
      class(x) <- "webmorph_list"
    }
  }

  # convert webmorph_stim to webmorph_list
  if ("webmorph_stim" %in% class(x)) {
    #convert to webmorph_list
    stim_list <- list(x)
    class(stim_list) <- "webmorph_list"
  } else if ("webmorph_list" %in% class(x)) {
    stim_list <- x
  } else {
    arg <- match.call()$x
    stop(arg, " needs to be a webmorph_list")
  }

  # add names
  if (is.null(names(stim_list))) {
    i <- sapply(stim_list, `[[`, "imgpath")
    t <- sapply(stim_list, `[[`, "tempath")
    nm <- mapply(function(i, t) { i %||% t }, i, t)
    names(stim_list) <- unique_names(nm)
  }

  # check images are available and reload if not
  for (i in seq_along(stim_list)) {
    img <- stim_list[[i]]$img
    if (!is.null(img)) {
      tryCatch({
        magick:::assert_image(img)
      }, error = function(e) {
        stim_list[[i]]$img <- magick::image_read(stim_list[[i]]$imgpath)
      })
    }
  }

  stim_list
}
