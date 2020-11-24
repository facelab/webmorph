#' Check for webmorph_list
#'
#' Check if an object is a list of class webmorph_list. If class webmorph_stim, wrap in webmorph_list. If a properly structured list without the right class, add the right class. If the img is not a magick_image or the pointer is dead, reloads from the imgpath.
#'
#' @param x The object
#'
#' @keywords internal
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
    stimlist <- list(x)
    class(stimlist) <- "webmorph_list"
  } else if ("webmorph_list" %in% class(x)) {
    stimlist <- x
  } else {
    arg <- match.call()$x
    stop(arg, " needs to be a webmorph_list")
  }

  # add names
  if (is.null(names(stimlist))) {
    i <- sapply(stimlist, `[[`, "imgpath")
    t <- sapply(stimlist, `[[`, "tempath")
    nm <- mapply(function(i, t) { i %||% t }, i, t)
    names(stimlist) <- unique_names(nm)
  }

  # check images are available and reload if not
  for (i in seq_along(stimlist)) {
    img <- stimlist[[i]]$img
    if (!is.null(img)) {
      stimlist[[i]]$img <- tryCatch({
        magick::image_info(img) # throws an error if img not valid
        # rather use magick:::assert_image, but ::: warning
        img
      }, error = function(e) {
        magick::image_read(stimlist[[i]]$imgpath)
      })
    }
  }

  stimlist
}
