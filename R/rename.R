#' Rename stimuli in a webmorph_list
#'
#' @param stimlist A webmorph_list
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
rename <- function(stimlist, prefix = "", suffix = "", new_names = NULL,
                   pattern = NULL, replacement = NULL, ...) {
  stimlist <- assert_webmorph(stimlist)

  if (is.null(new_names)) {
    new_names <- names(stimlist)
  } else if (length(new_names) != length(stimlist)) {
    stop("The length of new_names must be equal to the length of stimlist")
  }

  # search and replace strings
  if (!is.null(pattern) && !is.null(replacement)) {
    new_names <- gsub(pattern, replacement,
                      new_names, ...)
  }

  # add prefix and/or suffix
  new_names <- paste0(prefix, new_names, suffix)

  names(stimlist) <- new_names
  for (i in seq_along(stimlist)) {
    stimlist[[i]]$name <- new_names[i]
  }

  invisible(stimlist)

}
