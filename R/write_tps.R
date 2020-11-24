#' Create a TPS file from a webmorph_list
#'
#' @param stimlist list of class webmorph_list
#' @param path_to_tps optional filename to save TPS file
#'
#' @return text of tps file
#' @export
#'
#' @examples
#' # set path_to_tps to save to a file
#' faces("test") %>%
#'   write_tps() %>%
#'   cat()
#'
write_tps <- function(stimlist, path_to_tps = NULL) {
  stimlist <- assert_webmorph(stimlist)

  tps <- mapply(function(stim, name) {
    pt <- {stim$points * c(1, -1)} %>%
      t() %>% as.data.frame()

    pt_list <- paste(pt[[1]], pt[[2]], sep = "\t") %>%
      paste(collapse = "\n")

    sprintf("LM=%i\n%s\nID=%s",
            ncol(stim$points),
            pt_list,
            name)
  }, stimlist, names(stimlist) %||% seq_along(stimlist)) %>%
    paste(collapse = "\n")

  if (is.null(path_to_tps)) {
    return(tps)
  } else {
    write(tps, path_to_tps)
    invisible(stimlist)
  }
}

#' Convert stimlist to array for geomorph
#'
#' @param stimlist list of class webmorph_list
#'
#' @return 3D array
#' @export
#'
#' @examples
#' data <- faces() %>% tems_to_array()
#' dim(data)
#'
tems_to_array <- function(stimlist) {
  stimlist <- assert_webmorph(stimlist)

  # check number of points
  n_pts <- lapply(stimlist, `[[`, "points") %>%
    sapply(ncol) %>%
    unique()

  if (length(n_pts) > 1) {
    stop("Each tem must have the same length")
  }

  sapply(stimlist, function(tem) {
    t(tem$points * c(1, -1))
  }) %>%
    array(dim = c(n_pts, 2, length(stimlist)),
          dimnames = list(NULL, NULL, names(stimlist)))
}
