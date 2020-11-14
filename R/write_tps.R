#' Create a TPS file from a list of webmorph templates
#'
#' @param temlist list of webmorph templates
#'
#' @return text of tps file
#' @export
#'
write_tps <- function(temlist, path_to_tps = NULL) {
  temlist <- check_temlist(temlist)

  tps <- lapply(temlist, function(tem) {
    pt <- {tem$points * c(1, -1)} %>%
      t() %>% as.data.frame()

    pt_list <- paste(pt[[1]], pt[[2]], sep = "\t") %>%
      paste(collapse = "\n")

    sprintf("LM=%i\n%s\nID=%s",
            ncol(tem$points),
            pt_list,
            tem$name)
  }) %>%
    paste(collapse = "\n")

  if (is.null(path_to_tps)) {
    return(tps)
  } else {
    write(tps, path_to_tps)
    invisible(temlist)
  }
}

#' Convert temlist to array for geomorph
#'
#' @param temlist list of webmorph templates
#'
#' @return 3D array
#' @export
#'
tems_to_array <- function(temlist) {
  temlist <- check_temlist(temlist)

  # check number of points
  n_pts <- lapply(temlist, `[[`, "points") %>%
    sapply(ncol) %>%
    unique()

  if (length(n_pts) > 1) {
    stop("Each tem must have the same length")
  }

  nm <- sapply(temlist, `[[`, "name")

  sapply(temlist, function(tem) {
    t(tem$points * c(1, -1))
  }) %>%
    array(dim = c(n_pts, 2, length(temlist)),
          dimnames = list(NULL, NULL, nm))
}
