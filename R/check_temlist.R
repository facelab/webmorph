#' Check for temlist
#'
#' Check if an object is a temlist. If a single tem, convert to a temlist.
#'
#' @param x The object
#' @param arg The name of the argument
#'
#' @return A temlist
#'
check_temlist <- function(x, arg = "temlist") {
  # handle list without webmorph classes
  if (is.list(x) &&
      !"webmorph_temlist" %in% class(x) &&
      !"webmorph_tem" %in% class(x)) {

    # does x or the items in x have names consistent with a tem?
    tnames<- c("name", "points", "lines")
    is_tem <- tnames %in% names(x) %>% all()
    is_temlist <- sapply(x, function(xi) {
        tnames %in% names(xi) %>% all()
      }) %>% all()
    if (is_tem) {
      class(x) <- "webmorph_tem"
    } else if (is_temlist) {
      class(x) <- "webmorph_temlist"
    }
  }


  if ("webmorph_tem" %in% class(x)) {
    #convert to temlist
    temlist <- list(x)
    names(temlist) <- x$name
    class(temlist) <- "webmorph_temlist"
  } else if ("webmorph_temlist" %in% class(x)) {
    temlist <- x
  } else {
    stop("The argument ", arg, " needs to be a temlist")
  }

  temlist
}
