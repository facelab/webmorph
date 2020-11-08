#' Average templates
#'
#' @param temlist list of webmorph templates
#' @param name Name for the average
#'
#' @return webmorph_tem list
#' @export
#'
#' @examples
#' path <- system.file("extdata/composite/", package = "webmorph")
#' read_tem(path) %>%
#'   average() %>%
#'   plot()
#'
average <- function(temlist, name = "average") {
  # dim is coord (x/y), pt_i, tem_n
  pt <- sapply(temlist, `[[`, "points", simplify = "array")

  avg <- apply(pt, c(1, 2), mean)

  tem <- list(
    name = name,
    points = avg,
    lines = temlist[[1]]$lines,
    closed = temlist[[1]]$closed
  )

  class(tem) <- c("webmorph_tem", "list")
  tem
}
