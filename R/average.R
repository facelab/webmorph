#' Average templates
#'
#' @param temlist list of webmorph templates
#' @param name Name for the average
#'
#' @return webmorph_tem list
#' @export
#'
average <- function(temlist, name = "average") {
  pt <- sapply(temlist, `[[`, "points", simplify = "array")
  # dim is coord (x/y), pt_i, tem_n

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
