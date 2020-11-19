#' Average templates
#'
#' @param stimlist list of class webmorph_list
#' @param name Name for the average
#'
#' @return webmorph_stim
#' @export
#'
#' @examples
#' faces("london") %>%
#'   average() %>%
#'   plot(pt.plot = TRUE, line.plot = TRUE)
#'
average <- function(stimlist, name = "average") {
  stimlist <- assert_webmorph(stimlist)

  # dim is coord (x/y), pt_i, tem_n
  pt <- sapply(stimlist, `[[`, "points", simplify = "array")

  avg <- apply(pt, c(1, 2), mean)

  stim <- list(
    name = name,
    points = avg,
    lines = stimlist[[1]]$lines,
    closed = stimlist[[1]]$closed
  )

  class(stim) <- c("webmorph_stim", "list")

  stim
}
