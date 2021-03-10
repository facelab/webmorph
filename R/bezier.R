# getControlPoints <- function(v, i = 1) {
#   x0 <- v[1, i]
#   y0 <- v[2, i]
#   x1 <- v[1, i+1]
#   y1 <- v[2, i+1]
#   x2 <- v[1, i+2]
#   y2 <- v[2, i+2]
#
#   t = 0.3
#   d01 = sqrt(`^`(x1 - x0, 2) + `^`(y1 - y0, 2))
#   d12 = sqrt(`^`(x2 - x1, 2) + `^`(y2 - y1, 2))
#   fa = t * d01 / (d01 + d12) # scaling factor for triangle Ta
#   fb = t * d12 / (d01 + d12) # ditto for Tb, simplifies to fb=t-fa
#
#   p1x = x1 - fa * (x2 - x0) # x2-x0 is the width of triangle T
#   p1y = y1 - fa * (y2 - y0) # y2-y0 is the height of T
#   p2x = x1 + fb * (x2 - x0)
#   p2y = y1 + fb * (y2 - y0)
#
#   data.frame(
#     x = c(x0, p1x, p2x, x2),
#     y = c(y0, p1y, p2y, y2)
#   )
# }
#
#
# # path <- system.file("extdata/composite", package = "webmorphR")
# # temlist <- read_tem(path)
# # points <- temlist[[1]]$points
# # p <- data.frame(t(points))
# # colnames(p) <- c("x", "y")
# #
# # g <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y)) +
# #   ggplot2::geom_point(data = p)
# #
# # for (line in temlist[[1]]$lines) {
# #   v <- points[, line+1]
# #   if (length(line) > 2) {
# #     for (i in 1:(length(line)-2)) {
# #       g <- g + ggforce::geom_bezier(data = getControlPoints(v, i))
# #     }
# #   } else if (length(line) == 2) {
# #     d <- data.frame(t(v))
# #     colnames(d) <- c("x", "y")
# #     g <- g + ggplot2::geom_path(data = d)
# #   }
# # }
# #
# # g
