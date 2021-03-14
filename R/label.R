#' Add a Label to Stimuli
#'
#' @param stimlist list of class webmorph_list
#' @param text a vector of the label text(s)
#' @param color a vector of the label colour(s)
#' @param size a vector of the label size(s)
#' @param alpha a vector of the label alpha(s)
#' @param angle a vector of the label angle(s) in degrees
#' @param family a vector of the label font family(s)
#' @param fontface a vector of the label fontface(s)
#' @param lineheight a vector of the label lineheight(s)
#' @param position vertical (top, middle, bottom) and horizontal (left, center, right) position of the label (e.g., "top left")
#' @param x,y a vector of the label x and y position(s), overrides values set from position
#' @param hjust,vjust a vector of the label horizontal and vertical justification(s), overrides values set from position
#'
#' @return webmorph_list with labelled images
#' @export
#'
#' @examples
#'
#' faces("test") %>%
#'   label(c("CHINWE", "GEORGE"), color = "red") %>%
#'   plot_fig(labels = "")
label <- function(stimlist,
                  text = "LABEL",
                  color = "black",
                  size = 5,
                  alpha = 1,
                  angle = 0,
                  family =  "sans",
                  fontface  = "plain",
                  lineheight = 1.2,
                  position = "top center",
                  x = NULL,
                  y = NULL,
                  hjust = NULL,
                  vjust = NULL) {
  stimlist <- assert_webmorph(stimlist)

  # allows for arguments to be vectors of any length
  ith <- function(v, i) {
    v[[(i-1)%%length(v)+1]]
  }

  for (i in seq_along(stimlist)) {
    g <- plot_stim(stimlist[[i]],
                   label.text = ith(text, i),
                   label.color = ith(color, i),
                   label.size = ith(size, i),
                   label.alpha = ith(alpha, i),
                   label.angle = ith(angle, i),
                   label.family = ith(family, i),
                   label.fontface = ith(fontface, i),
                   label.lineheight = ith(lineheight, i),
                   label.position = ith(position, i),
                   label.x = ith(x, i),
                   label.y = ith(y, i),
                   hjust = ith(hjust, i),
                   vjust = ith(vjust, i),
                   margin = 0,
                   border.width = 0)

    filename <- tempfile(fileext = ".png")
    w = stimlist[[i]]$width/300
    h = stimlist[[i]]$height/300
    ggplot2::ggsave(filename, g, width = w, height = h, dpi = 300, units = "in")
    stimlist[[i]]$img <- magick::image_read(filename)
  }

  stimlist
}


