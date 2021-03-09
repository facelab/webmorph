#' Get template definition
#'
#' @param tem_id
#'
#' @return
#' @export
#'
#' @examples
#'
#' frl_face <- tem_def(1)
#' frl_face$points$name[[19]]
tem_def <- function(tem_id = 1, path = NULL) {
  # read file or url ----
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop(sprintf("The file at %s does not exist", path))
    }
    tem_def <- tryCatch({
      readLines(path)
    }, error = function(e) {
      stop("The file couldn't be read")
    })
  } else if (is.numeric(tem_id)) {
    url <- sprintf("https://webmorph.org/scripts/temDownload?tem_id=%d",
                   tem_id)
    tem_def <- tryCatch({
      readLines(url)
    }, error = function(e) {
      stop("You might not have an internet connection")
    })
  } else {
    stop("You must supply a numeric tem_id or a valid path to a template definition file.")
  }

  # parse text ----
  infotable <- read.csv(text = tem_def[1:8],
                        header = FALSE,
                        row.names = 1) %>%
    t() %>% as.data.frame()
  tem <- list()
  for (i in 1:8) {
    tem[i] <- strsplit(infotable[ , i], ";") %>%
      type.convert(as.is = TRUE)
  }
  names(tem) <- colnames(infotable)

  ptrange <- 10:(10+tem$points)
  linerange <- (12+tem$points):(12+tem$points+tem$lines)

  tem$points <- read.csv(text = tem_def[ptrange])
  tem$lines <- read.csv(text = tem_def[linerange])

  tem
}
