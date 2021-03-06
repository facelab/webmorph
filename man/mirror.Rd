% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mirror.R
\name{mirror}
\alias{mirror}
\title{Mirror templates and images}
\usage{
mirror(stimlist, sym_map = NULL, axis = "vertical")
}
\arguments{
\item{stimlist}{list of class webmorph_list}

\item{sym_map}{list of corresponding template points}

\item{axis}{vertical or horizontal axis of mirroring}
}
\value{
webmorph_list with mirrored images and tems
}
\description{
Use the function \code{frl_sym()} to get the sym_map for the standard webmorph template. If sym_map is omitted, images and templates will be fully reversed (e.g., if point 1 is the left eye in the original image, it will be the right eye in the mirrored image).
}
\examples{
o <- faces("london", "001_03")
m <- mirror(o, frl_sym())

c(o, m) \%>\%
  plot(pt.plot = TRUE, labels = c("original", "mirrored"))

}
