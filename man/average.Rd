% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/average.R
\name{average}
\alias{average}
\title{Average templates}
\usage{
average(stimlist, name = "average")
}
\arguments{
\item{stimlist}{list of class webmorph_list}

\item{name}{Name for the average}
}
\value{
webmorph_stim
}
\description{
Average templates
}
\examples{
faces("london") \%>\%
  average() \%>\%
  plot(pt.plot = TRUE, line.plot = TRUE)

}
