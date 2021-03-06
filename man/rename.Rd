% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename.R
\name{rename}
\alias{rename}
\title{Rename stimuli in a webmorph_list}
\usage{
rename(
  stimlist,
  prefix = "",
  suffix = "",
  new_names = NULL,
  pattern = NULL,
  replacement = NULL,
  ...
)
}
\arguments{
\item{stimlist}{A webmorph_list}

\item{prefix}{String to prefix to each name}

\item{suffix}{String to append to each name}

\item{new_names}{Vector of new names - must be the same length as the webmorph_list}

\item{pattern}{Pattern for gsub}

\item{replacement}{Replacement for gsub}

\item{...}{Additional arguments to pass on to \code{base::gsub()}}
}
\value{
a webmorph_list
}
\description{
Rename stimuli in a webmorph_list
}
\examples{
faces() \%>\%
  rename(prefix = "new_") \%>\%
  names()
}
