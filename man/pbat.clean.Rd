\name{clean}
\alias{pbat.clean}
\title{Cleaning pbat(), pbat.last(), pbat.m() output.}
\description{
  Removes many of the temporary output files from communicating with
  PBAT. Make sure you save how PBAT was run for future reference. Most
  useful for simulations.
}
\usage{
pbat.clean(pbatObj, plots=TRUE, all.output=FALSE)
}
\arguments{
  \item{pbatObj}{'pbat' object}
  \item{plots}{TRUE/FALSE - delete plot output}
  \item{all.output}{DEPRECATED: No longer does anything, deleting files from other (potentially running) PBAT runs can be dangerous.}
}
\seealso{
  \code{\link{pbat}}
}
\keyword{interface}
