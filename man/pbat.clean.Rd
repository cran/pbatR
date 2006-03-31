\name{clean}
\alias{pbat.clean}
\title{Cleaning pbat(), pbat.last(), pbat.m() output.}
\description{
  Removes many of the temporary output files from communicating with
  PBAT. Make sure you save how PBAT was run for future reference. Most
  useful for simulations.
}
\usage{
pbat.clean( pbatObj, plots=TRUE, all.output=FALSE )
}
\arguments{
  \item{pbatObj}{'pbat' object}
  \item{plots}{TRUE/FALSE - delete plot output}
  \item{all.output}{delete \emph{all} output - use with caution (deletes all
    pbat logs, even those not associated with the current run, as well).}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  
}
\seealso{
  \code{\link{pbat}}
}
\keyword{interface}
