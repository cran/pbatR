\name{generic}
\alias{plot.pbat}
\alias{print.pbat}
\alias{summary.pbat}
\title{`pbat' Object Generic Routines}
\description{
  Summary routines for the results of `pbat', `pbat.last', `pbat.m',
  `pbat.obj', and `pbat.files'.

  Only a logrank analysis can be plotted.
}
\usage{
print.pbat(x,...)

summary.pbat(object,...)

plot.pbat(x,...)
}
\arguments{
  \item{x}{`pbat' object (result of pbat.m(...) or pbat().}
  \item{object}{`pbat' object (result of pbat.m(...) or pbat().}
  \item{...}{Ignored.  Needed for S3 generic method consistency.}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  
}
\seealso{
  \code{\link{pbat}},
  \code{\link{pbat.last}}
}
\keyword{interface}