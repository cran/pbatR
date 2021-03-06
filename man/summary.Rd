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
\method{print}{pbat}(x,...)

\method{summary}{pbat}(object,...)

\method{plot}{pbat}(x,...)
}
\arguments{
  \item{x}{`pbat' object (result of pbat.m(...) or pbat().}
  \item{object}{`pbat' object (result of pbat.m(...) or pbat().}
  \item{...}{Ignored.  Needed for S3 generic method consistency.}
}
\references{
  Jiang, H., Harrington, D., Raby, B. A., Bertram, L., Blacker, D., Weiss, S. T. & Lange,
C. (2005) Family-based association test for time-to-onset data with time-dependent
differences between the hazard functions. Genetic Epidemiology.
}
\seealso{
  \code{\link{pbat}},
  \code{\link{pbat.last}}
}
\keyword{interface}
