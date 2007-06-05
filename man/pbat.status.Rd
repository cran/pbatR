\name{pbat.status}
\alias{pbat.status}
\title{PBAT Status of Run}
\description{
  Provides status information, i.e. if there were any errors.
}
\usage{
pbat.status(n=1,workFirst=FALSE)
}
\arguments{
  \item{n}{Returns the last `n' lines from the status file. If n=0, all
    lines are returned.}
  \item{workFirst}{Whether to check for `./pbatRwork/pbatstatus.txt' or
    `./pbatstatus.txt' first.  Generally only semi-important internally,
    but set this to true if you just typed pbat.unwork().}
}
\seealso{
  \code{\link{pbat}}
}
\keyword{interface}
