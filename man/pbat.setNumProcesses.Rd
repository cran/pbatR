\name{pbat.setNumProcesses}
\alias{pbat.setNumProcesses}
\alias{pbat.getNumProcesses}
\title{Sets number of processes PBAT can spawn}
\description{
  Gives the ability to get/set the number of processes of pbat to
  spawn. This is useful for those who have systems with multiple
  processors.
  This value is retained between sessions for each user.
}
\usage{
pbat.setNumProcesses(n)

pbat.getNumProcesses()
}
\arguments{
  \item{n}{Number of processes to spawn.  Set to the number of
    processors you have in your system (-1 if you want to do something
    else while it works) to speed up computation time.}
}
\value{
  \code{pbat.getNumProcesses} returns the current number of processes
  that it has been set to spawn.
}

\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  
}
\keyword{interface}
