\name{pbat.set}
\alias{pbat.set}
\alias{pbat.get}
\title{Set Pbat Executable}
\description{
  \code{pbat.set} sets the name of the pbat executable.  NOTE: This
  \emph{is retained between sessions} for a given user.
  
  \code{pbat.get} gets the current stored name of the pbat executable.
}
\usage{
pbat.set( executableStr="", CLEAR=FALSE )

pbat.get()
}
\arguments{
  \item{executableStr}{String of the pbat executable name,
    e.g. \dQuote{c:/pbat/pbat25.exe}.  If the string is omitted, then a
    file-choose dialogue will appear to selet the file (suggested).}
  \item{CLEAR}{If set to TRUE, the executable name is cleared,
    regardless of the string passed in the other option.}
}
\value{
  \code{pbat.get} returns a string of the currently stored name of the
  pbat executable.
}
\details{
  The default (upon installation) assumes that the executable is 'pbat'
  and is in the path; this function allows you to set the version you
  wish to use, and easily toggle between other versions of the software
  (in case you are testing the software it is easy to toggle to an older
  version).
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\seealso{
  \code{\link{pbat.m}}
}
\keyword{interface}
