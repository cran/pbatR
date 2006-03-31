\name{cluster}
\alias{is.finished}
\alias{pbat.load}
\alias{pbat.concatenate}
\title{Cluster result functions}
\description{
  Options when 'clusterRefresh' has been set to 0 (so that the current
  process is batched). See \code{\link{pbat.set}} for details.

  is.finished() returns if PBAT execution has terminated.

  pbat.load() loads in PBAT output.

  pbat.concatenate() concatenates output files (without reading them
  into an R object).
}
\usage{
is.finished( pbatObj=NULL, clean=TRUE )
pbat.load( pbatObj=NULL )
pbat.concatenate( pbatObj=NULL, filename="myResults.txt", clean=FALSE )
}
\arguments{
  \item{pbatObj}{'pbat' object (result of pbat.m, pbat.obj,
    pbat.files). If NULL / left empty, then the results are taken from
    what was run in the GUI interface.
  }
  \item{clean}{
    \emph{pbat.load}:  If TRUE it will remove intermediate files that setup the
    job submission interface (but no other results, and not the
    command-files sent to pbat); see also pbat.clean.  If FALSE, nothing
    else is done.  NOTE, when TRUE (which is the default), if you run
    this more than once, you will get TRUE the first time, and then
    FALSE the rest of the times.

    \emph{pbat.concatenate}:  If clean is TRUE, then output that went into the
    concatenated file will be deleted.  Default is FALSE, as this has
    been a primary area of continual changes in PBAT, and so you may not
    want to do this until you have checked the output.
  }
  \item{filename}{output filename}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  
}
\seealso{
  \code{\link{pbat.set}}
  
  \code{\link{pbat}}
}
\keyword{interface}
