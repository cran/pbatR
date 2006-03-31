\name{pbat.set}
\alias{pbat.set}
\alias{pbat.get}
\alias{pbat.setmode}
\alias{pbat.setmode.defaults}
\alias{pbat.getmode}
\title{Set Pbat Parameters}
\description{
  All of these values are retained, even after closing and restarting R.
  
  \code{pbat.set} sets the name of the pbat executable.
  
  \code{pbat.get} gets the current stored name of the pbat executable.

  \code{pbat.setmode} sets the mode (single, multiple, or cluster), with
  \code{pbat.setmode.defaults} returning it to default values.

  \code{pbat.getmode} returns the current mode specifications.
}
\usage{
pbat.set( executableStr="", CLEAR=FALSE )
pbat.get()

pbat.setmode.defaults( save=TRUE )
pbat.setmode( mode=NULL, jobs=NULL, clusterCommand=NULL,
              clusterRefresh=NULL )
pbat.getmode()
}
\arguments{
  \item{executableStr}{String of the pbat executable name,
    e.g. \dQuote{c:/pbat/pbat25.exe}.  If the string is omitted, then a
    file-choose dialogue will appear to selet the file (suggested).}
  \item{CLEAR}{If set to TRUE, the executable name is cleared,
    regardless of the string passed in the other option.}

  \item{save}{Used internally.}
  
  \item{mode}{'single', 'multiple', or 'cluster'. See details.}
  \item{jobs}{The number of jobs to partition into for 'multiple' or
    'cluster'.}
  \item{clusterCommand}{The command used to submit a job to your
    cluster. The default is 'bsub'.}
  \item{clusterRefresh}{How often (in seconds) to check if jobs have
    finished.}
}
\value{
  \code{pbat.get} returns a string of the currently stored name of the
  pbat executable.
}
\details{
  The default (upon installation) assumes that the executable is 'pbat'
  and is in the path; \code{pbat.set} allows you to set the version you
  wish to use, and easily toggle between other versions of the software
  (in case you are testing the software it is easy to toggle to an older
  version).

  ---------------------------------------------------------------------
  
  The 'single' mode is meant for
  only one processor on a users personal computer (see also
  cluster for some special features of that mode). However, with
  putting several cores on one chip, it may be more advantageous to use
  'multiple' or 'cluster'.

  WARNING: if the number of jobs is 1, it will always be in this
  'single' mode.

  ---------------------------------------------------------------------

  The 'multiple' mode is meant for a single multiprocessor system, or a
  cluster that acts like one.  You can then set 'jobs' to be however
  many processors that you have, or more depending on some of the newer
  technologies.  If you have a cluster that acts like a
  multi-processor system, then this can be more efficient than the 'cluster'
  mode \emph{if and only if you are waiting for the output}.  The
  'multiple' mode will work in all operating systems.

  ---------------------------------------------------------------------
  
  In 'cluster' mode, you must specify the command and any additional
  flags (to keep it generic for all platforms)
  to submit a job that is represented in a file (i.e
  'bsub -q normal sh' on some clusters, 'batch -f' or 'at now -f' in a very basic unix
  environment - see 'man batch' from the command line or search for it
  online).

  When the 'refresh' option is set to '0' (zero), then the
  output is batched, and R will not wait for the output to be
  finished.  If you do this, be sure to save your workspace when
  exiting; then you can reload your output back in.  See
  \code{\link{is.finished}},
  \code{\link{pbat.load}}, and
  \code{\link{pbat.concatenate}} for more information on this.

  When 'refresh' is set to an integer greater than zero, it indicates
  the seconds that should elapse before the current R
  session checks to see if the other processes are done.  The goal was
  to keep this as generic as possible.

  Cluster mode may work in windows, but I am unfamiliar with any
  batching command system available in windows.  Would there actually be
  any interest in this?  Please e-mail me if this might be useful.
  
  ---------------------------------------------------------------------

  System administrators could copy the '.pbat.Rmeta' file to the users
  home directory after using \code{pbat.set} to set it themselves.
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\seealso{
  \code{\link{pbat.m}}

  \code{\link{is.finished}},
  \code{\link{pbat.load}},
  \code{\link{pbat.concatenate}}
}
\keyword{interface}
