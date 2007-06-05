\name{pbat.work}
\alias{pbat.work}
\alias{pbat.unwork}
\title{pbatRwork Temp Directory}
\description{
  Creates a temporary 'pbatRwork' directory, and moves to it. This can be created in the same directory as a pedigree or phenotype file by passing those as parameters instead.
}
\usage{
pbat.work( pedOrPhe=NULL )
pbat.unwork( cur=NULL )
}
\arguments{
  \item{pedOrPhe}{If a `ped' or `phe' symbolic object (read in without `sym=TRUE' which is the default), then uses that directory + '/pbatRwork' as the current working directory (so all output will go there).  Otherwise it just creates a `pbatRwork' directory in the current working directory.}
  \item{cur}{Pass in the output of pbat.work(...), see example below.}
}
\examples{
\dontrun{
ped <- read.ped("mydata")
phe <- read.phe("myphe")

cur <- pbat.work( ped )

res <- pbat.m( trait ~ NONE, ped=ped, phe=phe, fbat="gee" )
...
## whatever you want to do with the results

pbat.unwork( cur ) ## sends you back to where you were
}
}
\keyword{interface}