\name{pbat.help}
\alias{pbat.help}
\alias{pbat.firsttime}
\title{Help and Reporting Information}
\description{
  Provides some helpful suggestions and information if you are having
  some troubles getting the package set up (often tends to be version
  related). Information useful for reporting.
}
\usage{
pbat.help( bug=FALSE, full=TRUE, ped=NULL, phe=NULL, lib.loc=NULL )
pbat.firsttime();
}
\arguments{
  \item{bug}{Whether you think a bug was found on a run. Will attempt to
    generate some useful information for the developers.}
  \item{full}{When bug=TRUE, runs a full bug report (slower, reruns
    analysis, but recommended.}
  \item{ped}{Pedigree object that can be obfuscated (see `obfuscate').}
  \item{phe}{Phenotype object that can be obfuscated (see `obfuscate').}
  \item{lib.loc}{a character vector describing the location of R library
    trees to search through, or 'NULL', i.e what/if you specified for
    this when the library was loaded.}
}
\references{
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  

  \url{http://www.biostat.harvard.edu/~clange/default.htm}
}
\seealso{
  \code{\link{pbat}}
  \code{\link{obfuscate}}
}
\keyword{interface}
