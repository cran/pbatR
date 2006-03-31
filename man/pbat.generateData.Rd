\name{pbat.generateData}
\alias{pbat.generateData}
\title{Data generation}
\description{
  Not intended for most users. Generates data.
}
\usage{
pbat.generateData( filenamePrefix, onlysnps=TRUE, trios='1000',
                   snps='1000', p='0.1', h='0.01' )
}
\arguments{
  \item{filenamePrefix}{Filename prefix for the '.ped' and '.phe'
    files.}
  \item{onlysnps}{Only snps? - TRUE/FALSE}
  \item{trios}{Number of trios}
  \item{snps}{Number of SNPs}
  \item{p}{p}
  \item{h}{h}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}  
}
\seealso{
  \code{\link{read.ped}},
  \code{\link{read.phe}}
}
\keyword{interface}
