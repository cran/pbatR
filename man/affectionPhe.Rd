\name{affectionPhe}
\alias{affectionPhe}
\title{Affection Phenotype Object}
\description{
  Creates a phenotype object from a pedigree object (see `ped' and
  `phe'), preserving affection status.  Mostly used internally, or as a
  substitute offset fix for the GUI.
}
\usage{
affectionPhe( ped, trait="affected", offset=0.0 )
}
\arguments{
  \item{ped}{An object of class \code{ped}.}
  \item{trait}{Name for the trait in the generated phenotype file.}
  \item{offset}{The offset to be used.}
}
\details{
  Returns an object of class \code{phe}.
}
\seealso{
  \code{\link{read.phe}},
  \code{\link{read.ped}}
}
\keyword{interface}
