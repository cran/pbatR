\name{c2bat}
\alias{c2bat}
\title{C2BAT}
\description{
  C2BAT (c) Matt McQueen, Jessica Su, Christoph Lange.
}
\usage{
c2bat(
  snps,
  case.aa, case.Aa, case.AA,
  control.aa, control.Aa, control.AA,
  replicates=0,
  statistic="armitage",
  method="exact")
}
\arguments{
  \item{snps}{Names of the snps (vector). Can be strings.}
  
  \item{case.aa}{Number of cases with aa genotype (a vector
    corresponding to the snps).}
  \item{case.Aa}{Number of cases with Aa genotype.}
  \item{case.AA}{Number of cases with AA genotype.}
  
  \item{control.aa}{Number of controls with aa genotype.}
  \item{control.Aa}{Number of controls with Aa genotype.}
  \item{control.AA}{Number of controls with AA genotype.}

  \item{replicates}{This value should be set to zero for the first
    analysis. Then, once the top snps have been selected, this should be
    rerun with replicates>1000 for those selected snps.}
  
  \item{statistic}{
    \code{"armitage"} = the Armitage trend test.
    \code{"logrank"} = logrank test
  }
  
  \item{method}{
    \code{"exact"} = exact likelihood function
    \code{"approximate"} = approximate likelihood function
  }
}
\details{
  The output is formatted as follows:
  SNP, case.aa, case.Aa, case.AA, control.aa, control.Aa, control.AA,
  Monte-Carlo p-value, p-value, non-centrality parameter (for power
  calculations), modelc2-based OR, allelic OR.

  Note that the non-centrality parameter and the ORs are independent of
  the pvalues.
}
\examples{
\dontrun{
c2bat( snps=c("snp1","snp2"),
       case.aa=c(10,11), case.Aa=c(30,31), case.AA=c(14,44),
       control.aa=c(33,32), control.Aa=c(100,99), control.AA=c(0,0) )
}
}
\keyword{interface}
