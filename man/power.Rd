\name{power}
\alias{pbat.power}
\alias{pbat.powerCmd}
\title{Power Exploration}
\description{
  Power has been completely rewritten from scratch, and
  is all done via monte carlo simulation internally now.  These routines
  do not require pbat, and should run on any machine.
}
\usage{
pbat.power( mode="continuous" )

pbat.powerCmd( numOffspring=1, numParents=2, numFamilies=500,
               additionalOffspringPhenos=TRUE,
               ascertainment="affected",
               model="additive",
               afreqMarker=NA,
               penAA=0.8, penAB=0.5, penBB=0.3,
               heritability=0.0, contsAscertainmentLower=0.0,
                contsAscertainmentUpper=1.0,
               pDiseaseAlleleGivenMarkerAllele=1.0, afreqDSL=0.1,
               alpha=0.01,
               offset="default",
               numSim=1000 )
}
\arguments{
  \item{mode}{"continuous" or "dichotomous"}
  
  \item{numOffspring}{Family - number of offspring}
  \item{numParents}{Family - number of parents (0,1,2)}
  \item{numFamilies}{Family - number of families}
  \item{additionalOffspringPhenos}{Only used when you have missing parents;
    additional offspring phenotypes. 1 for yes, 0 for no.}
  \item{ascertainment}{'unaffected', 'affected', or 'na' for anyone}
  \item{model}{'additive', 'dominant', 'recessive' or (only for binary /
    case control) 'multi'}
  \item{afreqMarker}{allele frequency at the marker}
  \item{penAA}{penetrance of AA genotype}
  \item{penAB}{penetrance of AB genotype}
  \item{penBB}{penetrance of BB genotype}
  \item{heritability}{heritibility - when this is zero, a binary trait
    according to the previously defined parameters is used; when it is
    nonzero, a continuous trait is used.}
  \item{contsAscertainmentLower}{Lower bound for affected ascertainment
    with a continuous trait, this is a vector for each member after the
    proband, defaulting to `0'. It represents the quantiles, so 0.05
    would indicate that the lower 5 percent of the phenotypes should be
    removed.}
  \item{contsAscertainmentUpper}{Upper bound, defaults to `1'.}

  \item{pDiseaseAlleleGivenMarkerAllele}
  {Pr(Disease allele A|marker allele A)}
  \item{afreqDSL}{allele frequency at DSL, defaults to marker
    frequency.}

  \item{alpha}{significance level}
  \item{offset}{"default" uses the population prevalence for dichotomous
    traits and the population mean for continuous traits. If a number is
    specified, then that number is used as the offset.}
  \item{numSim}{Number of monte-carlo simulations. I'd use a smaller
    number while starting out with it, and then turn it up to a much
    higher number of iterations later on.}
}
\details{
  pbat.powerCmd(...) does not really do any range checking, primarily
  because I don't expect most will use it directly, and instead will use
  the friendly GUI interface for power exploration.

  Be careful with the number of simulations! When you are first
  exploring, you can keep this low, but you should turn this all the way
  up before doing your final computation.
  
  Note that some values of `pDiseaseAlleleGivenMarkerAllele' in
  combination with `afreqMarker' are not possible. These will return
  negative values (these are error codes for the GUI, which will provide
  more helpful messages).

  Lastly, you might want to look into something like
    set.seed(1)
  e.g., if you want the results to be reproducable (set it to any
  number, but make note of this number, see set.seed for more details).
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}

  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}

  Hoffmann, T. and Lange, C. (2006) P2BAT: a massive parallel
  implementation of PBAT for genome-wide association studies in
  R. Bioinformatics. Dec 15;22(24):3103-5.

  Horvath, Steve, Xu, Xin, and Laird, Nan M. The family based association test method: computing means and variances for general statistics. Tech Report.
}
\seealso{
  \code{\link{pbat}},
  \code{\link{pbat.last}}
}
\keyword{interface}
