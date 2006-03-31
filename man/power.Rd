\name{power} 
\alias{pbat.power}
\alias{pbat.binaryFamily}
\alias{pbat.continuousFamily}
\alias{pbat.caseControl}
\alias{pbat.popQuant}
\title{Power And Sample Size Calculations}
\description{
  Does power and sample size calculations.  Note that this may be
  \emph{very highly} pbat version specific, and more finicky than other
  methods in this package, as it requires that the
  menuing interface does not change.

  The Windows version requires a slight hack to control PBAT, which
  appears to be stable, but may have a race condition on heavily loaded
  systems.  Unix/Linux users should not encounter this.

  'pbat.power()' starts the GUI interface (strongly recommended)
  'pbat.binaryFamily(...)' is for family based designs with binary
  'pbat.continuous(...)' is for family based designs with continuous
  'pbat.caseControl(...)' is for case/control designs
  'pbat.popQuant(...)' is for population designs with quantitative
  
  Currently less extensive range checking is done in these routines
  unlike the rest of the routines in pbatR.
}
\usage{
pbat.power()

pbat.binaryFamily(
           numOffspring=1, missingParents=0, numFam=0,
           addiOffspringPheno=1, ## only when you have missing parents
           ascertainment="unaffected",
           model="additive", model.afreq=0.2, model.incrAfreq=0,
           model.disLocIsMarker=TRUE,
           
           model.popPrev=NULL,  ## Options 1, 3, & 4
           model.genAF=NULL,    ## Option 1
           model.penAA=NULL, model.penAB=NULL, model.penBB=NULL, ## Option 2
           model.OR=NULL,       ## Option 3
           model.aOR=NULL,      ## Option 4
           
           stat.sigLevel=0.01,
           stat.offset="",  ## defaults to population prevalence
           comp="numerical",
           log="pbatLog.txt")

pbat.continuousFamily(
           numOffspring=1, missingParents=0, numFam=0,
           addiOffspringPheno, ## only when you have missing parents
           ascertainment="unaffected",
           model="additive", model.afreq=0.1, model.incrAfreq=0,
           model.disLocIsMarker=FALSE,                      
           model.heritability=0.1, model.afreqMarker=0,
           model.prDiseaseGivenMarker=1,
           stat.sigLevel=0.05, stat.offset="",
           comp="numerical",
           log="pbatLog.txt")

pbat.caseControl(
           model="additive", model.minafreq=0.1, model.incrAfreq=0.1,
           model.prevalence=0.1,
           model.ORofABvsBB=NULL, # Option 1 - default 1.5
           model.aOR=NULL,      # Option 2 - default 1.481
           comp.cases=500, comp.controls=500, comp.caseControlRatio=1.5,
           comp.power=0.8, comp.sigLevel=0.05, comp.numSim=1000,
           mode="power",
           log="pbatLog.txt")

pbat.popQuant(
           model="additive",
           model.minafreq=0.2, model.incrAfreq=0.1,
           model.heritability=0.001,
           comp.numProbands=2000,
           comp.power=0.8, comp.sigLevel=0.05, comp.numSim=10000,
           mode="power",
           log="pbatLog.txt")
}
\arguments{
  \item{numOffspring}{Family - number of offspring}
  \item{missingParents}{Family - number of missing parents (0,1,2)}
  \item{numFam}{Family - number of families}
  \item{addiOffspringPheno}{Only used when you have missing parents;
    additional offspring phenotypes. 1 for yes, 0 for no.}
  \item{ascertainment}{'unaffected', 'affected', or 'not applicable'}
  
  \item{model}{'additive', 'dominant', 'recessive' or (only for binary /
  case control) 'multi'}
  \item{model.afreq}{allele frequency}
  \item{model.incrAfreq}{increment allele frequency}
  \item{model.disLocIsMarker}{TRUE/FALSE - whether the disease locus is
    the same as the marker locus}
  \item{model.popPrev}{population prevalence}
  \item{model.prevalence}{population prevalence}
  \item{model.genAF}{genetic attributable fraction of the gene}
  \item{model.penAA}{penetrance of AA genotype}
  \item{model.penAB}{penetrance of AB genotype}
  \item{model.penBB}{penetrance of BB genotype}
  \item{model.OR}{odds ratio}
  \item{model.aOR}{allelic odds ratio}
  \item{model.heritability}{heritibility}
  \item{model.afreqMarker}{marker allele frequency}
  \item{model.prDiseaseGivenMarker}{Pr(Disease|marker)}
  \item{model.ORofABvsBB}{odds ratio of AB to BB}
  \item{model.minafreq}{minimum allele frequency}
  
  \item{stat.sigLevel}{significance level}
  \item{stat.offset}{offset; defaults to population prevalence}

  \item{comp}{'numerical':numerical integration,
    'approximation':approximation, 'simulation':simulation}
  \item{comp.cases}{number of cases}
  \item{comp.controls}{number of controls}
  \item{comp.caseControlRatio}{case control ratio}
  \item{comp.power}{power}
  \item{comp.sigLevel}{significance level}
  \item{comp.numSim}{number of simulations to run}
  \item{comp.numProbands}{number of probands}

  \item{mode}{'power' for power result given parameter specification,
    'ss' for sample size result given parameter specification; note that
    not all parameters will be used for 'power' and 'ss'.}
  
  \item{log}{logfile to write to (results will be stored here) - this
    will be over-written if the file exists}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\seealso{
  \code{\link{pbat}},
  \code{\link{pbat.last}}
}
\keyword{interface}
