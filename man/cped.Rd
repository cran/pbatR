\name{cped}
\alias{as.cped}
\alias{is.cped}
\alias{read.cped}
\alias{fread.cped}
\alias{write.cped}
\alias{sort.cped}
\alias{plotCPed}
\title{Phenotype Object}
\description{
  Creates, tests, reads, or writes an object of class \code{cped}.
}
\usage{
as.cped( x,
         pid="pid", id="id", idfath="idfath",
         idmoth="idmoth", sex="sex", affection="AffectionStatus",
         clearSym=FALSE )

is.cped( obj )

read.cped( filename, lowercase=TRUE, sym=TRUE, max=100, ... )
fread.cped( filename, ... )

write.cped( file, cped )

\method{sort}{cped}(x,decreasing=FALSE,...)

plotCPed( cped, sink=NULL )
}
\arguments{
  \item{x}{
    An object of class \code{cped} or \code{data.frame} as described below.

    If \code{x} is of class \code{cped}, no other options are used.

    When \code{x} is of class \code{data.frame}, the columns have entries
    that match the string parameters \code{idped,...,AffectionStatus};
    copy number variation measurements markers consist of \emph{subsequent} columns
    formated as follows: e.g. two copy number variants `cnv1' and `cnv2' with three intensities would need a total of six columns (three per cnv) past the first six named 'cnv1.1', 'cnv1.2', 'cnv1.3', 'cnv2.1', 'cnv2.2', 'cnv2.3'. See the examples below.

    A slightly different format can be used when reading data from disk, that is more or less consistent with how a pedigree file is loaded from disk. See the details below.
  }
  \item{pid}{String corresponding to column name for pedigree id.}
  \item{id}{String corresponding to column name for subject id.}
  \item{idfath}{String corresponding to column name for father id.}
  \item{idmoth}{String corresponding to column name for mother id.}
  \item{sex}{String corresponding to column name for sex.}
  \item{affection}{String corresponding to column name for affection
    status.}

  \item{filename}{Filename to open; does not need .phe extension. See the details below for the file format.}
  \item{lowercase}{When TRUE (and sym is FALSE), enforces all headers to
    lowercase for convenience.}

  \item{...}{Options for \code{\link{read.table}}, used only when sym is
    FALSE.  Do \emph{not} put in \code{header=TRUE}, as this will
    cause an error, as the header is automatically loaded.

    With the proper file formatting, this should not be used.
  }

  \item{file}{string representing filename, or a connection for file output}
  \item{cped}{an object of class \code{cped}}

  \item{obj}{an object}

  \item{sym}{When TRUE, only the header of the file is read in; only
    PBAT will load in the file.  When FALSE, the entire file will be
    read in, and can be modified before using with PBAT.}

  \item{max}{When sym is TRUE, the amount of headers to read in before
    going pure symbolic (so that the SNP usage consistency will not be
    assessed by pbatR, only by PBAT).}

  \item{clearSym}{When TRUE, if a symbolic file is found, it will be
    read in; otherwise, it will stay symbolic.}

  \item{decreasing}{Whether to sort in decreasing/increasing order.}

  \item{sink}{For `plot.cped', this is the name of a pdf file to output all of the plots to (there will be one plot per page).}
}
\examples{
###################
## First Example ##

## A highly artificial example with not enough subjects to be run;
##  however, it demonstrates how to put data in it.
## We have two cnvs here, cnv1 and cnv2.
## The data is just completely random.
set.seed(13)
x <- data.frame( pid       = c(1,1,1,1,1),
                 id        = c(1,2,3,4,5),
                 idfath    = c(4,4,4,0,0),
                 idmoth    = c(5,5,5,0,0),
                 sex       = c(1,2,1,1,2),
                 AffectionStatus = c(1,0,0,1,0),
                 cnv1.1     = runif(5),
                 cnv1.2     = runif(5),
                 cnv1.3     = runif(5),
                 cnv2.1     = runif(5),
                 cnv2.2     = runif(5),
                 cnv2.3     = runif(5) )
x
myCPed <- as.cped( x ) # Mark it with the class 'cped'
myCPed

\dontrun{
####################
## Second Example ##

## Again, a completely random dataset.
## Here we go through an analysis of it.
## However, see pbat.m for many more details on all of the options.
## Create a completely random dataset with one cnv.
set.seed(13)
NUMTRIOS <- 500
## The data is completely random, it does not really make any sense.
cped <- as.cped( data.frame( pid             = kronecker( 1:NUMTRIOS, rep(1,3) ),
                             id              = rep( 1:3, NUMTRIOS ),
                             idfath          = rep( c(0,0,1), NUMTRIOS ),
                             idmoth          = rep( c(0,0,2), NUMTRIOS ),
                             sex             = rep( c(2,1,1), NUMTRIOS ),
                             AffectionStatus = rep( c(0,0,2), NUMTRIOS ),
                             cnv1.1          = runif( 3*NUMTRIOS ),
                             cnv1.2          = runif( 3*NUMTRIOS ),
                             cnv1.3          = runif( 3*NUMTRIOS )    ) )

## Print out part of the dataset
print( head( cped ) )

## Command line run
pbat.work() ## Makes the intermediate files go in ./pbatRwork directory

## - Analyzing the first intensity
res1 <- pbat.m( AffectionStatus ~ NONE, ped=cped, phe=NULL, fbat="gee",
                cnv.intensity=1, cnv.intensity.num=3, offset="none" )
pbat.clean( res1, all.output=TRUE ) ## Removes all intermediate files
## - Analyzing the second intensity
res2 <- pbat.m( AffectionStatus ~ NONE, ped=cped, phe=NULL, fbat="gee",
                cnv.intensity=2, cnv.intensity.num=3, offset="none" )
pbat.clean( res2, all.output=TRUE )
## - Analyzing the third intensity
res3 <- pbat.m( AffectionStatus ~ NONE, ped=cped, phe=NULL, fbat="gee",
                cnv.intensity=3, cnv.intensity.num=3, offset="none" )
pbat.clean( res3, all.output=TRUE )

pbat.unwork() ## Close up work (head to original working directory)

## Print all of the results
print( res1$results )
print( res2$results )
print( res3$results )

## Or put all the results together and write to file
res1$results <- rbind( res1$results, res2$results, res3$results )
write.pbat( res1, "cpedResults.csv" )

## Otherwise, we could write the data to disk, and run with the GUI interface
## Write the data to disk:
write.cped( "cped.cped", cped )
}
}
\details{
  When reading in a file on disk using \code{read.cped}, a `.cped' file should have the following format. The file should be formatted as follows. The first six columns are \emph{unlabeled} (1) the pedigree id, (2) the individual id, (3) the father id, (4) the mother id, (5) sex [0=missing, 1=male, 2=female], and (6) AffectionStatus [0=missing, 1=unaffected, 2=affected]. The subsequent columns correspond to the intensities. So, suppose we have cnv1 and cnv2. The first line of the file would contain `cnv1 cnv2'. Then the subsequent lines would correspond to each individual, the first six columns being as described, and then NUMINTENSITY columns per cnv for a total of 6+2*NUMINTENSITY data columns. NUMINTENSITY is just however many intensities there are per cnv, you will need to specify this number at analysis time. NOTE: MISSING DATA in a cped file should be coded as `-1234.0', rather than the usual `.' or `-' (technically the `.' and `-' should still work with \code{fread.cped}, and when \code{sym=FALSE}).

  The best way to see all of this in action is to run the code in the examples below, and look at the cped file produced from it.

  `plotCPed' plots the data similar to the `plotPed' routine (in fact it transforms the data to use it).
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}

  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\seealso{
  \code{\link{read.ped}},
  \code{\link{read.cped}}
  \code{\link{write.cped}},
  \code{\link{plotCPed}}
}
\keyword{interface}

