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
    formated as follows: e.g. copy number variant `c' with three intensities would need a total of six columns: 'c1.1.a', 'c1.1.b', 'c1.2.a', 'c1.2.b', 'c1.3.a', 'c1.3.b'.
  }
  \item{pid}{String corresponding to column name for pedigree id.}
  \item{id}{String corresponding to column name for subject id.}
  \item{idfath}{String corresponding to column name for father id.}
  \item{idmoth}{String corresponding to column name for mother id.}
  \item{sex}{String corresponding to column name for sex.}
  \item{affection}{String corresponding to column name for affection
    status.}

  \item{filename}{Filename to open; does not need .phe extension.}
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
# A highly artificial example with not enough subjects to be run;
#  however, it demonstrates how to put data in it.
set.seed(13)
x <- data.frame( pid       = c(1,1,1,1,1),
                 id        = c(1,2,3,4,5),
                 idfath    = c(4,4,4,0,0),
                 idmoth    = c(5,5,5,0,0),
                 sex       = c(1,2,1,1,2),
                 AffectionStatus = c(1,0,0,1,0),
                 c.1.a     = runif(5),
                 c.1.b     = runif(5),
                 c.2.a     = runif(5),
                 c.2.b     = runif(5),
                 c.3.a     = runif(5),
                 c.3.b     = runif(5) )
x
myCPed <- as.cped( x ) # Mark it with the class 'ped'
myCPed
}
\details{
  When reading in a file on disk using \code{read.cped}, a `.cped' file should have the following format. The first line should indicate the names of the copy number markers. Each subsequent line stands for one individual/subject, starting with the pedigree id, followed by the individual/subject id, the id of the father, the id of the mother, the individual's sex, and affection status (just as in the standard pedigree file!). After this information, for each marker, both marker alleles for each copy number intensity are listed. The order corresponds to the order of the copy number markres in the first line of the file.

  must be encoded with a `0', unlike the phenotype file.

  `plotCPed' is a hack transforming the data to the `plotPed' routine.
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
]}
\keyword{interface}

