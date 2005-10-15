\name{phe}
\alias{as.phe}
\alias{is.phe}
\alias{read.phe}
\alias{write.phe}
\title{Phenotype Object}
\description{
  Creates, tests, reads, or writes an object of class \code{phe}.
}
\usage{
as.phe( df, pid="pid", id="id" )

is.phe( obj )

read.phe( filename, na.strings=c("-",".","NA"), lowercase=TRUE, ... )

write.phe( file, phe )
}
\arguments{
  \item{df}{Dataframe with the data}
  \item{pid}{String for the column header for 'pid' - pedigree ID.}
  \item{id}{String for the column header for 'id' - ID.}

  \item{obj}{any object}

  \item{filename}{Filename to open; does not need .phe extension.}
  \item{na.strings}{Strings that represent NA; defaults should be fine
    here.}
  \item{lowercase}{When TRUE (default), enforces all headers to
    lowercase for convenience.}
  \item{...}{Options for \code{\link{read.table}}.  Do \emph{not} put in
    \code{header=TRUE}, as this will
    cause an error, as the header is automatically loaded.

    With the proper file formatting, this should not be used.
  }

  \item{file}{string representing filename, or a connection for file output}
  \item{phe}{An object of class 'phe' (see \code{\link{as.phe}}).}
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\examples{
# A highly artificial example just to get you used to the syntax
#  of using 'as.phe'.
x <- data.frame( pid    = c(1,1,2,2,2),
                 id     = c(1,2,3,4,5),
                 age    = c(14,45,33,22,21),
                 weight = c(150,100,180,185,110) )
x
myPhe <- as.phe( x );
myPhe

# And just another e.g. capitolizing on prior code
names(x)[1:2] <- c("thepid","theid")
x
myPhe <- as.phe( x, pid="thepid", id="theid" )
myPhe  # same as before!
}
\seealso{
  \code{\link{read.ped}}, 
  \code{\link{write.ped}},
  \code{\link{as.ped}},
  \code{\link{as.pedlist}}
}
\keyword{interface}