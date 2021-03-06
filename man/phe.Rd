\name{phe}
\alias{as.phe}
\alias{is.phe}
\alias{read.phe}
\alias{fread.phe}
\alias{write.phe}
\alias{sort.phe}
\title{Phenotype Object}
\description{
  Creates, tests, reads, or writes an object of class \code{phe}.
}
\usage{
as.phe(df, pid="pid", id="id")

is.phe(obj)

read.phe(
  filename, na.strings=c("-",".","NA"), 
  lowercase=TRUE, sym=TRUE, ...)
fread.phe(filename, ...)

write.phe(file, phe)

\method{sort}{phe}(x,decreasing=FALSE,...)
}
\arguments{
  \item{df}{Dataframe with the data}
  \item{pid}{String for the column header for 'pid' - pedigree ID.}
  \item{id}{String for the column header for 'id' - subject ID.}

  \item{obj}{any object}

  \item{filename}{Filename to open; does not need .phe extension.}
  \item{na.strings}{Strings that represent NA; defaults should be fine
    here.}
  \item{lowercase}{When TRUE (default), enforces all headers to
    lowercase for convenience.}
  \item{sym}{When TRUE, only the header of the file is read in; only
    PBAT will load in the file (* - see exception).  When FALSE, the
    entire file will be
    read in, and can be modified before using with PBAT.}
  \item{...}{Options for \code{\link{read.table}}.  Do \emph{not} put in
    \code{header=TRUE}, as this will
    cause an error, as the header is automatically loaded.

    With the proper file formatting, this should not be used.
  }

  \item{file}{string representing filename, or a connection for file output}
  \item{phe}{An object of class 'phe' (see \code{\link{as.phe}}).}

  \item{x}{An object of class 'phe' (see \code{\link{as.phe}}).}
  \item{decreasing}{Whether to sort in decreasing/increasing order.}
}
\note{
  \bold{`read.phe' mild warning}: This function \emph{might change
    the names of headers}, so they may not correspond to what pbat is
  looking for if you call any of
  the pbat*files() commands (\emph{even} if lowercase=FALSE, as some
  symbols might be modified by R naming
  conventions).  In general this should not happen.
}
\details{
  When reading in a file on disk using \code{read.ped}, a `.phe' file
  should have the following format (taken from the PBAT web-page).
  The first line contains
  the names of the covariates and phenotypes and the subsequent lines
  contain the pedigree id, the id of the subject, followed by the values
  of the covariates and phenotypes for that subject.  Here missing
  values must be indicate with a `.' or `-', unlike the pedigree file.
  Examples of this type of file can be found on the PBAT webpage.

  Once the dataset is read in, missing values are converted into the
  usual R format of NA (see NA in the help files).

  When using \code{as.phe}, missing values should be in the native R
  format.
  The \code{write.ped} function will convert back into the missing
  format necessary for PBAT.

  (*) Exception. If symbolic is true, the dataset will be temporarily
  read in under one special circumstance.  This is when a stratification
  variable is used in pbat.m, pbat.obj, or pbat.files.  This is because
  the grouping values must be read in.  Alternatively, you can specify
  these values when calling those functions (see the groups.* options)
  in pbat.
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

# And just another e.g. capitalizing on prior code
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
