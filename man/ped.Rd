\name{ped}
\alias{as.ped}
\alias{as.pedlist}
\alias{is.ped}
\alias{is.pedlist}
\alias{read.ped}
\alias{write.ped}
\title{Pedigree Object}
\description{
  Creates, tests, reads, or writes objects of type \code{ped} or
  \code{pedlist} to be used with the pbat commands.

  The \code{ped} class inherits the \code{list} structure, and is almost
  identical to the \code{list} object described to create it, only with
  some special reserved names.

  The \code{pedlist} class inherits the \code{data.frame} structure, and
  is almost identical to the \code{data.frame} object described to
  create it, only with some special reserved names.
}
\usage{
as.ped( x,
        idped="idped", idsub="idsub", idfath="idfath",
        idmoth="idmoth", sex="sex", affection="affection" )

as.pedlist( x,
            idped="idped", idsub="idsub", idfath="idfath",
            idmoth="idmoth", sex="sex", affection="affection" )

is.ped( obj );

is.pedlist( obj );

read.ped( filename, format="ped", lowercase=TRUE, ... )

write.ped( file, ped )
}
\arguments{
  \item{x}{
    An object of class \code{ped}, \code{pedlist}, \code{data.frame}, or \code{list} as
    described below.

    If \code{x} is of class \code{ped} or \code{pedlist}, no other options are used.
    
    When \code{x} is of class \code{data.frame}, the columns have entries
    that match the string parameters \code{idped,...,censor};
    genetic markers consist of \emph{subsequent} columns
    formated as follows: e.g. marker `p5' would need
    two columns known as `p5.a' `p5.b' (or really
    just where the characters a and b are different).
    
    When \code{x} is of class \code{list}, the
    entries still include \code{idped,...,censor} as discussed
    previously, and genetic markers consist of a list object of two
    vectors for each unphased haplotype.
  }
  \item{idped}{String corresponding to column name for pedigree id.}
  \item{idsub}{String corresponding to column name for subject id.}
  \item{idfath}{String corresponding to column name for father id.}
  \item{idmoth}{String corresponding to column name for mother id.}
  \item{sex}{String corresponding to column name for sex.}
  \item{affection}{String corresponding to column name for affection
    status.}
  
  \item{filename}{Filename to open; does not need .phe extension.}
  \item{format}{Toggles the return structure, set to "ped" or "pedlist".}
  \item{lowercase}{When TRUE, enforces all headers to lowercase for
    convenience.}
  \item{...}{Options for \code{\link{read.table}}.  Do \emph{not} put in
    \code{header=TRUE}, as this will
    cause an error, as the header is automatically loaded.
    
    With the proper file formatting, this should not be used.
  }

  \item{file}{string representing filename, or a connection for file output}
  \item{ped}{an object of class \code{ped} or \code{pedlist} (see
    \code{as.ped} or \code{as.pedlist})}

  \item{obj}{an object}
}
\examples{
# A highly artificial example with not enough subjects to be run;
#  however, it demonstrates how to put data in it.
x <- data.frame( idped =     c(1,1,1,1,1),
                 idsub =     c(1,2,3,4,5),
                 idfath =    c(4,4,4,0,0),
                 idmoth =    c(5,5,5,0,0),
                 sex =       c(1,2,1,1,2),
                 affection = c(1,0,0,1,0),
                 m1.a =      c(1,1,4,4,4),
                 m1.b =      c(2,3,2,2,3),
                 m2.a =      c(4,4,4,4,4),
                 m2.b =      c(1,1,1,4,1) )
x
myPed <- as.ped( x )          # Mark it with the class 'ped'
myPedlist <- as.pedlist( x )  # Instead mark it with 'pedlist'
myPed
myPedlist

# an alternate example of creating
names( x )[1:6] <- c( "mypedid", "subid", "fathid",
                      "mothid", "gender", "affection" );
x
myPed <- as.ped( x, idped="mypedid", idsub="subid", idfath="fathid",
                 idmoth="mothid", sex="gender" ) # affection need not be
                                                 #  specified here
myPed  # Note it's the same as before!

myPed <- as.ped( myPedlist )       # Easy conversion back
myPedlist <- as.pedlist( myPed )   #  and forth between formats.
}
\details{
  When reading in a file on disk using \code{read.ped}, a `.ped' file should
  have the following format (taken from the PBAT web-page).
  The first line of the PBAT pedigree file contains the names of the
  markers.
  Each subsequent line stands for one individual/subject, starting with
  the pedigree id, followed by the individual/subject id, the id of the
  father, the id of the mother, the individual's sex and affection
  status. After this information, for each marker, both marker alleles
  are listed. The order of the markers has to correspond to the order of
  the marker names in the first line of the file. Missing values here
  must be encoded with a `0', unlike the phenotype file.
  Examples of this type of file can be found on the PBAT webpage.

  The usage of \code{as.ped} and \code{as.pedlist} should also follow
  the same missingness convention.
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\seealso{
  \code{\link{read.ped}},
  \code{\link{write.ped}},
  \code{\link{as.pedlist}}
}
\keyword{interface}
