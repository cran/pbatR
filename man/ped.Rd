\name{ped}
\alias{as.ped}
\alias{as.pedlist}
\alias{is.ped}
\alias{is.pedlist}
\alias{read.ped}
\alias{fread.ped}
\alias{write.ped}
\alias{is.pped}
\alias{as.pped}
\alias{read.pped}
\alias{sort.ped}
\alias{plotPed}
\alias{ped.markerNames}
\title{Pedigree Object}
\description{
  Creates, tests, reads, or writes objects of type \code{ped} or
  \code{pedlist} to be used with the pbat commands.

  The \code{ped} class inherits the \code{data.frame} structure, and
  is almost identical to the \code{data.frame} object described to
  create it, only with some special reserved names.

  The \code{pedlist} class inherits the \code{list} structure, and is almost
  identical to the \code{list} object described to create it, only with
  some special reserved names.

  The `pped' functions provide support for a more compressed form of
  input, that can be read in faster, and so may be faster when running
  in clustered mode.
}
\usage{
as.ped( x,
        pid="pid", id="id", idfath="idfath",
        idmoth="idmoth", sex="sex", affection="AffectionStatus",
        clearSym=FALSE )

as.pedlist( x,
            pid="pid", id="id", idfath="idfath",
            idmoth="idmoth", sex="sex", affection="AffectionStatus",
            clearSym=FALSE )

is.ped( obj, pure.ped=FALSE )

is.pedlist( obj )

read.ped( filename, format="ped", lowercase=TRUE, sym=TRUE, max=100, ... )
fread.ped( filename, ... )

write.ped( file, ped )

is.pped( obj )

read.pped( filename, max=100 )

as.pped( ped, ppedname="" )

\method{sort}{ped}(x,decreasing=FALSE,...)

plotPed(ped, sink=NULL, geno="percent", phe=NULL, pheCols=NULL)

ped.markerNames( ped )
}
\arguments{
  \item{x}{
    An object of class \code{ped}, \code{pedlist}, \code{data.frame}, or \code{list} as
    described below.

    If \code{x} is of class \code{ped} or \code{pedlist}, no other options are used.

    When \code{x} is of class \code{data.frame}, the columns have entries
    that match the string parameters \code{idped,...,AffectionStatus};
    genetic markers consist of \emph{subsequent} columns
    formated as follows: e.g. marker `p5' would need
    two columns known as `p5.a' `p5.b' (or really
    just where the characters a and b are different).

    When \code{x} is of class \code{list}, the
    entries still include \code{idped,...,censor} as discussed
    previously, and genetic markers consist of a list object of two
    vectors for each unphased haplotype.
  }
  \item{pid}{String corresponding to column name for pedigree id.}
  \item{id}{String corresponding to column name for subject id.}
  \item{idfath}{String corresponding to column name for father id.}
  \item{idmoth}{String corresponding to column name for mother id.}
  \item{sex}{String corresponding to column name for sex.}
  \item{affection}{String corresponding to column name for affection
    status.}

  \item{filename}{Filename to open; does not need .phe extension.}
  \item{format}{Toggles the return structure, set to "ped" or "pedlist".}
  \item{lowercase}{When TRUE (and sym is FALSE), enforces all headers to
    lowercase for convenience.}

  \item{...}{Options for \code{\link{read.table}}, used only when sym is
    FALSE.  Do \emph{not} put in \code{header=TRUE}, as this will
    cause an error, as the header is automatically loaded.

    With the proper file formatting, this should not be used.
  }

  \item{file}{string representing filename, or a connection for file output}
  \item{ped}{an object of class \code{ped} or \code{pedlist} (see
    \code{as.ped} or \code{as.pedlist})}

  \item{obj}{an object}

  \item{sym}{When TRUE, only the header of the file is read in; only
    PBAT will load in the file.  When FALSE, the entire file will be
    read in, and can be modified before using with PBAT.}

  \item{max}{When sym is TRUE, the amount of headers to read in before
    going pure symbolic (so that the SNP usage consistency will not be
    assessed by pbatR, only by PBAT).}

  \item{clearSym}{When TRUE, if a symbolic file is found, it will be
    read in; otherwise, it will stay symbolic.}

  \item{pure.ped}{When FALSE, tests if an object is a `ped' or
    `pped'. When TRUE, tests only if the object is a `ped'.}

  \item{ppedname}{Name of the `pped' file.  If a symbolic ped, it
    defaults to that name except with a pped extension; otherwise, it
    defaults to `pped.pped'.}

  \item{decreasing}{Whether to sort in decreasing/increasing order.}

  \item{sink}{For `plot.ped', this is the name of a pdf file to output
    all of the plots to (there will be one plot per page).}
  \item{geno}{For 'plot.ped'; "none" includes no genotype summary information, "percent" gives an overall summary of how much each individual is genotyped, i.e. non-missing (the default), and "each" gives all of the genotypes of each individual.}
  \item{phe}{For 'plot.ped', a phenotype object for extra information.}
  \item{pheCols}{For 'plot.ped', names in the phenotype object of columns that should be displayed in the pedigree.}
}
\examples{
# A highly artificial example with not enough subjects to be run;
#  however, it demonstrates how to put data in it.
x <- data.frame( pid       = c(1,1,1,1,1),
                 id        = c(1,2,3,4,5),
                 idfath    = c(4,4,4,0,0),
                 idmoth    = c(5,5,5,0,0),
                 sex       = c(1,2,1,1,2),
                 AffectionStatus = c(1,0,0,1,0),
                 m1.a      = c(1,1,1,1,1),
                 m1.b      = c(1,2,1,1,2),
                 m2.a      = c(4,4,4,4,4),
                 m2.b      = c(3,3,3,4,3) )
x
myPed <- as.ped( x )          # Mark it with the class 'ped'
myPedlist <- as.pedlist( x )  # Instead mark it with 'pedlist'
myPed
myPedlist

# an alternate example of creating
names( x )[1:6] <- c( "mypedid", "subid", "fathid",
                      "mothid", "gender", "affection" );
x
myPed <- as.ped( x, pid="mypedid", id="subid", idfath="fathid",
                 idmoth="mothid", sex="gender", affection="affection" )
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

  `plot.ped' attempts to make use of the `kinship' package to draw the
  pedigrees. In my personal experience, this package cannot handle all
  pedigrees. My preferred alternative would be to use Madeline, which
  makes beautiful pictures (amongst other things):
    http://eyegene.ophthy.med.umich.edu/
    
  \code{ped.markerNames} returns the names of the markers, without the '.a' and '.b' extension for markers (and not duplicated for markers).
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
