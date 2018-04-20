\name{obfuscate}
\alias{obfuscate}
\title{Obfuscating Pedigree and Phenotype Files}
\description{
  Randomly permutes the data in a pedigree or phenotype file and changes
  column headers so as to mangle the data. Used for debugging requests,
  in the hopes that the mangled data will produce the same bad output,
  but will not be identifiable.
}
\usage{
obfuscate(obj)
}
\arguments{
  \item{obj}{object of class `ped' or `phe' to be used (the same object
    type is returned, only mangled; you must write this out to file).}
}
\examples{
\dontrun{
ped <- read.ped( "myped" );  ## reads in myped.ped
oped <- obfuscate( "myped" );
write.ped( "obfuscate.ped", ped );

phe <- read.phe( "myphe" );  ## reads in myphe.phe
ophe <- obfuscate( "myphe" );
write.phe( "obfuscate.phe", ped );
}
}
\seealso{
  \code{\link{pbat}}
  \code{\link{pbat.help}}
}
\keyword{interface}
