\name{write.pbat}
\alias{write.pbat}
\alias{write.pbat.csv}
\title{Saving `pbat' Object}
\description{
  Writes a `pbat' object (the result of calling \code{pbat.m}) to disk.
}
\usage{
write.pbat( x, filename )
write.pbat.csv( x, filename )
}
\arguments{
  \item{x}{Object of class \code{pbat}.}
  \item{filename}{Filename to save to.}
}
\details{
  With \code{write.pbat}, a text file is written out with an `&' symbol
  delimiting the output section. Both the calling and results section are
  preserved. If the file has the `.csv' extension, then
  \code{write.pbat.csv} is called.

  With \code{write.pbat.csv}, the text file is of the standard `csv'
  format to delimit the text file. This should be readable with any
  spreadsheet program. Note that to use the R output (for the logrank
  plots), you should open the file in a spreadsheet program and just
  copy and paste it into R.
}
\references{
  \url{http://www.biostat.harvard.edu/~clange/default.htm}
  
  \url{http://www.people.fas.harvard.edu/~tjhoffm/pbatR.html}
}
\keyword{interface}