\name{top}
\alias{top}
\title{top}
\description{
  The top n markers by conditional power.
}
\usage{
top(pbatObj, n=10, sortBy=NULL)
}
\arguments{
  \item{pbatObj}{An object of class 'pbat', i.e. the results of
    'pbat.m(...)'.}
  \item{n}{How many markers to print. 0 means all will be returned.}
  \item{sortBy}{Name of conditional power column to sort by, for
    instance if you wanted to look at the GxE, you might specify
    "FBATGxE" instead.}
}

\details{
  Prints out the top markers according to conditional power.
}
\keyword{interface}
