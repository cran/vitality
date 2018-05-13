\name{indexFinder}
\alias{indexFinder}
\title{Finds the first value of a vector that is less than a value.}
\usage{
  indexFinder(x, val)
}
\arguments{
  \item{x}{Vector to search}

  \item{val}{Threshold}
}
\value{
  Gives the index of the first value of x that is <= val.
  returns -1 if no value satisfies the condition
}
\description{
  For use in the primary vitality model fitting functions in this package. See package documentation.}

