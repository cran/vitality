\name{mu.vd3.6p}
\alias{mu.vd3.6p}
\title{Adult extrinsic mortality rate for the 2-process 6-parameter vitality model}
\usage{
  mu.vd3.6p(x, r, lambda, beta)
}
\arguments{
  \item{x}{age}

  \item{r}{r value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}
}
\value{
  Vector of adult extrinsic age-specific mortality rates at ages \code{x}
}
\description{
  Gives the extrinsic age-specific mortality rates for a given set of \code{r} and the adult extrinsic parameters.
}
\seealso{\code{\link{mu.vd.6p}}, \code{\link{mu.vd1.6p}}, \code{\link{mu.vd2.6p}}, \code{\link{mu.vd4.6p}}}
