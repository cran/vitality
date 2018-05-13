\name{mu.vd2.6p}
\alias{mu.vd2.6p}
\title{Extrinsic mortality rate for the 2-process 6-parameter vitality model}
\usage{
  mu.vd2.6p(x, r, lambda, beta, gamma, alpha)
}
\arguments{
  \item{x}{age}

  \item{r}{r value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}

  \item{gamma}{gamma value}

  \item{alpha}{alpha value}
}
\value{
  Vector of extrinsic age-specific mortality rates at ages \code{x}
}
\description{
  Gives the extrinsic age-specific mortality rates for a given set of \code{r} and the extrinsic parameters.
}
\seealso{\code{\link{mu.vd.6p}}, \code{\link{mu.vd1.6p}},  \code{\link{mu.vd3.6p}},  \code{\link{mu.vd4.6p}}}
