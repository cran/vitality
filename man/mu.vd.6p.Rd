\name{mu.vd.6p}
\alias{mu.vd.6p}
\title{Total mortality rate for the 2-process 6-parameter vitality model}
\usage{
  mu.vd.6p(t, r, s, lambda, beta, gamma, alpha)
}
\arguments{
  \item{t}{age}

  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}

  \item{gamma}{gamma value}

  \item{alpha}{alpha value}
}
\value{
  Total age-specific mortality rates
}
\description{
  Gives the total age-specific mortality rates for a given set of the six parameters. See \code{\link{mu.vd1.6p}} for calculation of intrinsic age-specific mortality rates. See \code{\link{mu.vd2.6p}} for calculation of extrinsic age-specific mortality rates.
}
\seealso{\code{\link{mu.vd1.6p}}, \code{\link{mu.vd2.6p}}}

