\name{mu.vd.4p}
\alias{mu.vd.4p}
\title{Total mortality rate for the 2-process 4-parameter vitality model}
\usage{
  mu.vd.4p(t, r, s, lambda, beta)
}
\arguments{
  \item{t}{age}

  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}
}
\value{
  Total age-specific mortality rates
}
\description{
  Gives the total age-specific mortality rates for a given set of the four parameters. See \code{\link{mu.vd1.4p}} for calculation of intrinsic age-specific mortality rates. See \code{\link{mu.vd2.4p}} for calculation of extrinsic age-specific mortality rates.
}
\seealso{\code{\link{mu.vd1.4p}}, \code{\link{mu.vd2.4p}}}

