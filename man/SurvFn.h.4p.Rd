\name{SurvFn.h.4p}
\alias{SurvFn.h.4p}
\title{Intrinsic cumulative survival distribution for 4 parameter model}
\usage{
  SurvFn.h.4p(xx, r, s, u)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}

  \item{u}{u value}
}
\value{
  intrinsic cumulative survival distribution
}
\description{
  Gives the intrinsic cumulative survival distribution at \code{xx}.
}
\details{For use in \code{\link{vitality.4p}}.}
\seealso{\code{\link{vitality.4p}}}