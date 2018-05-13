\name{survProbInc.4p}
\alias{survProbInc.4p}
\title{Incremental survival probability for 2-process 4-parameter model}
\usage{
  survProbInc.4p(r, s, lambda, beta, xx1, xx2)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}
}
\value{
  incremental survival probabilities
}
\description{
  Calculates the incremental survival probabilities (between xx1 and xx2) for 2-process 4-parameter model.
}
\details{For use in \code{\link{vitality.4p}}.}
\seealso{\code{\link{vitality.4p}}, \code{\link{logLikelihood.4p}}}

