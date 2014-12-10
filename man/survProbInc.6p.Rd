\name{survProbInc.6p}
\alias{survProbInc.6p}
\title{Incremental survival probability for 2-process 6-parameter model}
\usage{
  survProbInc.6p(r, s, lambda, beta, gamma, alpha, xx1, xx2)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}
  
  \item{gamma}{gamma value}
  
  \item{alpha}{alpha value}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}
}
\value{
  incremental survival probabilities
}
\description{
  Calculates the incremental survival probabilities (between xx1 and xx2) for 2-process 6-parameter model.
}
\details{For use in \code{\link{vitality.6p}}.}
\seealso{\code{\link{vitality.6p}}, \code{\link{logLikelihood.6p}}}

