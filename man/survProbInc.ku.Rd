\name{survProbInc.ku}
\alias{survProbInc.ku}
\title{Incremental survival probability for 4-parameter model}
\usage{
  survProbInc.ku(r, s, k, u, xx1, xx2)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}
  
  \item{u}{u value}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}
}
\value{
  incremental survival probabilities
}
\description{
  Calculates the incremental survival probabilities (between xx1 and xx2) for 4-parameter model.
}
\details{For use in \code{\link{vitality.ku}}.}
\seealso{\code{\link{vitality.ku}}, \code{\link{logLikelihood.ku}}}

