\name{survProbInc.k}
\alias{survProbInc.k}
\title{Incremental survival probability for 3-parameter model}
\usage{
  survProbInc.k(r, s, k, xx1, xx2)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}
}
\value{
  incremental survival probabilities
}
\description{
  Calculates the incremental survival probabilities (between xx1 and xx2) for 3-parameter model.
}
\details{For use in \code{\link{vitality.k}}.}
\seealso{\code{\link{vitality.k}}, \code{\link{logLikelihood.k}}}

