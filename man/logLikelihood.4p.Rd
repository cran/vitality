\name{logLikelihood.4p}
\alias{logLikelihood.4p}
\title{Log likelihood of 2-process 4-parameter model}
\usage{
  logLikelihood.4p(par, xx1, xx2, NNi)
}
\arguments{
  \item{par}{vector of parameter(r, s, lambda, beta)}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}

  \item{NNi}{survival fractions}
}
\value{
  log likelihood
}
\description{
  Gives the log likelihood of 2-process 6 parameter vitality model.
}
\details{For use in \code{\link{vitality.4p}}.}
\seealso{\code{\link{logLikelihood.6p}}}
