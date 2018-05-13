\name{logLikelihood.6p}
\alias{logLikelihood.6p}
\title{Log likelihood of 2-process 6-parameter vitality model}
\usage{
  logLikelihood.6p(par, xx1, xx2, NNi)
}
\arguments{
  \item{par}{vector of parameters (r, s, lambda, beta, alpha, gamma)}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}

  \item{NNi}{survival fractions}
}
\value{
  log likelihood
}
\description{
  Gives the log likelihood of 6-parameter vitality model.
}
\details{For use in \code{\link{vitality.6p}}.}
\seealso{\code{\link{vitality.6p}}}

