\name{logLikelihood.ku}
\alias{logLikelihood.ku}
\title{Log likelihood of 4-parameter (r,s,k,u) model}
\usage{
  logLikelihood.ku(par, xx1, xx2, NNi)
}
\arguments{
  \item{par}{vector of parameters (r, s, k, u)}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}

  \item{NNi}{survival fractions}
}
\value{
  log likelihood
}
\description{
  Gives the log likelihood of 4-parameter vitality model.
}
\details{For use in \code{\link{vitality.ku}}.}
\seealso{\code{\link{vitality.ku}}}

