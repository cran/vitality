\name{logLikelihood.k}
\alias{logLikelihood.k}
\title{Log likelihood of 3-parameter (r,s,k) model}
\usage{
  logLikelihood.k(par, xx1, xx2, NNi)
}
\arguments{
  \item{par}{vector of parameters (r, s, k)}

  \item{xx1}{xx1 vector}

  \item{xx2}{xx2 vector}

  \item{NNi}{survival fractions}
}
\value{
  log likelihood
}
\description{
  Gives the log likelihood of 3-parameter vitality model.
}
\details{For use in \code{\link{vitality.k}}.}
\seealso{\code{\link{vitality.k}}}

