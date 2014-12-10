\name{SurvFn.ku}
\alias{SurvFn.ku}
\title{The cumulative survival distribution function for 4-parameter vitality model}
\usage{
  SurvFn.ku(xx, r, s, k, u)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}
  
  \item{u}{u value}

}
\value{
  cumulative survival proportions at \code{xx}  
}
\description{
  Gives the cumulative survival proportions at \code{xx} for a given set of parameter values.
}
\details{Used within \code{\link{vitality.ku}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.ku}}, \code{\link{survProbInc.ku}}}

