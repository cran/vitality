\name{SurvFn.k}
\alias{SurvFn.k}
\title{The cumulative survival distribution function for 3-parameter vitality model}
\usage{
  SurvFn.k(xx, r, s, k)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}

}
\value{
  cumulative survival proportions at \code{xx}  
}
\description{
  Gives the cumulative survival proportions at \code{xx} for a given set of parameter values.
}
\details{Used within \code{\link{vitality.k}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.k}}, \code{\link{survProbInc.k}}}

