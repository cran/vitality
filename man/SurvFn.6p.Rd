\name{SurvFn.6p}
\alias{SurvFn.6p}
\title{The cumulative survival distribution function for 2-process 6-parameter vitality model}
\usage{
  SurvFn.6p(xx, r, s, lambda, beta, gamma, alpha)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}
  
  \item{gamma}{gamma value}

  \item{alpha}{alpha value}
}
\value{
  vector of cumulative survival proportions at \code{xx} from all processes
}
\description{
  Gives the cumulative survival proportions at \code{xx} from all processes for a given set of parameter values.
}
\details{Used within \code{\link{vitality.6p}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.6p}}, \code{\link{survProbInc.6p}}, \code{\link{SurvFn.in.6p}}, \code{\link{SurvFn.ex.6p}}}

