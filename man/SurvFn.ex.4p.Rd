\name{SurvFn.ex.4p}
\alias{SurvFn.ex.4p}
\title{The extrinsic cumulative survival distribution function for 2-process 4-parameter vitality model}
\usage{
  SurvFn.ex.4p(xx, r, s, lambda, beta)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}

  \item{lambda}{lambda value}

  \item{beta}{beta value}
}
\value{
  vector of cumulative survival proportions at \code{xx} from extrinsic process
}
\description{
  Gives the cumulative survival proportions at \code{xx} from extrinsic process for a given set of parameter values.
}
\details{Used within \code{\link{vitality.4p}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.4p}}, \code{\link{survProbInc.4p}}, \code{\link{SurvFn.4p}}, \code{\link{SurvFn.in.4p}}}

