\name{SurvFn.in.6p}
\alias{SurvFn.in.6p}
\title{The intrinsic cumulative survival distribution function for 2-process 6-parameter vitality model}
\usage{
  SurvFn.in.6p(xx, r, s)
}
\arguments{
  \item{xx}{vector of ages}

  \item{r}{r value}

  \item{s}{s value}
}
\value{
  vector of cumulative survival proportions at \code{xx} from intrinsic process
}
\description{
  Gives the cumulative survival proportions at \code{xx} from the intrinsic process for a given set of parameter values.
}
\details{Used within \code{\link{vitality.6p}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.6p}}, \code{\link{survProbInc.6p}}, \code{\link{SurvFn.6p}}}

