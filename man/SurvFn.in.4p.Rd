\name{SurvFn.in.4p}
\alias{SurvFn.in.4p}
\title{The intrinsic cumulative survival distribution function for 2-process 4-parameter vitality model}
\usage{
  SurvFn.in.4p(xx, r, s)
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
  Gives the cumulative survival proportions at \code{xx} from intrinsic process for a given set of parameter values.
}
\details{Used within \code{\link{vitality.4p}} for estimating model parameters based on the observed cumulative survival function.}
\seealso{\code{\link{vitality.4p}}, \code{\link{survProbInc.4p}}, \code{\link{SurvFn.4p}}, , \code{\link{SurvFn.ex.4p}}}

