\name{stdErr.4p}
\alias{stdErr.4p}
\title{Standard errors for 4-parameters: r, s, lambda, beta}
\usage{
  stdErr.4p(r, s, k, u, x1, x2, Ni, pop)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{lambda value}

  \item{u}{beta value}

  \item{x1}{age 1 (corresponding 1:(t-1) and 2:t)}

  \item{x2}{age 2}

  \item{Ni}{survival fraction}

  \item{pop}{initial population (total population of the
  study)}
}
\value{
  standard error for r, s, lambda, beta
}
\description{
  Gives the standard errors for the 4 parameter model. Primarily used within \code{\link{vitality.4p}}.
}
\note{if k <= 0, cannot find standard error for k}
\seealso{\code{\link{vitality.4p}}}

