\name{stdErr.6p}
\alias{stdErr.6p}
\title{Standard errors for 6-parameters: r, s, lambda, beta, gamma, alpha}
\usage{
  stdErr.6p(r, s, k, u, g, a, x1, x2, Ni, pop)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{lambda value}

  \item{u}{beta value}
  
  \item{g}{gamma value}
  
  \item{a}{alpha value}

  \item{x1}{age 1 (corresponding 1:(t-1))}

  \item{x2}{age 2 (corresponding 2:t}

  \item{Ni}{age-specific survival fractions}

  \item{pop}{initial population (total population of the
  study)}
}
\value{
  standard error for r, s, lambda, beta, gamma, and alpha.
}
\description{
  Gives the standard errors for the 6 parameter model. Primarily used within \code{\link{vitality.6p}}.
}
\note{if k <= 0, cannot find standard error for k}
\seealso{\code{\link{vitality.6p}}}

