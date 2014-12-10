\name{stdErr.k}
\alias{stdErr.k}
\title{Standard errors for 3-parameters: r, s, k}
\usage{
  stdErr.k(r, s, k, x1, x2, Ni, pop)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}

  \item{x1}{age 1 (corresponding 1:(t-1))}

  \item{x2}{age 2 (corresponding 2:t}

  \item{Ni}{age-specific survival fractions}

  \item{pop}{initial population (total population of the
  study)}
}
\value{
  standard error for r, s, k.
}
\description{
  Gives the standard errors for the 3 parameter model. Primarily used within \code{\link{vitality.k}}.
}
\note{k is restricted to be >0.}
\seealso{\code{\link{vitality.k}}}

