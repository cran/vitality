\name{stdErr.ku}
\alias{stdErr.ku}
\title{Standard errors for 4-parameters: r, s, k, u}
\usage{
  stdErr.ku(r, s, k, u, x1, x2, Ni, pop)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}
  
  \item{u}{u value}

  \item{x1}{age 1 (corresponding 1:(t-1))}

  \item{x2}{age 2 (corresponding 2:t}

  \item{Ni}{age-specific survival fractions}

  \item{pop}{initial population (total population of the
  study)}
}
\value{
  standard error for r, s, k, u.
}
\description{
  Gives the standard errors for the 4 parameter model. Primarily used within \code{\link{vitality.ku}}.
}
\note{k is restricted to be >0.}
\seealso{\code{\link{vitality.ku}}}

