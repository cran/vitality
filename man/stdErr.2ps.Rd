\name{stdErr.2ps}
\alias{stdErr.2ps}
\title{Standard errors for 4-param r, s, lambda, beta}
\usage{
  stdErr.2ps(r, s, k, u, x1, x2, Ni, pop)
}
\arguments{
  \item{r}{r value}

  \item{s}{s value}

  \item{k}{k value}

  \item{u}{u value corresponding to beta?}

  \item{x1}{age 1 (corresponding 1:(t-1) and 2:t)}

  \item{x2}{age 2}

  \item{Ni}{survival fraction}

  \item{pop}{initial population (total population of the
  study)}
}
\value{
  standard error for r, s, k, u.
}
\description{
  Note: if k <= 0, can not find std Err for k.
}

