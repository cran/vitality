\name{plotting.4p}
\alias{plotting.4p}
\title{Plotting function for 2-process 4-parameter vitality model}
\usage{
  plotting.4p(r.final, s.final, lambda.final, beta.final,
    mlv, time, sfract, x1, x2, Ni, pplot, Iplot, Mplot,
    tlab, rc.data)
}
\arguments{
  \item{r.final}{r estimate}

  \item{s.final}{s estimate}

  \item{lambda.final}{lambda estimate}

  \item{beta.final}{beta estimate}

  \item{mlv}{TODO mlv}

  \item{time}{time vector}

  \item{sfract}{survival fraction}

  \item{x1}{Time 1}

  \item{x2}{Time 2}

  \item{Ni}{Initial population}

  \item{pplot}{Boolean. Plot cumulative survival fraction?}

  \item{Iplot}{Boolean. Plot incremental survival?}

  \item{Mplot}{Boolean. Plot mortality rate? Note: plotted mortality data is derived from survival data and is approximate}

  \item{tlab}{Character, label for time axis}

  \item{rc.data}{Boolean, right-censored data?}
}
\description{
  This function plots the estimated results from the 4 parameter vitality model. It is used within the function \code{\link{vitality.4p}}. 
}
\details{See \code{\link{vitality.4p}} for further description of function arguments.

Only one of \code{Iplot} or \code{Mplot} should be set to \code{TRUE} at once.}
\seealso{\code{\link{vitality.4p}}, \code{\link{mu.vd.4p}}, \code{\link{mu.vd1.4p}}, \code{\link{mu.vd2.4p}}}

