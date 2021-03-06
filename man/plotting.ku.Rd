\name{plotting.ku}
\alias{plotting.ku}
\title{Plotting function for 4-parameter vitality model}
\usage{
  plotting.ku(r.final,s.final,k.final,u.final,mlv,time,sfract,x1,x2,Ni,
    pplot,tlab,lplot,cplot,Iplot,gfit)
}
\arguments{
  \item{r.final}{r estimate}

  \item{s.final}{s estimate}

  \item{k.final}{k estimate}
  
  \item{u.final}{u estimate}

  \item{mlv}{TODO mlv}

  \item{time}{time vector giving the ages at which calculate the estimated value}

  \item{sfract}{observed survival fraction}

  \item{x1}{Time 1}

  \item{x2}{Time 2}

  \item{Ni}{Initial population}

  \item{pplot}{Boolean. Plot cumulative survival fraction?}

  \item{Iplot}{Boolean. Plot incremental survival?}

  \item{lplot}{Boolean. Plot likelihood functions? Provides likelihood function plotting. Defaults to \code{FALSE}. Note:  these plots are not "likelihood profiles" in that while one parameter is varied, the others are held fixed, rather than re-optimized. (must also have pplot=T.)}
  
  \item{cplot}{Boolean. Plot likelihood contour plot? Provides a likelihood contour plot for a range of r and s values (can be slow so default is \code{FALSE}). Must also have \code{lplot=T} and \code{pplot=T} to get contour plots.}

  \item{tlab}{Character, label for time axis. Defaults to "days".}
  
  \item{gfit}{Provides a Pearson C type test for goodness of fit. Default is \code{FALSE}. Must provide the initial study population to compute goodness of fit.}

}
\description{
  This function plots the estimated results from the 4 parameter vitality model. It is used within the function \code{\link{vitality.ku}}. 
}
\details{See \code{\link{vitality.ku}} for further description of function arguments.}
\seealso{\code{\link{vitality.ku}}}

