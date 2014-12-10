\name{vitality.k}
\alias{vitality.k}
\title{Fitting routine for the 3-parameter vitality model.}
\usage{
vitality.k(time, sdata, rc.data=F, se=F, gfit=F, datatype="CUM", ttol=.000001, 
    init.params=F, lower=c(0,-1,0), upper=c(100,50,50), pplot=T, tlab="days", 
    lplot=F, cplot=F, Iplot=F, silent=F)
}
\arguments{
  \item{time}{Vector. Time component of data: Defaults to
  \code{0:(length(sdata)-1)}. Typically this refers to ages.}

  \item{sdata}{Required. Survival or mortality data.  The
  default expects cumulative survival fraction.  If
  providing incremental mortality fraction instead, use
  option: datatype = "INC". The default also expects the data to represent full mortality. 
  Otherwise, use option: rc.data = T to indicate right censored data.}

  \item{rc.data}{Optional, boolean. Specifies Right
  Censored data.  If the data does not represent full
  mortality, it is probably right censored.  The default is
  rc.data = F.  A third option is rc.data = "TF".  Use this
  case to add a near-term zero survival point to data which
  displays nearly full mortality ( <.01 survival at end).
  If rc.data = F but the data does not show full mortality,
  rc.data = "TF" will be invoked automatically.}

  \item{se}{Optional, boolean. Calculates the standard
  errors for the MLE parameters.  Default is FALSE. Set
  equal to the initial study population to compute standard
  errors.}

  \item{datatype}{Optional. Defaults to \code{"CUM"} for
  cumulative survival fraction data.  Use \code{"INC"} -
  for incremental mortality fraction data.}

  \item{ttol}{Optional. Stopping criteria tolerance.
  Default is 1e-6.  Specify as ttol = .0001. If one of the
  liklihood plots (esp. for "k") does not look optimal, try
  decreasing ttol.  If the program crashes, try increasing
  ttol.}

  \item{init.params}{Optional. Please specify the initial
  param values.  specify \code{init.params =} \code{c(r, s,
  k)} in that order (eg. init.params = c(.1,
  .02, .3)).}

  \item{lower}{vector of lower paramter bounds in order of \code{c(r, s,
  k)}. see \code{\link{nlminb}}}
  
  \item{upper}{vector of upper paramter bounds in order of \code{c(r, s,
  k)}. see \code{\link{nlminb}}}

  \item{pplot}{Optional, boolean. Plots of cumulative
  survival for both data and fitted curves?  Default:
  \code{TRUE}. \code{FALSE} produces no plots.Note: the incremental mortality plot is a continuous
  representation of the appropriately binned histogram of
  incremental mortalities.}

  \item{Iplot}{Boolean. Plot incremental survival? Must have \code{pplot=TRUE}}

  \item{lplot}{Boolean. Plot likelihood functions? Provides likelihood function plotting. Defaults to \code{FALSE}. Note:  these plots are not "likelihood profiles" in that while one parameter is varied, the others are held fixed, rather than re-optimized. (must also have \code{pplot=T} and \code{Iplot=F}.)}
  
  \item{cplot}{Boolean. Plot likelihood contour plot? Provides a likelihood contour plot for a range of r and s values (can be slow so default is \code{FALSE}). Must also have \code{lplot=T} and \code{pplot=T} to get contour plots.}

  \item{tlab}{Character, label for time axis. Defaults to "days".}
  
  \item{gfit}{Provides a Pearson C type test for goodness of fit. Default is \code{FALSE}. Must provide the initial study population to compute goodness of fit.}

  \item{silent}{Optional, boolean. Stops all print and plot
  options (still get most warning and all error messages)
  Default is \code{FALSE}.  A third option, \code{"verbose"} 
  also enables the trace setting in the ms
  (minimum sum) S-Plus routine.}
}
\value{
  vector of final MLE r, s, k parameter estimates.  standard errors of MLE
  parameter estimates (if se = <population> is specified).
}
\description{
This function provides the fitting routine for the 3-parameter vitality model. Intrinsic mortality is characterized by the mean (r) and variability (s) in vitality loss rate. Extrinsic mortality is characterized by the frequency (k) of lethal random challenges. Model is appropriate to animal mortality data (e.g. Anderson 2000).
}
\references{
  \itemize{
  \item Anderson, J.J. (2000). "A vitality-based model relating stressors and environmental properties to organism survival." Ecological Monographs 70(3):445-470.
  }
}

