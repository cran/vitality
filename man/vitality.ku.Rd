\name{vitality.ku}
\alias{vitality.ku}
\title{Fitting routine for the 4-parameter vitality model.}
\usage{
vitality.ku(time, sdata, rc.data=F, se=F, gfit=F, datatype="CUM", ttol=.000001, 
  init.params=F, lower=c(0,-1,0,0), upper=c(100,100,50,50), pplot=T, tlab="days", 
  lplot=F, cplot=F, Iplot=F, silent=F, L=0)
}
\arguments{
  \item{time}{Vector. Time component of data. Typically this refers to ages.}

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
  k, u)} in that order (eg. init.params = c(.1,
  .02, .3, .25)).}
  
  \item{lower}{vector of lower paramter bounds in order of \code{c(r, s,
  k, u)}. see \code{\link{nlminb}}}
  
  \item{upper}{vector of upper paramter bounds in order of \code{c(r, s,
  k, u)}. see \code{\link{nlminb}}}

  \item{pplot}{Optional, boolean. Plots of cumulative
  survival for both data and fitted curves?  Default:
  \code{TRUE}. \code{FALSE} produces no plots. Note: the incremental mortality plot is a continuous
  representation of the appropriately binned histogram of
  incremental mortalities.}

  \item{Iplot}{Boolean. Plot incremental survival?}

  \item{lplot}{Boolean. Plot likelihood functions? Provides likelihood function plotting. Defaults to \code{FALSE}. Note:  these plots are not "likelihood profiles" in that while one parameter is varied, the others are held fixed, rather than re-optimized. (must also have pplot=T.)}
  
  \item{cplot}{Boolean. Plot likelihood contour plot? Provides a likelihood contour plot for a range of paramter values (can be slow so default is \code{FALSE}). Must also have \code{lplot=T} and \code{pplot=T} to get contour plots.}

  \item{tlab}{Character, label for time axis. Defaults to "days".}
  
  \item{gfit}{Provides a Pearson C type test for goodness of fit. Default is \code{FALSE}. Must provide the initial study population to compute goodness of fit.}

  \item{silent}{Optional, boolean. Stops all print and plot
  options (still get most warning and all error messages)
  Default is \code{FALSE}.}
  
  \item{L}{Number of times of running simulated annealing. Default is 0, use Newton-Ralphson method only.}
}
\value{
  vector of final MLE r, s, k, u parameter estimates.  standard errors of MLE
  parameter estimates (if se = <population> is specified).
}
\description{
This function provides the fitting routine for the 4-parameter vitality model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate and the standard deviation of initial vitality (u). Extrinsic mortality is characterized by the frequency (k) of lethal random challenges. Model is appropriate to animal mortality data (e.g. Li and Anderson 2009)
}
\references{
  \itemize{
  \item Li, T. and J.J. Anderson. (2009). "The vitality model: A way to understand population survival and demographic heterogeneity." Theoretical Population Biology 76: 118-131.
  }
}

