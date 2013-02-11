\name{vitality.2ps}
\alias{vitality.2ps}
\title{Fitting routine for the 2-process, 4-parameter vitality model (no childhood hook).}
\usage{
  vitality.2ps(time = 0:(1 - length(sdata)), sdata,
    init.params = FALSE, rc.data = FALSE, se = FALSE,
    datatype = c("CUM", "INC"), ttol = 1e-06, pplot = TRUE,
    Iplot = FALSE, Mplot = FALSE, tlab = "years",
    silent = FALSE)
}
\arguments{
  \item{time}{Vector. Time component of data: Defaults to
  \code{0:(1-length(sdata))}.}

  \item{sdata}{Required. Survival or mortality data.  The
  default expects cumulative survival fraction.  If
  providing incremental mortality fraction instead, use
  option: datatype = "INC".  The default also expects the
  data to represent full mortality.  Otherwise, use option:
  rc.data = T to indicate right censored data.}

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
  param values.  specify \code{init.params = c(r, s,
  lambda, beta)} in that order (eg. init.params = c(.1,
  .02, .3, 0.12)).}

  \item{pplot}{Optional, boolean. Plots of cumulative
  survival for both data and fitted curves?  Default
  \code{TRUE}. \code{FALSE} Produce no plots. A A third
  option: \code{pplot = n} (n >= 1) extends the time axis
  of the fitting plots (beyond the max time in data).  For
  example: \code{pplot = 1.2} extends the time axis by 20%.
  Note: the incremental mortality plot is a continuous
  representation of the appropriately- binned histogram of
  incremental mortalities.}

  \item{Iplot}{Optional, boolean. Incremental mortality for
  both data and fitted curves?  Default: \code{FALSE}.}

  \item{Mplot}{Optional, boolean. Plot fitted mortality
  curve? Default is \code{FALSE}.}

  \item{tlab}{Optional, character. specifies units for
  x-axis of plots.  Default is "days".}

  \item{silent}{Optional, boolean. Stops all print and plot
  options (still get most warning and all error messages)
  Default is \code{FALSE}.  A third option,
  \code{"verbose"} also enables the trace setting in the ms
  (minimum sum) S-Plus routine.}
}
\value{
  vector of final MLE r, s, lambda, beta w/wo gamma and
  alpha parameter estimates.  standard errors of MLE
  parameter estimates (if se = <population> is specified).
}
\description{
  Based on code by D.H. Salinger, J.J. Anderson and O.
  Hamel (2003). "A parameter fitting routine for the
  vitality based survival model." Ecological Modeling
  166(3): 287--294.
}

