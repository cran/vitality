\name{vitality.4p}
\alias{vitality.4p}
\title{Fitting routine for the 2-process, 4-parameter vitality model (no childhood hook).}
\usage{
  vitality.4p(time = 0:(length(sdata)-1), sdata, init.params = FALSE, 
    lower = c(0, 0, 0, 0), upper = c(100,50,100,50),rc.data = FALSE, 
    se = FALSE, datatype = c("CUM", "INC"), ttol = 1e-06, pplot = TRUE,
    Iplot = FALSE, Mplot = FALSE, tlab = "years", silent = FALSE)
}
\arguments{
  \item{time}{Vector. Time component of data: Defaults to
  \code{0:(length(sdata)-1)}.}

  \item{sdata}{Required. Survival or mortality data.  The
  default expects cumulative survival fraction.  If
  providing incremental mortality fraction instead, use
  option: datatype = "INC".  The default also expects the
  data to represent full mortality.  Otherwise, use option:
  rc.data = T to indicate right censored data. If first element of \code{sdata}
  <1 data will be rescaled.}

  \item{rc.data}{Optional, Boolean. Specifies Right
  Censored data.  If the data does not represent full
  mortality, it is probably right censored.  The default is
  rc.data = F.  A third option is rc.data = "TF".  Use this
  case to add a near-term zero survival point to data which
  displays nearly full mortality ( <.01 survival at end).
  If rc.data = F but the data does not show full mortality,
  rc.data = "TF" will be invoked automatically.}

  \item{se}{Optional, Boolean. Calculates the standard
  errors for the MLE parameters.  Default is FALSE. Set
  equal to the initial study population to compute standard
  errors.}

  \item{datatype}{Optional. Defaults to \code{"CUM"} for
  cumulative survival fraction data.  Use \code{"INC"} -
  for incremental mortality fraction data.}

  \item{ttol}{Optional. Stopping criteria tolerance.
  Default is 1e-6.  Specify as ttol = .0001. If one of the
  likelihood plots (esp. for "k") does not look optimal, try
  decreasing ttol.  If the program crashes, try increasing
  ttol.}

  \item{init.params}{Optional. Please specify the initial
  param values.  
  specify \code{init.params =} \code{c(r, s, lambda, beta)} in that order (e.g.. init.params = c(.1,
  .02, .3, 0.12)).}
  
  \item{lower}{vector of lower parameter bounds in order of \code{c(r, s,
  lambda, beta)}. see \code{\link{nlminb}}}
  
  \item{upper}{vector of upper parameter bounds in order of \code{c(r, s,
  lambda, beta)}. see \code{\link{nlminb}}}

  \item{pplot}{Optional, Boolean. Plots of cumulative
  survival for both data and fitted curves?  Default
  \code{TRUE}. \code{FALSE} Produce no plots. A A third
  option: \code{pplot = n} (n >= 1) extends the time axis
  of the fitting plots (beyond the max time in data).  For
  example: \code{pplot = 1.2} extends the time axis by 20%.
  Note: the incremental mortality plot is a continuous
  representation of the appropriately- binned histogram of
  incremental mortalities.}

  \item{Iplot}{Optional, Boolean. Incremental mortality for
  both data and fitted curves?  Default: \code{FALSE}.}

  \item{Mplot}{Optional, Boolean. Plot fitted mortality
  curve? Default is \code{FALSE}. If \code{TRUE} 'observed' mortality rates for plotting are approximate and estimated from the input survival data. Depending on time (ages) and the change in mortality over age, these may be unrealistic but the fitted lines are derived from the parameter estimates themselves. For human data, the approximated mortality rates diverge at ages >80. \code{pplot} must be set to TRUE and \code{Iplot=FALSE}.}

  \item{tlab}{Optional, character. specifies units for
  x-axis of plots.  Default is "days".}

  \item{silent}{Optional, Boolean. Stops all print and plot
  options (still get most warning and all error messages)
  Default is \code{FALSE}.  A third option,
  \code{"verbose"} also enables the trace setting in the ms
  (minimum sum) S-Plus routine.}
}
\value{
  vector of final MLE r, s, lambda, beta parameter estimates.
  standard errors of MLE parameter estimates (if se = <population> is specified).
}
\description{
This function provides the fitting routine for the 4-parameter 2-process vitality model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate. Extrinsic mortality is characterized by random challenges of frequency (lambda) and random magnitude (beta) exceeding the remaining average vitality. Model is appropriate to adult human mortality data (e.g. Li and Anderson 2013).
}
\references{
  D.H. Salinger, J.J. Anderson and O. Hamel (2003). "A parameter fitting routine for the
  vitality based survival model." Ecological Modeling
  166(3): 287--294.
  
  Li, T. and J.J. Anderson (2013).
  "Shaping human mortality patterns through intrinsic and
  extrinsic vitality processes."  Demographic Research 28(12): 341-372.
}

\examples{
data(swedish_females)
swe <- swedish_females
initial_age <- 20 # Could be adjusted
time <- initial_age:max(swedish_females$age)
survival_fraction <- swe$lx / swe$lx[1]
survival_fraction <- survival_fraction[time] # when first element <1 data is adjusted
sample_size <- swe$Lx[initial_age] #sample size

results.4par <- vitality.4p(time = time,
                            sdata = survival_fraction,
                            #init.params=FALSE,
                            init.params=c(0.012, 0.01, 0.1, 0.1),
                            lower = c(0, 0, 0, 0), upper = c(100,50,1,50),
                            rc.data = TRUE, 
                            se = sample_size, 
                            datatype = "CUM", 
                            ttol = 1e-06,
                            pplot = TRUE,
                            Iplot = TRUE,
                            Mplot = TRUE,
                            tlab = "years",
                            silent = FALSE)
}

