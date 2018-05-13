\name{vitality.6p}
\alias{vitality.6p}
\title{Fitting routine for the 2-process, 6-parameter vitality model (with childhood hook).}
\usage{
  vitality.6p(time = 0:(length(sdata)-1), sdata, init.params = FALSE, 
    lower = c(0, 0, 0, 0, 0, 0),upper = c(100,50,100,50,50,10), rc.data = FALSE, 
    se = FALSE, datatype = c("CUM", "INC"), ttol = 1e-06, pplot = TRUE,
    Iplot = FALSE, Mplot = FALSE, tlab = "years", silent = FALSE)
}
\arguments{
  \item{time}{Vector. Time component of data: Defaults to
  \code{0:(length(sdata)-1)}. Typically this refers to ages.}

  \item{sdata}{Required. Survival or mortality data.  The
  default expects cumulative survival fraction.  If
  providing incremental mortality fraction instead, use
  option: datatype = "INC". The default also expects the data to represent full mortality. 
  Otherwise, use option: rc.data = T to indicate right censored data.}

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
  param values.  specify \code{init.params =} \code{c(r, s,
  lambda, beta, gamma, alpha)} in that order (e.g.. init.params = c(.1,
  .02, .3, 0.12, 0.1, 1)).}
  
  \item{lower}{vector of lower parameter bounds in order of \code{c(r, s,
  lambda, beta, gamma, alpha)}. see \code{\link{nlminb}}}
  
  \item{upper}{vector of upper parameter bounds in order of \code{c(r, s,
  lambda, beta, gamma, alpha)}. see \code{\link{nlminb}}}

  \item{pplot}{Optional, Boolean. Plots of cumulative
  survival for both data and fitted curves?  Default:
  \code{TRUE}. \code{FALSE} produces no plots. A third
  option: \code{pplot = n} (n >= 1) extends the time axis
  of the fitting plots (beyond the max time in data).  For
  example: \code{pplot = 1.2} extends the time axis by 20%.
  Note: the incremental mortality plot is a continuous
  representation of the appropriately binned histogram of
  incremental mortalities.}

  \item{Iplot}{Optional, Boolean. Incremental mortality for
  both data and fitted curves?  Default: \code{FALSE}.}

  \item{Mplot}{Optional, Boolean. Plot fitted total, intrinsic, extrinsic (child), and extrinsic (adult) mortality
  curves? Default: \code{FALSE}. If \code{TRUE} 'observed' mortality rates for plotting are approximate and estimated from the input survival data. Depending on time (ages) and the change in mortality over age, the approximated, plotted mortality rates may be inaccurate (e.g. For human mortality data, mortality rates at the oldest ages are underestimated because nax is assumed to be half the length of the age interval but the true nax is likely larger). This caveat applies only to the plotted mortality rates (black circles in plot). The plotted, fitted lines are calculated with the parameter estimates themselves and are unaffected by the estimation of the mortality rates for this plot. \code{pplot} must be set to TRUE and \code{Iplot=FALSE}.}

  \item{tlab}{Optional, character. specifies units for
  x-axis of plots.  Default is "years".}

  \item{silent}{Optional, Boolean. Stops all print and plot
  options (still get most warning and all error messages)
  Default is \code{FALSE}.  A third option,
  \code{"verbose"} also enables the trace setting in the ms
  (minimum sum) S-Plus routine.}
}
\value{
  vector of final MLE r, s, lambda, beta, gamma and
  alpha parameter estimates.  standard errors of MLE
  parameter estimates (if se = <population> is specified).
}
\description{
This function provides the fitting routine for the 6-parameter 2-process vitality model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate. Adult extrinsic mortality is characterized by random challenges of frequency (lambda) and random magnitude (beta) exceeding the remaining average vitality. Child extrinsic mortality is characterized by childhood challenges of frequency (gamma) exceeding childhood vitality development rate (alpha). Model is appropriate to full lifespan of human mortality data (e.g. Anderson and Li 2015).
}
\references{
  D.H. Salinger, J.J. Anderson and O. Hamel (2003). "A parameter fitting routine for the
  vitality based survival model." Ecological Modeling
  166(3): 287--294.
  
  Anderson, J.J. and T. Li. (2015). "A two-process mortality model with extensions 
  to juvenile mortality, populations and evolution." Population Association of America Annual Meeting 2015 \url{http://paa2015.princeton.edu/abstracts/153144}
}

\examples{
data(swedish_females)
swe <- swedish_females
initial_age <- 0 
time <- swedish_females$age
survival_fraction <- swe$lx / swe$lx[1]
sample_size <- swe$Lx[1] #sample size

results.6par <- vitality.6p(time = time,
                            sdata = survival_fraction,
                            #init.params=FALSE,
                            init.params=c(0.012, 0.01, 0.1, 0.1, 0.1, 1),
                            lower = c(0, 0, 0, 0, 0, 0), upper = c(100,50,1,50,50,50),
                            rc.data = TRUE, 
                            se=FALSE,
                            #se = sample_size, 
                            datatype = "CUM", 
                            ttol = 1e-06,
                            pplot = TRUE,
                            Iplot = TRUE,
                            Mplot = TRUE,
                            tlab = "years",
                            silent = FALSE)
}
