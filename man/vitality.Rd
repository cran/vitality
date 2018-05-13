\docType{package}
\name{vitality}
\alias{vitality}
\alias{vitality-package}
\title{Fitting routines for the Vitality family of mortality models.}
\description{
This package provides support for fitting the vitality family of mortality models that characterize mortality in terms of the loss vitality, an abstract measure of survival capacity. Mortality occurs by two processes. Intrinsic mortality occurs when vitality is depleted by stochastic losses. Extrinsic mortality occurs when a random external challenge exceeds the available vitality. The package contains four model versions:

\itemize{
\item{\code{\link{vitality.k}} is a 3-parameter model. Intrinsic mortality is characterized by the mean (r) and variability (s) in vitality loss rate. Extrinsic mortality is characterized by the frequency (k) of lethal random challenges. Model is appropriate to animal mortality data (e.g. Anderson 2000).}
\item{\code{\link{vitality.ku}} is a 4-parameter model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate and the standard deviation of initial vitality (u). Extrinsic mortality is characterized by the frequency (k) of lethal random challenges. Model is appropriate to animal mortality data (e.g. Li and Anderson 2009).}
\item{\code{\link{vitality.4p}} is a 4-parameter model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate. Extrinsic mortality is characterized by random challenges of frequency (lambda) and random magnitude (beta) exceeding the remaining average vitality. Model is appropriate to adult human mortality data (e.g. Li and Anderson 2013).}
\item{\code{\link{vitality.6p}} is a 6-parameter model. Intrinsic mortality is characterized by the mean (r) and variability (s) in the vitality loss rate. Adult extrinsic mortality is characterized by random challenges of frequency (lambda) and random magnitude (beta) exceeding the remaining average vitality. Child extrinsic mortality is characterized by childhood challenges of frequency (gamma) exceeding childhood vitality development rate (alpha). Model is appropriate to full lifespan of human mortality data (e.g. Anderson and Li 2015).}
}

Model parameters are estimated on survival or mortality rate data using maximum log likelihood methods based on Salinger et al. (2003). 

Version 1.1 adds the versions \code{\link{vitality.k}}, \code{\link{vitality.ku}} and \code{\link{vitality.6p}} to the original code for the \code{\link{vitality.4p}} model previously designated vitality.2ps.

Version 1.2 makes previously invisible functions to produce the survival and mortality rate functions from a given set of parameters (e.g. \code{\link{SurvFn.4p}}, \code{\link{SurvFn.6p}}, \code{\link{mu.vd.4p}}, \code{\link{mu.vd.6p}}) usable. The child mortality rate formula in \code{\link{mu.vd.6p}}, \code{\link{mu.vd2.6p}}, and \code{\link{mu.vd4.6p}} has been updated to improve fit.
}
\details{
Support for package development was provided by the National Institute of Ageing Grant 1R21AG046760-01, the Bonneville Power Administration, and the University of Washington Center for Statistics and the Social Sciences and Center for Studies in Demography and Ecology.
}
\examples{
# vitality.k
data(daphnia)
time <- daphnia$days
survival_fraction <- daphnia$lx

results.modk <- vitality.k(time = time,
                           sdata = survival_fraction,
                           rc.data=TRUE, 
                           se=FALSE,
                           gfit=FALSE, 
                           datatype="CUM", 
                           ttol=.000001, 
                           init.params=FALSE, 
                           lower=c(0,-1,0), upper=c(100,50,50), 
                           pplot=TRUE, 
                           tlab="days", 
                           lplot=TRUE, 
                           cplot=TRUE, 
                           Iplot=TRUE, 
                           silent=TRUE)

# vitality.ku
data(rainbow_trout_for_k)
time <- rainbow_trout_for_k$days
survival_fraction <- rainbow_trout_for_k$survival

results.modku <- vitality.ku(time = time,
                             sdata = survival_fraction,
                             rc.data=TRUE,
                             se=FALSE,
                             gfit=FALSE,
                             datatype="CUM",
                             ttol=.000001,
                             init.params=FALSE,
                             lower=c(0,-1,0,0),upper=c(100,100,50,50),
                             pplot=TRUE,
                             tlab="days",
                             lplot=TRUE,
                             cplot=TRUE,
                             Iplot=TRUE,
                             silent=TRUE,
                             L=0)
# vitality.4p
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

# vitality.6p
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
\references{
  \itemize{
  \item Anderson, J.J. (2000). "A vitality-based model relating stressors and environmental properties to organism survival." Ecological Monographs 70(3):445-470.
  
  \item Anderson, J.J. and Li, T. (2015). "A two-process mortality model with extensions to juvenile mortality, populations and evolution." Population Association of America Annual Meeting 2015 \url{http://paa2015.princeton.edu/abstracts/153144}
    
  \item Li, T. and J.J. Anderson. (2009).  "The
  vitality model: A way to understand population survival
  and demographic heterogeneity."  Theoretical Population
  Biology 76: 118-131.
  
  \item Li, T. and J.J. Anderson (2013). "Shaping human mortality patterns through intrinsic and extrinsiv vitality processes."  Demographic Research 28(12): 341-372.
  
  \item Salinger, D.H., J.J. Anderson, and O.S. Hamel.
  (2003).  "A parameter estimation routine for the
  vitality-based survival model."  Ecological Modelling 166
  (3): 287-29

  }
}

