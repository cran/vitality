####################################################################################################
## Mortality Rate Functions

#' Total mortality rate
#' 
#' None
#' 
#' @param t age
#' @param r r value
#' @param s s value
#' @param lambda lambda value
#' @param beta beta value
#' @param gamma gamma value
#' @param alpha alpha value
#' @return Total mortality rate (?)
mu.vd <- function(t, r, s, lambda, beta, gamma, alpha){
  mu.vd1(t, r, s) + mu.vd2(t, r, lambda, beta, gamma, alpha)
}


#' Intrinsic mortality rate
#' 
#' None
#' 
#' @param x age
#' @param r r value
#' @param s s value
#' @return Intrinsic mortality rate (?)
mu.vd1 <- function(x, r, s) {
  vft(x, r, s, 0) / SurvFn.h(x, r, s, 0)
}


#' Intrinsic mortality rate
#' 
#' None
#' 
#' @param x age
#' @param r r value
#' @param lambda lambda value
#' @param beta beta value
#' @param gamma gamma value
#' @param alpha alpha value
#' @return Intrinsic mortality rate (?)
mu.vd2 <- function(x, r, lambda, beta, gamma, alpha){
  lambda * exp(-(1 - r * x) / beta) + gamma * exp(-1 / alpha * x)
}
