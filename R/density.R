####################################################################################################
## Density functions

#' Density function for 3-param r, s, u
#' 
#' None
#' @param xx age
#' @param r r value
#' @param s s value
#' @param u u value
#' @return density
ft <- function(xx, r, s, u) {
  temp1 <- s^2 * xx + u^2
  temp2 <- u^2 * r + s^2
  if (xx==0) value = 0 
  else value <- temp2 / sqrt(2 * pi * temp1^3) * exp(-(1-r * t)^2/(2 * temp1))
  return(value)
}


#' Vectorized density function
#' 
#' None
#' 
#' @param xx vector of ages
#' @param r r value
#' @param s s value
#' @param u u value
#' @return vector of densities
vft <- Vectorize(FUN = ft, vectorize.args = "xx")
