\name{dataPrep}
\alias{dataPrep}
\title{Function for data preparation}
\usage{
  dataPrep(time, sdata, datatype, rc.data,
    returnMatrix = FALSE)
}
\arguments{
  \item{time}{A vector of observation dates}

  \item{sdata}{A vector of survival data of the same length
  as \code{time}}

  \item{datatype}{either \code{"CUM"} for cumulative or
  \code{"INC"} for incremental}

  \item{rc.data}{Boolean. Is data right-censored?}

  \item{returnMatrix}{Boolean. False returns a data frame,
  true returns a matrix. (as in the original), if "matrix"
  returns a matrix instead, with the "rc.data" column being
  0 for FALSE, 1 for TRUE, or 2 for TF}
}
\value{
  Returns a data.frame or matrix with columns time, sfract,
  x1, x2, Ni (incremental survival fraction), rc.data.
}
\description{
  Function to deal with NAs, right truncated data and
  datatype cumulative survival or incremental motality.
}

