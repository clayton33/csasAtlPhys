% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gridMask.R
\name{makeGridMask}
\alias{makeGridMask}
\title{Create a grid mask}
\usage{
makeGridMask(
  gridLongitudeOriginal,
  gridLatitudeOriginal,
  gridPtLongitude,
  gridPtLatitude,
  gridDiffLongitude,
  gridDiffLatitude,
  useCtd = FALSE,
  ctdLongitude = NULL,
  ctdLatitude = NULL,
  ctd = NULL,
  ctdDistance = 30
)
}
\arguments{
\item{gridLongitudeOriginal}{a numerical vector of the unique grid longitude values}

\item{gridLatitudeOriginal}{a numerical vector of the unique grid latitude values}

\item{gridPtLongitude}{a numerical vector of the grid point longitude values}

\item{gridPtLatitude}{a numerical vector of the grid point latitude values}

\item{gridDiffLongitude}{a numerical value indicating the delta value between \code{gridLongitude}}

\item{gridDiffLatitude}{a numerical value indicating the delta value between \code{gridLatitude}}

\item{useCtd}{a logical value indicating whether or not to use ctd data when making the mask, default is \code{FALSE}.}

\item{ctdLongitude}{a numerical vector of the ctd longitude, ignored if \code{useCTD = FALSE}, default is \code{NULL}.}

\item{ctdLatitude}{a numerical vector of the ctd latitude, ignored if \code{useCTD = FALSE}, default is \code{NULL}.}

\item{ctd}{a list of ctd objects, default is \code{NULL}}

\item{ctdDistance}{a numerical value indicating the minimum distance, in kilometers, that grid points should
be to a ctd profile, default is \code{30}.}
}
\value{
a list containing the gridmask, which is a vector of TRUE/FALSE values,the expanded grid,
and the gridLongitude and gridLatitude
}
\description{
This function takes supplied longitude, latitude along with the grid spacing, and creates
an equally spaced rectangular grid mask.
}
\author{
Chantelle Layton
}
