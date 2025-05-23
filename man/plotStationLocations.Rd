% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plotStationLocations}
\alias{plotStationLocations}
\title{Plot station locations}
\usage{
plotStationLocations(
  distance,
  plabel,
  distanceOffset = NULL,
  cex = 9/10,
  pch = 25,
  col = "black"
)
}
\arguments{
\item{distance}{a numerical vector indicating the distance}

\item{plabel}{a numerical vector indication the placement of the labels}

\item{distanceOffset}{a numerical value indicating the value to add and subtract from distace that will plot a line, default value is NULL}

\item{cex}{a numerical value indicating the magnification}

\item{pch}{a numerical value indicating the symbol}

\item{col}{a character string indicating the color}
}
\description{
Plots station locations at the top of plots using the points function
}
\author{
Chantelle Layton
}
